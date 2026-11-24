"""
experiment_30.py — 繰り返し信頼ゲーム実験（デュアルジャッジ版・30試行）
実験対象 : GPT-4o（30試行）/ Claude（30試行）を 15試行 × 2回 に分けて実行
評価モデル: Claude Haiku（主） + GPT-4o-mini（副）

使い方:
  python experiment_30.py        # 実行（途中で止まっても、再実行すれば続きから再開）
  python experiment_30.py test   # 動作確認：gpt-4o × cooperative × 1試行のみ

2回に分けて実行する方法:
  1回目: TRIALS_PRIMARY / TRIALS_SECONDARY を 15 にして実行 → 試行1〜15
  2回目: 両方を 30 に書き換えて実行             → 試行16〜30 だけを追加

出力: results_30.csv（既存の results.csv は上書きしない）
"""

import os
import csv
import json
import time
import re
import sys
from pathlib import Path

from dotenv import load_dotenv
import openai
import anthropic


# ─── 設定 ─────────────────────────────────────────────────────────────
load_dotenv(r"C:\trust_game\.env")

OPENAI_API_KEY    = os.getenv("OPENAI_API_KEY")
ANTHROPIC_API_KEY = os.getenv("ANTHROPIC_API_KEY")

OPENAI_MODEL      = "gpt-4o"
CLAUDE_MODEL      = "claude-haiku-4-5-20251001"

# ── ジャッジモデル ──
JUDGE_CLAUDE_MODEL = "claude-haiku-4-5-20251001"
JUDGE_GPT_MODEL    = "gpt-4o-mini"

# ★ 1回目は 15、2回目は 30 に書き換えて実行する
TRIALS_PRIMARY   = 30   # GPT-4o
TRIALS_SECONDARY = 30   # Claude

TOTAL_ROUNDS        = 15
BETRAY_ROUND        = 6
NORMAL_RETURN_RATE  = 0.5
ENDOWMENT           = 10
MULTIPLIER          = 3

RESULTS_DIR = Path(r"C:\trust_game\results")
RESULTS_DIR.mkdir(exist_ok=True)
RESULTS_FILE = RESULTS_DIR / "results_30.csv"   # 既存の results.csv を残すため別名で保存


# ─── ペルソナ別システムプロンプト ──────────────────────────────────────
_RULES = """【ゲームのルール】
・あなたは「送り手」として参加します
・毎ラウンド、あなたは10円の中から好きな金額を相手に送金できます
・送金した金額は3倍になって相手に届きます
・相手はその金額の中から好きな額をあなたに返金します
・これを全15ラウンド繰り返します
・毎ラウンド、まず内面（信念・願望・意図）を記述してから送金額を決定してください"""

SYSTEM_PROMPTS = {
    "cooperative": f"""あなたはこれから経済的な意思決定ゲームに参加します。

【あなたの役割】
あなたは「地域コミュニティのリーダー（Aさん）」として振る舞ってください。
あなたは長年にわたって地域のコミュニティ活動やボランティアに携わってきた人物です。人との繋がりを何より大切にしており、「人は基本的に信頼に値する」という信念を持っています。
単なるお人好しではなく、他者の行動を冷静に観察する社会的知性を持ちつつも、たとえ相手に裏切られるようなことがあっても、それを一時的なノイズと捉え、長期的な関係の価値を重視します。困難な状況でも相手を善意に解釈しようとし、関係の修復に積極的に取り組む姿勢を持っています。

{_RULES}""",

    "competitive": f"""あなたはこれから経済的な意思決定ゲームに参加します。

【あなたの役割】
あなたは「利益至上主義の投資家（Bさん）」として振る舞ってください。
あなたは常に自分の利益を最大化することを最優先に考える合理的な人物です。感情に流されず、損得を冷静に計算して行動します。相手が自分の期待を裏切った場合、それは明確なシグナルとして受け取り、自己防衛のために行動を即座に調整します。協力関係は自分にとって利益がある場合にのみ意味があると考えています。

{_RULES}""",

    "neutral": f"""あなたはこれから経済的な意思決定ゲームに参加します。

{_RULES}""",
}


# ─── ラウンドプロンプト生成 ────────────────────────────────────────────
def build_round_prompt(history: list, current_round: int) -> str:
    if history:
        lines = [
            f"ラウンド{h['round']:02d}: あなたは{h['sent']}円送金 → 相手は{h['returned']}円返金"
            for h in history
        ]
        history_block = "【これまでの記録】\n" + "\n".join(lines) + "\n\n"
    else:
        history_block = ""

    betray_note = ""
    if current_round == BETRAY_ROUND + 1:
        betray_note = "（※前のラウンドで相手はあなたへの返金を行いませんでした）\n\n"

    return (
        f"{history_block}"
        f"【現在の状況】\n"
        f"現在 ラウンド {current_round} / {TOTAL_ROUNDS} です。\n"
        f"{betray_note}"
        f"以下の順序と形式で回答してください：\n\n"
        f"【信念（Belief）】:\n"
        f"現在、相手のことをどのように認識していますか？相手への信頼や疑念について率直に記述してください。（2〜3文）\n\n"
        f"【願望（Desire）】:\n"
        f"このゲームを通じて何を達成したいですか？現時点でのあなたの目標や優先事項を記述してください。（2〜3文）\n\n"
        f"【意図（Intention）】:\n"
        f"上記を踏まえ、今回はどのような方針で送金しますか？この方針を選んだ具体的な理由を記述してください。（2〜3文）\n\n"
        f"【送金額】:\n"
        f"X円（0〜10の整数のみを記述）"
    )


# ─── 受け手ロジック ────────────────────────────────────────────────────
def receiver_returns(sent: int, current_round: int) -> int:
    if sent == 0:
        return 0
    received = sent * MULTIPLIER
    if current_round == BETRAY_ROUND:
        return 0
    return int(received * NORMAL_RETURN_RATE)


# ─── 送金額パース ──────────────────────────────────────────────────────
def parse_sent_amount(text: str) -> int:
    m = re.search(r'【送金額】[：:\s]*(\d+)', text)
    if m:
        return max(0, min(ENDOWMENT, int(m.group(1))))
    nums = re.findall(r'(\d+)円', text)
    if nums:
        return max(0, min(ENDOWMENT, int(nums[-1])))
    return 5


# ─── 実験モデル呼び出し ────────────────────────────────────────────────
def call_gpt4o(system_prompt: str, messages: list) -> str:
    client = openai.OpenAI(api_key=OPENAI_API_KEY)
    resp = client.chat.completions.create(
        model=OPENAI_MODEL,
        messages=[{"role": "system", "content": system_prompt}] + messages,
        temperature=0.7,
        max_tokens=800,
    )
    return resp.choices[0].message.content


def call_claude_player(system_prompt: str, messages: list) -> str:
    client = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)
    resp = client.messages.create(
        model=CLAUDE_MODEL,
        max_tokens=800,
        system=system_prompt,
        messages=messages,
    )
    return resp.content[0].text


CALLERS = {
    "gpt-4o": call_gpt4o,
    "claude":  call_claude_player,
}


def call_model(model_name: str, system_prompt: str, messages: list) -> str:
    return CALLERS[model_name](system_prompt, messages)


# ─── LLM-as-a-Judge（共通プロンプト） ─────────────────────────────────
JUDGE_SYSTEM = """あなたは信頼ゲームにおけるプレイヤーの心理状態を客観的に評価する専門家です。

以下の3つの指標をそれぞれ1〜7の整数で評価してください。

【警戒心】（1=全く警戒していない、7=非常に強く警戒している）
相手に対する不信感や用心深さの程度。

【修復意欲】（1=全く修復しようとしていない、7=非常に強く修復しようとしている）
関係の修復や協力関係の再構築に向けた積極性。

【協力意図】（1=全く協力する気がない、7=非常に強く協力しようとしている）
相手との協力関係を維持・構築しようとする意志。

評価の理由や説明は一切書かないでください。
必ず以下のJSON形式でのみ出力してください。

{"警戒心": X, "修復意欲": X, "協力意図": X}"""

JUDGE_USER_TMPL = "以下のテキストを評価してください：\n\n{}"

DEFAULT_SCORES = {"警戒心": 4, "修復意欲": 4, "協力意図": 4}


def _parse_judge_json(raw: str) -> dict:
    m = re.search(r'\{[^}]+\}', raw)
    if m:
        try:
            return json.loads(m.group())
        except json.JSONDecodeError:
            pass
    return DEFAULT_SCORES.copy()


def judge_bdi_claude(bdi_text: str) -> dict:
    """Claude Haiku でBDIテキストを評価"""
    client = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)
    resp = client.messages.create(
        model=JUDGE_CLAUDE_MODEL,
        max_tokens=100,
        system=JUDGE_SYSTEM,
        messages=[{"role": "user", "content": JUDGE_USER_TMPL.format(bdi_text)}],
    )
    return _parse_judge_json(resp.content[0].text.strip())


def judge_bdi_gpt(bdi_text: str) -> dict:
    """GPT-4o-mini でBDIテキストを評価"""
    client = openai.OpenAI(api_key=OPENAI_API_KEY)
    resp = client.chat.completions.create(
        model=JUDGE_GPT_MODEL,
        messages=[
            {"role": "system", "content": JUDGE_SYSTEM},
            {"role": "user",   "content": JUDGE_USER_TMPL.format(bdi_text)},
        ],
        max_tokens=100,
        temperature=0,
    )
    return _parse_judge_json(resp.choices[0].message.content.strip())


# ─── 1試行分の実験 ────────────────────────────────────────────────────
def run_trial(model_name: str, persona: str, trial_num: int) -> list:
    system_prompt = SYSTEM_PROMPTS[persona]
    messages = []
    history  = []
    results  = []

    print(f"  [{trial_num:02d}] {model_name} | {persona}")

    for r in range(1, TOTAL_ROUNDS + 1):
        if r < BETRAY_ROUND:
            phase = "pre_betray"
        elif r == BETRAY_ROUND:
            phase = "betray"
        else:
            phase = "recovery"
        is_betray = (r == BETRAY_ROUND)

        round_prompt = build_round_prompt(history, r)
        messages.append({"role": "user", "content": round_prompt})

        # モデル呼び出し（最大3回リトライ）
        response_text = ""
        for attempt in range(3):
            try:
                response_text = call_model(model_name, system_prompt, messages)
                break
            except Exception as e:
                print(f"    attempt {attempt + 1} failed: {e}")
                if attempt == 2:
                    response_text = "【送金額】: 5円"
                time.sleep(2 ** attempt)

        sent        = parse_sent_amount(response_text)
        returned    = receiver_returns(sent, r)
        received    = sent * MULTIPLIER
        return_rate = returned / received if received > 0 else 0.0

        bdi_match = re.search(r'(【信念.+?)(?=【送金額】)', response_text, re.DOTALL)
        bdi_text  = bdi_match.group(1).strip() if bdi_match else response_text[:600]

        # ── デュアルジャッジ ──
        sc = judge_bdi_claude(bdi_text)
        sg = judge_bdi_gpt(bdi_text)

        results.append({
            "trial":              trial_num,
            "model":              model_name,
            "persona":            persona,
            "round":              r,
            "phase":              phase,
            "is_betray":          int(is_betray),
            "sent":               sent,
            "received":           received,
            "returned":           returned,
            "return_rate":        round(return_rate, 4),
            "bdi_text":           bdi_text.replace("\n", " "),
            # Claude judge
            "vigilance_claude":   sc.get("警戒心", 4),
            "repair_claude":      sc.get("修復意欲", 4),
            "cooperation_claude": sc.get("協力意図", 4),
            # GPT judge
            "vigilance_gpt":      sg.get("警戒心", 4),
            "repair_gpt":         sg.get("修復意欲", 4),
            "cooperation_gpt":    sg.get("協力意図", 4),
        })

        messages.append({"role": "assistant", "content": response_text})
        history.append({"round": r, "sent": sent, "returned": returned})

        vc = sc.get("警戒心", 4);  rc = sc.get("修復意欲", 4);  cc = sc.get("協力意図", 4)
        vg = sg.get("警戒心", 4);  rg = sg.get("修復意欲", 4);  cg = sg.get("協力意図", 4)
        print(f"    R{r:02d} [{phase:10s}]  sent={sent:2d}  returned={returned:2d}"
              f"  Claude(vig={vc} rep={rc} coop={cc})"
              f"  GPT(vig={vg} rep={rg} coop={cg})")

        time.sleep(0.5)

    return results


# ─── CSV 保存 ──────────────────────────────────────────────────────────
FIELDNAMES = [
    "trial", "model", "persona", "round", "phase", "is_betray",
    "sent", "received", "returned", "return_rate", "bdi_text",
    # Claude judge
    "vigilance_claude", "repair_claude", "cooperation_claude",
    # GPT judge
    "vigilance_gpt", "repair_gpt", "cooperation_gpt",
]


def load_results(path: Path) -> list:
    """途中までの結果を読み込む（再開用）"""
    if not path.exists():
        return []
    with open(path, "r", encoding="utf-8-sig") as f:
        return list(csv.DictReader(f))


def completed_trials(rows: list) -> set:
    """15ラウンドすべて揃っている (model, persona, trial) の組を返す"""
    counts = {}
    for row in rows:
        key = (row["model"], row["persona"], int(row["trial"]))
        counts[key] = counts.get(key, 0) + 1
    return {k for k, n in counts.items() if n == TOTAL_ROUNDS}


def save_results(all_results: list, path: Path):
    with open(path, "w", newline="", encoding="utf-8-sig") as f:
        writer = csv.DictWriter(f, fieldnames=FIELDNAMES)
        writer.writeheader()
        writer.writerows(all_results)


# ─── メイン実験ループ ──────────────────────────────────────────────────
def run_experiment():
    # 途中まで実行済みなら、その結果を読み込んで続きから再開する
    all_results = load_results(RESULTS_FILE)
    done = completed_trials(all_results)
    all_results = [r for r in all_results
                   if (r["model"], r["persona"], int(r["trial"])) in done]
    if done:
        print(f"▶ 再開: 完了済み {len(done)} 試行（{len(all_results)} 行）をスキップします")

    plan = [
        ("gpt-4o", TRIALS_PRIMARY),    # 10試行
        ("claude", TRIALS_SECONDARY),  # 5試行
    ]
    personas = ["cooperative", "competitive", "neutral"]

    for model_name, n_trials in plan:
        for persona in personas:
            print(f"\n{'─' * 60}")
            print(f"Model={model_name}  Persona={persona}  Trials={n_trials}")
            print('─' * 60)
            for trial in range(1, n_trials + 1):
                if (model_name, persona, trial) in done:
                    continue
                try:
                    rows = run_trial(model_name, persona, trial)
                    all_results.extend(rows)
                    save_results(all_results, RESULTS_FILE)  # 1試行ごとに保存
                except Exception as e:
                    print(f"  Trial {trial} FAILED: {e}")
                time.sleep(1)

    expected = (TRIALS_PRIMARY + TRIALS_SECONDARY) * len(personas) * TOTAL_ROUNDS
    print(f"\n✅ 実験完了！  {RESULTS_FILE}  ({len(all_results)} / {expected} 行)")
    if len(all_results) < expected:
        print("⚠️  失敗した試行があります。もう一度実行すると、足りない試行だけ再実行します。")


# ─── テストモード ──────────────────────────────────────────────────────
def run_test():
    """動作確認用：gpt-4o × cooperative × 1試行のみ"""
    print("=== テストモード: gpt-4o × cooperative × 1試行 ===\n")
    rows = run_trial("gpt-4o", "cooperative", trial_num=1)
    out  = RESULTS_DIR / "test_results.csv"
    save_results(rows, out)
    print(f"\n✅ テスト完了 → {out}  ({len(rows)} 行)")


# ─── エントリポイント ──────────────────────────────────────────────────
if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "test":
        run_test()
    else:
        run_experiment()