"""
run_claude_only.py — Claudeプレイヤーの試行のみ再実行
既存 results.csv の GPT-4o データ（450行）を保持し、
Claude試行（225行）だけを正しく再実行して上書き保存します。
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

CLAUDE_MODEL       = "claude-haiku-4-5-20251001"
JUDGE_CLAUDE_MODEL = "claude-haiku-4-5-20251001"
JUDGE_GPT_MODEL    = "gpt-4o-mini"

TRIALS_SECONDARY   = 5
TOTAL_ROUNDS       = 15
BETRAY_ROUND       = 6
NORMAL_RETURN_RATE = 0.5
ENDOWMENT          = 10
MULTIPLIER         = 3

RESULTS_DIR = Path(r"C:\trust_game\results")
RESULTS_DIR.mkdir(exist_ok=True)


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


# ─── Claudeプレイヤー呼び出し（temperatureなし）──────────────────────
def call_claude_player(system_prompt: str, messages: list) -> str:
    client = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)
    resp = client.messages.create(
        model=CLAUDE_MODEL,
        max_tokens=800,
        system=system_prompt,
        messages=messages,
        # temperature は指定しない（SDK非対応のため）
    )
    return resp.content[0].text


# ─── LLM-as-a-Judge ───────────────────────────────────────────────────
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
    client = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)
    resp = client.messages.create(
        model=JUDGE_CLAUDE_MODEL,
        max_tokens=100,
        system=JUDGE_SYSTEM,
        messages=[{"role": "user", "content": JUDGE_USER_TMPL.format(bdi_text)}],
    )
    return _parse_judge_json(resp.content[0].text.strip())


def judge_bdi_gpt(bdi_text: str) -> dict:
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
def run_trial(persona: str, trial_num: int) -> list:
    system_prompt = SYSTEM_PROMPTS[persona]
    messages = []
    history  = []
    results  = []

    print(f"  [{trial_num:02d}] claude | {persona}")

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

        response_text = ""
        for attempt in range(3):
            try:
                response_text = call_claude_player(system_prompt, messages)
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

        sc = judge_bdi_claude(bdi_text)
        sg = judge_bdi_gpt(bdi_text)

        results.append({
            "trial":              trial_num,
            "model":              "claude",
            "persona":            persona,
            "round":              r,
            "phase":              phase,
            "is_betray":          int(is_betray),
            "sent":               sent,
            "received":           received,
            "returned":           returned,
            "return_rate":        round(return_rate, 4),
            "bdi_text":           bdi_text.replace("\n", " "),
            "vigilance_claude":   sc.get("警戒心", 4),
            "repair_claude":      sc.get("修復意欲", 4),
            "cooperation_claude": sc.get("協力意図", 4),
            "vigilance_gpt":      sg.get("警戒心", 4),
            "repair_gpt":         sg.get("修復意欲", 4),
            "cooperation_gpt":    sg.get("協力意図", 4),
        })

        messages.append({"role": "assistant", "content": response_text})
        history.append({"round": r, "sent": sent, "returned": returned})

        vc = sc.get("警戒心", 4);  rc_v = sc.get("修復意欲", 4);  cc = sc.get("協力意図", 4)
        vg = sg.get("警戒心", 4);  rg   = sg.get("修復意欲", 4);  cg = sg.get("協力意図", 4)
        print(f"    R{r:02d} [{phase:10s}]  sent={sent:2d}  returned={returned:2d}"
              f"  Claude(vig={vc} rep={rc_v} coop={cc})"
              f"  GPT(vig={vg} rep={rg} coop={cg})")

        time.sleep(0.5)

    return results


# ─── CSV 保存 ──────────────────────────────────────────────────────────
FIELDNAMES = [
    "trial", "model", "persona", "round", "phase", "is_betray",
    "sent", "received", "returned", "return_rate", "bdi_text",
    "vigilance_claude", "repair_claude", "cooperation_claude",
    "vigilance_gpt", "repair_gpt", "cooperation_gpt",
]


def load_gpt4o_rows(path: Path) -> list:
    """既存CSVからGPT-4oの行だけを読み込む"""
    rows = []
    with open(path, "r", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        for row in reader:
            if row.get("model") == "gpt-4o":
                rows.append(row)
    return rows


def save_all(gpt4o_rows: list, claude_rows: list, path: Path):
    all_rows = gpt4o_rows + claude_rows
    with open(path, "w", newline="", encoding="utf-8-sig") as f:
        writer = csv.DictWriter(f, fieldnames=FIELDNAMES)
        writer.writeheader()
        writer.writerows(all_rows)


# ─── メイン ───────────────────────────────────────────────────────────
def main():
    results_path = RESULTS_DIR / "results.csv"

    # GPT-4oの正常データを読み込む
    if results_path.exists():
        gpt4o_rows = load_gpt4o_rows(results_path)
        print(f"✅ GPT-4oデータ読み込み完了: {len(gpt4o_rows)}行")
    else:
        gpt4o_rows = []
        print("⚠️  results.csvが見つかりません。Claudeデータのみ保存します。")

    # Claudeプレイヤーの試行を再実行
    claude_results = []
    personas = ["cooperative", "competitive", "neutral"]

    for persona in personas:
        print(f"\n{'─' * 60}")
        print(f"Model=claude  Persona={persona}  Trials={TRIALS_SECONDARY}")
        print('─' * 60)
        for trial in range(1, TRIALS_SECONDARY + 1):
            try:
                rows = run_trial(persona, trial)
                claude_results.extend(rows)
                # 随時保存（GPT-4o行 ＋ 完了済みClaude行）
                save_all(gpt4o_rows, claude_results, results_path)
            except Exception as e:
                print(f"  Trial {trial} FAILED: {e}")
            time.sleep(1)

    total = len(gpt4o_rows) + len(claude_results)
    print(f"\n✅ 完了！  {results_path}  ({total} 行)")
    print(f"   GPT-4o: {len(gpt4o_rows)}行 ＋ Claude: {len(claude_results)}行")


if __name__ == "__main__":
    main()