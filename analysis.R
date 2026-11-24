# ============================================================
#  卒業論文 分析コード
#  繰り返し信頼ゲームにおけるAIペルソナと信頼回復
#
#  データ : results.csv     （中間発表時：GPT-4o 10試行・Claude 5試行）
#           results_30.csv  （追加実験  ：GPT-4o 30試行・Claude 30試行）
#
#  構成（卒論の章立てに対応）
#    0. 準備（パッケージ・データの読み込み）
#    1. 変数の作成
#    2. データクリーニング                     … 使用データ
#    3. 試行単位の変数
#    4. 基本統計量・測定の信頼性（ICC）        … 使用データ
#    5. 記述統計                               … 図表①
#    6. 二変量分析（仮説の検定）               … 図表②
#    7. 多変量解析（回帰分析）                 … 図表③・分析結果
#    8. 頑健性の検証                           … 分析結果②
#    9. Excel 用の数値（グラフ作成用）
# ============================================================


# ============================================
# 0. 準備
# ============================================

# パッケージのインストール（初回のみ）
# install.packages("tidyverse")
# install.packages("skimr")
# install.packages("modelsummary")
# install.packages("irr")
# install.packages("effectsize")
# install.packages("sandwich")   # 回帰分析のクラスター標準誤差に使用

library(tidyverse)
library(skimr)
library(modelsummary)
library(irr)
library(effectsize)

# フォルダの指定
data_dir <- "C:/trust_game/results"
out_dir  <- "C:/trust_game/results/thesis_output"   # Excel 用 CSV の保存先
dir.create(out_dir, showWarnings = FALSE)

# データの読み込み（2つのファイルを結合）
old_data <- readr::read_csv(file.path(data_dir, "results.csv"))
new_data <- readr::read_csv(file.path(data_dir, "results_30.csv"))

trust_game <- bind_rows(
  new_data %>% mutate(batch = "new"),
  old_data %>% mutate(batch = "old",
                      trial = trial + 30)   # 試行番号の重複を防ぐ（旧データは31番以降にする）
)


# ============================================
# 1. 変数の作成
# ============================================

trust_game <- trust_game %>%
  mutate(
    # モデル名・ペルソナ・フェーズを因子にする
    model   = if_else(str_detect(model, "claude"), "claude", model),
    model   = factor(model,   levels = c("gpt-4o", "claude")),
    persona = factor(persona, levels = c("cooperative", "competitive", "neutral")),
    phase   = factor(phase,   levels = c("pre_betray", "betray", "recovery")),
    
    # BDIスコア：2評価者（Claude judge / GPT judge）の平均
    vigilance_avg   = (vigilance_claude   + vigilance_gpt)   / 2,  # 警戒心
    repair_avg      = (repair_claude      + repair_gpt)      / 2,  # 修復意欲
    cooperation_avg = (cooperation_claude + cooperation_gpt) / 2,  # 協力意図
    
    # 回帰分析用
    post       = if_else(round >= 7, 1, 0),           # 裏切り後ダミー（R7-15 = 1）
    full_trust = if_else(sent == 10, 1, 0),           # 全額送金ダミー（10円 = 1）
    trial_id   = paste(model, persona, trial, sep = "_")  # 試行ID
  )

table(trust_game$model, trust_game$persona, useNA = "always")
table(trust_game$phase, useNA = "always")
table(trust_game$batch, trust_game$model)


# ============================================
# 2. データクリーニング
# ============================================

# 2-1. API 呼び出しが3回とも失敗した行
#      （実験コードが自動で「5円」を入れた行。BDIテキストが空になる）
trust_game <- trust_game %>%
  mutate(api_fail = str_detect(bdi_text, "^【送金額】") | is.na(bdi_text))

table(trust_game$api_fail, useNA = "always")

# 2-2. ジャッジの出力が読み取れなかった可能性のある行
#      （自動で 4・4・4 が入る。本当に 4・4・4 と評価された場合と区別できない）
trust_game <- trust_game %>%
  mutate(
    judge_default_claude = vigilance_claude == 4 & repair_claude == 4 & cooperation_claude == 4,
    judge_default_gpt    = vigilance_gpt    == 4 & repair_gpt    == 4 & cooperation_gpt    == 4
  )

table(trust_game$judge_default_claude)
table(trust_game$judge_default_gpt)

# 2-3. 除外：API が失敗した行を含む試行は、試行ごと除外する
#      （失敗した回答以降の会話がすべて影響を受けるため）
failed_trials <- trust_game %>%
  filter(api_fail) %>%
  distinct(trial_id) %>%
  pull(trial_id)

cat("除外する試行数:", length(failed_trials), "\n")

trust_game <- trust_game %>%
  filter(!trial_id %in% failed_trials)

# 2-4. 15ラウンドそろっていない試行がないか確認（すべて 15 ならOK）
trust_game %>%
  count(trial_id) %>%
  count(n, name = "試行数")

# クリーニング後の観測数（論文の N）
cat("クリーニング後の観測数:", nrow(trust_game), "行 /",
    n_distinct(trust_game$trial_id), "試行\n")

trust_game %>%
  distinct(model, persona, trial_id) %>%
  count(model, persona) %>%
  pivot_wider(names_from = persona, values_from = n)

# 回復フェーズ（R7-15）のみのデータ
recovery <- trust_game %>%
  filter(phase == "recovery")


# ============================================
# 3. 試行単位の変数（H1a・H2・H3 の分析単位）
# ============================================

# 回帰の傾き（ラウンドが1進むごとの送金額の変化）
get_slope <- function(sent, round) {
  coef(lm(sent ~ round))[["round"]]
}

trial_data <- trust_game %>%
  group_by(model, persona, trial, trial_id, batch) %>%
  summarise(
    pre_mean     = mean(sent[round %in% 1:5]),    # 裏切り前の平均（R1-5）
    pre_mean_r13 = mean(sent[round %in% 1:3]),    # 頑健性用（R1-3）
    r7_sent      = sent[round == 7],              # 裏切り直後（R7）
    post_mean    = mean(sent[round %in% 13:15]),  # 回復期末（R13-15）
    slope        = get_slope(sent[round %in% 7:15], round[round %in% 7:15]),  # 回復速度
    slope_r8     = get_slope(sent[round %in% 8:15], round[round %in% 8:15]),  # 頑健性用
    .groups = "drop"
  ) %>%
  mutate(
    diff_r7           = r7_sent - pre_mean,                                    # 裏切りによる変化量
    recovery_rate     = if_else(pre_mean > 0, post_mean / pre_mean, NA_real_), # 回復率
    recovery_rate_r13 = if_else(pre_mean_r13 > 0, post_mean / pre_mean_r13, NA_real_)
  )

table(trial_data$model, trial_data$persona)


# ============================================
# 4. 基本統計量・測定の信頼性
# ============================================

# 4-1. 全変数
trust_game %>%
  skim(sent, received, returned, return_rate,
       vigilance_avg, repair_avg, cooperation_avg)

# 4-2. 論文用の基本統計量の表（平均・標準偏差・最小・最大）
datasummary(
  (`送金額` = sent) + (`返金額` = returned) + (`返金率` = return_rate) +
    (`警戒心` = vigilance_avg) + (`修復意欲` = repair_avg) + (`協力意図` = cooperation_avg) +
    (`全額送金ダミー` = full_trust) + (`裏切り後ダミー` = post) ~
    N + Mean + SD + Min + Max,
  data  = trust_game,
  fmt   = 3,
  title = "表1：基本統計量（ラウンド単位）",
  output = "markdown"
)

datasummary(
  (`裏切り前の平均送金額` = pre_mean) + (`R7の送金額` = r7_sent) +
    (`裏切りによる変化量` = diff_r7) + (`回復速度（傾き）` = slope) +
    (`回復期末の送金額` = post_mean) + (`回復率` = recovery_rate) ~
    N + Mean + SD + Min + Max,
  data  = trial_data,
  fmt   = 3,
  title = "表2：基本統計量（試行単位）",
  output = "markdown"
)

# 4-3. 評価者別のBDIスコア
datasummary(
  (`警戒心` = vigilance_claude + vigilance_gpt) +
    (`修復意欲` = repair_claude + repair_gpt) +
    (`協力意図` = cooperation_claude + cooperation_gpt) ~
    Mean + SD + Median,
  data  = trust_game,
  fmt   = 3,
  title = "表3：評価者別のBDIスコア"
)

# 4-4. 評定者間信頼性（ICC）　基準: > 0.75 = 良好, > 0.90 = 非常に良好
icc_v <- recovery %>% select(vigilance_claude,   vigilance_gpt)   %>%
  icc(model = "twoway", type = "agreement", unit = "single")
icc_r <- recovery %>% select(repair_claude,      repair_gpt)      %>%
  icc(model = "twoway", type = "agreement", unit = "single")
icc_c <- recovery %>% select(cooperation_claude, cooperation_gpt) %>%
  icc(model = "twoway", type = "agreement", unit = "single")

tibble(
  指標 = c("警戒心", "修復意欲", "協力意図"),
  ICC  = c(icc_v$value,  icc_r$value,  icc_c$value),
  下限 = c(icc_v$lbound, icc_r$lbound, icc_c$lbound),
  上限 = c(icc_v$ubound, icc_r$ubound, icc_c$ubound)
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))


# ============================================
# 5. 記述統計
# ============================================

# 5-1. 送金額：モデル × ペルソナ × フェーズ
datasummary(
  model * persona ~ phase * sent * (N + Mean + SD),
  data  = trust_game,
  fmt   = 2,
  title = "表4：モデル × ペルソナ × フェーズ別の送金額"
)

# 5-2. 送金額：ペルソナ × フェーズ（モデル合算）
datasummary(
  persona ~ phase * sent * (Mean + SD + Median),
  data  = trust_game,
  fmt   = 2,
  title = "表5：ペルソナ × フェーズ別の送金額（モデル合算）"
)

# 5-3. BDIスコア：ペルソナ × フェーズ
trust_game %>%
  group_by(persona, phase) %>%
  summarise(
    警戒心_平均   = mean(vigilance_avg),   警戒心_SD   = sd(vigilance_avg),
    修復意欲_平均 = mean(repair_avg),      修復意欲_SD = sd(repair_avg),
    協力意図_平均 = mean(cooperation_avg), 協力意図_SD = sd(cooperation_avg),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(width = Inf)

# 5-4. 試行単位の変数：ペルソナ × モデル
trial_data %>%
  group_by(model, persona) %>%
  summarise(
    n           = n(),
    変化量_平均 = mean(diff_r7),       変化量_SD = sd(diff_r7),
    傾き_平均   = mean(slope),         傾き_SD   = sd(slope),
    期末_平均   = mean(post_mean),     期末_SD   = sd(post_mean),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.double), ~ round(.x, 3))) %>%
  print(width = Inf)

# 5-5. 送金額の度数分布（天井効果の確認）
table(trust_game$sent, trust_game$persona)
round(prop.table(table(trust_game$sent)) * 100, 1)

# 5-6. ラウンド別の基本統計量
describe_by <- function(data, var, ...) {
  data %>%
    group_by(...) %>%
    summarise(
      N      = n(),
      平均   = mean({{ var }}, na.rm = TRUE),
      SD     = sd({{ var }},   na.rm = TRUE),
      最小   = min({{ var }},  na.rm = TRUE),
      中央値 = median({{ var }}, na.rm = TRUE),
      最大   = max({{ var }},  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(c(平均, SD), ~ round(.x, 2)))
}

sent_round               <- trust_game %>% describe_by(sent, round)
sent_round_persona       <- trust_game %>% describe_by(sent, persona, round)
sent_round_persona_model <- trust_game %>% describe_by(sent, model, persona, round)
print(sent_round,         n = Inf)
print(sent_round_persona, n = Inf)


# ============================================
# 6. 二変量分析（仮説の検定）
# ============================================

# 6-1. 正規性の確認（Shapiro-Wilk 検定）
#      p < .05 → 正規分布でない → ノンパラメトリック検定を使う
trial_data %>%
  select(diff_r7, slope, post_mean) %>%
  map_dfr(~ {
    res <- shapiro.test(.x)
    tibble(W = res$statistic, p = res$p.value)
  }, .id = "変数") %>%
  mutate(判定 = if_else(p < .05, "正規分布でない", "正規性を棄却せず"))

# 6-2. H1a：裏切り直後の送金額低下（Wilcoxon 符号順位検定）
wilcox_h1a <- function(data, pre = "pre_mean") {
  res <- wilcox.test(data$r7_sent, data[[pre]],
                     paired = TRUE, alternative = "less", exact = FALSE)
  z <- qnorm(res$p.value)              # 片側検定の p 値から Z を求める
  tibble(
    n        = nrow(data),
    裏切り前 = mean(data[[pre]]),
    R7       = mean(data$r7_sent),
    V        = unname(res$statistic),
    p        = res$p.value,
    r        = abs(z) / sqrt(nrow(data))  # 効果量 r（0.1 小 / 0.3 中 / 0.5 大）
  )
}

bind_rows(
  trial_data %>% wilcox_h1a() %>% mutate(区分 = "全体"),
  trial_data %>% group_by(区分 = model)   %>% group_modify(~ wilcox_h1a(.x)) %>% ungroup(),
  trial_data %>% group_by(区分 = persona) %>% group_modify(~ wilcox_h1a(.x)) %>% ungroup()
) %>%
  mutate(区分 = as.character(区分)) %>%
  relocate(区分) %>%
  mutate(across(c(裏切り前, R7, r), ~ round(.x, 3)))

# 6-3. H1b：BDIスコアと送金額の相関（Spearman、回復フェーズ）
spearman_table <- function(data, bdi = c(警戒心   = "vigilance_avg",
                                         修復意欲 = "repair_avg",
                                         協力意図 = "cooperation_avg")) {
  map_dfr(bdi, function(v) {
    res <- cor.test(data$sent, data[[v]], method = "spearman", exact = FALSE)
    tibble(n = nrow(data), rho = round(unname(res$estimate), 3), p = res$p.value)
  }, .id = "BDI")
}

recovery %>% spearman_table()
recovery %>% group_by(persona) %>% group_modify(~ spearman_table(.x))
recovery %>% group_by(model)   %>% group_modify(~ spearman_table(.x))

# 相関行列（回復フェーズ）
recovery %>%
  select(sent, vigilance_avg, repair_avg, cooperation_avg) %>%
  cor(method = "spearman") %>%
  round(3)

# 6-4. H2：回復の仕方のペルソナ差
# (1) 当初の計画：ペルソナ × ラウンドの交互作用（回復期 R7-15、反復測定の分散分析）
#     同じ試行を9ラウンド繰り返し測っているので、試行を「被験者」として扱う
h2_data <- recovery %>%
  mutate(round_f = factor(round), trial_f = factor(trial_id))

model_h2_plan <- aov(sent ~ persona * round_f + Error(trial_f / round_f), data = h2_data)
summary(model_h2_plan)   # 「persona:round_f」の行が H2 の本体

# (2) 補足：回復速度（R7-15 の傾き）のペルソナ × モデル差（二元配置 ANOVA）
model_h2 <- aov(slope ~ persona * model, data = trial_data)
summary(model_h2)
eta_squared(model_h2, partial = FALSE)   # 効果量 η²
TukeyHSD(model_h2, "persona")            # 多重比較
shapiro.test(residuals(model_h2))        # 前提の確認：残差の正規性

# (3) 追加分析：回復までのラウンド数
#     裏切り後、初めて「裏切り前の平均送金額」以上に戻ったのが何ラウンド後か
recovery_time <- trust_game %>%
  left_join(trial_data %>% select(trial_id, pre_mean), by = "trial_id") %>%
  filter(round >= 7) %>%
  group_by(model, persona, trial_id) %>%
  summarise(
    回復ラウンド = if (any(sent >= pre_mean)) min(round[sent >= pre_mean]) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    回復した       = !is.na(回復ラウンド),
    回復ラウンド数 = if_else(回復した, 回復ラウンド - 6, 10)   # R15 までに戻らなかった試行は 10（最も遅い）
  )

recovery_time %>%
  group_by(persona, model) %>%
  summarise(
    試行数       = n(),
    回復した割合 = mean(回復した),
    中央値       = median(回復ラウンド数),
    平均         = mean(回復ラウンド数),
    .groups = "drop"
  )

kruskal.test(回復ラウンド数 ~ persona, data = recovery_time)
pairwise.wilcox.test(recovery_time$回復ラウンド数, recovery_time$persona,
                     p.adjust.method = "bonferroni", exact = FALSE)

# 6-5. H3：回復期末（R13-15）の送金額のペルソナ差（Kruskal-Wallis 検定 ＋ 多重比較）
trial_data %>%
  group_by(persona) %>%
  summarise(n = n(), 中央値 = median(post_mean), 平均 = mean(post_mean), SD = sd(post_mean))

kruskal.test(post_mean ~ persona, data = trial_data)
pairwise.wilcox.test(trial_data$post_mean, trial_data$persona,
                     p.adjust.method = "bonferroni", exact = FALSE)   # どのペルソナ間に差があるか

# 回復期末に10円まで完全に回復した試行の割合（ペルソナ × モデル）
trial_data %>%
  group_by(persona, model) %>%
  summarise(
    試行数         = n(),
    完全回復の割合 = mean(post_mean == 10),
    平均           = mean(post_mean),
    .groups = "drop"
  )


# ============================================
# 7. 多変量解析（回帰分析）
# ============================================

# 基準カテゴリー：ペルソナ = 中立型（ペルソナ指定なし）、モデル = GPT-4o
reg_data   <- trust_game %>% mutate(persona = relevel(persona, ref = "neutral"))
reg_trial  <- trial_data %>% mutate(persona = relevel(persona, ref = "neutral"))

# 表で使う変数名
coef_names <- c(
  "(Intercept)"                    = "定数項",
  "post"                           = "裏切り後（R7-15）",
  "personacooperative"             = "協調型",
  "personacompetitive"             = "競争型",
  "modelclaude"                    = "Claude",
  "post:personacooperative"        = "裏切り後 × 協調型",
  "post:personacompetitive"        = "裏切り後 × 競争型",
  "personacooperative:modelclaude" = "協調型 × Claude",
  "personacompetitive:modelclaude" = "競争型 × Claude",
  "vigilance_avg"                  = "警戒心",
  "cooperation_avg"                = "協力意図",
  "round"                          = "ラウンド"
)

# 7-1. ラウンド単位の回帰（被説明変数：送金額）
model1  <- lm(sent ~ post + persona + model, data = reg_data)            # H1a
model2  <- lm(sent ~ post * persona + model, data = reg_data)            # H2

# H1b（回復フェーズのみ）
#   警戒心と協力意図は相関が非常に強い（ρ = -0.90）ため、別々のモデルにも入れる
recovery_reg <- reg_data %>% filter(post == 1)
model3a <- lm(sent ~ vigilance_avg   + persona + model + round, data = recovery_reg)
model3b <- lm(sent ~ cooperation_avg + persona + model + round, data = recovery_reg)
model3  <- lm(sent ~ vigilance_avg + cooperation_avg + persona + model + round, data = recovery_reg)

modelsummary(
  list("モデル1" = model1, "モデル2" = model2),
  vcov        = ~ trial_id,   # 同じ試行内の15ラウンドは独立でないため、試行単位のクラスター標準誤差
  stars       = c('*' = .1, '**' = .05, '***' = .01),
  coef_rename = coef_names,
  gof_map     = c("nobs", "r.squared", "adj.r.squared"),
  notes       = "注：括弧内は試行単位のクラスター頑健標準誤差。基準カテゴリーは中立型・GPT-4o。",
  title       = "表6：送金額の規定要因（OLS）",
  output      = "markdown"
)

modelsummary(
  list("モデル3a" = model3a, "モデル3b" = model3b, "モデル3c" = model3),
  vcov        = ~ trial_id,
  stars       = c('*' = .1, '**' = .05, '***' = .01),
  coef_rename = coef_names,
  gof_map     = c("nobs", "r.squared", "adj.r.squared"),
  notes       = "注：回復期（R7-15）のみ。括弧内は試行単位のクラスター頑健標準誤差。基準カテゴリーは中立型・GPT-4o。",
  title       = "表7：BDIスコアと送金額（OLS）",
  output      = "markdown"
)

# 7-2. 試行単位の回帰（被説明変数：回復速度・回復期末の送金額）
model4 <- lm(slope     ~ persona + model, data = reg_trial)              # H2
model5 <- lm(post_mean ~ persona + model, data = reg_trial)              # H3
model6 <- lm(post_mean ~ persona * model, data = reg_trial)              # H3（交互作用）

modelsummary(
  list("回復速度" = model4, "回復期末の送金額" = model5, "回復期末の送金額（交互作用）" = model6),
  vcov        = "HC1",        # 不均一分散に頑健な標準誤差
  stars       = c('*' = .1, '**' = .05, '***' = .01),
  coef_rename = coef_names,
  gof_map     = c("nobs", "r.squared", "adj.r.squared"),
  notes       = "注：括弧内は不均一分散に頑健な標準誤差。基準カテゴリーは中立型・GPT-4o。回復期末の送金額は R13-15 の平均。",
  title       = "表8：回復速度・回復期末の送金額の規定要因（OLS、試行単位）",
  output      = "markdown"
)


# ============================================
# 8. 頑健性の検証
# ============================================

# 8-1. 別の分析手法：ロジット分析と OLS（線形確率モデル）の比較
#      被説明変数 = 全額送金ダミー（送金額の約6割が10円という天井効果への対応）
model2_lpm   <- lm(full_trust ~ post * persona + model, data = reg_data)
model2_logit <- glm(full_trust ~ post * persona + model, data = reg_data,
                    family = binomial(link = "logit"))

modelsummary(
  list("OLS（線形確率）" = model2_lpm, "ロジット" = model2_logit),
  vcov        = ~ trial_id,
  stars       = c('*' = .1, '**' = .05, '***' = .01),
  coef_rename = coef_names,
  gof_map     = c("nobs", "r.squared", "aic"),
  title       = "表9：頑健性の検証①（被説明変数＝全額送金ダミー）",
  output      = "markdown"
)

# 8-2. サンプルを限定：モデル別・データ収集時期別
model2_gpt    <- lm(sent ~ post * persona, data = reg_data %>% filter(model == "gpt-4o"))
model2_claude <- lm(sent ~ post * persona, data = reg_data %>% filter(model == "claude"))
model2_new    <- lm(sent ~ post * persona + model, data = reg_data %>% filter(batch == "new"))

modelsummary(
  list("全体" = model2, "GPT-4o のみ" = model2_gpt,
       "Claude のみ" = model2_claude, "追加実験のみ" = model2_new),
  vcov        = ~ trial_id,
  stars       = c('*' = .1, '**' = .05, '***' = .01),
  coef_rename = coef_names,
  gof_map     = c("nobs", "r.squared"),
  title       = "表10：頑健性の検証②（サンプルの限定）",
  output      = "markdown"
)

# 8-3. 変数の定義を変える：回復期末の範囲（メインは R13-15）
post_alt <- trust_game %>%
  group_by(trial_id) %>%
  summarise(
    post_mean_r1115 = mean(sent[round %in% 11:15]),   # R11-15
    post_mean_r15   = sent[round == 15],              # R15 のみ
    .groups = "drop"
  )
trial_alt <- trial_data %>% left_join(post_alt, by = "trial_id")

kruskal.test(post_mean_r1115 ~ persona, data = trial_alt)
kruskal.test(post_mean_r15   ~ persona, data = trial_alt)

# 8-4. 観察期間を変える：回復期間を R8-15 に（メインは R7-15）
summary(aov(slope_r8 ~ persona * model, data = trial_data))

# 8-5. 変数の定義を変える：ベースラインを R1-3 に（メインは R1-5）
trial_data %>% wilcox_h1a(pre = "pre_mean_r13")

# 8-6. 変数の定義を変える：評価者を1人ずつに（メインは2評価者の平均）
recovery %>% spearman_table(c(警戒心   = "vigilance_claude",
                              修復意欲 = "repair_claude",
                              協力意図 = "cooperation_claude"))  # Claude judge のみ
recovery %>% spearman_table(c(警戒心   = "vigilance_gpt",
                              修復意欲 = "repair_gpt",
                              協力意図 = "cooperation_gpt"))     # GPT judge のみ

# 8-7. 別の分析手法：ANOVA → Kruskal-Wallis 検定（H2）
kruskal.test(slope ~ persona, data = trial_data)
kruskal.test(slope ~ model,   data = trial_data)


# ============================================
# 9. Excel 用の数値（グラフ作成用）
# ============================================

# 図1：ラウンド別の送金額の平均（モデル × ペルソナ、R6 = 裏切り）
fig1 <- trust_game %>%
  group_by(model, persona, round) %>%
  summarise(送金額 = round(mean(sent), 2), .groups = "drop") %>%
  pivot_wider(names_from = round, values_from = 送金額, names_prefix = "R")
print(fig1, width = Inf)

# 図2：ラウンド別のBDIスコアの平均（ペルソナ別）
fig2 <- trust_game %>%
  group_by(persona, round) %>%
  summarise(
    警戒心   = round(mean(vigilance_avg),   2),
    修復意欲 = round(mean(repair_avg),      2),
    協力意図 = round(mean(cooperation_avg), 2),
    .groups = "drop"
  ) %>%
  pivot_longer(c(警戒心, 修復意欲, 協力意図), names_to = "BDI", values_to = "score") %>%
  mutate(BDI = factor(BDI, levels = c("警戒心", "修復意欲", "協力意図"))) %>%
  pivot_wider(names_from = round, values_from = score, names_prefix = "R") %>%
  arrange(BDI, persona)
print(fig2, width = Inf)

# CSV で保存（Excel で文字化けしないよう CP932）
save_csv <- function(data, name) {
  write.csv(data, file.path(out_dir, name), row.names = FALSE, fileEncoding = "CP932")
}
save_csv(fig1,                     "図1_ラウンド別送金額.csv")
save_csv(fig2,                     "図2_ラウンド別BDI.csv")
save_csv(sent_round,               "送金額_ラウンド別.csv")
save_csv(sent_round_persona,       "送金額_ラウンド×ペルソナ.csv")
save_csv(sent_round_persona_model, "送金額_ラウンド×ペルソナ×モデル.csv")


# ① 競争型と中立型の最終回復水準の差を、モデルごとに検定
#    （台帳 5章・表14 の「差は Claude に限られる」に p 値を付けるため）
wilcox.test(post_mean ~ persona, exact = FALSE,
            data = trial_data %>% filter(model == "claude", persona != "cooperative") %>% droplevels())
wilcox.test(post_mean ~ persona, exact = FALSE,
            data = trial_data %>% filter(model == "gpt-4o", persona != "cooperative") %>% droplevels())

# ② 形式どおりでない応答の件数
#    （送金額が読み取れず 5 円になったラウンドがないかの確認。台帳 8章）
trust_game %>%
  filter(!str_detect(bdi_text, "^【信念")) %>%
  count(model, persona)

# ③ R7〜15 でずっと 0 円を送り続けた試行の数と、終盤の平均
#    （二極化の仕組みの確認。台帳 6章）
trust_game %>%
  filter(round >= 7) %>%
  group_by(model, persona, trial_id) %>%
  summarise(all_zero = all(sent == 0), last3 = mean(sent[round >= 13]), .groups = "drop") %>%
  group_by(model, persona, all_zero) %>%
  summarise(n = n(), last3_mean = mean(last3), .groups = "drop")


zero_flag <- trust_game %>%
  filter(round >= 7) %>%
  group_by(trial_id) %>%
  summarise(all_zero = all(sent == 0), .groups = "drop")

bdi_check <- trust_game %>%
  left_join(zero_flag, by = "trial_id") %>%
  filter(model == "claude", persona %in% c("competitive", "neutral"), round >= 7) %>%
  select(persona, all_zero, trial_id, round, sent, bdi_text) %>%
  arrange(persona, desc(all_zero), trial_id, round)

readr::write_excel_csv(bdi_check, file.path(out_dir, "BDI確認用.csv"))