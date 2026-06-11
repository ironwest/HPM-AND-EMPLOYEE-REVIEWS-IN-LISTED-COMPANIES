# NOTE: 07 の改修版。EDINET 単体ベース 
library(tidyverse)

# データ--------------------------
kkpanel    <- read_rds("middledata/panel_kenkoukeiei.rds")   # 健康経営パネル wide
kktoedinet <- read_rds("middledata/kktoedinetcode.rds")      # 健康経営 ↔ EDINET
kktotk     <- read_rds("middledata/kktotk.rds")              # 健康経営 ↔ tk
tk         <- read_rds("middledata/tk.rds")            # tk + tkid
edinet     <- read_rds("middledata/wideedinet.rds")  # EDINET wide
edinetmeta <- read_csv("data/edinet/edinet_codes.csv",
               skip = 1,
               locale = locale(encoding = "CP932"),
               show_col_types = FALSE)
# 健康経営の集約計算関数--------------------------
# 各企業ごとに 認定 2022〜2025 出現年度の偏差値の値から:
# - latest: 最新出現年度の値
# - mean  : 出現年度の平均
# - slope : 年度に対する OLS スロープ(2 点以上 + 変動ありの場合のみ)
make_agg <- function(panel, varname) {
  panel |>
    select(fixedcode, matches(str_c("n\\d{4}_", varname, "$"))) |>
    pivot_longer(cols = !fixedcode,
                 names_pattern = str_c("n(\\d{4})_", varname),
                 names_to  = "year",
                 values_to = "value") |>
    mutate(year = as.integer(year)) |>
    filter(!is.na(value),
           year >= 2022, year <= 2025) |>   # 認定 2023-2026 = 評価年 2022-2025
    group_by(fixedcode) |>
    summarise(
      var_latest     = value[which.max(year)],
      var_mean       = mean(value),
      var_slope      = if_else( n() >= 2 && sd(value) > 0, 
                                coef(lm(value ~ year))[["year"]], 
                                NA_real_),
      var_nyears     = n(),
      var_latestyear = max(year),
      .groups = "drop"
    ) |> 
    rename(
      !!str_c(varname,"_latest") := var_latest,
      !!str_c(varname,"_mean") := var_mean,
      !!str_c(varname,"_slope") := var_slope,
      !!str_c(varname,"_nyears") := var_nyears,
      !!str_c(varname,"_latestyear") := var_latestyear
    )
}

# 5 変数 × 3 集約 を計算
agghensati <- make_agg(kkpanel, "hensati")
aggside1   <- make_agg(kkpanel, "side1")
aggside2   <- make_agg(kkpanel, "side2")
aggside3   <- make_agg(kkpanel, "side3")
aggside4   <- make_agg(kkpanel, "side4")

# OK
agghensati |> head()
agghensati |> summary()

# 企業属性(最新社名・上場有無・4年連続フラグ)--------------------------
# NOTE: 4 年連続フラグ = 認定 2023-2026 すべてに出現(_nyears == 4)
attr_latestname <- kkpanel |> 
  select(fixedcode,matches("companyname")) |> 
  pivot_longer(cols = !fixedcode) |> 
  filter(!is.na(value)) |> 
  arrange(fixedcode, name) |> 
  group_by(fixedcode) |> 
  slice_tail(n=1) |> 
  rename(latest_name = value) |> 
  ungroup() |> 
  mutate(final_appear_year = as.numeric(str_extract(name,"\\d+"))) |> 
  select(!name)

attr_jojo <- kkpanel |> 
  select(fixedcode, matches("jojo")) |> 
  pivot_longer(cols=!fixedcode) |> 
  filter(!is.na(value)) |> 
  arrange(fixedcode, name) |> 
  group_by(fixedcode) |> 
  summarise(
    latest_jojo = last(value),
    n4_consecutive = n() == 4
  )

attr_gyosyu <- kkpanel |> 
  select(fixedcode, matches("gyosyu")) |> 
  pivot_longer(cols=!fixedcode) |> 
  filter(!is.na(value)) |> 
  arrange(fixedcode, name) |> 
  group_by(fixedcode) |> 
  summarise(
    latest_gyosyu = last(value)
  )

attr <- left_join(attr_latestname, attr_jojo, by="fixedcode") |> 
  left_join(attr_gyosyu, by ="fixedcode")

# 口コミ(転職会議)を 1 行/企業 に--------------------------
# tkid → fixedcode の対応(kktotk)で fixedcode に紐付ける
tklite <- tk |>
  select(tkid, 総合評点, 口コミ件数, 仕事のやりがい, 給与水準, `教育・研修制度`,
         福利厚生, 面接_選考, `企業の成長性・将来性`, 社員の魅力,
         ワークライフバランス, 女性の働きやすさ, 入社後のギャップ, 退職理由, 社長の魅力)

# 横断マスターパネル(1 行 = 1 fixedcode)--------------------------
master <- attr |>
  left_join(agghensati, by = "fixedcode") |>
  left_join(aggside1,   by = "fixedcode") |>
  left_join(aggside2,   by = "fixedcode") |>
  left_join(aggside3,   by = "fixedcode") |>
  left_join(aggside4,   by = "fixedcode") |>
  left_join(kktotk,     by = "fixedcode") |>          # tkid を付与
  left_join(tklite,     by = "tkid") |>               # 口コミ評点を付与
  left_join(kktoedinet, by = "fixedcode") |>          # edinetCode を付与
  left_join(edinet, by = c("code" = "edinetCode")) |>
  rename(edinetCode = code)

# 出力保存--------------------------
write_rds(master, "middledata/master_cross_v3.rds")

