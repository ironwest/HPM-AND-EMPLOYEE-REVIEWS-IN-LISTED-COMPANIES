# NOTE: Table 1 — 主解析サンプル(n=886)を 健康経営度偏差値 _latest の tertile
# で 3 群に割り、各群の企業特性・口コミ・財務の分布を比較する基礎統計表。

library(tidyverse)
rm(list=ls())
samp <- read_rds("middledata/datafor_analysis.rds")

# 健康経営度偏差値 _latest で tertile 分割--------------------------
samp <- samp |>
  mutate(hensati_tertile = Hmisc::cut2(hensati_latest, g = 3))
samp |> count(hensati_tertile)

colnames(samp)

# gtsummaryでTable1を作成する--------
library(gtsummary)


samp$総資産_単体 |> summary()
samp$当期純利益_単体 |> summary()


winsorize <- function(var){
  pmin(
    pmax(
      var,
      quantile(var,0.01,na.rm=TRUE)
    ),
    quantile(var,0.99, na.rm=TRUE)
  )
}

samp <- samp |> 
  mutate(
    total_assets_bn    = `総資産_単体` / 1e9,
    net_income_bn      = `当期純利益_単体`/1e9,
    salary_mn          = `平均年間給与` / 1e6,
    n4_cons            = factor(n4_consecutive, levels = c(TRUE, FALSE),
                                labels = c("Yes", "No"))
  ) |> 
  rename(
    tks_total = `総合評点`,
    tks_yarigai = `仕事のやりがい`,
    tks_kyuyo = `給与水準`,
    tks_education = `教育・研修制度`,
    tks_benefit = `福利厚生`,
    tks_interview = `面接_選考`,
    tks_future = `企業の成長性・将来性`,
    tks_employeeattract = `社員の魅力`,
    tks_wlb = `ワークライフバランス`,
    tks_women = `女性の働きやすさ`,
    tks_gap = `入社後のギャップ`,
    tks_out = `退職理由`,
    tks_ceoattract = `社長の魅力`,
    tkn_kensu = `口コミ件数`,
    
    employee_n = `従業員数_単体`,
    employee_age = `平均年齢`,
    employee_tenure = `平均勤続年数`,
    
  ) |> 
  rename(
    total_assets = `総資産_単体`,
    net_income = `当期純利益_単体`,
  ) |> 
  mutate(
    roa = 100*net_income/total_assets
  )

samp <- samp |>
  mutate(
    industry = fct_lump_n(
      factor(latest_gyosyu),
      n = 7,
      other_level = "Other industries"
    ) |>
      fct_infreq()              # 頻度順に並べる(top 5 が上から、Other が末尾)
  )

samp <- samp |>
  mutate(
    industry = fct_recode(industry,
                          "Information & Communication" = "情報・通信業",
                          "Wholesale Trade" = "卸売業",
                          "Electric Appliances" = "電気機器",
                          "Chemicals" = "化学",
                          "Services" = "サービス業",
                          "Machinery" = "機械",
                          "Retail Trade" = "小売業"
    )
  )

samp <- samp |> 
  mutate(roa_win = winsorize(roa))


hist(samp$roa, breaks=100)
hist(samp$roa_win, breaks=100)


samp |> count(industry)

vars <- list(
  # ── Firm attributes(企業属性)─────────────────────────
  "industry"               = "Industry",
  "n4_cons"                = "Four-year-consecutive participation",
  
  # ── Exposure: HPM score(most recent)────────────────────
  "hensati_latest"         = "HPM total deviation score",
  # "side1_latest"         = "Aspect 1 (Management philosophy)",
  # "side2_latest"         = "Aspect 2 (Organizational structure)",
  # "side3_latest"         = "Aspect 3 (Implementation)",
  # "side4_latest"         = "Aspect 4 (Evaluation and improvement)",
  
  # ── Outcome: Tenshoku-kaigi scores ────────────────────
  "tks_total"              = "Tenshoku-kaigi overall rating",
  "tkn_kensu"              = "Number of reviews",
  # (sub-scores commented out)
  
  # ── Firm size(EDINET、most recent)─規模変数 ────────
  "employee_n"             = "Number of employees",
  "total_assets_bn"        = "Total assets (non-consolidated; JPY billion)",
  
  # ── Profitability(EDINET、most recent) ──
  "roa_win"          = "Return on assets (non-consolidated; %)",
  
  # ── Human resource indicators(EDINET、most recent)─────
  "employee_age"           = "Mean age of employees",
  "employee_tenure"        = "Average tenure (years)",
  "salary_mn"              = "Average annual salary (JPY million)"
)


digits_list <- list(
  all_continuous()       ~ 1,    # 多くは小数 1 桁
  `employee_n`             ~ 0,    # 従業員数は整数
  `tkn_kensu`           ~ 0,    # 口コミ件数も整数
  # hensati_latest         ~ 2,    
  # side1_latest           ~ 2,
  # side2_latest           ~ 2,
  # side3_latest           ~ 2,
  # side4_latest           ~ 2,
  matches("^tks_") ~ 2,
  salary_mn              ~ 2     # million は小数 2 桁
)


dat <- samp |> select(hensati_tertile, all_of(names(vars)))

tbl1 <- tbl_summary(
  data = dat,
  by = hensati_tertile,
  label = vars,
  digits = digits_list,
  statistic = list(
    all_continuous() ~ "{median} [{p25}, {p75}]",
    all_categorical() ~ "{n} ({p}%)"
  ),
  missing="no"
) |> 
  add_overall() 


gtsummary::as_kable(tbl1)

write_rds(samp,"middledata/analye_this.rds")
