# NOTE: Table 3 — 感度分析 (A)-(G)。主結果が以下の条件変化でも頑健であることを示す:
rm(list=ls())

library(tidyverse)
library(fixest)
library(broom)
rm(list=ls())
dat <- read_rds("middledata/data_for_sensitivityanalysis.rds")
kkpanel <- read_rds("middledata/panel_kenkoukeiei.rds") |> 
  select(fixedcode, matches("hensati"))

dat <- dat |> 
  left_join(kkpanel,by="fixedcode")

run_m5 <- function(ddd,spec, 
                   covar = " + log_ta + log_n + roa_win + n4int + employee_tenure +  salary_mn| industry", 
                   covarz = "+ log_ta_z + log_n_z + roa_win_z + n4int + kinzoku_z +  salary_z | industry", 
                   main_varraw = "hensatiper10_latest", main_var="hensati_z"){
  
  fmla <- str_c("tks_total ~ ",main_varraw,covar)
  mod <- feols(as.formula(fmla), data = ddd, cluster = ~industry)

  fmla_z <- str_c("tks_total_z ~ ",main_var,covarz)
  mod_z <- feols(as.formula(fmla_z), data = ddd, cluster = ~industry)
  
  td <- tidy(mod_z) |> filter(term == main_var)
  
  
  tidy(mod, conf.int = TRUE) |> 
    filter(term == main_varraw) |> 
    mutate(N = mod$nobs, .before=1) |> 
    mutate(Expl = spec, .before=1) |> 
    rename(beta = estimate) |> 
    mutate(betaSD = td$estimate, .after = beta)
  
}

run_m5weight <- function(ddd,spec, main_varraw = "hensatiper10_latest", main_var="hensati_z", ...){
  
  fmla <- str_c("tks_total ~ ",main_varraw," + log_ta + log_n + roa_win + n4int + employee_tenure +  salary_mn| industry")
  mod <- feols(as.formula(fmla), data = ddd, cluster = ~industry, ...)
  
  fmla_z <- str_c("tks_total_z ~ ",main_var,"+ log_ta_z + log_n_z + roa_win_z + n4int + kinzoku_z +  salary_z | industry")
  mod_z <- feols(as.formula(fmla_z), data = ddd, cluster = ~industry, ...)
  
  td <- tidy(mod_z) |> filter(term == main_var)
  
  
  tidy(mod, conf.int = TRUE) |> 
    filter(term == main_varraw) |> 
    mutate(N = mod$nobs, .before=1) |> 
    mutate(Expl = spec, .before=1) |> 
    rename(beta = estimate) |> 
    mutate(betaSD = td$estimate, .after = beta)
  
}



dat <- dat |> 
  mutate(hensati_mean_z = as.numeric(scale(hensati_mean)))

# Primary--------------------------
Z <- run_m5(dat,"Primary analysis (full three-source sample)")

# (A) 口コミ件数 >= 100--------------------------
sA <- dat |> filter(tkn_kensu >= 100)
A  <- run_m5(sA,"(A) Firms with >=100 reviews")

# (B) 上場 + 4 年連続--------------------------
sB <- dat |> filter(n4_consecutive)
B  <- run_m5(sB,
             "(B) Listed and four-year-consecutive firms only",
             covar = " + log_ta + log_n + roa_win  + employee_tenure +  salary_mn| industry", 
             covarz = "+ log_ta_z + log_n_z + roa_win_z  + kinzoku_z +  salary_z | industry")

# (C) log(件数+1) 重み付け--------------------------
C <- run_m5weight(dat,"(C) Weighted by log(reviews + 1)",weights = ~log(tkn_kensu + 1))
# fmla <- "tks_total_z ~ hensati_z + log_ta_z + log_n_z + roa_win_z + n4int + kinzoku_z +  salary_z | industry"
# model_wt <- feols(as.formula(fmla), data = dat, cluster = ~industry, weights = ~log(tkn_kensu + 1))
# 
# td <- tidy(model_wt) |> filter(term == "hensati_z")
# ev <- evalues.OLS(est = td$estimate[1],
#                   se  = td$std.error[1],
#                   sd  = 1)
# 
# td_C <- tidy(model_wt, conf.int = TRUE) |> 
#   filter(term == "hensati_z") |> 
#   mutate(
#     ev_point = ev["E-values","point"],
#     ev_lower = ev["E-values","lower"]
#   )
#C <- td_C |> mutate(Expl = "(C) Weighted by log(reviews + 1)", N = model_wt$nobs, .before=1)

# (D) KK = mean--------------------------

dat |> select(matches("hensati"))
sD <- dat |> filter(hensati_nyears>1)
sD <- sD |> mutate(hensati_mean10 = hensati_mean/10)
D <- run_m5(sD,"(D) Primary explanatory variable = mean HPM score",
            main_varraw = "hensati_mean10",
            main_var = "hensati_mean_z")

# (E) 金融業除外--------------------------
# sE <- dat |> filter(!str_detect(latest_gyosyu,"銀行|保険|証券|その他金融|金融"))
# E  <- run_m5(sE,"(E) Excluding financial industries")

# (E) 3 月期決算のみ--------------------------
# sE <- dat |> filter(`endOfPeriod` == ymd("2025-3-31"))
# E <- run_m5(sF,"(E) March-fiscal-year firms only (HPM-EDINET period alignment)")

# (E) HD疑いを除外1
sE1 <- dat |> filter(empratio >= 0.02)
E1  <- run_m5(sE1,"(E1) Remove Holdings s/o 2%")

sE2 <- dat |> filter(empratio >= 0.10)
E2  <- run_m5(sE2,"(E2) Remove Holdings s/o 10%")



table3 <- bind_rows(
  Z,A,B,C,D,E1,E2
) |> 
  mutate(p.value = scales::number(p.value,accuracy=0.0001)) |> 
  select(!term) 

knitr::kable(table3,digits=3) #|> clipr::write_clip()

write_rds(dat,"middledata/datforgraph2.rds")
