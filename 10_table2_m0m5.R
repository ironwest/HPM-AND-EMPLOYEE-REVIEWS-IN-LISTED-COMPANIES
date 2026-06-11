# NOTE: Table 2 — メイン解析(M0-M5 階層)。健康経営度総合偏差値 _latest を
# 主要曝露とし、主アウトカム = 総合評点 のみに対して M0(粗)→ M5(完全モデル)
rm(list=ls())

library(tidyverse)
library(fixest)
library(broom)

dat <- read_rds("middledata/analye_this.rds") |> 
  select(!matches("^side")) |> 
  select(!c(
    tks_yarigai,
    tks_kyuyo,               
    tks_education,
    tks_benefit,             
    tks_interview,
    tks_future,              
    tks_employeeattract,
    tks_wlb,                 
    tks_women,
    tks_gap,                 
    tks_out,
    tks_ceoattract,
    
  ))
colnames(dat)





dat |> count(latest_gyosyu)
dat$hensati_latest |> summary()
dat$tks_total |> summary()

dat$total_assets |> summary()
hist(dat$total_assets,breaks = 100)
hist(log(dat$total_assets),breaks = 100)

dat$net_income |> summary()
hist(dat$net_income,breaks = 100)
hist(asinh(dat$net_income),breaks = 100)

dat$employee_n |> summary()
hist(dat$employee_n,breaks = 100)
hist(log(dat$employee_n),breaks = 100)

dat$employee_tenure |> hist()
dat$salary_mn |> hist()

dat$roa |> summary()
hist(dat$roa,breaks=100)
dat |> count(n4_consecutive)

# 主解析サンプル--------------------------
nrow(dat)
dat0 <- dat
dat <- dat0 |>
  filter(!is.na(hensati_latest),
         !is.na(`tks_total`),
         !is.na(`total_assets`),
         !is.na(`roa_win`),
         !is.na(`employee_tenure`),
         !is.na(industry)) |>
  mutate(log_ta   = log(total_assets),
         log_n    = log(`employee_n`),
         industry    = factor(latest_gyosyu),
         n4int       = as.integer(n4_consecutive))
nrow(dat)

dat <- dat |> 
  mutate(hensatiper10_latest = hensati_latest/10)
cat("解析サンプル:", nrow(dat), "社\n")

#1業種に1社のデータはFEOLSでは落ちるので、Mod0でも正しく落ちるように除外しておく。

dat |> count(latest_gyosyu) |> filter(n == 1)

dat <- dat |> filter(latest_gyosyu != "鉱業")

# M0-M5 を 1 outcome × 1 KK 指標で回す関数--------------------------
# M0=粗、M1=+業種FE、M2=+規模、M3=+ROA、M4=+4年連続、M5=+勤続年数＋給与

fmla0 <- "tks_total ~ hensatiper10_latest " 
fmla1 <- "tks_total ~ hensatiper10_latest | industry"
fmla2 <- "tks_total ~ hensatiper10_latest + log_ta + log_n | industry"
fmla3 <- "tks_total ~ hensatiper10_latest + log_ta + log_n + roa_win | industry"
fmla4 <- "tks_total ~ hensatiper10_latest + log_ta + log_n + roa_win + n4int | industry"
fmla5 <- "tks_total ~ hensatiper10_latest + log_ta + log_n + roa_win + n4int + employee_tenure +  salary_mn| industry"



mod0 <- feols(as.formula(fmla0), data = dat, cluster = ~industry)
mod1 <- feols(as.formula(fmla1), data = dat, cluster = ~industry)
mod2 <- feols(as.formula(fmla2), data = dat, cluster = ~industry)
mod3 <- feols(as.formula(fmla3), data = dat, cluster = ~industry)
mod4 <- feols(as.formula(fmla4), data = dat, cluster = ~industry)
mod5 <- feols(as.formula(fmla5), data = dat, cluster = ~industry)

resbase <- bind_rows(
  tidy(mod0, conf.int = TRUE) |> mutate(mod = "M0", .before=1) |> filter(term == "hensatiper10_latest"),
  tidy(mod1, conf.int = TRUE) |> mutate(mod = "M1", .before=1) |> filter(term == "hensatiper10_latest"),
  tidy(mod2, conf.int = TRUE) |> mutate(mod = "M2", .before=1) |> filter(term == "hensatiper10_latest"),
  tidy(mod3, conf.int = TRUE) |> mutate(mod = "M3", .before=1) |> filter(term == "hensatiper10_latest"),
  tidy(mod4, conf.int = TRUE) |> mutate(mod = "M4", .before=1) |> filter(term == "hensatiper10_latest"),
  tidy(mod5, conf.int = TRUE) |> mutate(mod = "M5", .before=1) |> filter(term == "hensatiper10_latest")
) |> 
  mutate(p.value = scales::number(p.value,accuracy=0.0001))


#件数
fmla6<- "tks_total ~ hensatiper10_latest +  tkn_kensu| industry"
mod6 <- feols(as.formula(fmla6), data = dat, cluster = ~industry)

# 標準化版--------------------------
dat <- dat |>
  mutate(tks_total_z     = as.numeric(scale(`tks_total`)),
         hensati_z       = as.numeric(scale(hensati_latest)),
         log_ta_z        = as.numeric(scale(log_ta)),
         log_n_z         = as.numeric(scale(log_n)),
         roa_z           = as.numeric(scale(roa)),
         roa_win_z       = as.numeric(scale(roa_win)),
         salary_z        = as.numeric(scale(salary_mn)),
         kinzoku_z       = as.numeric(scale(employee_tenure)))



write_rds(dat,"middledata/data_for_sensitivityanalysis.rds")

fmla0z <- "tks_total_z ~ hensati_z " 
fmla1z <- "tks_total_z ~ hensati_z | industry"
fmla2z <- "tks_total_z ~ hensati_z + log_ta_z + log_n_z | industry"
fmla3z <- "tks_total_z ~ hensati_z + log_ta_z + log_n_z + roa_win_z | industry"
fmla4z <- "tks_total_z ~ hensati_z + log_ta_z + log_n_z + roa_win_z + n4int | industry"
fmla5z <- "tks_total_z ~ hensati_z + log_ta_z + log_n_z + roa_win_z + n4int + kinzoku_z +  salary_z | industry"

mod0_z <- feols(as.formula(fmla0z), data = dat, cluster = ~industry)
mod1_z <- feols(as.formula(fmla1z), data = dat, cluster = ~industry)
mod2_z <- feols(as.formula(fmla2z), data = dat, cluster = ~industry)
mod3_z <- feols(as.formula(fmla3z), data = dat, cluster = ~industry)
mod4_z <- feols(as.formula(fmla4z), data = dat, cluster = ~industry)
mod5_z <- feols(as.formula(fmla5z), data = dat, cluster = ~industry)

res_z <- bind_rows(
  tidy(mod0_z , conf.int = TRUE) |> mutate(mod = "M0", .before=1) |> filter(term == "hensati_z"),
  tidy(mod1_z , conf.int = TRUE) |> mutate(mod = "M1", .before=1) |> filter(term == "hensati_z"),
  tidy(mod2_z , conf.int = TRUE) |> mutate(mod = "M2", .before=1) |> filter(term == "hensati_z"),
  tidy(mod3_z , conf.int = TRUE) |> mutate(mod = "M3", .before=1) |> filter(term == "hensati_z"),
  tidy(mod4_z , conf.int = TRUE) |> mutate(mod = "M4", .before=1) |> filter(term == "hensati_z"),
  tidy(mod5_z , conf.int = TRUE) |> mutate(mod = "M5", .before=1) |> filter(term == "hensati_z")
) |> 
  mutate(p.value = scales::number(p.value,accuracy=0.0001))


res1 <- resbase |> select(!term) |> rename(`beta` = estimate)
res2 <- res_z |> select(mod, `betaSD`=estimate)

table2 <- res1 |> 
  left_join(res2, by = "mod") |> 
  relocate(mod, beta, betaSD)

print(table2)

table2 |> 
  mutate(mod = str_c("Model ",1:n())) |> 
  knitr::kable(digits = 3) |> 
  clipr::write_clip()


sd(dat$hensati_latest)

#一応、Robustnessのチェック：問題なし。
# install.packages("clubSandwich")
library(clubSandwich)
m5_lm <- lm(tks_total ~ hensatiper10_latest + log_ta + log_n + roa_win +
              n4int + employee_tenure + salary_mn + factor(industry),
            data = dat)

coef_test(m5_lm, vcov = "CR2", cluster = dat$industry)
conf_int(m5_lm, vcov = "CR2", cluster = dat$industry)


#モデルチェック
library(fixest)

library(fixest)
library(splines)

feols_check <- function(model, data,
                        exposure,                 # 主要曝露の係数名(必須)
                        cluster      = NULL,      # 例: ~latest_gyosyu(SEを主解析と揃える)
                        spline_df    = 3,         # 自然スプラインの自由度
                        vif_threshold = 10,
                        dfbetas_cut  = NULL,
                        qqplot       = FALSE,
                        spline_plot  = FALSE,
                        dfbetas_on = "exposure"   # "exposure" / "any" / 係数名ベクトル
                        ) {
  
  stopifnot(inherits(model, "fixest"), exposure %in% names(coef(model)))
  
  lin_fml <- tryCatch(formula(model, type = "linear"), error = function(e) model$fml)
  y_chr   <- deparse(lin_fml[[2]])
  x_terms <- attr(terms(lin_fml), "term.labels")
  fe_vars <- model$fixef_vars
  fe_str  <- if (length(fe_vars)) paste("|", paste(fe_vars, collapse = " + ")) else ""
  fe_fac  <- if (length(fe_vars)) paste0("factor(", fe_vars, ")") else character(0)
  
  num_preds  <- x_terms[vapply(x_terms,
                               function(v) v %in% names(data) && is.numeric(data[[v]]), logical(1))]
  cont_preds <- num_preds[vapply(num_preds,
                                 function(v) length(unique(data[[v]])) > 10, logical(1))]
  
  vars_all <- unique(c(all.vars(lin_fml), fe_vars))
  adat <- data[stats::complete.cases(data[, vars_all, drop = FALSE]), , drop = FALSE]
  n    <- nrow(adat)
  if (is.null(dfbetas_cut)) dfbetas_cut <- 2 / sqrt(n)
  
  base_fml <- stats::as.formula(paste(y_chr, "~", paste(x_terms, collapse=" + "), fe_str))
  m0   <- feols(base_fml, data = adat, warn = FALSE, notes = FALSE)
  
  ct_of <- function(m) as.data.frame(
    (if (is.null(cluster)) summary(m) else summary(m, cluster = cluster))$coeftable)
  b0  <- ct_of(m0)[exposure, "Estimate"]
  se0 <- ct_of(m0)[exposure, "Std. Error"]
  
  ## [1] VIF ----------------------------------------------------------
  vif_tbl <- do.call(rbind, lapply(num_preds, function(v) {
    rhs <- c(setdiff(num_preds, v), setdiff(x_terms, num_preds), fe_fac)
    r2  <- summary(lm(reformulate(rhs, response = v), data = adat))$r.squared
    data.frame(variable = v, VIF = 1/(1-r2), flag = (1/(1-r2)) > vif_threshold)
  }))
  
  ## [2] 関数形: 自然スプライン --------------------------------------
  sub <- function(vec, old, new) replace(vec, vec == old, new)
  
  ## (A) 各交絡因子をスプライン化 → 曝露推定値の安定性
  cov_sp <- setdiff(cont_preds, exposure)
  covA <- if (length(cov_sp))
    do.call(rbind, lapply(c(as.list(cov_sp), list(cov_sp)), function(vs) {
      rhs <- x_terms
      for (v in vs) rhs <- sub(rhs, v, sprintf("ns(%s, %d)", v, spline_df))
      m  <- feols(as.formula(paste(y_chr, "~", paste(rhs, collapse=" + "), fe_str)),
                  data = adat, warn = FALSE, notes = FALSE)
      ct <- ct_of(m)
      data.frame(splined = if (length(vs) == 1) vs else "ALL covariates",
                 est_exposure = ct[exposure, "Estimate"],
                 se_exposure  = ct[exposure, "Std. Error"],
                 pct_change   = 100 * (ct[exposure, "Estimate"] - b0) / b0)
    })) else NULL
  
  ## (B) 曝露をスプライン化 → 非線形性検定 + IQR効果の比較
  rhsB <- sub(x_terms, exposure, sprintf("ns(%s, %d)", exposure, spline_df))
  mB   <- feols(as.formula(paste(y_chr, "~", paste(rhsB, collapse=" + "), fe_str)),
                data = adat, warn = FALSE, notes = FALSE)
  rss0 <- sum(resid(m0)^2); rssB <- sum(resid(mB)^2)
  dfn  <- spline_df - 1
  dfd  <- degrees_freedom(mB, type = "resid")
  Fst  <- ((rss0 - rssB)/dfn) / (rssB/dfd)
  pval <- stats::pf(Fst, dfn, dfd, lower.tail = FALSE)
  
  qx      <- stats::quantile(adat[[exposure]], c(.25, .75), na.rm = TRUE)
  iqr_lin <- unname(b0 * (qx[2] - qx[1]))
  iqr_sp  <- tryCatch({
    nd <- adat[c(1, 1), , drop = FALSE]; nd[[exposure]] <- qx
    pr <- predict(mB, newdata = nd); unname(pr[2] - pr[1])
  }, error = function(e) NA_real_)
  
  exp_lin <- list(nonlin_F = Fst, nonlin_p = pval, df = c(dfn, dfd),
                  iqr_effect_linear = iqr_lin, iqr_effect_spline = iqr_sp)
  
  if (spline_plot) tryCatch({
    g  <- seq(min(adat[[exposure]]), max(adat[[exposure]]), length.out = 100)
    nd <- adat[rep(1, 100), , drop = FALSE]; nd[[exposure]] <- g
    plot(g, predict(mB, newdata = nd), type = "l", lwd = 2,
         xlab = exposure, ylab = paste("predicted", y_chr),
         main = "exposure-response: spline vs linear")
    lines(g, predict(m0, newdata = nd), lty = 2, col = "red")
    legend("topleft", c("spline","linear"), lty = c(1,2), col = c("black","red"), bty = "n")
  }, error = function(e) message("plot skipped: ", conditionMessage(e)))
  
  ## [3] DFBETAS + 影響観測を除外した感度分析 ------------------------
  lm_fit <- lm(reformulate(c(x_terms, fe_fac), response = y_chr), data = adat)
  db  <- as.data.frame(dfbetas(lm_fit))
  foc <- intersect(num_preds, colnames(db))
  dfb_tbl <- data.frame(variable = foc,
                        max_abs  = vapply(foc, function(v) max(abs(db[[v]])), numeric(1)),
                        n_exceed = vapply(foc, function(v) sum(abs(db[[v]]) > dfbetas_cut), integer(1)),
                        cutoff   = round(dfbetas_cut, 4), row.names = NULL)
  
  ## どの係数のDFBETASで除外するか
  on_cols <- if (identical(dfbetas_on, "any")) foc
  else if (identical(dfbetas_on, "exposure")) exposure
  else dfbetas_on
  flagged <- if (length(on_cols) == 1) abs(db[[on_cols]]) > dfbetas_cut
  else apply(abs(db[, on_cols, drop = FALSE]) > dfbetas_cut, 1, any)
  n_drop  <- sum(flagged)
  
  ## 同一モデル(feols仕様)を除外後に再推定
  m_red <- feols(base_fml, data = adat[!flagged, , drop = FALSE],
                 warn = FALSE, notes = FALSE)
  ct_r  <- ct_of(m_red)
  dfb_sens <- data.frame(
    sample       = c("full", paste0("drop |DFBETAS|>cut @ ", paste(on_cols, collapse="/"))),
    n            = c(n, n - n_drop),
    n_dropped    = c(0L, n_drop),
    est_exposure = c(b0, ct_r[exposure, "Estimate"]),
    se_exposure  = c(se0, ct_r[exposure, "Std. Error"]),
    p_exposure   = c(ct_of(m0)[exposure, "Pr(>|t|)"], ct_r[exposure, "Pr(>|t|)"]),
    row.names    = NULL)
  dfb_sens$pct_change <- 100 * (dfb_sens$est_exposure - b0) / b0
  
  ## [4] 残差正規性 ---------------------------------------------------
  r <- resid(m0)
  norm_out <- list(
    shapiro_p = if (length(r) <= 5000) shapiro.test(r)$p.value else NA_real_,
    skewness  = mean((r-mean(r))^3)/sd(r)^3,
    excess_kurtosis = mean((r-mean(r))^4)/sd(r)^4 - 3)
  if (qqplot) { qqnorm(r); qqline(r) }
  
  ## [5] cluster vs HC1 ----------------------------------------------
  cl <- as.data.frame(summary(model)$coeftable)
  hc <- as.data.frame(summary(model, vcov = "hetero")$coeftable)
  se_tbl <- data.frame(variable = rownames(cl),
                       estimate = cl[,1], se_cluster = cl[,2], p_cluster = cl[,4],
                       se_HC1 = hc[,2], p_HC1 = hc[,4], row.names = NULL)
  
  structure(list(n = n, exposure = exposure, baseline = c(est = b0, se = se0),
                 vif = vif_tbl, covariate_robustness = covA,
                 exposure_linearity = exp_lin, dfbetas = dfb_tbl,
                 dfbetas = dfb_tbl, dfbetas_sensitivity = dfb_sens,
                 normality = norm_out, se_compare = se_tbl),
            class = "feols_check")
}

print.feols_check <- function(x, ...) {
  cat(sprintf("feols check  (N=%d, exposure=%s: est=%.4f, se=%.4f)\n",
              x$n, x$exposure, x$baseline["est"], x$baseline["se"]))
  cat("\n[1] VIF\n"); print(x$vif, row.names = FALSE)
  cat("\n[2A] 曝露推定値の安定性(交絡因子をスプライン化)\n")
  if (!is.null(x$covariate_robustness)) print(x$covariate_robustness, row.names = FALSE)
  el <- x$exposure_linearity
  cat(sprintf("\n[2B] 曝露の非線形性: F(%d,%d)=%.2f, p=%.3f | IQR効果 線形=%.4f / spline=%.4f\n",
              el$df[1], el$df[2], el$nonlin_F, el$nonlin_p,
              el$iqr_effect_linear, el$iqr_effect_spline))
  cat("\n[3] DFBETAS\n"); print(x$dfbetas, row.names = FALSE)
  cat("\n[3b] 影響観測を除外した感度分析\n"); print(x$dfbetas_sensitivity, row.names = FALSE)
  cat(sprintf("\n[4] Residuals: Shapiro p=%.3g | skew=%.2f | exkurt=%.2f\n",
              x$normality$shapiro_p, x$normality$skewness, x$normality$excess_kurtosis))
  cat("\n[5] cluster vs HC1\n"); print(x$se_compare, row.names = FALSE)
  invisible(x)
}


colnames(dat)

# 使い方:
# feols_check(M5, data = your_df, exposure = "hensatiper10_latest",
#             cluster = ~latest_gyosyu, spline_plot = TRUE)

fmla5 <- "tks_total ~ hensatiper10_latest + log_ta + log_n + roa_win + n4int + employee_tenure +  salary_mn| industry"
mod5 <- feols(as.formula(fmla5), data = dat, cluster = ~industry)

chk5 <- feols_check(mod5, data = dat,
                    exposure = "hensatiper10_latest",
                    dfbetas_on = "any",
                    cluster  = ~industry,
                    qqplot   = TRUE)

library(clubSandwich)
lm_fit <- lm(tks_total ~ hensatiper10_latest + log_ta + log_n + roa_win +
               n4int + employee_tenure + salary_mn + factor(industry), data = dat)
coef_test(lm_fit, vcov = "CR2", cluster = dat$industry)["hensatiper10_latest", ]

print(chk5)
