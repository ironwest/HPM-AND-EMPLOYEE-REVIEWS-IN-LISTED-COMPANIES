library(tidyverse)
library(fixest)
library(broom)

dat <- read_rds("middledata/analye_this.rds")

label_map <- c(
  wlb = "Work–life balance",   women = "Work environment for women",
  benefit = "Employee benefits", gap = "Expectation–reality gap",
  kyuyo = "Compensation level", ceoattract = "CEO appeal",
  future = "Growth and future prospects", interview = "Interview and selection",
  education = "Training and development", employeeattract = "Quality of employees",
  out = "Reasons for leaving", yarigai = "Work fulfillment"
)
facet_order <- names(label_map)

# 主解析サンプル
dat <- dat |>
  filter(!is.na(hensati_latest), !is.na(tks_total), !is.na(total_assets),
         !is.na(roa_win), !is.na(employee_tenure), !is.na(industry)) |>
  mutate(log_ta = log(total_assets), log_n = log(employee_n),
         industry = factor(latest_gyosyu), n4int = as.integer(n4_consecutive),
         hensatiper10_latest = hensati_latest / 10)
cat("解析サンプル:", nrow(dat), "社\n")

run_all <- function(dat, outcome){
  covs_full <- c(outcome, "hensatiper10_latest","log_ta","log_n",
                 "roa_win","n4int","employee_tenure","salary_mn","industry")
  d <- dat |> tidyr::drop_na(dplyr::all_of(covs_full))
  fmlas <- list(
    "Model 1" = "{o} ~ hensatiper10_latest",
    "Model 2" = "{o} ~ hensatiper10_latest | industry",
    "Model 3" = "{o} ~ hensatiper10_latest + log_ta + log_n | industry",
    "Model 4" = "{o} ~ hensatiper10_latest + log_ta + log_n + roa_win | industry",
    "Model 5" = "{o} ~ hensatiper10_latest + log_ta + log_n + roa_win + n4int | industry",
    "Model 6" = "{o} ~ hensatiper10_latest + log_ta + log_n + roa_win + n4int + employee_tenure + salary_mn | industry"
  ) |> purrr::map_chr(~ stringr::str_replace_all(.x, "\\{o\\}", outcome))
  purrr::imap_dfr(fmlas, ~{
    m <- feols(as.formula(.x), data = d, cluster = ~industry)
    broom::tidy(m, conf.int = TRUE) |>
      dplyr::filter(term == "hensatiper10_latest") |>
      dplyr::mutate(mod = .y, nobs = m$nobs, .before = 1)
  })
}

res <- bind_rows(
  run_all(dat,"tks_yarigai")         |> mutate(outcome="yarigai",        .before=1),
  run_all(dat,"tks_kyuyo")           |> mutate(outcome="kyuyo",          .before=1),
  run_all(dat,"tks_education")       |> mutate(outcome="education",      .before=1),
  run_all(dat,"tks_benefit")         |> mutate(outcome="benefit",        .before=1),
  run_all(dat,"tks_interview")       |> mutate(outcome="interview",      .before=1),
  run_all(dat,"tks_future")          |> mutate(outcome="future",         .before=1),
  run_all(dat,"tks_employeeattract") |> mutate(outcome="employeeattract",.before=1),
  run_all(dat,"tks_wlb")             |> mutate(outcome="wlb",            .before=1),
  run_all(dat,"tks_women")           |> mutate(outcome="women",          .before=1),
  run_all(dat,"tks_gap")             |> mutate(outcome="gap",            .before=1),
  run_all(dat,"tks_out")             |> mutate(outcome="out",            .before=1),
  run_all(dat,"tks_ceoattract")      |> mutate(outcome="ceoattract",     .before=1)
)
tk_total <- run_all(dat,"tks_total") |> mutate(outcome="total", .before=1)

# 次元内 N 一定チェック
res |> distinct(outcome, mod, nobs) |>
  group_by(outcome) |> summarise(n_min = min(nobs), n_max = max(nobs), .groups="drop")

res |> 
  knitr::kable(digits = 3) |> 
  clipr::write_clip()

# 作図
plot_df <- res |>
  mutate(mod = factor(mod, levels = paste("Model", 1:6)),
         outcome = factor(outcome, levels = facet_order)) |>
  arrange(outcome, mod)

m6 <- plot_df |>
  filter(mod == "Model 6") |>
  mutate(holm_p = p.adjust(p.value, method = "holm"),
         holm   = if_else(holm_p < 0.05, "p < 0.05 (Holm)", "n.s."))

n_by_dim <- res |> filter(mod == "Model 6") |> distinct(outcome, nobs)
strip_lab <- setNames(
  paste0(label_map[n_by_dim$outcome], "\n(n = ", n_by_dim$nobs, ")"),
  n_by_dim$outcome
)
# facet_wrap(~ outcome, ncol = 4, labeller = as_labeller(strip_lab))
plot_df |> 
  print(n = 72)
p <- ggplot(plot_df, aes(estimate, mod)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey75") +
  geom_path(aes(group = outcome), linewidth = 0.4, colour = "grey55", linetype = "dotted") +
  geom_point(size = 1.1, colour = "grey55") +
  geom_errorbarh(data = m6, aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.5, colour = "grey20") +
  geom_point(data = m6, aes(fill = holm), shape = 21, size = 2.6, stroke = 0.5, colour = "grey20") +
  facet_wrap(~ outcome, ncol = 4, labeller = as_labeller(strip_lab)) +
  scale_fill_manual(values = c("p < 0.05 (Holm)" = "black", "n.s." = "white")) +
  labs(x = expression(beta~"(per 10-point increase in HPM deviation score)"),
       y = NULL, fill = NULL) + #,
       # caption = paste("Model 1 (crude) \u2192 Model 6 (fully adjusted).",
       #                 "Models 1\u20135: point estimates only. Model 6: 95% CI;",
       #                 "filled = p<.05 after Holm correction across 12 dimensions.")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 10),
        plot.caption = element_text(size = 9, colour = "grey40", hjust = 0))

#install.packages("ragg")   # 初回のみ
library(ragg)

# 高品質PNG(投稿でPNG/TIFF可ならこれが最も安全)
ggsave("output/figure2.png", p,
       width = 26.0, height = 20, units = "cm",
       dpi = 600, device = ragg::agg_png, bg = "white")
