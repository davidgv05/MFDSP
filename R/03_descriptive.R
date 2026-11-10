# Descriptive analysis: summary statistics, correlations, rolling correlation,
# rolling volatility and drawdowns for Bitcoin, the Nasdaq-100 and gold.
# Input:  data/processed/returns.csv
# Output: output/tables/*.csv, output/figures/*.png

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(zoo)
library(patchwork)
library(here)

d <- read_csv(here("data", "processed", "returns.csv"), show_col_types = FALSE)

dir.create(here("output", "tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

# Shared plot settings -----------------------------------------------------------
asset_labels <- c(r_BTC = "Bitcoin", r_NDX = "Nasdaq-100", r_GOLD = "Gold")
asset_colours <- c("Bitcoin" = "#E69F00", "Nasdaq-100" = "#0072B2", "Gold" = "#8C6D1F")

events <- tibble(
  start = as.Date(c("2020-02-19", "2022-03-16")),
  end   = as.Date(c("2020-03-23", "2023-07-26")),
  label = c("COVID crash", "Fed rate hikes")
)
etf_date <- as.Date("2024-01-11")

add_events <- function(p, y_label) {
  p +
    geom_rect(data = events, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "grey85", alpha = 0.6) +
    geom_text(data = events, aes(x = start, y = y_label, label = label),
              inherit.aes = FALSE, hjust = 0, vjust = 1, size = 3, colour = "grey30") +
    geom_vline(xintercept = etf_date, linetype = "dotted", colour = "grey40") +
    annotate("text", x = etf_date, y = y_label, label = " Spot BTC ETFs",
             hjust = 0, vjust = 1, size = 3, colour = "grey30")
}

# ---- Step 1: summary statistics per asset ------------------------------------
long <- d |>
  select(date, rf, r_BTC, r_NDX, r_GOLD) |>
  pivot_longer(c(r_BTC, r_NDX, r_GOLD), names_to = "asset", values_to = "r") |>
  mutate(asset = asset_labels[asset])

stats <- long |>
  group_by(asset) |>
  summarise(
    n_days         = n(),
    mean_daily_pct = mean(r),
    ann_return_pct = mean(r) * 252,
    ann_vol_pct    = sd(r) * sqrt(252),
    sharpe         = mean(r - rf) * 252 / (sd(r) * sqrt(252)),
    worst_day_pct  = min(r),
    best_day_pct   = max(r),
    neg_days_pct   = mean(r < 0) * 100,
    .groups = "drop"
  )

max_dd <- tibble(
  asset = c("Bitcoin", "Nasdaq-100", "Gold"),
  max_drawdown_pct = c(min(d$BTC  / cummax(d$BTC)  - 1),
                       min(d$NDX  / cummax(d$NDX)  - 1),
                       min(d$GOLD / cummax(d$GOLD) - 1)) * 100
)

stats <- stats |> left_join(max_dd, by = "asset")
print(stats, width = Inf)
write_csv(stats, here("output", "tables", "summary_stats.csv"))

# ---- Step 2: correlations, overall and by sub-sample ---------------------------
cors <- function(df, label) {
  tibble(sample   = label,
         n_days   = nrow(df),
         BTC_NDX  = cor(df$r_BTC,  df$r_NDX),
         GOLD_NDX = cor(df$r_GOLD, df$r_NDX),
         BTC_GOLD = cor(df$r_BTC,  df$r_GOLD))
}

cor_tab <- bind_rows(
  cors(d,                       "All days 2017-2025"),
  cors(filter(d, post2020 == 0), "2017-2019"),
  cors(filter(d, post2020 == 1), "2020-2025"),
  cors(filter(d, post_etf == 1), "Since spot ETFs (2024-)"),
  cors(filter(d, D5 == 0),       "Normal days (not worst 5%)"),
  cors(filter(d, D5 == 1),       "Nasdaq worst 5% days")
)
print(cor_tab, width = Inf)
write_csv(cor_tab, here("output", "tables", "correlations.csv"))

# ---- Step 3: 90-day rolling correlation (main figure) -------------------------
window_cor <- 90
roll_cor <- function(x, y) {
  rollapplyr(cbind(x, y), width = window_cor, by.column = FALSE, fill = NA,
             FUN = function(m) cor(m[, 1], m[, 2]))
}

rc <- d |>
  transmute(date,
            `Bitcoin vs Nasdaq-100` = roll_cor(r_BTC,  r_NDX),
            `Gold vs Nasdaq-100`    = roll_cor(r_GOLD, r_NDX),
            `Bitcoin vs Gold`       = roll_cor(r_BTC,  r_GOLD)) |>
  pivot_longer(-date, names_to = "pair", values_to = "cor") |>
  filter(!is.na(cor))

fig_cor <- ggplot(rc, aes(date, cor, colour = pair)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(linewidth = 0.6)
fig_cor <- add_events(fig_cor, y_label = 1) +
  scale_colour_manual(values = c("Bitcoin vs Nasdaq-100" = "#D55E00",
                                 "Gold vs Nasdaq-100"    = "#0072B2",
                                 "Bitcoin vs Gold"       = "#009E73")) +
  coord_cartesian(ylim = c(-0.6, 1)) +
  labs(title = "Rolling correlation of daily returns",
       subtitle = paste0(window_cor, "-trading-day window, 2017-2025"),
       x = NULL, y = "Correlation", colour = NULL,
       caption = "Data: FRED (CBBTCUSD, NASDAQ100), Yahoo Finance (GLD)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(here("output", "figures", "fig1_rolling_correlation.png"),
       fig_cor, width = 9, height = 5, dpi = 300)

# ---- Step 4: 60-day rolling volatility ----------------------------------------
window_vol <- 60
rv <- long |>
  group_by(asset) |>
  arrange(date, .by_group = TRUE) |>
  mutate(vol = rollapplyr(r, width = window_vol, FUN = sd, fill = NA) * sqrt(252)) |>
  ungroup() |>
  filter(!is.na(vol))

fig_vol <- ggplot(rv, aes(date, vol, colour = asset)) +
  geom_line(linewidth = 0.6)
fig_vol <- add_events(fig_vol, y_label = max(rv$vol)) +
  scale_colour_manual(values = asset_colours) +
  labs(title = "Rolling annualised volatility",
       subtitle = paste0(window_vol, "-trading-day window, standard deviation of daily log returns x sqrt(252)"),
       x = NULL, y = "Volatility (% per year)", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(here("output", "figures", "fig2_rolling_volatility.png"),
       fig_vol, width = 9, height = 5, dpi = 300)

# ---- Step 5: drawdowns ---------------------------------------------------------
dd <- d |>
  transmute(date,
            Bitcoin      = (BTC  / cummax(BTC)  - 1) * 100,
            `Nasdaq-100` = (NDX  / cummax(NDX)  - 1) * 100,
            Gold         = (GOLD / cummax(GOLD) - 1) * 100) |>
  pivot_longer(-date, names_to = "asset", values_to = "drawdown")

fig_dd <- ggplot(dd, aes(date, drawdown, colour = asset)) +
  geom_line(linewidth = 0.6)
fig_dd <- add_events(fig_dd, y_label = 0) +
  scale_colour_manual(values = asset_colours) +
  labs(title = "Drawdown from previous peak",
       subtitle = "Loss relative to the highest price reached so far",
       x = NULL, y = "Drawdown (%)", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(here("output", "figures", "fig3_drawdowns.png"),
       fig_dd, width = 9, height = 5, dpi = 300)

# ---- Step 6: combined comparison of correlation and volatility ----------------
# Table: one row per period, volatility of each asset + the three correlations
period_stats <- function(df, label) {
  tibble(period       = label,
         n_days       = nrow(df),
         vol_BTC      = sd(df$r_BTC)  * sqrt(252),
         vol_NDX      = sd(df$r_NDX)  * sqrt(252),
         vol_GOLD     = sd(df$r_GOLD) * sqrt(252),
         cor_BTC_NDX  = cor(df$r_BTC,  df$r_NDX),
         cor_GOLD_NDX = cor(df$r_GOLD, df$r_NDX),
         cor_BTC_GOLD = cor(df$r_BTC,  df$r_GOLD))
}

comparison <- bind_rows(
  period_stats(filter(d, post2020 == 0),                 "2017-2019"),
  period_stats(filter(d, post2020 == 1, post_etf == 0),  "2020-2023"),
  period_stats(filter(d, post_etf == 1),                 "2024-2025"),
  period_stats(d,                                        "2017-2025 (all)")
) |>
  mutate(across(starts_with("vol"), ~ round(.x, 1)),
         across(starts_with("cor"), ~ round(.x, 2)))

print(comparison, width = Inf)
write_csv(comparison, here("output", "tables", "correlation_volatility_by_period.csv"))

# Figure: rolling correlation (top) and rolling volatility (bottom) on one time axis
fig_combo <-
  (fig_cor + labs(title = "A  Rolling correlation (90 days)", subtitle = NULL, caption = NULL) +
     theme(legend.position = "right")) /
  (fig_vol + labs(title = "B  Rolling volatility (60 days, % per year)", subtitle = NULL) +
     theme(legend.position = "right")) +
  plot_annotation(
    title   = "Correlation and volatility of Bitcoin, the Nasdaq-100 and gold, 2017-2025",
    caption = "Data: FRED (CBBTCUSD, NASDAQ100), Yahoo Finance (GLD)"
  )

ggsave(here("output", "figures", "fig4_correlation_volatility.png"),
       fig_combo, width = 10, height = 8, dpi = 300)

message("Done: 3 tables in output/tables, 4 figures in output/figures")