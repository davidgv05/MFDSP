# Turns the raw downloads into one analysis-ready dataset:
# aligned trading days, daily log returns, supporting series and crash/period dummies.
# Input:  data/raw/*.csv        Output: data/processed/returns.csv


#!!!!!! Known data issue: CBBTCUSD has no value for 2020-09-04 (Nasdaq and gold traded).
# The row is dropped; returns on 2020-09-08 therefore span two trading days.
#Bitcoin price missing on 4 Sep 2020 (FRED/Coinbase); returns for 8 Sep 2020 span two trading days.

library(dplyr)
library(readr)
library(tidyr)
library(here)

read_raw <- function(name) {
  read_csv(here("data", "raw", paste0(name, ".csv")), show_col_types = FALSE) |>
    rename(!!name := value)
}

# ----1: align the three assets to common trading days --------------------
prices_all <- read_raw("NDX") |>
  inner_join(read_raw("BTC"),  by = "date") |>
  inner_join(read_raw("GOLD"), by = "date") |>
  arrange(date)

prices <- prices_all |> filter(!is.na(NDX), !is.na(BTC), !is.na(GOLD))

message("Rows after join: ", nrow(prices_all),
        " | dropped (holiday/missing): ", nrow(prices_all) - nrow(prices),
        " | kept: ", nrow(prices))
stopifnot(!any(duplicated(prices$date)))

# ---- 2: daily log returns in percent -------------------------------------
rets <- prices |>
  mutate(across(c(NDX, BTC, GOLD),
                ~ 100 * (log(.x) - log(lag(.x))),
                .names = "r_{.col}")) |>
  filter(!is.na(r_NDX))

# ----3: sanity checks ----------------------------------------------------
# a) gaps longer than a long weekend may indicate missing data
gaps <- rets |> mutate(gap_days = as.numeric(date - lag(date))) |> filter(gap_days > 4)
message("Gaps > 4 calendar days: ", nrow(gaps))
if (nrow(gaps) > 0) print(gaps |> select(date, gap_days))

# b) the most extreme days per asset -> check a few against the news
for (col in c("r_NDX", "r_BTC", "r_GOLD")) {
  cat("\nMost extreme days for", col, "\n")
  print(rets |> arrange(desc(abs(.data[[col]]))) |> select(date, all_of(col)) |> head(5))
}

# c) summary statistics
print(summary(rets |> select(r_NDX, r_BTC, r_GOLD)))

# ---- 4: supporting series (stress indicator, rates) ----------------------
# Rates and VXN can be missing on single days (different holiday calendars):
# carry the last known value forward.
support <- read_raw("VXN") |>
  full_join(read_raw("TBILL"),    by = "date") |>
  full_join(read_raw("FEDFUNDS"), by = "date") |>
  arrange(date) |>
  fill(VXN, TBILL, FEDFUNDS, .direction = "down")

rets <- rets |>
  left_join(support, by = "date") |>
  mutate(rf = TBILL / 252)   # daily risk-free rate in %, matches return units

# ---- 5: crash and period dummies -----------------------------------------
q <- quantile(rets$r_NDX, probs = c(0.10, 0.05, 0.01))

rets <- rets |>
  mutate(
    D10      = as.integer(r_NDX <= q[1]),
    D5       = as.integer(r_NDX <= q[2]),
    D1       = as.integer(r_NDX <= q[3]),
    post2020 = as.integer(date >= as.Date("2020-01-01")),
    post_etf = as.integer(date >= as.Date("2024-01-11"))  # first trading day of US spot BTC ETFs
  )

message(sprintf("Crash thresholds (Nasdaq daily return): 10%% = %.2f%%, 5%% = %.2f%%, 1%% = %.2f%%",
                q[1], q[2], q[3]))
message("Crash days: D10 = ", sum(rets$D10), ", D5 = ", sum(rets$D5), ", D1 = ", sum(rets$D1))

# ---- 6: save -------------------------------------------------------------
out <- rets |>
  select(date, NDX, BTC, GOLD, r_NDX, r_BTC, r_GOLD,
         VXN, TBILL, FEDFUNDS, rf, D10, D5, D1, post2020, post_etf)

dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
write_csv(out, here("data", "processed", "returns.csv"))
message("Saved ", nrow(out), " rows to data/processed/returns.csv")