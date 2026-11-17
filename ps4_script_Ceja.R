library(tidyverse)
library(fpp3)
library(BVAR)

# Part 2 - A Business Forecasting Horse Race: ARIMA vs. ETS

# 2.1a.
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

# 2.1b.
liquor |>
  autoplot(sales) +
  labs(
    title = "Monthly Retail Sales",
    x = "Year",
    y = "Sales (Millions of Dollars)"
  ) +
  theme_minimal()

liquor |>
  gg_season(log(sales)) +
  labs(
    title = "Seasonal Plot: Monthly Retail Sales",
    x = "Month",
    y = "Log Sales (Millions of Dollars)"
  ) +
  theme_minimal()
# The series shows an upward trend over time, each year being higher than the
# previous year. Sales increase and hit the highest in December, and then drop 
# to the lowest in January. The seasonal swings seem to be growing with the
# level of the series, which means a multiplicative seasonal pattern. It 
# suggests that using the log transformation is appropriate to help stabilize
# the seasonality.

# 2.2a.
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()
# The ARIMA model is ARIMA(1,0,2)(2,1,1)[12] with drift. This means that the
# model applies one seasonal difference but no regular difference. The AR and
# MA terms capture short-term dependencies in the data. The chosen ETS model is
# ETS(A,A,A). The letter indicate that the model uses additive error, additive
# trend, and additive seasonality. 

# 2.2b.
liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)
# The residuals look like white noise with no obvious patterns. However, the
# Ljung-Box test shows that the residuals do not behave like white noise since
# the p-value is less than 0.05, meaning that we reject the null hypothesis. 

# 2.3a.
liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(snaive = SNAIVE(sales),
        ets    = ETS(log(sales)),
        arima  = ARIMA(log(sales)))

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")
# The TS-CV considers 216 forecast origins.

# 2.3b.
liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

accuracy_results <- liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

accuracy_results |>
  knitr::kable(digits = 2)
# Since ARIMA has the lowest RMSE (81.43) and the lowest MAE (65.65), ARIMA is
# the best forecasting model among the three. The two metrics agree that ARIMA
# is the most accurate forecasting model.

# 2.4
liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)

forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)
# The DM test comparing ARIMA and ETS has a test statistic = -2.9062 and a
# p-value = 0.004042. Meaning we reject the null hypothesis that the both models
# have equal forecast accuracy. The DM test comparing ARIMA and the seasonal
# random walk benchmark has a test statistic = -10.755 and a p-value = 2.2e-16.
# This provides more evidence that the models perform differently in accuracy.

# Based on the results, I would choose the ARIMA model because it has the lowest
# RMSE and MAE. It also performs better than both the ETS model and seasonal 
# random walk benchmark.





