# LIBRERIE E IMPORT
library(readxl)
library(tidyverse)
library(dplyr)
library(PerformanceAnalytics)
library(xts)
library(broom)

# Import dati
CP <- read_excel("AZIENDE_CP_REFINITIVE.xlsx")
OP <- read_excel("AZIENDE_OP_REFINITIVE.xlsx")
MV <- read_excel("AZIENDE_MV_REFINITIVE.xlsx")

LP <- read_excel("LONG_PICKS_LLM.xlsx")
SP <- read_excel("SHORT_PICKS_LLM.xlsx")

OPSP500 <- read_excel("SP500_OP_REFINITIVE.xlsx")
CPSP500 <- read_excel("SP500_CP_REFINITIVE.xlsx")
MT <- read_excel("MARKET_TIMING.xlsx")

# COSTRUZIONE DATASET BASE
aziende <- OP %>%
  inner_join(CP, by = c("Date", "Ticker", "Company")) %>%
  inner_join(MV, by = c("Date", "Ticker", "Company")) %>%
  mutate(return_oc = (Closing_Price - Opening_Price) / Opening_Price)

aziende_short <- aziende %>%
  mutate(return_ocshort = -return_oc)

sp500 <- OPSP500 %>%
  inner_join(CPSP500, by = "Date") %>%
  mutate(return_oc = (SP500_ClosingPrice - SP500_OpeningPrice) / SP500_OpeningPrice)

# HIT RATIO
# LONG
long_eval <- LP %>%
  inner_join(aziende, by = c("Date", "Ticker")) %>%
  mutate(correct = ifelse(
    Predicted_Sign == "Positive" & return_oc > 0, 1,
    ifelse(Predicted_Sign == "Negative" & return_oc < 0, 1, 0)
  ))
hit_long <- mean(long_eval$correct, na.rm = TRUE)

# SHORT
short_eval <- SP %>%
  inner_join(aziende_short, by = c("Date", "Ticker")) %>%
  mutate(correct = ifelse(
    Predicted_Sign == "Negative" & return_oc < 0, 1,
    ifelse(Predicted_Sign == "Positive" & return_oc > 0, 1, 0)
  ))
hit_short <- mean(short_eval$correct, na.rm = TRUE)

# TEST SIGNIFICATIVITÀ vs 0.5
test_long <- t.test(long_eval$correct, mu = 0.5)
test_short <- t.test(short_eval$correct, mu = 0.5)

print(test_long)
print(test_short)

# MARKET
market_eval <- MT %>%
  inner_join(sp500, by = "Date") %>%
  mutate(correct =
           ifelse(SP500_Sign == "Positive" & return_oc > 0, 1,
                  ifelse(SP500_Sign == "Negative" & return_oc < 0, 1, 0)))

market_eval %>%
  group_by(Model) %>%
  summarise(
    hit_ratio = mean(correct, na.rm = TRUE),
    successi = sum(correct, na.rm = TRUE),
    totale = n()
  )
# TEST SIGNIFICATIVITÀ MARKET TIMING
market_test <- market_eval %>%
  group_by(Model) %>%
  summarise(
    successi = sum(correct, na.rm = TRUE),
    totale = n()
  ) %>%
  rowwise() %>%
  mutate(
    p_value = binom.test(successi, totale, p = 0.5)$p.value
  )

print(market_test)
# COSTRUZIONE PORTAFOGLI
long_eval <- LP %>%
  inner_join(aziende, by = c("Date", "Ticker")) %>%
  group_by(Date, Model) %>%
  filter(n() == 3) %>%
  ungroup()
# Equally Weighted
portfolio_eq <- long_eval %>%
  group_by(Date, Model) %>%
  summarise(
    return_portfolio = mean(return_oc, na.rm = TRUE),
    .groups = "drop"
  )
# Signal Weighted
portfolio_signal <- long_eval %>%
  group_by(Date, Model) %>%
  mutate(weight = case_when(
    Rank == 1 ~ 3,
    Rank == 2 ~ 2,
    Rank == 3 ~ 1
  )) %>%
  mutate(weight = weight / sum(weight)) %>%
  summarise(
    return_portfolio = sum(weight * return_oc, na.rm = TRUE),
    .groups = "drop"
  )
#Value Weighted
portfolio_value <- long_eval %>%
  group_by(Date, Model) %>%
  mutate(weight = Market_Value / sum(Market_Value)) %>%
  summarise(
    return_portfolio = sum(weight * return_oc, na.rm = TRUE),
    .groups = "drop"
  )
#ANALISI MAGNITUDINE
price_pred <- read_excel("PRICE_DIRECTION_LLM.xlsx")

prices_t1 <- CP %>%
  arrange(Ticker, Date) %>%
  group_by(Ticker) %>%
  mutate(Closing_Price_t1 = lead(Closing_Price)) %>%
  ungroup()

reg_data <- price_pred %>%
  inner_join(prices_t1, by = c("Date", "Ticker")) %>%
  mutate(
    return_real = (Closing_Price_t1 - Closing_Price) / Closing_Price,
    return_pred = (Predicted_Price - Closing_Price) / Closing_Price
  ) %>%
  drop_na()

model <- lm(return_real ~ return_pred, data = reg_data)
summary(model)

# TEST coefficiente = 0
coeftest <- summary(model)$coefficients
print(coeftest)

# R-squared
cat("R-squared:", summary(model)$r.squared, "\n")

# ANALISI PER MODELLO
library(broom)

reg_results <- reg_data %>%
  group_by(Model) %>%
  do(tidy(lm(return_real ~ return_pred, data = .)))

print(reg_results)
#STATISTICHE E SHARPE RATIO 
summary_stats <- function(data) {
  data %>%
    group_by(Model) %>%
    summarise(
      mean_return = mean(return_portfolio),
      sd_return = sd(return_portfolio),
      skewness = PerformanceAnalytics::skewness(return_portfolio),
      kurtosis = PerformanceAnalytics::kurtosis(return_portfolio),
      p5 = quantile(return_portfolio, 0.05),
      p25 = quantile(return_portfolio, 0.25),
      p75 = quantile(return_portfolio, 0.75),
      p95 = quantile(return_portfolio, 0.95),
      .groups = "drop"
    )
}

summary_eq <- summary_stats(portfolio_eq)
summary_signal <- summary_stats(portfolio_signal)
summary_value <- summary_stats(portfolio_value)

summary_eq
summary_signal
summary_value

rf <- 0  # intra-day

sharpe_stats <- function(data) {
  data %>%
    group_by(Model) %>%
    summarise(
      mean_ret = mean(return_portfolio, na.rm = TRUE),
      sd_ret = sd(return_portfolio, na.rm = TRUE),
      sharpe = (mean_ret / sd_ret),
      t_stat = (mean_ret / sd_ret) * sqrt(n()),
      n_obs = n(),
      .groups = "drop"
    )
}
sharpe_eq <- sharpe_stats(portfolio_eq)
sharpe_signal <- sharpe_stats(portfolio_signal)
sharpe_value <- sharpe_stats(portfolio_value)

sharpe_eq
sharpe_signal
sharpe_value
# GRAFICI PERFORMANCE
# EQUALLY WEIGHTED
temp_eq <- portfolio_eq %>%
  pivot_wider(names_from = Model, values_from = return_portfolio) %>%
  arrange(Date)

xts_eq <- xts(
  as.matrix(temp_eq[,-1]),
  order.by = as.Date(temp_eq$Date)
)

xts_eq <- na.omit(xts_eq)

charts.PerformanceSummary(
  xts_eq,
  main = "Equally Weighted Portfolio Performance"
)

#  SIGNAL WEIGHTED 
temp_signal <- portfolio_signal %>%
  pivot_wider(names_from = Model, values_from = return_portfolio) %>%
  arrange(Date)

xts_signal <- xts(
  as.matrix(temp_signal[,-1]),
  order.by = as.Date(temp_signal$Date)
)

xts_signal <- na.omit(xts_signal)

charts.PerformanceSummary(
  xts_signal,
  main = "Signal Weighted Portfolio Performance"
)

# VALUE WEIGHTED 
temp_value <- portfolio_value %>%
  pivot_wider(names_from = Model, values_from = return_portfolio) %>%
  arrange(Date)

xts_value <- xts(
  as.matrix(temp_value[,-1]),
  order.by = as.Date(temp_value$Date)
)

xts_value <- na.omit(xts_value)

charts.PerformanceSummary(
  xts_value,
  main = "Value Weighted Portfolio Performance"
)

#UNIONE DEI PORTAFOGLI
portfolio_all <- portfolio_eq %>%
  mutate(type = "EW") %>%
  bind_rows(
    portfolio_signal %>% mutate(type = "SW"),
    portfolio_value %>% mutate(type = "VW")
  )
#SHARPE COMPARATIVO
portfolio_all %>%
  group_by(type, Model) %>%
  summarise(
    sharpe = mean(return_portfolio) / sd(return_portfolio),
    t_stat = sharpe * sqrt(n()),
    n_obs = n(),
    .groups = "drop"
  )
#CUMULATIVE RETURNS COMPARATI
portfolio_all_cum <- portfolio_all %>%
  group_by(type, Model) %>%
  arrange(Date) %>%
  mutate(cum_return = cumprod(1 + return_portfolio)) %>%
  ungroup()
#GRAFICO COMPLETO
ggplot(portfolio_all_cum, aes(x = Date, y = cum_return, color = Model)) +
  geom_line(size = 1) +
  facet_wrap(~type) +
  theme_minimal() +
  labs(
    title = "Cumulative Returns by Portfolio Type",
    y = "Cumulative Return",
    x = "Date"
  )
#CONFORNTO PESI
ggplot(portfolio_all_cum, aes(x = Date, y = cum_return, color = type)) +
  geom_line(size = 1) +
  facet_wrap(~Model) +
  theme_minimal() +
  labs(
    title = "Portfolio Performance by Weighting Scheme"
  )
#TABELLA 
portfolio_all %>%
  group_by(type, Model) %>%
  summarise(
    mean = mean(return_portfolio),
    sd = sd(return_portfolio),
    sharpe = mean / sd,
    .groups = "drop"
  )
#FAMA-FRENCH (ALPHA)
ff <- read_excel("F-F_Research_Data_Factors_daily.xlsx") %>%
  mutate(
    Date = as.Date(Date),
    Mkt_RF = `Mkt-RF` / 100,
    SMB = SMB / 100,
    HML = HML / 100,
    RF = RF / 100
  )

mom <- read_excel("F-F_Momentum_Factor_daily.xlsx") %>%
  mutate(
    Date = as.Date(Date),
    MOM = Mom / 100
  )

ff_all <- ff %>%
  inner_join(mom, by = "Date")
#REGRESSIONI
run_alpha <- function(data) {
  data %>%
    inner_join(ff_all, by = "Date") %>%
    mutate(excess_return = return_portfolio - RF) %>%
    group_by(Model) %>%
    do(tidy(lm(excess_return ~ Mkt_RF + SMB + HML + MOM, data = .)))
}

alpha_eq <- run_alpha(portfolio_eq)
alpha_signal <- run_alpha(portfolio_signal)
alpha_value <- run_alpha(portfolio_value)
alpha_eq
alpha_signal
alpha_value
# PORTAFOGLIO FISSO 5 TITOLI 
# IMPORT DATI

FP <- read_excel("FIXED_UNIVERSE_PRICE_LLM.xlsx")
FS <- read_excel("FIXED_UNIVERSE_SIGN_LLM.xlsx")

# MERGE PREVISIONI

fixed_llm <- FS %>%
  inner_join(FP, by = c("Date", "Model", "Ticker"))

# MERGE CON DATI MERCATO

fixed_data <- fixed_llm %>%
  inner_join(aziende, by = c("Date", "Ticker"))

# COSTRUZIONE STRATEGIA
fixed_data <- fixed_data %>%
  mutate(
    position = ifelse(Predicted_Sign == "Positive", 1, -1),
    strategy_ret = position * return_oc
  )

# PESI S&P500 (VALUE WEIGHTED)

fixed_data <- fixed_data %>%
  group_by(Date) %>%
  mutate(weight_sp = Market_Value / sum(Market_Value)) %>%
  ungroup()

# PORTAFOGLIO GIORNALIERO

portfolio_fixed <- fixed_data %>%
  group_by(Date, Model) %>%
  summarise(
    return_portfolio = sum(weight_sp * strategy_ret, na.rm = TRUE),
    .groups = "drop"
  )

# STATISTICHE
stats_fixed <- portfolio_fixed %>%
  group_by(Model) %>%
  summarise(
    mean_return = mean(return_portfolio, na.rm = TRUE),
    sd_return = sd(return_portfolio, na.rm = TRUE),
    sharpe = mean_return / sd_return,
    n_obs = n(),
    .groups = "drop"
  )

print(stats_fixed)

# SHARPE 
sharpe_fixed <- sharpe_stats(portfolio_fixed)
print(sharpe_fixed)

portfolio_fixed_wide <- portfolio_fixed %>%
  pivot_wider(names_from = Model, values_from = return_portfolio) %>%
  arrange(Date)

portfolio_fixed_xts <- xts(
  as.matrix(portfolio_fixed_wide[, -1]),
  order.by = portfolio_fixed_wide$Date
)

portfolio_fixed_xts <- na.omit(portfolio_fixed_xts)

# GRAFICI PERFORMANCE

charts.PerformanceSummary(portfolio_fixed_xts)
SharpeRatio(portfolio_fixed_xts, Rf = 0)

# RENDIMENTI CUMULATI (COMPARAZIONE)

portfolio_fixed_cum <- portfolio_fixed %>%
  arrange(Date) %>%
  group_by(Model) %>%
  mutate(cum_return = cumprod(1 + return_portfolio)) %>%
  ungroup()

ggplot(portfolio_fixed_cum, aes(x = Date, y = cum_return, color = Model)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    title = "Cumulative Returns - Fixed Portfolio (5 Titoli)",
    y = "Cumulative Return",
    x = "Date"
  )

# DISTRIBUZIONE RENDIMENTI

ggplot(portfolio_fixed, aes(x = return_portfolio, fill = Model)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  theme_minimal() +
  labs(title = "Distribution of Returns by Model")


alpha_fixed <- run_alpha(portfolio_fixed)
print(alpha_fixed)

# REGRESSIONE MAGNITUDINE

fixed_reg <- fixed_data %>%
  mutate(
    return_pred = (Predicted_Price - Opening_Price) / Opening_Price
  )

model_fixed <- lm(return_oc ~ return_pred, data = fixed_reg)
summary(model_fixed)

print(unique(fixed_data$Ticker))
