library(tidyquant)
library(timetk)
library(dplyr)
library(tidyr)
library(ggplot2)


# Banco de dados
criptomoedas <- tq_get(c("LTC-USD","XRP-USD","USDT-USD","ADA-USD"),
                       from = "2012-01-01",
                       to = "2022-06-21"
) %>%
  select(date, symbol, close) %>%
  mutate(symbol = sub("-USD", "", symbol))
# Gráfico simples
criptomoedas %>%
  plot_time_series(date, close, symbol, .smooth = FALSE)

# Gráfico com log
criptomoedas %>%
  mutate(close_log = log1p(close)) %>%
  plot_time_series(date, close_log, symbol, .smooth = FALSE)

# Grafico padronizado com log
criptomoedas %>%
  group_by(symbol) %>%
  mutate(close_standardize = standardize_vec(log1p(close))) %>%
  ungroup() %>%
  plot_time_series(date, close_standardize, symbol, .smooth = FALSE)

# Estimando a correlação
criptomoedas_wider <- criptomoedas %>%
  pivot_wider(names_from = symbol, values_from = close) %>%
  drop_na() %>%
  mutate(across(LTC:XRP, .fns = log1p))
  mutate(across(LTC:USDT, .fns = log1p))
  mutate(across(LTC:ADA, .fns = log1p))
  mutate(across(USDT:XRP, .fns = log1p))
  mutate(across(USDT:ADA, .fns = log1p))
  mutate(across(XRP:ADA, .fns = log1p))
  
# Correlações das criptomoedas
cor(criptomoedas_wider$LTC, criptomoedas_wider$XRP)
cor(criptomoedas_wider$LTC, criptomoedas_wider$USDT)
cor(criptomoedas_wider$LTC, criptomoedas_wider$ADA)
cor(criptomoedas_wider$USDT, criptomoedas_wider$XRP)
cor(criptomoedas_wider$USDT, criptomoedas_wider$ADA)
cor(criptomoedas_wider$XRP, criptomoedas_wider$ADA)

#Gráfico das correlações
ggplot(criptomoedas_wider) +
  aes(x = LTC, y = XRP) +
  geom_point()

ggplot(criptomoedas_wider) +
  aes(x = LTC, y = USDT) +
  geom_point()

ggplot(criptomoedas_wider) +
  aes(x = LTC, y = ADA) +
  geom_point()

ggplot(criptomoedas_wider) +
  aes(x = USDT, y = XRP) +
  geom_point()

ggplot(criptomoedas_wider) +
  aes(x = USDT, y = ADA) +
  geom_point()

ggplot(criptomoedas_wider) +
  aes(x = XRP, y = ADA) +
  geom_point()
