pacotes <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS","feasts",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal","devtools","transformr","gganimate")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

load("consumo_brasil.RData")

View(consumo_brasil)

# Para gerar um gráfico estático:
consumo_brasil %>% 
  mutate(Data = as.Date(paste0(gsub("\\.", "-", Data),"-","01"))) %>% 
  ggplot(aes(x = Data, y = Consumo, color = regiao, group = regiao)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(limits = as.Date(c("1979-01-01","2023-01-01")),
               date_labels = "%Y",
               date_breaks = "1 years") +
  scale_color_viridis_d("Região") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 8),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey95"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")


# Para gerar um gráfico interativo:
ggplotly(
  consumo_brasil %>% 
    mutate(Data = as.Date(paste0(gsub("\\.", "-", Data),"-","01"))) %>% 
    ggplot(aes(x = Data, y = Consumo, color = regiao, group = regiao)) +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(limits = as.Date(c("1979-01-01","2023-01-01")),
                 date_labels = "%Y",
                 date_breaks = "1 years") +
    scale_color_viridis_d("Região") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 8),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey95"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "bottom")
) %>% layout(showlegend = TRUE,
             legend = list(orientation = "h"))

# Convertendo para TSIBBLE
consumo <- as_tsibble(consumo_brasil %>% 
                        mutate(Data = gsub("\\.", "-", Data) %>% 
                                 yearmonth()), 
                      index = Data, 
                      key = regiao)

# Observando os dados
glimpse(consumo)
consumo

#------------------------------------------------------------------------------
# separando somente a série do consumo da região SE
cons_SE = consumo %>%
  filter(regiao == "SE") %>%
  select(Consumo)

consumo %>%
  filter(regiao == "SE") %>%
  autoplot(Consumo, color = "#DCE319FF") +
  geom_line(aes(y = Consumo), color = "#440154FF") +
  labs(x="Meses",
       y="Consumo em GWh", title="Consumo em GWh - Região Sudeste") +
  theme_bw()

#Função model() gerar o modelo
#Está configurado para gerar somente o modelo para a base SE

fit_SE = cons_SE %>% 
  model(ets = ETS(Consumo))


# Separando somente SE de um segundo modo
consumo_fit_SE=consumo %>%
  filter(regiao == "SE") %>%
  model(
    energia.ets <- ETS(Consumo)
  )


# Previsão somente para SE
consumo_fc_SE = consumo_fit_SE %>%
  forecast(h = 12)

# Observando as previsões

consumo_fc_SE %>% View()

# Se quiser ver somente as previsões somente para SE
consumo_fc_SE %>%
  autoplot() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# Se quiser ver somente as previsões para SE em conjunto com os dados
consumo_fc_SE %>%
  autoplot(consumo) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# Escolhendo as regiões SE, E e CO

consumo_fit2=consumo %>%
  filter(regiao %in% c("SE", "S", "CO")) %>%
  model(
    energia.ets <- ETS(Consumo)
  )

# Previsao para mais regiões
# Previsão para as regiões SE, S e CO

consumo_fc2 = consumo_fit2 %>%
  forecast(h = 12)
consumo_fc2 %>% View()

autoplot(consumo_fc2)

consumo_fc2 %>%
  autoplot(consumo) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()
