#IMPORTAR LIBRARIES
library(data.table)
library(dplyr)
library(zoo)
library(geojsonio)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(ggplot2)
library(plotly)
library(stringdist)
library(Ecfun)
library(tibble)
library(ggpmisc)
library(corrr)

# IMPORTAR BASE DE DADOS DO FACEBOOK SOBRE MOBILIDADE DIÁRIA POR DISTRITOS NO MUNDO DISPONIVEIS  EM: <https://data.humdata.org/dataset/movement-range-maps>
# mobilidade_r_facebook <- fread("C:/Users/rakac/OneDrive - Universidade de Lisboa/R/Faculdade/2.COVID19 Portugal/Partilhado/Mobilidade_COVID19/dados_mobilidade/movement-range-2020-10-10.txt")

mobilidade_c_facebook <- fread("C:/Users/karol/Documents/R/Covid-19_estagio/Epivet2020/movement-range-2020-10-10.txt")



# IMPORTAR BASE DE DADOS DO COVID19 EM PORTUGAL DISPONIVEL EM: <https://github.com/dssg-pt/covid19pt-data>
covid19pt <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")

## por as datas em formato data
covid19pt$data <- as.Date(as.character(covid19pt$data),format = "%d-%m-%Y")



# TRATAR BASE DE DADOS DA MOBILIDADE DO FACEBOOK

## Selecionar Portugal na base de dados
mobilidade_facebook_pt <- mobilidade_c_facebook %>% 
  filter(country=="PRT")

## Normalizar mobility rate para que o 0 passe a representar a ausência de mobilidade
mobilidade_facebook_pt$all_day_bing_tiles_visited_relative_change = mobilidade_facebook_pt$all_day_bing_tiles_visited_relative_change + 1



# TAXA DE CRESCIMENTO DE NOVAS MORTES (Death Rate - DR)

## Numero de mortes diarias 

mortes_diarias <- cbind(covid19pt$data, as.data.frame(covid19pt$obitos - lag(covid19pt$obitos)))
names(mortes_diarias) <- c("data", "mortes_diarias")

## Calcula-se o DR fazendo o logaritmo da media das novas mortes dos ultimos 3 dias a dividir 
## pelo logaritmo da media de novas mortes dos ultimos 7 dias

dr <- as.data.frame(cbind(mortes_diarias[7:nrow(mortes_diarias),1], as.data.frame(log(rollmean(mortes_diarias[5:nrow(mortes_diarias),2], k=3))
                                                                                  /log(rollmean(mortes_diarias[,2], k = 7)))))
names(dr) <- c("data", "death_rate")




# FACEBOOK

## Correlacao marco - hoje

### Fazer uma tabela com data, death rate nacional e mobilidade nacional

dr_mr_lag <- left_join(dr, mobilidade_nacional, by = "data")  %>% 
  filter(data > "2020-03-16")


### Criar variavel com valores do 0 ao 90

lags <- seq(90)


### Atribuir nome a cada futura coluna comecando com mr_ tendo depois o numero respetivo

lag_names <- paste("mr", formatC(lags, width = nchar(max(lags))), 
                   sep = "_")

### Funcao para fazer com que cada coluna seja a coluna anterior descendo uma linha

lag_functions <- setNames(paste("lag(., ", lags, ")"), lag_names)


### Adicionar as colunas anteriores a tabela correlacao

dr_mr_lag <- dr_mr_lag %>% 
  mutate_at(vars(mobilidade_ponderada), funs_(lag_functions))


### Calcular a correlacao entre a mobilidade e a death rate de marco - hoje para 90 dias de lag

correlacao_dr <- dr_mr_lag[-1] %>% 
  correlate() %>% 
  focus(death_rate)
correlacao_dr[1] = 0:90
names(correlacao_dr) = c("Lag", "correlacao")

correlacao_dr_grafico <- ggplot(correlacao_dr, aes(x = Lag, y = correlacao)) +
  geom_point(aes(text = paste('Lag: ', Lag,
                              '<br>Correlação: ', correlacao))) +
  geom_line() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size=9),
        legend.text = element_text(size=6),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  labs(title = "Correlação entre Taxa de Mobilidade (MR) e Taxa de Crescimento \nde Novas Mortes (DR) em Diferentes Desfasamentos (Lag) de Março a Hoje",
       x = "Lag (dias)",
       y = "Correlação entre MR e DR") +
  scale_x_continuous(breaks = seq(0, 90, 5))

ggplotly(correlacao_dr_grafico, tooltip = "text")


## Relacao marco - hoje

### Grafico da relacao de entre a mobilidade e a death rate de marco - hoje para 90 dias de lag

relacao_dr <- melt(dr_mr_lag[,-1], id.vars = "death_rate")

levels(relacao_dr$variable) <- 0:90

ggplot(relacao_dr, aes(value, death_rate, fill = variable)) +
  geom_point() +
  facet_wrap(relacao_dr$variable) +
  stat_poly_eq(aes(label = paste(..eq.label..)),
               formula = y~x, parse = TRUE, label.y = 0.2) + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Relação da Taxa de Crescimento de Novas Mortes (DR) com a Taxa de Mobilidade (MR) para Diferentes Desfasamentos de Março a Hoje",
       x = "MR",
       y = "DR")




## Correlacao de marco a maio

## Filtrar as datas de marco a maio

dr_mr_lag_marco_maio <- dr_mr_lag[,1:33] %>% 
  filter(data > "2020-03-16" & data < "2020-05-02")


### Calcular a correlacao entre a mobilidade e a death rate de marco - maio

correlacao_dr_marco_maio <- dr_mr_lag_marco_maio[-1] %>% 
  correlate() %>% 
  focus(death_rate)
correlacao_dr_marco_maio[1] = 0:30
names(correlacao_dr_marco_maio) = c("Lag", "correlacao")

correlacao_dr_marco_maio_grafico <- ggplot(correlacao_dr_marco_maio, aes(x = Lag, y = correlacao)) +
  geom_point(aes(text = paste('Lag: ', Lag,
                              '<br>Correlação: ', correlacao))) +
  geom_line() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size=10),
        legend.text = element_text(size=6),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  labs(title = "Correlação entre Taxa de Mobilidade (MR) e Taxa de Crescimento \nde Novas Mortes (DR) em Diferentes Desfasamentos (Lag) de Março a Maio",
       x = "Lag (dias)",
       y = "Correlação entre MR e DR") +
  scale_x_continuous(breaks = seq(0, 30, 2))

ggplotly(correlacao_dr_marco_maio_grafico, tooltip = "text")


## Relacao marco - maio

### Grafico da relacao de entre a mobilidade e a death rate de marco - maio para 30 dias de lag (nao temos valores para ser maior)

relacao_dr_marco_maio <- melt(dr_mr_lag_marco_maio[,-1], id.vars = "death_rate")

levels(relacao_dr_marco_maio$variable) <- 0:30

ggplot(relacao_dr_marco_maio, aes(value, death_rate, fill = variable)) +
  geom_point() +
  facet_wrap(relacao_dr_marco_maio$variable) +
  stat_poly_eq(aes(label = paste(..eq.label..)),
               formula = y~x, parse = TRUE, label.y = 0.2) + 
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  labs(title = "Relação da Taxa de Crescimento de Novas Mortes (DR) com a Taxa de Mobilidade (MR) para Diferentes Desfasamentos de Março a Hoje",
       x = "MR",
       y = "DR")




#GLM Março a Maio

### Calcular a Generalized Linear Regression (glm) entre death rate e mobility rate nacional para cada lag (30 lags)

#### Com gaussian

glm_dr_marco_maio <- as.data.frame(coefficients(glm(death_rate ~ mobilidade_ponderada, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr1 = coefficients(glm(death_rate ~ `mr_ 1`, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr2 = coefficients(glm(death_rate ~ `mr_ 2`, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr3 = coefficients(glm(death_rate ~ `mr_ 3`, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr4 = coefficients(glm(death_rate ~ `mr_ 4`, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr5 = coefficients(glm(death_rate ~ `mr_ 5`, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr6 = coefficients(glm(death_rate ~ `mr_ 6`, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr7 = coefficients(glm(death_rate ~ `mr_ 7`, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr8 = coefficients(glm(death_rate ~ `mr_ 8`, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr9 = coefficients(glm(death_rate ~ `mr_ 9`, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr10 = coefficients(glm(death_rate ~ mr_10, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr11 = coefficients(glm(death_rate ~ mr_11, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr12 = coefficients(glm(death_rate ~ mr_12, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr13 = coefficients(glm(death_rate ~ mr_13, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr14 = coefficients(glm(death_rate ~ mr_14, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr15 = coefficients(glm(death_rate ~ mr_15, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr16 = coefficients(glm(death_rate ~ mr_16, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr17 = coefficients(glm(death_rate ~ mr_17, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr18 = coefficients(glm(death_rate ~ mr_18, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr19 = coefficients(glm(death_rate ~ mr_19, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr20 = coefficients(glm(death_rate ~ mr_20, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr21 = coefficients(glm(death_rate ~ mr_21, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr22 = coefficients(glm(death_rate ~ mr_22, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr23 = coefficients(glm(death_rate ~ mr_23, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr24 = coefficients(glm(death_rate ~ mr_24, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr25 = coefficients(glm(death_rate ~ mr_25, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr26 = coefficients(glm(death_rate ~ mr_26, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr27 = coefficients(glm(death_rate ~ mr_27, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr28 = coefficients(glm(death_rate ~ mr_28, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr29 = coefficients(glm(death_rate ~ mr_29, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  mutate(mr30 = coefficients(glm(death_rate ~ mr_30, family = "gaussian", data = dr_mr_lag_marco_maio))) %>%
  rbind(0:30)

names(glm_dr_marco_maio)[1] = "mr0"
glm_inv_marco_maio <- as.data.frame(t(glm_dr_marco_maio[c(2, 3),])) %>%
  rownames_to_column(var = "mr")
names(glm_inv_marco_maio) = c("mr", "coeficiente", "lag")


lag_grafico_dr_marco_maio <- ggplot(glm_inv_marco_maio, aes(x = lag, y = coeficiente)) +
  geom_point() +
  geom_line() +
  #geom_rect(xmin= 26, xmax= 27, ymin=-0.04, ymax=0.15, fill="coral2", size=0.1, alpha = 0.4,
  #aes(text="Correlação \nsuperior a 0.06")) +
  labs(title = "Correlação entre Mobility Rate e Growth Rate entre Março e Maio em Diferentes Desfasamentos (lag)",
       x = "Lag (dias)",
       y = "Correlação entre MR e GR") +
  scale_x_continuous(breaks = seq(0, 30, 2))

ggplotly(lag_grafico_dr_marco_maio)
