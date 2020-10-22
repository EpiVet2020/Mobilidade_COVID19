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
mobilidade_facebook_pt <- mobilidade_r_facebook %>% 
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


### Criar variavel com valores do 0 ao 30

lags <- seq(30)


### Atribuir nome a cada futura coluna comecando com mr_ tendo depois o numero respetivo

lag_names <- paste("mr", formatC(lags, width = nchar(max(lags))), 
                   sep = "_")

### Funcao para fazer com que cada coluna seja a coluna anterior descendo uma linha

lag_functions <- setNames(paste("lag(., ", lags, ")"), lag_names)


### Adicionar as colunas anteriores a tabela correlacao

dr_mr_lag <- dr_mr_lag %>% 
  mutate_at(vars(mobilidade_ponderada), funs_(lag_functions))


### Calcular a correlacao entre a mobilidade e a death rate de marco - hoje

correlacao_dr <- dr_mr_lag[-1] %>% 
  correlate() %>% 
  focus(death_rate)
correlacao_dr[1] = 0:30
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
  scale_x_continuous(breaks = seq(0, 30, 2))

ggplotly(correlacao_dr_grafico, tooltip = "text")


## Relacao marco - hoje

### Grafico da relacao de entre a mobilidade e a death rate de marco - hoje

relacao_dr <- melt(dr_mr_lag[,-1], id.vars = "death_rate")

levels(relacao_dr$variable) <- 0:30

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

dr_mr_lag_marco_maio <- dr_mr_lag %>% 
  filter(data > "2020-03-16" & data < "2020-05-12")


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


