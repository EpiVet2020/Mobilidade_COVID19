# IMPORTAR LIBRARIES
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



#IMPORTAR BASE DE DADOS DO GOOGLE DISPONÍVEL EM <https://www.google.com/covid19/mobility/>
mobilidade_google <- fread("https://raw.githubusercontent.com/EpiVet2020/Mobilidade_COVID19/main/google_mobilidade_pt.csv?token=AQ6V32IJWSYATKEH6U6XW327TF52W")

## por as datas em formato data
mobilidade_google$date <- as.Date(mobilidade_google$date,format = "%d-%m-%Y")


# IMPORTAR BASE DE DADOS DA RECIPITACAO DE LISBOA 
lisboa_precipitacao <- fread("https://api.ipma.pt/open-data/observation/climate/precipitation-total/lisboa/mrrto-1106-lisboa.csv")

## por as datas em formato data
lisboa_precipitacao$date <- as.Date(lisboa_precipitacao$date,format = "%d-%m-%Y")


# TRATAR BASE DE DADOS DA MOBILIDADE

## Selecionar apenas dados de Lisboa (concelho) e a partir de 2020-07-31 porque e quanto temos valores de precipitacao
mobilidade_google_lisboa <- mobilidade_google %>% 
  filter(sub_region_2 == "Lisbon") %>% 
  filter(date >= "2020-07-31") %>% 
  select(8:14)
  

## Normalizar a mobilidade para que o 0 passe a representar a ausência de mobilidade
mobilidade_google_lisboa[,2:7] = lapply(mobilidade_google_lisboa[,2:7], function(x) {(x/100)+1})



# RELACIONAR A MOBILIDADE DE LISBOA (CONCELHO) COM A PRECIPITACAO

## Fazer uma tabela em que junte a informacao das 2 bases de dados e depois que tire a data
mob_pre_lisboa <- left_join(mobilidade_google_lisboa, lisboa_precipitacao[,c(1,5)], by = "date") 
names(mob_pre_lisboa)[2:7] <- c("Retalho e Lazer", "Mercearias e Farmácias", "Parques", "Estações Transp. Público", "Locais de Trabalho",
                                "Residencial")

## Fazer um melt por data para termos apenas 3 colunas
mob_pre_lisboa_melt <- melt(mob_pre_lisboa[,-1], id.vars = "mean")


## Fazer um grafico da relacao da mobilidade de lisboa com a mediana da precipitacao
mob_pre_lisboa_grafico <- ggplot(mob_pre_lisboa_melt, aes(x = mean, y = value, color= variable)) +
  geom_point(aes(text = paste('Categoria de Local: ', variable,
                              '<br>Taxa de Mobilidade: ', value,
                              '<br>Mediana de Precipitação ', mean))) +
  geom_smooth(method = "lm")+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +  
  facet_wrap(.~variable) +
  theme(plot.title = element_text(size=11, face = "bold"),
        legend.text = element_text(size=10),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  labs(title = "Relação entre a Mobilidade do Concelho de Lisboa para Diferentes Categorias de Locais com a Mediana da Precipitação em mm",
       x = "Mediana de Precipitação (mm)",
       y = "Mobilidade do Concelho de Lisboa") 
  

ggplotly(mob_pre_lisboa_grafico, tooltip = "text")


# CORRELACAO DA MOBILIDADE EM LISBOA (CONCELHO) PARA AS DIFERENTES CATEGORIAS DE LOCAIS COM A MEDIANA DA PRECIPITACAO
lapply(mob_pre_lisboa[,retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline], function(x) cor(x,mob_pre_lisboa$mean, use = "complete.obs"))

