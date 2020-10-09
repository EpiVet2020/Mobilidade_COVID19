# IMPORTAR LIBRARIES
library(data.table)
library(dplyr)
library(zoo)
library(geojsonio)
library(leaflet)

# IMPORTAR BASE DE DADOS DISPONIVEIS  EM: <https://data.humdata.org/dataset/movement-range-maps>
mobilidade_r <- fread("C:/Users/rakac/OneDrive - Universidade de Lisboa/R/Faculdade/2.COVID19 Portugal/Partilhado/Mobilidade_COVID19/dados_mobilidade/movement-range-2020-10-04.txt")

mobilidade_c <- fread("C:/Users/karol/Documents/R/Covid-19_estagio/Epivet2020/movement-range-2020-10-06.txt")


# FONTE DOS DADOS DO COVID-19
covid19pt <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")

## por as datas em formato data
covid19pt$data <- as.Date(as.character(covid19pt$data),format = "%d-%m-%Y")


# MAPA DOS DISTRITOS DE PORTUGAL BASE DE DADOS DISPONIVEIS  EM: https://github.com/ufoe/d3js-geojson/blob/master/Portugal.json


mapa_distritos <- geojson_read("https://raw.githubusercontent.com/ufoe/d3js-geojson/master/Portugal.json", what = "sp")


# SELECIONAR PORTUGAL NA BASE DE DADOS
mobilidade_pt <- mobilidade_c %>% 
  filter(country=="PRT")

# CORRIGIR NOMES DAS REGIEOS
mobilidade_pt$polygon_name[mobilidade_pt$polygon_name == "Santar-m" | mobilidade_pt$polygon_name == "SantarÃ©m"] <- "Santarem"

mobilidade_pt$polygon_name[mobilidade_pt$polygon_name == "Set-bal" | mobilidade_pt$polygon_name == "SetÃºbal"] <- "Setubal"

mobilidade_pt$polygon_name[mobilidade_pt$polygon_name == "Bragan-a" | mobilidade_pt$polygon_name == "BraganÃ§a"] <- "Braganca"

mobilidade_pt$polygon_name[mobilidade_pt$polygon_name == "-vora" | mobilidade_pt$polygon_name == "Ã‰vora"] <- "Evora"


# GROWTH RATE RATIO (gr)

## O gr calcula-se dividindo o logaritmo da media de novos casos dos ultimos 3 dias pelo logaritmo da media de novos casos dos ultimos 7 dias.
## Para isso, fizemos uma tabela com uma coluna para a data e outra coluna para a divisao.
## Para a data, começa na linha 7 porque e o primeiro dia em que temos registos dos 7 dias anteriores, o mesmo foi feito para o numerador para as datas coicidirem.

gr <- as.data.frame(cbind(covid19pt[7:nrow(covid19pt),1], as.data.frame(log(rollmean(covid19pt[5:nrow(covid19pt),12], k=3))
                    /log(rollmean(covid19pt[,12], k = 7)))))
names(gr) <- c("Data", "Growth_Rate")

gr_norte <- as.data.frame(cbind(covid19pt[7:nrow(covid19pt),1], as.data.frame(log10(rollmean(covid19pt[5:nrow(covid19pt),(confirmados_arsnorte-lag(confirmados_arsnorte))], k=3))/
                                                                                log10(rollmean(covid19pt[,(confirmados_arsnorte-lag(confirmados_arsnorte))], k = 7)))))
names(gr_norte) <- c("Data", "Growth_Rate_Norte")


# MaPA MOBILIDADE POR REGIOES

# Mapa do dia 2020-03-01

## Selecionar todas as linhas do dia 2020-03-01
mobilidade_pre_covid <- as.data.frame(with(mobilidade_pt, mobilidade_pt[(ds=="2020-03-01")]))

## Ordenar as regioes pela mesma ordem do que as do mapa
ordem <- c("Setubal", "Azores", "Madeira", "Aveiro", "Leiria", "Viana do Castelo", "Beja", "Evora", "Faro", "Lisboa", "Portalegre", "Santarem", "Braga", "Braganca", "Castelo Branco", "Coimbra", "Guarda", "Porto", "Viseu", "Vila Real")

mobilidade_pre_covid_ordem <- mobilidade_pre_covid %>% 
  slice(match(ordem,polygon_name))

## Fazer o mapa
pal_mobilidade_pre_covid <- colorBin("Blues",  bins = c(-20, -17, -15, -13, -10, -8, -5, 0)) ## tonalidade das cores consoante os casos


labels_mobilidade_pre_covid <- paste( 
  "<strong>", mobilidade_pre_covid_ordem[,5],"</strong><br/>", 
  mobilidade_pre_covid_ordem[,6]*100, " &#37<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)


leaflet(mapa_distritos) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_mobilidade_pre_covid(mobilidade_pre_covid_ordem$all_day_bing_tiles_visited_relative_change*100),
              label = labels_mobilidade_pre_covid, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addLegend("bottomleft", pal = pal_mobilidade_pre_covid, values = mobilidade_pre_covid_ordem$all_day_bing_tiles_visited_relative_change*100, opacity = 0.5, title = "Mobility Rate por distrito (%)") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Satélite", "Dark"), options = layersControlOptions(collapsed = FALSE))





