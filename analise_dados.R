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


# IMPORTAR BASE DE DADOS SOBRE MOBILIDADE DIÁRIA POR DISTRITOS NO MUNDO DISPONIVEIS  EM: <https://data.humdata.org/dataset/movement-range-maps>
mobilidade_r <- fread("C:/Users/rakac/OneDrive - Universidade de Lisboa/R/Faculdade/2.COVID19 Portugal/Partilhado/Mobilidade_COVID19/dados_mobilidade/movement-range-2020-10-04.txt")

mobilidade_c <- fread("C:/Users/karol/Documents/R/Covid-19_estagio/Epivet2020/movement-range-2020-10-06.txt")


# IMPORTAR BASE DE DADOS DO COVID19 EM PORTUGAL
covid19pt <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")

## por as datas em formato data
covid19pt$data <- as.Date(as.character(covid19pt$data),format = "%d-%m-%Y")


# IMPORTAR BASE DE DADOS DOS CASOS POR CONCELHO
covid_concelhos <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data_concelhos.csv")


# IMPORTAR BASE DE DADOS QUE CORRELACIONA CONCELHOS COM DSTRITOS
concelho_distrito <- fread("C:/Users/rakac/OneDrive - Universidade de Lisboa/R/Faculdade/2.COVID19 Portugal/Partilhado/Mobilidade_COVID19/dados_mobilidade/DistritosConcelhosFreguesias_CAOP2013_Populacao_Censos2011.csv") %>% 
  select("DesignaÃ§Ã£o DT", "DesignaÃ§Ã£o CC")


# IMPORTAR MAPA DOS DISTRITOS DE PORTUGAL DISPONIVEIS  EM: https://github.com/ufoe/d3js-geojson/blob/master/Portugal.json
mapa_distritos <- geojson_read("https://raw.githubusercontent.com/ufoe/d3js-geojson/master/Portugal.json", what = "sp")



# TRATAR BASE DE DADOS DA MOBILIDADE

## Selecionar Portugal na base de dados
mobilidade_pt <- mobilidade_r %>% 
  filter(country=="PRT")

## Corrigir os nomes dos distritos
mobilidade_pt$polygon_name[mobilidade_pt$polygon_name == "Santar-m" | mobilidade_pt$polygon_name == "SantarÃ©m"] <- "Santarem"

mobilidade_pt$polygon_name[mobilidade_pt$polygon_name == "Set-bal" | mobilidade_pt$polygon_name == "SetÃºbal"] <- "Setubal"

mobilidade_pt$polygon_name[mobilidade_pt$polygon_name == "Bragan-a" | mobilidade_pt$polygon_name == "BraganÃ§a"] <- "Braganca"

mobilidade_pt$polygon_name[mobilidade_pt$polygon_name == "-vora" | mobilidade_pt$polygon_name == "Ã‰vora"] <- "Evora"

## Normalizar mobility rate para que o 0 passe a representar a ausência de mobilidade
mobilidade_pt$all_day_bing_tiles_visited_relative_change = mobilidade_pt$all_day_bing_tiles_visited_relative_change + 1



# MAPA DA MOBILIDADE POR DISTRITOS

## Mapa do dia 2020-03-01 (antes da quarentena)

### Selecionar todas as linhas do dia 2020-03-01
mobilidade_pre_covid <- as.data.frame(with(mobilidade_pt, mobilidade_pt[(ds=="2020-03-01")]))

### Ordenar os distritos pela mesma ordem do que as do mapa
ordem <- c("Setubal", "Azores", "Madeira", "Aveiro", "Leiria", "Viana do Castelo", "Beja", "Evora", "Faro", "Lisboa", "Portalegre", "Santarem", "Braga", "Braganca", "Castelo Branco", "Coimbra", "Guarda", "Porto", "Viseu", "Vila Real")

mobilidade_pre_covid_ordem <- mobilidade_pre_covid %>% 
  slice(match(ordem,polygon_name))

### Fazer uma palete de cores com 100 tonalidades e aplica-las ao intervalo entre 0.3 e 1.21 que sao o mínimo e o maximo do mobility rate
palete <- colorRampPalette(colors = c("white", "yellow", "pink", "red"), space = "Lab")(100)

pal_mobilidade_covid <-  colorNumeric(palete, domain = c(0.3, 1.21))

### Criar legenda para quando se passa o rato por cima
labels_mobilidade_pre_covid <- paste( 
  "<strong>", mobilidade_pre_covid_ordem[,5],"</strong><br/>", 
  mobilidade_pre_covid_ordem[,6], " <br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

### Criar o mapa com os valores por distrito
leaflet(mapa_distritos) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_mobilidade_covid(mobilidade_pre_covid_ordem$all_day_bing_tiles_visited_relative_change),
              label = labels_mobilidade_pre_covid, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addLegend("bottomleft", pal = pal_mobilidade_covid, values = mobilidade_pre_covid_ordem$all_day_bing_tiles_visited_relative_change, 
            opacity = 0.5, title = "Mobility Rate por distrito dia 01-03-2020")



## Mapa do dia 2020-04-10 (em quarentena)

### Selecionar todas as linhas do dia 2020-04-10
mobilidade_covid_quarentena <- as.data.frame(with(mobilidade_pt, mobilidade_pt[(ds=="2020-04-10")]))

### Ordenar os distritos pela mesma ordem do que as do mapa
mobilidade_covid_quarentena_ordem <- mobilidade_covid_quarentena %>% 
  slice(match(ordem,polygon_name))

### Criar legenda para quando se passa o rato por cima
labels_mobilidade_covid_quarentena <- paste( 
  "<strong>", mobilidade_covid_quarentena_ordem[,5],"</strong><br/>", 
  mobilidade_covid_quarentena_ordem[,6], " <br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

### Criar o mapa com os valores por distrito
leaflet(mapa_distritos) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_mobilidade_covid(mobilidade_covid_quarentena_ordem$all_day_bing_tiles_visited_relative_change),
              label = labels_mobilidade_covid_quarentena, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addLegend("bottomleft", pal = pal_mobilidade_covid, values = mobilidade_covid_quarentena_ordem$all_day_bing_tiles_visited_relative_change, 
            opacity = 0.5, title = "Mobility Rate por distrito dia 10-04-2020")


## Mapa do dia 2020-09-14 (regresso às aulas)

### Selecionar todas as linhas do dia 2020-09-14
mobilidade_covid_aulas <- as.data.frame(with(mobilidade_pt, mobilidade_pt[(ds=="2020-09-14")]))

### Ordenar os distritos pela mesma ordem do que as do mapa
mobilidade_covid_aulas_ordem <- mobilidade_covid_aulas %>% 
  slice(match(ordem,polygon_name))

### Criar legenda para quando se passa o rato por cima
labels_mobilidade_covid_aulas <- paste( 
  "<strong>", mobilidade_covid_aulas_ordem[,5],"</strong><br/>", 
  mobilidade_covid_aulas_ordem[,6], " <br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

### Criar o mapa com os valores por distrito
leaflet(mapa_distritos) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_mobilidade_covid(mobilidade_covid_aulas_ordem$all_day_bing_tiles_visited_relative_change),
              label = labels_mobilidade_covid_aulas, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addTiles(group = "Normal") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Claro") %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Escuro") %>% 
  addLayersControl(
    baseGroups = c("Normal", "Claro", "Escuro"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addLegend("bottomleft", pal = pal_mobilidade_covid, values = mobilidade_covid_aulas_ordem$all_day_bing_tiles_visited_relative_change, 
            opacity = 0.5, title = "Mobility Rate por distrito dia 14-09-2020")


## Mapa da evolucao do mobility rate por distrito




# EVOLUCAO DO MOBILITY RATE POR DISTRITO

## Adicionar a base de dados coluna com o mobility rate feito com a media rolante dos ultimos 7 dias
mobilidade_pt <- mobilidade_pt %>% 
  group_by(polygon_name) %>% 
  mutate(mobilidade_media = rollapply(all_day_bing_tiles_visited_relative_change, 7, mean, na.pad = TRUE, align = "right"))

## Fazer um grafico de linhas com data no eixo do x, mobility rate feito com media rolante no eixo do y e um distrito em cada linha
mobilidade_grafico <- ggplot(mobilidade_pt, aes(x = ds, y = mobilidade_media, color = polygon_name, group = 1, 
                                                text = paste('Data: ', ds,
                                                             '<br>Mobilidade Média:', mobilidade_media,
                                                             '<br>Distrito:', polygon_name))) +
  geom_line() +
  labs(title = "Evolução da Mobility Rate por Distrito - Média Rolante",
       x = "Mês",
       y = "Mobility Rate") +
  theme_classic() +
  theme(legend.title = element_blank()) +
  scale_x_date(breaks = "months", date_labels = "%b") +
  geom_line(aes(y = 1, text = ""), color = "grey", linetype = "dotted")

ggplotly(mobilidade_grafico, tooltip = "text")



# GROWTH RATE RATIO (gr)

## gr nacional 

### O gr calcula-se dividindo o logaritmo da media de novos casos dos ultimos 3 dias pelo logaritmo da media de novos casos dos ultimos 7 dias.
### Para isso, fizemos uma tabela com uma coluna para a data e outra coluna para a divisao.
### Para a data, começa na linha 7 porque e o primeiro dia em que temos registos dos 7 dias anteriores, o mesmo foi feito para o numerador para 
### as datas coicidirem.

gr <- as.data.frame(cbind(covid19pt[7:nrow(covid19pt),1], as.data.frame(log(rollmean(covid19pt[5:nrow(covid19pt),12], k=3))
                                                                        /log(rollmean(covid19pt[,12], k = 7)))))
names(gr) <- c("Data", "Growth_Rate")


## gr por distrito

### Tabela com coluna para data, coluna para concelho e outra para mobility rate
covid_concelhos_melt <- melt(covid_concelhos, id.vars = "data")

### Juntar tabela anterior a tabela que associa os concelhos ao distrito de forma a poder calcular o mobility rate por distrito
i <- as.data.frame(amatch(covid_concelhos_melt$variable, concelho_distrito$`DesignaÃ§Ã£o CC`, maxDist = 2))
covid_concelhos_melt$match[!is.na(i)] <- concelho_distrito$`DesignaÃ§Ã£o CC`[i[!is.na(i)]]




# CÁLCULO DO LAG (dias de desfasamento do efeito da mobilidade no numero de casos de covid19)

## Tabela com data, growth ratio, mobility rate 0, mobility rate -1 até mobility rate -30 (a mobilidade de à mais de 30 dias em princípio não
## influencia os casos atuais)

mobilidade_pt %>%
  select(ds, polygon_name, all_day_bing_tiles_visited_relative_change) %>% 
  group_by(polygon_name) %>% 
  merge(gr[,2])


















# MOBILITY RATE NACIONAL POR DIA (feito com média de cada distrito o que não é o melhor)

mobilidade_nacional <- mobilidade_pt %>% 
  group_by(ds) %>%
  mutate(mobilidade_nacional = mean(all_day_bing_tiles_visited_relative_change)) %>% 
  distinct(ds, mobilidade_nacional)
names(mobilidade_nacional)[1] = "Data"


# CÁLCULO DO LAG COM BASE NA CORRELACAO DO MOBILITY RATE COM O GROWTH RATIO PARA DIA 10-10-2020 (não fazer divisão mas sim correlacao glm(gr~mr) 
# para vários dias)

## Criar uma tabela com uma coluna para as datas até 30 dias antes do maior pico da pandemia (dia 10-10-2020) e outra coluna com os 
## valores da mobility rate
correlacao_lag <- mobilidade_nacional %>%
  filter(Data >= "2020-09-10" & Data <= "2020-10-10") %>% 
  mutate(correlacao_lag = mobilidade_nacional / gr[222, 2]) %>% 
  cbind(as.data.frame(30:6))
names(correlacao_lag)[4] = "Lag"

correlacao_lag_grafico <- ggplot(correlacao_lag, aes(x = Lag, y = correlacao_lag)) +
  geom_point() +
  geom_line(color = "coral2") +
  labs(y = "Correlação MR e GR")

ggplotly(correlacao_lag_grafico)

# CORRELACAO DO MOBILITY RATE COM O GROWTH RATIO
correlacao <- merge(mobilidade_nacional, gr, by = "Data")

correlacao <- correlacao %>%
  mutate(correlacao = mobilidade_nacional / Growth_Rate)
