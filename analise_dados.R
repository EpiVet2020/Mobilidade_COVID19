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


# IMPORTAR BASE DE DADOS SOBRE MOBILIDADE DIÁRIA POR DISTRITOS NO MUNDO DISPONIVEIS  EM: <https://data.humdata.org/dataset/movement-range-maps>
mobilidade_r <- fread("C:/Users/rakac/OneDrive - Universidade de Lisboa/R/Faculdade/2.COVID19 Portugal/Partilhado/Mobilidade_COVID19/dados_mobilidade/movement-range-2020-10-10.txt")

mobilidade_c <- fread("C:/Users/karol/Documents/R/Covid-19_estagio/Epivet2020/movement-range-2020-10-06.txt")


# IMPORTAR BASE DE DADOS DO COVID19 EM PORTUGAL DISPONIVEL EM: <https://github.com/dssg-pt/covid19pt-data>
covid19pt <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")

## por as datas em formato data
covid19pt$data <- as.Date(as.character(covid19pt$data),format = "%d-%m-%Y")


# IMPORTAR BASE DE DADOS DOS CASOS POR CONCELHO DISPONIVEL EM: <https://github.com/dssg-pt/covid19pt-data>
covid_concelhos <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data_concelhos.csv")


# IMPORTAR BASE DE DADOS QUE CORRELACIONA CONCELHOS COM DSTRITOS DISPONIVEL EM: <https://www.factorvirtual.com/blog/distritos-concelhos-e-freguesias-de-portugal>
concelho_distrito <- fread("https://raw.githubusercontent.com/EpiVet2020/Mobilidade_COVID19/main/concelho_distrito.csv?token=AO4UTAUUOJ6LGPFVWJNOHEC7Q4HYC") %>% 
  select("DesignaÃ§Ã£o DT", "DesignaÃ§Ã£o CC")


# IMPORTAR MAPA DOS DISTRITOS DE PORTUGAL DISPONIVEIS  EM: <https://github.com/ufoe/d3js-geojson/blob/master/Portugal.json>
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

## Curva da tendencia

### Grafico com data no eixo do x, mobility rate no eixo do y e distrito nas cores das linhas

mobilidade_grafico <- ggplot(mobilidade_pt, aes(x = ds, y = all_day_bing_tiles_visited_relative_change, color = polygon_name)) +
  geom_point(size = 0.7,  aes(text = paste('Data: ', ds,
                                           '<br>Mobilidade:', all_day_bing_tiles_visited_relative_change,
                                           '<br>Distrito:', polygon_name))) +
  geom_smooth(se = FALSE, size = 0.7) +
  labs(title = "Evolução da Mobility Rate por Distrito",
       x = "Mês",
       y = "Mobility Rate") +
  theme_classic() +
  theme(legend.title = element_blank()) +
  scale_x_date(breaks = "months", date_labels = "%b") +
  geom_line(aes(y = 1, text = ""), size = 0.5, color = "black", linetype = "dotted")

ggplotly(mobilidade_grafico, tooltip = "text")



## Media rolante ultimos 7 dias

### Adicionar a base de dados coluna com o mobility rate feito com a media rolante dos ultimos 7 dias
mobilidade_pt <- mobilidade_pt %>% 
  group_by(polygon_name) %>% 
  mutate(mobilidade_media = rollapply(all_day_bing_tiles_visited_relative_change, 7, mean, na.pad = TRUE, align = "right"))

### Fazer um grafico de linhas com data no eixo do x, mobility rate feito com media rolante no eixo do y e um distrito em cada linha
mobilidade_media_grafico <- ggplot(mobilidade_pt, aes(x = ds, y = mobilidade_media, color = polygon_name, group = 1, 
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

ggplotly(mobilidade_media_grafico, tooltip = "text")




# GROWTH RATE RATIO (gr)

## gr nacional 

### O gr calcula-se dividindo o logaritmo da media de novos casos dos ultimos 3 dias pelo logaritmo da media de novos casos dos ultimos 7 dias.
### Para isso, fizemos uma tabela com uma coluna para a data e outra coluna para a divisao.
### Para a data, começa na linha 7 porque e o primeiro dia em que temos registos dos 7 dias anteriores
### Para o numerador tem de se comecar na linha 5 pois o primeiro valor que queremos e para a linha 7 e ele precisa das duas linhas
### anteriores para fazer a rollmean dos ultimos 3 dias 
### Para o demoninador nao precisamos de especificar onde queremos que comece porque ele so comeca quando tem 7 registos disponiveis

gr <- as.data.frame(cbind(covid19pt[7:nrow(covid19pt),1], as.data.frame(log(rollmean(covid19pt[5:nrow(covid19pt),12], k=3))
                                                                        /log(rollmean(covid19pt[,12], k = 7)))))
names(gr) <- c("data", "Growth_Rate")


## gr por distrito

### Tabela com coluna para data, coluna para concelho e outra para mobility rate
covid_concelhos_melt <- melt(covid_concelhos, id.vars = "data")

### Por tudo em letras maiúsculas na base de dados concelho_distrito
concelho_distrito$`DesignaÃ§Ã£o CC` = toupper(concelho_distrito$`DesignaÃ§Ã£o CC`)

### Remover concelhos repetidos
concelho_distrito <- unique(concelho_distrito)

### Juntar tabela anterior a tabela que associa os concelhos ao distrito de forma a poder calcular o mobility rate por distrito
names(covid_concelhos_melt) = c("data", "concelho_melt", "incidencia")
names(concelho_distrito) = c("distrito", "concelho")

i <- amatch(covid_concelhos_melt$concelho_melt, concelho_distrito$concelho, maxDist = 2)
covid_concelhos_melt$match[!is.na(i)] <- concelho_distrito$concelho[i[!is.na(i)]]

names(covid_concelhos_melt)[4] = "concelho"

covid_concelho_distrito <- left_join(covid_concelhos_melt, concelho_distrito, by = "concelho")

### Corrigir nomes de distritos em falta
covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "ANGRA DO HEROÃSMO" | 
                                   covid_concelho_distrito$concelho_melt == "CALHETA (AÃ‡ORES)" |
                                   covid_concelho_distrito$concelho_melt == "CORVO" |
                                   covid_concelho_distrito$concelho_melt == "HORTA" |
                                   covid_concelho_distrito$concelho_melt == "LAGOA" |
                                   covid_concelho_distrito$concelho_melt == "LAJES DAS FLORES" |
                                   covid_concelho_distrito$concelho_melt == "LAJES DO PICO" |
                                   covid_concelho_distrito$concelho_melt == "MADALENA" |
                                   covid_concelho_distrito$concelho_melt == "NORDESTE" |
                                   covid_concelho_distrito$concelho_melt == "PONTA DELGADA" |
                                   covid_concelho_distrito$concelho_melt == "POVOAÃ‡ÃƒO" |
                                   covid_concelho_distrito$concelho_melt == "RIBEIRA GRANDE" |
                                   covid_concelho_distrito$concelho_melt == "SANTA CRUZ DA GRACIOSA" |
                                   covid_concelho_distrito$concelho_melt == "SANTA CRUZ DAS FLORES" |
                                   covid_concelho_distrito$concelho_melt == "SÃƒO ROQUE DO PICO" |
                                   covid_concelho_distrito$concelho_melt == "VELAS" |
                                   covid_concelho_distrito$concelho_melt == "VILA DA PRAIA DA VITÃ“RIA" |
                                   covid_concelho_distrito$concelho_melt == "VILA DO PORTO" |
                                   covid_concelho_distrito$concelho_melt == "VILA FRANCA DO CAMPO"] = "Acores"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "CALHETA" |
                                   covid_concelho_distrito$concelho_melt == "CÃ‚MARA DE LOBOS" |
                                   covid_concelho_distrito$concelho_melt == "FUNCHAL" |
                                   covid_concelho_distrito$concelho_melt == "MACHICO" |
                                   covid_concelho_distrito$concelho_melt == "PONTA DO SOL" |
                                   covid_concelho_distrito$concelho_melt == "PORTO MONIZ" |
                                   covid_concelho_distrito$concelho_melt == "PORTO SANTO" |
                                   covid_concelho_distrito$concelho_melt == "RIBEIRA BRAVA" |
                                   covid_concelho_distrito$concelho_melt == "SANTA CRUZ" |
                                   covid_concelho_distrito$concelho_melt == "SANTANA" |
                                   covid_concelho_distrito$concelho_melt == "SÃƒO VICENTE"] = "Madeira"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "ALCOBAÃ‡A" |
                                   covid_concelho_distrito$concelho_melt == "FIGUEIRÃ“ DOS VINHOS" |
                                   covid_concelho_distrito$concelho_melt == "NAZARÃ‰" |
                                   covid_concelho_distrito$concelho_melt == "PEDRÃ“GÃƒO GRANDE" |
                                   covid_concelho_distrito$concelho_melt == "PORTO DE MÃ“S"] = "Leiria"


covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "ALFÃ‚NDEGA DA FÃ‰" | 
                                   covid_concelho_distrito$concelho_melt == "BRAGANÃ‡A" |
                                   covid_concelho_distrito$concelho_melt == "FREIXO DE ESPADA Ã€ CINTA"] = "Braganca"


covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "ALIJÃ“" |
                                   covid_concelho_distrito$concelho_melt == "MURÃ‡A" |
                                   covid_concelho_distrito$concelho_melt == "PESO DA RÃ‰GUA" |
                                   covid_concelho_distrito$concelho_melt == "VALPAÃ‡OS"] = "Vila Real"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "ALMODÃ”VAR" |
                                   covid_concelho_distrito$concelho_melt == "MÃ‰RTOLA"] = "Beja"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "ALPIARÃ‡A" |
                                   covid_concelho_distrito$concelho_melt == "CONSTÃ‚NCIA" |
                                   covid_concelho_distrito$concelho_melt == "MAÃ‡ÃƒO" |
                                   covid_concelho_distrito$concelho_melt == "OURÃ‰M" |
                                   covid_concelho_distrito$concelho_melt == "SANTARÃ‰M"] = "Santarem"


covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "GRÃ‚NDOLA" |
                                 covid_concelho_distrito$concelho_melt == "SANTIAGO DO CACÃ‰M"] = "Setubal"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "GÃ“IS"] = "Coimbra"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "LAGOA (FARO)" |
                                   covid_concelho_distrito$concelho_melt == "LOULÃ‰" |
                                   covid_concelho_distrito$concelho_melt == "SÃƒO BRÃS DE ALPORTEL" |
                                   covid_concelho_distrito$concelho_melt == "VILA REAL DE SANTO ANTÃ“NIO"] = "Faro"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "MELGAÃ‡O" |
                                   covid_concelho_distrito$concelho_melt == "MONÃ‡ÃƒO" |
                                   covid_concelho_distrito$concelho_melt == "VALENÃ‡A"] = "Viana do Castelo"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "OLIVEIRA DE AZEMÃ‰IS" |
                                   covid_concelho_distrito$concelho_melt == "SÃƒO JOÃƒO DA MADEIRA"] = "Aveiro"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "PAÃ‡OS DE FERREIRA" |
                                   covid_concelho_distrito$concelho_melt == "PÃ“VOA DE VARZIM"] = "Porto"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "PROENÃ‡A-A-NOVA" |
                                   covid_concelho_distrito$concelho_melt == "VILA VELHA DE RÃ“DÃƒO"] = "Castelo Branco"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "PÃ“VOA DE LANHOSO"] = "Braga"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "SOBRAL DE MONTE AGRAÃ‡O"] = "Lisboa"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "SÃTÃƒO" |
                                   covid_concelho_distrito$concelho_melt == "SÃƒO JOÃƒO DA PESQUEIRA" |
                                   covid_concelho_distrito$concelho_melt == "TABUAÃ‡O"] = "Viseu"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "VILA NOVA DE FOZ CÃ”A"] = "Guarda"

covid_concelho_distrito$distrito[covid_concelho_distrito$concelho_melt == "VILA VIÃ‡OSA"] = "Evora"


### Corrigir nome dos distritos que não estão bem escritos

covid_concelho_distrito$distrito[covid_concelho_distrito$distrito == "SantarÃ©m"] <- "Santarem"

covid_concelho_distrito$distrito[covid_concelho_distrito$distrito == "Ã‰vora"] <- "Evora"

covid_concelho_distrito$distrito[covid_concelho_distrito$distrito == "SetÃºbal"] <- "Setubal"

covid_concelho_distrito$distrito[covid_concelho_distrito$distrito == "BraganÃ§a"] <- "Braganca"

### Calcular o numero de casos por distrito somando o número de casos dos concelhos desse distrito por dia

incidencia_distrito <- covid_concelho_distrito %>% 
  group_by(distrito, data) %>% 
  summarise(incidencia = sum(incidencia, na.rm = TRUE))

incidencia_distrito = incidencia_distrito[c("data", "distrito", "incidencia")]


### Mudar a coluna da data de character para data

incidencia_distrito$data <- as.Date(as.character(incidencia_distrito$data),format = "%d-%m-%Y")


### Criar tabela sem melt, ou seja, com uma coluna para cada distrito
incidencia_distrito_unmelt <- dcast(incidencia_distrito, data~distrito, value.var = "incidencia")


### Calculo dos casos diários para o intervalo de tempo em que temos o numero de casos acumulados por dia 
incidencia_distrito_lag <- cbind(incidencia_distrito_unmelt[2:105,1], 
                                 incidencia_distrito_unmelt[2:105, 2:21] - incidencia_distrito_unmelt[1:104, 2:21])

### Passar valores negativos para zero
incidencia_distrito_lag[incidencia_distrito_lag < 0] <- 0


### Calcular o growth rate tal como fizemos para Portugal 
### Apesar da tabela incidencia_distrito_lag comecar na linha 2, o R identifica-a como sendo linha 1 por isso comecamos na linha 
### 7 e nao na 8
gr_distritos <- as.data.frame(cbind(incidencia_distrito_lag[7:104,1], 
                                    as.data.frame(log(rollapply(incidencia_distrito_lag[5:nrow(incidencia_distrito_lag), 2:21], 3, mean))
                                                  /log(rollapply(incidencia_distrito_lag[, 2:21], 7, mean)))))




# MOBILITY RATE 

## Nacional por dia (feito com media ponderada)

### Dados do numero de pessoas por distrito disponiveis em <https://pt.db-city.com/Portugal>

pop_guarda = 176086
pop_leiria = 472895
pop_lisboa = 2203503
pop_madeira = 244286
pop_portalegre = 121653
pop_porto = 1805015
pop_santarem = 463676
pop_setubal = 829007
pop_vianadocastelo = 251937
pop_vilareal = 221218
pop_aveiro = 727041
pop_viseu = 395202
pop_acores = 241206
pop_beja = 156259
pop_braga = 851337
pop_braganca = 280180
pop_castelobranco = 203769
pop_coimbra = 437642
pop_evora = 171130
pop_faro = 411468

### Selecionar na tabela da mobilidade as colunas da data, distrito e mobilidade

mobilidade_distritos <- mobilidade_pt %>% 
  select(ds, polygon_name, all_day_bing_tiles_visited_relative_change)
names(mobilidade_distritos) = c("data", "distrito", "mobilidade")


### Tabela com a populacao por distrito

pop_distritos <- data.frame(distrito = c("Guarda", "Leiria", "Lisboa", "Madeira", "Portalegre", "Porto", "Santarem", "Setubal", 
                                         "Viana do Castelo","Vila Real", "Aveiro", "Viseu", "Azores", "Beja", "Braga", "Braganca", 
                                         "Castelo Branco", "Coimbra", "Evora", "Faro"), 
                            populacao = c(pop_guarda, pop_leiria , pop_lisboa, pop_madeira, pop_portalegre, pop_porto, pop_santarem, 
                                          pop_setubal, pop_vianadocastelo,pop_vilareal, pop_aveiro, pop_viseu, pop_acores, pop_beja, 
                                          pop_braga, pop_braganca, pop_castelobranco, pop_coimbra, pop_evora,pop_faro))


###Juntar as duas tabelas anteriores pelo distrito

mobilidade_distritos <- left_join(mobilidade_distritos, pop_distritos, by = "distrito")


### Nova coluna com a multiplicacao da mobilidade pela populacao de cada distrito (para a media ponderada)

mobilidade_distritos <- mobilidade_distritos %>% 
  mutate(mobilidadexpopulacao = mobilidade * populacao)


### Tabela com a media ponderada do mobility rate nacional por dia (soma das multiplicacoes anteriores a dividir pela populacao de Portugal)

mobilidade_nacional <- mobilidade_distritos %>% 
  group_by(data) %>% 
  summarise(mobilidade_ponderada = sum(mobilidadexpopulacao) / sum(pop_distritos$populacao))


mobilidade_nacional$data <- as.Date(mobilidade_nacional$data,format = "%d-%m-%Y")




# CÁLCULO DO LAG (dias de desfasamento do efeito da mobilidade no numero de casos de covid19)

## Nacional

### Fazer uma tabela com data, growth rate nacional e mobilidade nacional

correlacao <- left_join(gr, mobilidade_nacional, by = "data")


### Criar variavel com valores do 0 ao 30

lags <- seq(30)


### Atribuir nome a cada futura coluna comecando com mr_ tendo depois o numero respetivo

lag_names <- paste("mr", formatC(lags, width = nchar(max(lags))), 
                   sep = "_")

### Funcao para fazer com que cada coluna seja a coluna anterior descendo uma linha

lag_functions <- setNames(paste("lag(., ", lags, ")"), lag_names)


### Adicionar as colunas anteriores a tabela correlacao

correlacao <- correlacao %>% 
  mutate_at(vars(mobilidade_ponderada), funs_(lag_functions))

 
### Calcular a Generalized Linear Regression (glm) entre growth rate nacional e mobility rate nacional para cada lag

#### Com gaussian

glm <- as.data.frame(coefficients(glm(Growth_Rate ~ mobilidade_ponderada, family = "gaussian", data = correlacao))) %>% 
  mutate(mr1 = coefficients(glm(Growth_Rate ~ `mr_ 1`, family = "gaussian", data = correlacao))) %>% 
  mutate(mr2 = coefficients(glm(Growth_Rate ~ `mr_ 2`, family = "gaussian", data = correlacao))) %>% 
  mutate(mr3 = coefficients(glm(Growth_Rate ~ `mr_ 3`, family = "gaussian", data = correlacao))) %>% 
  mutate(mr4 = coefficients(glm(Growth_Rate ~ `mr_ 4`, family = "gaussian", data = correlacao))) %>% 
  mutate(mr5 = coefficients(glm(Growth_Rate ~ `mr_ 5`, family = "gaussian", data = correlacao))) %>% 
  mutate(mr6 = coefficients(glm(Growth_Rate ~ `mr_ 6`, family = "gaussian", data = correlacao))) %>% 
  mutate(mr7 = coefficients(glm(Growth_Rate ~ `mr_ 7`, family = "gaussian", data = correlacao))) %>% 
  mutate(mr8 = coefficients(glm(Growth_Rate ~ `mr_ 8`, family = "gaussian", data = correlacao))) %>% 
  mutate(mr9 = coefficients(glm(Growth_Rate ~ `mr_ 9`, family = "gaussian", data = correlacao))) %>% 
  mutate(mr10 = coefficients(glm(Growth_Rate ~ mr_10, family = "gaussian", data = correlacao))) %>% 
  mutate(mr11 = coefficients(glm(Growth_Rate ~ mr_11, family = "gaussian", data = correlacao))) %>% 
  mutate(mr12 = coefficients(glm(Growth_Rate ~ mr_12, family = "gaussian", data = correlacao))) %>% 
  mutate(mr13 = coefficients(glm(Growth_Rate ~ mr_13, family = "gaussian", data = correlacao))) %>% 
  mutate(mr14 = coefficients(glm(Growth_Rate ~ mr_14, family = "gaussian", data = correlacao))) %>% 
  mutate(mr15 = coefficients(glm(Growth_Rate ~ mr_15, family = "gaussian", data = correlacao))) %>% 
  mutate(mr16 = coefficients(glm(Growth_Rate ~ mr_16, family = "gaussian", data = correlacao))) %>% 
  mutate(mr17 = coefficients(glm(Growth_Rate ~ mr_17, family = "gaussian", data = correlacao))) %>% 
  mutate(mr18 = coefficients(glm(Growth_Rate ~ mr_18, family = "gaussian", data = correlacao))) %>% 
  mutate(mr19 = coefficients(glm(Growth_Rate ~ mr_19, family = "gaussian", data = correlacao))) %>% 
  mutate(mr20 = coefficients(glm(Growth_Rate ~ mr_20, family = "gaussian", data = correlacao))) %>% 
  mutate(mr21 = coefficients(glm(Growth_Rate ~ mr_21, family = "gaussian", data = correlacao))) %>% 
  mutate(mr22 = coefficients(glm(Growth_Rate ~ mr_22, family = "gaussian", data = correlacao))) %>% 
  mutate(mr23 = coefficients(glm(Growth_Rate ~ mr_23, family = "gaussian", data = correlacao))) %>% 
  mutate(mr24 = coefficients(glm(Growth_Rate ~ mr_24, family = "gaussian", data = correlacao))) %>% 
  mutate(mr25 = coefficients(glm(Growth_Rate ~ mr_25, family = "gaussian", data = correlacao))) %>% 
  mutate(mr26 = coefficients(glm(Growth_Rate ~ mr_26, family = "gaussian", data = correlacao))) %>% 
  mutate(mr27 = coefficients(glm(Growth_Rate ~ mr_27, family = "gaussian", data = correlacao))) %>% 
  mutate(mr28 = coefficients(glm(Growth_Rate ~ mr_28, family = "gaussian", data = correlacao))) %>% 
  mutate(mr29 = coefficients(glm(Growth_Rate ~ mr_29, family = "gaussian", data = correlacao))) %>% 
  mutate(mr30 = coefficients(glm(Growth_Rate ~ mr_30, family = "gaussian", data = correlacao))) %>% 
  rbind(0:30)

names(glm)[1] = "mr0"
glm_inv <- as.data.frame(t(glm[c(2, 3),])) %>% 
  rownames_to_column(var = "mr")
names(glm_inv) = c("mr", "coeficiente", "lag")


lag_grafico <- ggplot(glm_inv, aes(x = lag, y = coeficiente)) +
  geom_point() +
  geom_line() +
  geom_rect(xmin= 9, xmax= 11, ymin=-0.09, ymax=0.15, fill="coral2", size=0.1, alpha = 0.4) +
  #annotate("rect", xmin= 9, xmax= 11, ymin=-Inf, ymax=Inf, fill = "coral2", alpha = 0.4) +
  labs(title = "Correlação entre Mobility Rate e Growth Rate em Diferentes Desfasamentos (lag)",
       x = "Lag (dias)",
       y = "Correlação entre MR e GR") +
  scale_x_continuous(breaks = seq(0, 30, 2))
  
ggplotly(lag_grafico)
  

### Com pearson

coeficientes <- correlacao[-1] %>% 
  correlate() %>% 
  focus(Growth_Rate)
coeficientes[1] = 0:30
names(coeficientes) = c("Lag", "coeficiente")

ggplot(coeficientes, aes(x = Lag, y = coeficiente)) +
  geom_point() +
  geom_line()


## Por distrito







# GROWTH RATE POR MOBILITY RATE COM LAG 10 (A)

## Nacional

grmr_grafico <- ggplot(correlacao, aes(x = mr_10, y = Growth_Rate)) +
  geom_point(size = 0.7, aes(text = paste('Mobility Rate: ', mr_10,
                          '<br>Growth Rate:', Growth_Rate))) +
  geom_smooth(method = "lm", color = "black", se = FALSE, formula = y~x, size = 0.7) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +  
  ylim(0, 1.5) +
  labs(title = "Relação entre Mobility Rate e Growth Rate para Lag de 10 dias",
       x = "Mobility Rate",
       y = "Growth Rate") +
  scale_x_continuous(breaks = seq(0, 1, 0.1))

ggplotly(grmr_grafico, tooltip = "text")


## Por distrito




# EVOLUCAO DO GROWTH RATE (B)

## Nacional

gr_evolucao_grafico <- ggplot(correlacao, aes(x = data, y = Growth_Rate)) +
  geom_point(size = 0.7, aes(text = paste('Data: ', data,
                              '<br>Growth Rate:', Growth_Rate))) +
  geom_smooth(color = "black", se = FALSE, formula = y~x, size = 0.7) +
  ylim(0, 2) + # ver se isto pode ser mesmo aplicado
  labs(title = "Evolução do Growth Ratio",
       x = "Mês",
       y = "Growth Rate") +
  scale_x_date(breaks = "months", date_labels = "%b")
  

ggplotly(gr_evolucao_grafico, tooltip = "text")

## Por distrito





# EVOLUCAO MEDIA DOS ULTIMOS 3 DIAS (D)

## Nacional

rollmean_3_nacional <- as.data.frame(cbind(covid19pt[3:nrow(covid19pt),1], as.data.frame(rollmean(covid19pt[,12], k=3))))

rollmean_3_nacional_grafico <- ggplot(rollmean_3_nacional, aes(x = data, y = confirmados_novos)) + 
  geom_point(size = 0.7, aes(text = paste('Data: ', data,
                                          '<br>Novos casos (Média):', confirmados_novos))) +
  geom_smooth(color = "black", se = FALSE, formula = y~x, size = 0.7) +
  labs(title = "Evolução dos Novos Casos (Média dos Últimos 3 dias)",
       x = "Mês",
       y = "Novos Casos (Média dos Últimos 3 dias)") +
  scale_x_date(breaks = "months", date_labels = "%b")

ggplotly(rollmean_3_nacional_grafico, tooltip = "text")


## Por distrito


