#IMPORTAR LIBRARIES
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpmisc)
library(corrr)
library(ggthemes)

#IMPORTAR BASE DE DADOS DO GOOGLE DISPONÍVEL EM <https://www.google.com/covid19/mobility/>
mobilidade <- fread("https://raw.githubusercontent.com/EpiVet2020/Mobilidade_COVID19/main/google_mobilidade_pt.csv?token=AQ6V32LFTRBQ7C7ITEKZMIS7SBPAQ")

## por as datas em formato data
mobilidade$date <- as.Date(mobilidade$date,format = "%d-%m-%Y")


# IMPORTAR BASE DE DADOS DO COVID19 EM PORTUGAL DISPONIVEL EM: <https://github.com/dssg-pt/covid19pt-data>
covid19pt <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")

## por as datas em formato data
covid19pt$data <- as.Date(as.character(covid19pt$data),format = "%d-%m-%Y")


# TRATAR BASE DE DADOS DA MOBILIDADE

## Selecionar apenas dados de Portugal (não por distrito) e a partir de 2020-03-03 porque e quanto temos valores de GR
mobilidade_pt <- mobilidade %>% 
  filter(sub_region_1 == "") %>% 
  filter(date >= "2020-03-03") 

## Normalizar a mobilidade para que o 0 passe a representar a ausência de mobilidade
mobilidade_pt[,9:14] = lapply(mobilidade_pt[,9:14], function(x) {(x/100)+1})



# CALCULO TAXA DE CRESCIMENTO DE NOVOS CASOS
##Para isso, fizemos uma tabela com uma coluna para a data e outra coluna para a divisao. Para a data, começa na linha 7 porque e o 
##primeiro dia em que temos registos dos 7 dias anteriores. Para o numerador tem de se comecar na linha 5 pois o primeiro valor que queremos e 
##para a linha 7 e ele precisa das duas linhas anteriores para fazer a rollmean dos ultimos 3 dias. Para o demoninador nao precisamos de 
##especificar onde queremos que comece porque ele so comeca quando tem 7 registos disponiveis

gr <- as.data.frame(cbind(covid19pt[7:nrow(covid19pt),1], as.data.frame(log(rollmean(covid19pt[5:nrow(covid19pt),12], k=3))
                                                                        /log(rollmean(covid19pt[,12], k = 7)))))
names(gr) <- c("data", "Growth_Rate")

# Grafico da evolucao da taxa de crescimento de novos casos a nivel nacional
gr_evolucao_grafico <- ggplot(gr, aes(x = data, y = Growth_Rate)) +
  geom_point(size = 0.7, aes(text = paste('Data: ', data,
                                          '<br>Taxa de Crescimento de Novos Casos:', Growth_Rate))) +
  geom_smooth(color = "#64CEAA", se = FALSE, formula = y~x, size = 0.7) +
  ylim(0.7, 1.5) + # ver se isto pode ser mesmo aplicado
  labs(title = "Evolução da Taxa de Crescimento de Novos Casos (GR)",
       x = "Mês",
       y = "GR") +
  theme(plot.title = element_text(size=14),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10)) +
  scale_x_date(breaks = "months", date_labels = "%b")


ggplotly(gr_evolucao_grafico, tooltip = "text")



# TABELA COM AS VÁRIAS MOBILIDADES E TAXA DE CRESCIMENTO DE NOVOS CASOS
## mudar nome da coluna de date para data
names(mobilidade_pt)[8] = "data"

## juntar pela coluna da data
gr_mobilidade <- left_join(mobilidade_pt, gr, by = "data")



# GRAFICOS DA EVOLUCAO DAS DIFERENTES MOBILIDADES

gr_mobilidade_melt <- melt(gr_mobilidade[,8:14], id.vars = "data")

ggplot(gr_mobilidade_melt, aes(x = data, y = value, color = variable)) +
  geom_point(size = 1, aes(text = paste('Data: ', data,
                                          '<br>Taxa de Mobilidade:', value,
                                          '<br>Categoria do Local: ', variable))) +
  geom_smooth(aes(group=variable), se = FALSE, formula = y~x, size = 1) +
  scale_color_discrete(name = "Categoria do Local", labels = c("Retalho e Lazer", "Mercearias e Farmácias", "Parques", "Estações Transp. Público", "Locais de Trabalho",
                                "Residencial")) +
  labs(title = "Evolução da Taxa de Mobilidade (MR) para as Diferentes Categorias de Locais",
       x = "Mês",
       y = "MR") +
  theme(plot.title = element_text(size=13),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10)) +
  scale_x_date(breaks = "months", date_labels = "%b")



# CORRELACAO MArCO - HOJE


## Criar variavel com valores do 0 ao 30

lags_google <- seq(30)

## Atribuir nome a cada futura coluna comecando com mr_ tendo depois o numero respetivo

lag_names_google <- paste("mr", formatC(lags_google, width = nchar(max(lags_google))), 
                   sep = "_")

## Funcao para fazer com que cada coluna seja a coluna anterior descendo uma linha

lag_functions_google <- setNames(paste("lag(., ", lags_google, ")"), lag_names_google)


## Adicionar as colunas anteriores a tabela correlacao

gr_mobilidade_lags <- gr_mobilidade %>% 
  mutate_at(vars(retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline), funs_(lag_functions_google))


## Tabela com a correlacao da mobilidade para cada categoria de local para diferentes desfasamentos com a 
##taxa de crescimento de novos casos de marco a hoje

correlacao_google <- gr_mobilidade_lags[,-c(1:8)] %>% 
  correlate() %>% 
  focus(Growth_Rate) %>% 
  mutate(Lag = rep(0:30, each=6))

correlacao_google[1] = rep(c("Retalho e Lazer", "Mercearias e Farmácias", "Parques", "Estações Transp. Público", "Locais de Trabalho",
                             "Residencial"), times=31)
names(correlacao_google)[1:2] <-c("Categoria de local", "Correlacao")  


## Fazer um grafico da correlacao da mobilidade para cada categoria de local para diferentes desfasamentos com a 
##taxa de crescimento de novos casos de marco a hoje

correlacao_google_grafico <- ggplot(correlacao_google, aes(x = Lag, y = Correlacao, color= `Categoria de local`)) +
  geom_point(aes(text = paste('Lag: ', Lag,
                              '<br>Correlação: ', Correlacao,
                              '<br>Categoria de local: ', `Categoria de local`))) +
  geom_line() +
  facet_wrap(.~`Categoria de local`)+
  theme(plot.title = element_text(size=12, face = "bold"),
        legend.text = element_text(size=8),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  labs(title = "Correlação entre a MR para Diferentes Categorias de Locais e a GR em Diferentes Desfasamentos (Lag) entre Março e Hoje",
       x = "Lag (dias)",
       y = "Correlação entre MR e GR") +
  scale_x_continuous(breaks = seq(0, 30, 4))

ggplotly(correlacao_google_grafico, tooltip = "text")




# CORRELACAO MARCO - MAIO

## Tabela com a correlacao da mobilidade para cada categoria de local para diferentes desfasamentos com a 
##taxa de crescimento de novos casos de marco a maio

gr_mobilidade_lags_marco_maio <- gr_mobilidade_lags %>% 
  filter(data <= "2020-05-11")

correlacao_google_marco_maio <- gr_mobilidade_lags_marco_maio[,-c(1:8)] %>% 
  correlate() %>% 
  focus(Growth_Rate) %>% 
  mutate(Lag = rep(0:30, each=6))

correlacao_google_marco_maio[1] = rep(c("Retalho e Lazer", "Mercearias e Farmácias", "Parques", "Estações Transp. Público", "Locais de Trabalho",
                             "Residencial"), times=31)
names(correlacao_google_marco_maio)[1:2] <-c("Categoria de local", "Correlacao")  


## Fazer um grafico da correlacao da mobilidade para cada categoria de local para diferentes desfasamentos com a
##taxa de crescimento de novos casos de marco e maio

correlacao_google_marco_maio_grafico <- ggplot(correlacao_google_marco_maio, aes(x = Lag, y = Correlacao, color= `Categoria de local`)) +
  geom_point(aes(text = paste('Lag: ', Lag,
                              '<br>Correlação: ', Correlacao,
                              '<br>Categoria de local: ', `Categoria de local`))) +
  geom_line() +
  geom_rect(xmin= c(8, 8, 8, 4, 4, 4), xmax= c(9, 9, 9, 10, 5, 6),
            ymin=-1, ymax=1, fill = c("#F564E3", "#00BA38", "#00BFC4", "#F8766D", "#B79F00", "#619CFF"), size=0.1,
            alpha = 0.4,data = correlacao_google[1:6,],
            aes(text= c("Correlação \nsuperior a 0.8", "Correlação \nsuperior a 0.7", "Correlação \nsuperior a 0.75",
                        "Correlação \nsuperior a 0.8", "Correlação \nsuperior a 0.8", "Correlação \ninferior a -0.75"))) +
  facet_wrap(.~`Categoria de local`)+
  theme(plot.title = element_text(size=11, face = "bold"),
        legend.text = element_text(size=8),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  labs(title = "Correlação entre a MR para Diferentes Categorias de Locais e a GR em Diferentes Desfasamentos (Lag) de Março a Maio",
       x = "Lag (dias)",
       y = "Correlação entre MR e GR") +
  scale_x_continuous(breaks = seq(0, 30, 4)) + 
  scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2))

ggplotly(correlacao_google_marco_maio_grafico, tooltip = "text")


## Ver relacao para lags otimos de marco a maio

### Selecionar as colunas para as diferentes categorias de local com o lag otimo calculado anteriormente
relacao_marco_maio <- gr_mobilidade_lags_marco_maio %>% 
  select(Growth_Rate, `transit_stations_percent_change_from_baseline_mr_ 5`,`workplaces_percent_change_from_baseline_mr_ 5`,
         `grocery_and_pharmacy_percent_change_from_baseline_mr_ 9`, `parks_percent_change_from_baseline_mr_ 9`, 
         `residential_percent_change_from_baseline_mr_ 5`,`retail_and_recreation_percent_change_from_baseline_mr_ 9`)

### Dar nomes as colunas
names(relacao_marco_maio)[-1] <- c("Estações Transp. Público Lag 5 dias", "Locais de Trabalho Lag 5 dias", 
                                   "Mercearias e Farmácias Lag 9 dias", "Parques Lag 9 dias",
                                   "Residencial Lag 5 dias","Retalho e Lazer Lag 9 dias")

### Fazer um melt para ficarmos com apenas 3 colunas
relacao_marco_maio_melt <- melt(relacao_marco_maio, id.vars = "Growth_Rate")
names(relacao_marco_maio_melt)[-1] <- c("Categoria de Local", "MR")


## Fazer um grafico da relacao da mobilidade para cada categoria de local para lag otimo com a
##taxa de crescimento de novos entre casos de marco e maio
ggplot(relacao_marco_maio_melt, aes(x = MR, y = Growth_Rate, color = `Categoria de Local`)) +
  geom_point(size = 1, aes(text = paste('Taxa de Mobilidade: ', MR,
                                          '<br>Taxa de Crescimento de Novos Casos:', Growth_Rate,
                                          '<br>Categoria de Local: ', `Categoria de Local`))) +
  facet_wrap(relacao_marco_maio_melt$`Categoria de Local`)+
  geom_smooth(method = "lm", se = FALSE, formula = y~x, size = 1) +
  stat_poly_eq(formula = y~x,
               aes(label = paste(..eq.label..)),
               parse = TRUE, label.y = 0.9) +
  theme(plot.title = element_text(size=11, face = "bold"),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  ylim(0.8, 1.3) +
  labs(title = "Relação da MR com a GR de Diferentes Categorias para o respetivo Lag Ótimo entre Março e Maio",
       x = "MR para Lag Ótimo",
       y = "GR") +
   scale_x_continuous(breaks = seq(0, 1.6, 0.2))





# CORRELACAO MAIO - HOJE


## Tabela com a correlacao da mobilidade para cada categoria de local para diferentes desfasamentos com a 
##taxa de crescimento de novos casos de maio a hoje

gr_mobilidade_lags_maio_hoje <- gr_mobilidade_lags %>% 
  filter(data > "2020-05-11")

correlacao_google_maio_hoje <- gr_mobilidade_lags_maio_hoje[,-c(1:8)] %>% 
  correlate() %>% 
  focus(Growth_Rate) %>% 
  mutate(Lag = rep(0:30, each=6))

correlacao_google_maio_hoje[1] = rep(c("Retalho e Lazer", "Mercearias e Farmácias", "Parques", "Estações Transp. Público", "Locais de Trabalho",
                                        "Residencial"), times=31)
names(correlacao_google_maio_hoje)[1:2] <-c("Categoria de local", "Correlacao")  


## Fazer um grafico da correlacao da mobilidade para cada categoria de local para diferentes desfasamentos com a
##taxa de crescimento de novos casos de maio a hoje

correlacao_google_maio_hoje_grafico <- ggplot(correlacao_google_maio_hoje, aes(x = Lag, y = Correlacao, color= `Categoria de local`)) +
  geom_point(aes(text = paste('Lag: ', Lag,
                              '<br>Correlação: ', Correlacao,
                              '<br>Categoria de local: ', `Categoria de local`))) +
  geom_line() +
  facet_wrap(.~`Categoria de local`)+
  theme(plot.title = element_text(size=11, face = "bold"),
        legend.text = element_text(size=8),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  labs(title = "Correlação entre a MR para Diferentes Categorias de Locais e a GR em Diferentes Desfasamentos (Lag) de Maio a Hoje",
       x = "Lag (dias)",
       y = "Correlação entre MR e GR") +
  scale_x_continuous(breaks = seq(0, 30, 4))

ggplotly(correlacao_google_maio_hoje_grafico, tooltip = "text")


