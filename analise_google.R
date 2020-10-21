#IMPORTAR LIBRARIES
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggpmisc)
library(corrr)

#IMPORTAR BASE DE DADOS DO GOOGLE DISPONÍVEL EM <https://www.google.com/covid19/mobility/>
mobilidade <- fread("https://raw.githubusercontent.com/EpiVet2020/Mobilidade_COVID19/main/google_mobilidade_pt.csv?token=AQ6V32ILQLRFMW2HVSYRQXC7SAUT4")

## por as datas em formato data
mobilidade$date <- as.Date(mobilidade$date,format = "%d-%m-%Y")


# IMPORTAR BASE DE DADOS DO COVID19 EM PORTUGAL DISPONIVEL EM: <https://github.com/dssg-pt/covid19pt-data>
covid19pt <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")

## por as datas em formato data
covid19pt$data <- as.Date(as.character(covid19pt$data),format = "%d-%m-%Y")


# TRATAR BASE DE DADOS DA MOBILIDADE

## Selecionar apenas dados de Portugal (não por distrito)
mobilidade_pt <- mobilidade %>% 
  filter(sub_region_1 == "")

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



# CORRELACAO

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




# RETAIL E LAZER

## Correlacao 

correlacao_google_retail <- gr_mobilidade_lags[,-c(1:8)] %>% 
  select(starts_with("retail_and_recreation_percent_change_from_baseline")) %>% 
  mutate(Growth_Rate= gr_mobilidade_lags$Growth_Rate) %>% 
  correlate() %>% 
  focus(Growth_Rate)
correlacao_google_retail[1] = 0:30
names(correlacao_google_retail) = c("Lag", "correlacao")

correlacao_google_retail_grafico <- ggplot(correlacao_google_retail, aes(x = Lag, y = correlacao)) +
  geom_point(aes(text = paste('Lag: ', Lag,
                              '<br>Correlação: ', correlacao))) +
  geom_line() +
  geom_rect(xmin= 23, xmax= 24, ymin=-0.09, ymax=1, fill="#64CEAA", size=0.1, alpha = 0.4, 
            aes(fill="Correlação \nsuperior a 0.4")) +
  theme(legend.title = element_blank(),
        plot.title = element_text(size=11),
        legend.text = element_text(size=6),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  labs(title = "Correlação entre Taxa de Mobilidade (MR) em Retail e Lazer e Taxa de Crescimento \nde Novos Casos (GR) em Diferentes Desfasamentos (Lag)",
       x = "Lag (dias)",
       y = "Correlação entre MR em Retail e Lazer e GR") +
  scale_x_continuous(breaks = seq(0, 30, 2))

ggplotly(correlacao_google_retail_grafico, tooltip = "text")


## Ver relacao para lag 24 dias

relacao_retail_gr_grafico <- ggplot(gr_mobilidade_lags, aes(x = retail_and_recreation_percent_change_from_baseline_mr_24, y = Growth_Rate)) +
  geom_point(size = 0.7, aes(text = paste('Taxa de Mobilidade: ', retail_and_recreation_percent_change_from_baseline_mr_24,
                                          '<br>Taxa de Crescimento de Novos Casos:', Growth_Rate))) +
  geom_smooth(method = "lm", color = "#64CEAA", se = FALSE, formula = y~x, size = 0.7) +
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +  
  theme(plot.title = element_text(size=11),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  ylim(0.5, 1.3) +
  labs(title = "Relação da Taxa de Crescimento de Novos Casos (GR) com a Taxa \nde Mobilidade (MR) em Retail e Lazer para Lag de 24 dias",
       x = "MR em Retail e Lazer para Lag de 24 dias",
       y = "GR") +
  scale_x_continuous(breaks = seq(0, 1, 0.1))

ggplotly(relacao_retail_gr_grafico, tooltip = "text") %>% 
  layout(annotations = list(x = 0.7, y = 0.7, text = "y = 0.957 + 0.0727 x", showarrow = FALSE))

