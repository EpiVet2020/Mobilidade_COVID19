#Importar libraries
library(data.table)

#Importar bases de dados disponíveis em <https://data.humdata.org/dataset/movement-range-maps>
mobilidade <- fread("C:/Users/rakac/OneDrive - Universidade de Lisboa/R/Faculdade/2.COVID19 Portugal/Partilhado/Mobilidade_COVID19/dados_mobilidade/movement-range-2020-10-04.txt")

mobilidade_2 <- fread("C:/Users/karol/Documents/R/Covid-19_estagio/Epivet2020/movement-range-2020-10-06.txt")

#"We interpreted this metric as a proxy for social distancing on the basis of the assumption that
#when individuals make fewer trips, they physically interact less."