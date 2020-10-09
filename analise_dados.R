#Importar libraries


#Importar bases de dados disponíveis em <https://data.humdata.org/dataset/movement-range-maps>
temp <- tempfile()
download.file("https://data.humdata.org/dataset/c3429f0e-651b-4788-bb2f-4adbf222c90e/resource/31ca909c-10d9-458a-8720-88b54b3e3627/download/movement-range-data-2020-10-06.zip", temp)
mobilidade <- read.table(unz(temp, "movement-range-data-2020-10-06.zip"))
unlink(temp)

<<<<<<< HEAD
mobilidade_2 <- fread("C:/Users/karol/Documents/R/Covid-19_estagio/Epivet2020/movement-range-2020-10-06.txt")

#"We interpreted this metric as a proxy for social distancing on the basis of the assumption that
#when individuals make fewer trips, they physically interact less."
=======
download("https://data.humdata.org/dataset/c3429f0e-651b-4788-bb2f-4adbf222c90e/resource/31ca909c-10d9-458a-8720-88b54b3e3627/download/movement-range-data-2020-10-06.zip", dest="movement-range-data-2020-10-06.zip", mode="wb") 
unzip ("movement-range-data-2020-10-06.zip")

library(data.table)
temp <- tempfile()
download.file("https://data.humdata.org/dataset/c3429f0e-651b-4788-bb2f-4adbf222c90e/resource/31ca909c-10d9-458a-8720-88b54b3e3627/download/movement-range-data-2020-10-06.zip", temp)
timeUse <- fread(unzip(temp, files = "movement-range-2020-10-06.txt"))
rm(temp)
>>>>>>> ceb2cf5864d1d6ab1bd883dd80d559d61eec4165
