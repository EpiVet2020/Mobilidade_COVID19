#Importar libraries


#Importar bases de dados disponíveis em <https://data.humdata.org/dataset/movement-range-maps>
temp <- tempfile()
download.file("https://data.humdata.org/dataset/c3429f0e-651b-4788-bb2f-4adbf222c90e/resource/31ca909c-10d9-458a-8720-88b54b3e3627/download/movement-range-data-2020-10-06.zip", temp)
mobilidade <- read.table(unz(temp, "movement-range-data-2020-10-06.zip"))
unlink(temp)

download("https://data.humdata.org/dataset/c3429f0e-651b-4788-bb2f-4adbf222c90e/resource/31ca909c-10d9-458a-8720-88b54b3e3627/download/movement-range-data-2020-10-06.zip", dest="movement-range-data-2020-10-06.zip", mode="wb") 
unzip ("movement-range-data-2020-10-06.zip")

library(data.table)
temp <- tempfile()
download.file("https://data.humdata.org/dataset/c3429f0e-651b-4788-bb2f-4adbf222c90e/resource/31ca909c-10d9-458a-8720-88b54b3e3627/download/movement-range-data-2020-10-06.zip", temp)
timeUse <- fread(unzip(temp, files = "movement-range-2020-10-06.txt"))
rm(temp)
