# IMPORTE DATOS -----------------------------------------------------------

getwd()
setwd("C:/Users/Ramiro/rcadavid@grameenfoundation.org/7. Proyectos/Activos/DelosAndes Cooperativa/Dise√±o de herramientas/07. Calidad de taza")
archivos <- list.files(pattern = "*.csv")
n.archivos <- length(archivos)
data <- read.csv("exportCatacion2017-03-02_07_55_29.csv", sep = ";")
noms.cols <- names(data)
data <- data.frame(matrix(nrow = 0, ncol = length(names(data)))) 

for(i in 1:n.archivos) {
     if(i == 1) {
          data <- read.csv(archivos[i], sep = ";")
     } else {
          temp <- read.csv(archivos[i], sep = ";")
          data <- rbind(data, temp)
     }
}
rm(temp)
library(dplyr)
data <- select(data, -X)


# INSPECCI”N DATOS --------------------------------------------------------

sapply(data, class)
str(data)
summary(data)
View(data)
tabl <- sapply(data, table)


# PREPARACI”N DATOS PARA CARGAR -------------------------------------------



# CARGA DE DATOS ----------------------------------------------------------

# Login en Salesforce



