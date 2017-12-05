# RESPALDO DE DATOS EXISTENTES --------------------------------------------

# Conectar a Salesforce
library(RForcecom)
username <- "admin@andes.org"
password <- "gfadm913XQWRiDpPU6NzJC9Cmm185FF2"
instanceURL <- "https://taroworks-8629.cloudforce.com/"
apiVersion <- "36.0"
session <- rforcecom.login(username, password, instanceURL, apiVersion)

# Campos a exportar
campos.muestras <- rforcecom.getObjectDescription(session, "CQT_CoffeeSample__c")
campos.muestras <- campos.muestras$name
campos.analisis <- rforcecom.getObjectDescription(session, "CQT_Analysis__c")
campos.analisis <- campos.analisis$name

# Descargar datos
bu.muestras <- rforcecom.retrieve(session, "CQT_CoffeeSample__c",
                                  campos.muestras)
bu.analisis <- rforcecom.retrieve(session, "CQT_Analysis__c",
                                  campos.analisis)

# Incluir fecha al nombre del archivo
setwd("Datos")
nom.bu.muestras <- paste("backup-muestras_", as.character(Sys.Date()),
                         ".csv", sep = "")
nom.bu.analisis <- paste("backup-analisis_", as.character(Sys.Date()),
                         ".csv", sep = "")

# Guardar copia de los datos
write.csv(bu.muestras, nom.bu.muestras)
write.csv(bu.analisis, nom.bu.analisis)


# DESCARGAR DATOS NUEVOS --------------------------------------------------

# 1. Descargar los datos del enlace de Comtic teniendo en cuenta de las
# fechas cargadas previamente http://catacion.comtic.co/dev/form_export
# 
# Ingresar a este reporte en Salesforce para consultar la fecha del registro
# más reciente creado: https://taroworks-8629.cloudforce.com/00O36000007EPdO

# ATENCIÓN: ACTUALIZAR NOMBRE DE ARCHIVO QUE SE VA A CARGAR (archivo.importar)
archivo.importar <- "exportCatacion2017-12-05_02_14_31.csv"

calidad <- read.csv(archivo.importar, sep = ";",
                    encoding = "latin1")

# INSPECCIÓN DATOS --------------------------------------------------------

sapply(calidad, class)
str(calidad)
summary(calidad)
View(calidad)
tabl <- sapply(calidad, table)


# MUESTRAS ----------------------------------------------------------------

###
# Seleccionar variables de Muestras y cambiar formatos
###

# Seleccionar variables
library(dplyr)
muestra <- select(calidad, fecha.facturacion:Laboratorio)

# Para fecha.facturacion, eliminar la hora 
muestra$fecha.facturacion <- substr(muestra$fecha.facturacion, 1, 10)
muestra$fecha.facturacion <- as.Date(muestra$fecha.facturacion)

# Cambiar todas las ',' en sesión por ';'
muestra$Sesion <- gsub(",", ";", muestra$Sesion)

# El campo factura y Muestra deben ser formateados a texto
class(muestra$factura) # Si no es character o factor, correr la línea siguiente
# muestra$factura <- as.character(muestra$factura)
class(muestra$Muestra) # Si no es character o factor, correr la línea siguiente
# muestra$Muestra <- as.character(muestra$Muestra)

# Dejar solo valores únicos para cada muestra
muestra <- unique(muestra) 


####
# Agregar ids de laboratorios
####

## Consultar nombres de laboratorios
labs.query <- "SELECT Id, Name FROM CQT_Lab__c"
labs <- rforcecom.query(session, labs.query)

## Agreagar ID de Salesforce a laboratorios
muestra <- left_join(muestra, labs, by = c("Laboratorio" = "Name"))
muestra <- select(muestra, -Laboratorio)


###
# Cargar a Salesforce
###

# Agregar API Names
names(muestra) <- c("Date__c", "Invoice_Number__c", "Name",
                    "NationalId_ExternalFile__c", "FarmName_ExternalFile__c",
                    "Session__c", "Lab__c")

# Cargar datos

## Insert job
job_info <- rforcecom.createBulkJob(session, 
                                    operation='insert', 
                                    object='CQT_CoffeeSample__c')
## Cargar datos
batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=job_info$id, 
                                          muestra, 
                                          multiBatch = TRUE, 
                                          batchSize=100)
# Estado de cada batch
batches_status <- lapply(batches_info,
                         FUN=function(x){
                               rforcecom.checkBatchStatus(session,
                                                          jobId=x$jobId,
                                                          batchId=x$id)
                         })
batches_status
# Detalles de cada batch
batches_detail <- lapply(batches_info,
                         FUN=function(x){
                               rforcecom.getBatchDetails(session,
                                                         jobId=x$jobId,
                                                         batchId=x$id)
                         })
close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)


# ANÁLISIS ----------------------------------------------------------------

###
# Seleccionar variables de Análisis
###
analisis <- select(calidad, Muestra, Catador:Notas.Adicionales)


###
# Agregar ids de catadores
###

# Consultar nombres de catadores
catadores.query <- "SELECT Id, FullName__c FROM CQT_Taster__c"
catadores <- rforcecom.query(session, catadores.query)

# Agregar ID de Salesforce a catadores
analisis <- left_join(analisis, catadores, by = c("Catador" = "FullName__c"))
table(is.na(analisis$Id)) # Todos deben tener valor FALSE

analisis <- data.frame(Muestra = analisis$Muestra,
                       Taster__c = analisis$Id,
                       select(analisis, nivelTueste:Notas.Adicionales))


###
# Reemplazar ',' en pickilist multi-select por ';'
###

descrip.noms <- c("descripFragancia", "descriSabor", "descriResabio",
                  "descriAcidez", "descriCuerpo", "descriBalance",
                  "descriDefectos")

for(i in 1:length(descrip.noms)) {
      analisis[, descrip.noms[i]] <- gsub(",", ";",
                                          analisis[, descrip.noms[i]])
}


###
# Cargar a Salesforce
###

# Descargar nombres de muestras
muestras.query <- "SELECT Id, Name FROM CQT_CoffeeSample__c"
muestras.sf <- rforcecom.query(session, muestras.query)

# Agregar ID de muestras a los registros de análisis
analisis <- left_join(analisis, muestras.sf, by = c("Muestra" = "Name"))
table(is.na(analisis$Id)) #Todas las observaciones deben tener valor FALSE
analisis <- data.frame(muestra.id = analisis$Id,
                       select(analisis, Taster__c:Notas.Adicionales))

# Eliminar variables que no se van a subir
analisis <- select(analisis, -one_of("descriSabor", "descriResabio",
                                     "descriAcidez", "descriCuerpo",
                                     "descriBalance", "Tipo.Defecto")) 

# Agregar API Names a variables que se van a subir
names(analisis) <- c("CoffeeSample__c", "Taster__c", "RoastLevelOfSample__c",
                     "Qualities__c", "FraganceAroma__c", "Flavor__c", 
                     "AfterTaste__c", "Acidity__c",  "Body__c", "Balance__c",
                     "Uniformity__c", "OrganolepticDefects__c",
                     "NumDefectCups__c", "TotalScore__c", "Notes__c")

# Cargar datos
## Insert job
job_info <- rforcecom.createBulkJob(session, 
                                    operation='insert', 
                                    object='CQT_Analysis__c')
## Cargar datos
batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=job_info$id, 
                                          analisis, 
                                          multiBatch = TRUE, 
                                          batchSize=100)
# Estado de cada batch
batches_status <- lapply(batches_info,
                         FUN=function(x){
                              rforcecom.checkBatchStatus(session,
                                                         jobId=x$jobId,
                                                         batchId=x$id)
                         })
batches_status
# Detalles de cada batch
batches_detail <- lapply(batches_info,
                         FUN=function(x){
                              rforcecom.getBatchDetails(session,
                                                        jobId=x$jobId,
                                                        batchId=x$id)
                         })
close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)

# Validación final: estos registros deben salir en el siguiente reporte en SF:
# https://taroworks-8629.cloudforce.com/00O36000007EPdO


