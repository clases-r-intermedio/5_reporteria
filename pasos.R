
# PENDIENTES 

# crear varias variables a la vez,
# DT[, c("v6", "v7") := .(sqrt(V1), "X")]

# DT[, ':='(v6 = sqrt(V1),
#          v7 = "X")]     # same, functional form

# modificar valores dentro de una variable 
# DT[V2 < 4, V2 := 0L]

# eliminar variables 
# DT[, v5 := NULL] o varias DT[, c("v6", "v7") := NULL]


#### By y agregar keyby

###  homologación de slice

# df[df[, .I[seq_len(10)], by = b]$V1]



library(data.table)
library(dplyr)
library(tictoc)


# podemos comparar el tiempo de ejecución entre read.csv y fread()
tictoc::tic()
censo_viviendas <- readr::read_csv("data/viviendas/Microdato_Censo2017-Viviendas.csv")
tictoc::toc()
# 66.472 sec elapsed
tictoc::tic()
censo_viviendas <- data.table::fread("data/viviendas/Microdato_Censo2017-Viviendas.csv")
tictoc::toc()
# 2.968 sec elapsed
## estandarizamos los nombres de las variables
censo_viviendas <- janitor::clean_names(censo_viviendas)

### escribiendo datos

tictoc::tic()
readr::write_csv(censo_viviendas,"csv-viviendas-censo-2017/ejemplo_viviendas.csv")
tictoc::toc()
# 99.184 sec elapsed
tictoc::tic()
data.table::fwrite(censo_viviendas,"csv-viviendas-censo-2017/ejemplo_viviendas.csv")
tictoc::toc()
# 1.17 sec elapsed

## Primer paso

# Convertir data.frame en data.table:
  
dt_viviendas <- data.table(censo_viviendas)   
# o
dt_viviendas <- as.data.table(censo_viviendas)
# o 
setdt_(censo_viviendas)




library(data.table)
set.seed(1L)

## Create a data table
DT <- data.table(V1 = rep(c(1L, 2L), 5)[-10],
                 V2 = 1:9,
                 V3 = c(0.5, 1.0, 1.5),
                 V4 = rep(LETTERS[1:3], 3))

class(DT)
DT


library(dplyr)
set.seed(1L)

## Create a data frame (tibble)
DF <- tibble(V1 = rep(c(1L, 2L), 5)[-10],
             V2 = 1:9,
             V3 = rep(c(0.5, 1.0, 1.5), 3),
             V4 = rep(LETTERS[1:3], 3))

class(DF)
DF


DT[, .(sumV2 = sum(V2)), 
   by = "V4"]

# reordered and indented:
DT[, by = V4,
   .(sumV2 = sum(V2))]

# By several groups

DT[, keyby = .(V4, V1),
   .(sumV2 = sum(V2))]



DT[, keyby = .(abc = tolower(V4)),
   .(sumV1 = sum(V1))]



DT[, keyby = V4 == "A",
   sum(V1)]


DF %>%
  group_by(V4 == "A") %>%
  summarise(sum(V1))


DT[1:6,]

DT[1:2,                # i
   .(sumV1 = sum(V1)), # j
   by = V4]            # by

DT[, .I[seq_len(10)]


