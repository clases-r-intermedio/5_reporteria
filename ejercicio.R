library(purrr)
library(stringr)
library(dplyr)

### Primer Paso

### listamos los archivos en la carpeta
files <- list.files("data/",full.names = T)

### extraemos el nombre de los archivos
nombres <- str_extract_all(files,"(2022|2023)-(\\d{2})-[a-zA-Z]{3}") %>% str_replace_all("-","_")

### asignamos nombre a los archivos
tictoc::tic()
ene <- map(files,read.csv2)  
tictoc::toc()

### asignamos nombres anteponiendo la "a" para evitar las comillas en los nombres que comienzan con números
names(ene) <- paste0("a",nombres)

### extraemos año de los nombres de los archvo
agno <- str_extract_all(nombres,"2022|2023") %>% unlist()

### extraemos trimestres de los nombres
trimestre <- str_extract_all(nombres,"(2022|2023)_(\\d{2})_[a-zA-Z]{3}") %>% str_replace_all("-","_") %>% str_remove_all("2022_|2023_")

### generamos una variable con el año y una variable con el trimestre para cada archivo
ene <- map2(ene,agno,~mutate(.x,agno = .y))

ene <- map2(ene,trimestre,~mutate(.x,trimestre = .y))

### revisamos las variables
# ene$a2022_01_def$agno
# ene$a2022_01_def$trimestre


## Segundo Paso

# 1. generemos una función que cree las variables de interés:


# Población en edad de trabaja = pet
# Fuerza de trabajo = fdt
# Ocupados
# desocupados

f_variables_interes = function(datos){
  
  datos %>%
    mutate(pet = if_else(edad >= 15 ,1,0),
           fdt = if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0), # fuerza de trabajo
           ocupados = if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0), # persona ocupada
           desocupados = if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0)) # personas desocupadas 
  
}

ene <- map(ene,f_variables_interes)

sum(ene$a2023_05_amj$fdt*ene$a2023_05_amj$fact_cal)

# 2 Es importante considerar que estas variables deben estar ponderabas 
# por el factor de expansión para obtener los datos a nivel poblacionales,
# Si se comparan con los valores publicados los valores son similares?

## Para lograr lo anterior es necesara una función que multiplique cualquier variable por el factor de expansión, utilizando lazy_eval

ponderar = function(var,fact){
  
 {{var}}*{{fact}}
  
}

# Ahora necesitamos calcular la ponderación por todas nuestras variables de interés, intenta utilizar lo aprendido en clases, para evitar repetir código

# seleccionar solamente variables de interés 

variables = c("sexo","hogar","region","provincia","sector","fact_cal","agno","trimestre","fdt","ocupados","desocupados","pet","ocupados_exp","desocupados_exp",
  "pet_exp","fdt_exp","agno","trimestre")         

ene <- map_df(ene,~mutate(.x,across(c(ocupados,desocupados,pet,fdt), list(exp = ~ ponderar(.,fact_cal)))) %>% select(all_of(variables)))

# Paso 3 calculamos 3 tasas principales
# ahora calcularemos las principales tasas publicadas

# Tasa desocupación
# Tasa ocupación
# Tasa participación

### tasa desocupados 6.1.1 Tasa de desocupación (TD)
## Corresponde al número de personas desocupadas, expresado como porcentaje de la fuerza
## de trabajo (entendida como la sumatoria de personas ocupadas y desocupadas):

# TD = [DO/FT] * 100

# Donde,
# 𝐷𝑂 = Personas desocupadas
# 𝐹𝑇 = Fuerza de trabajo 

###  Tasa de ocupación (TO)
# Corresponde al número de personas ocupadas como porcentaje de la población en edad de
# trabajar (es decir, de quince años o más):

# TO = [O/PET]*100

# Donde,
# 𝐷𝑂 = Personas desocupadas
# 𝑃𝐸𝑇 = Población en edad de trabajar 

# Tasa de participación (TP)
# Corresponde al número de personas en la fuerza de trabajo expresado como porcentaje de la población en edad de trabajar.

# TP = [FT/PET]*¨100

# Donde,
# 𝐹𝑇 = Fuerza de trabajo
# 𝑃𝐸𝑇 = Población en edad de trabajar 


# Tasa desocupación


  
# Tasa ocupación
  
ene_df = ene %>% bind_rows()

tictoc::tic()

ene_df %>% 
  group_by(agno, trimestre, region, provincia) %>% 
  summarise(tasa_desocupacion = sum(desocupados_exp)/sum(pet_exp)*100) %>% 
  group_by(region) %>% 
  arrange(-tasa_desocupacion) %>% 
  slice(1)
  

ene_df %>% 
  group_by(agno, trimestre, region, provincia) %>% 
  summarise(tasa_desocupacion = sum(desocupados_exp)/sum(pet_exp)*100) %>% 
  group_by(region) %>% 
  arrange(-tasa_desocupacion) %>% 
  slice(1)
  

tictoc::toc()
    
library(data.table)

ene_dt <- as.data.table(ene_df)

tictoc::tic()

ene_dt[,tasa_desocupacion := sum(desocupados_exp)/sum(pet_exp)*100,by=.(agno, trimestre, region, provincia)][
  tasa_desocupacion==max(tasa_desocupacion),by=.(region)]


tictoc::toc()


  
# Tasa participación

DT2 = data.table(A=5:1, B=letters[5:1])
  setkey(DT, B)   # reorders table and marks it sorted
DT[J("b")]      # returns the 2nd row
DT[list("b")]   # same
DT[.("b")] 





ene %>% 
map(setDT(.)[,tasa_desocupacion := sum(desocupados_exp)/sum(pet_exp)*100,by=.(agno, trimestre, region, provincia)][])



dat %>% 
  summarise(tasa_ocupacion = sum(ocupado*fact_cal)/sum(pet*fact_cal)*100)

sum(dat$ocupado*dat$fact_cal)/sum(dat$pet*dat$fact_cal)*100










