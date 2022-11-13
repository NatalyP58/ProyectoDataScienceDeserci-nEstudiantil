# Librerias ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(readr)
library(psych)
library(stats)
library(openxlsx)
library(lubridate)
getwd()
options(scipen=999)



# Carga de la base de datos ----------------
df<- read_csv("Diplomado_DS_Proyecto.csv",
              na = "NULL")




df <- df %>% mutate(PROM_ANUAL=case_when(PROMEDIO_ANUAL <= 4 ~ 1,PROMEDIO_ANUAL <= 5 ~ 2,   PROMEDIO_ANUAL <=7~ 3)) 


df %>% mutate(deuda_total = ifelse(!is.na(TOTAL_DEUDA), 
                                   case_when(TOTAL_DEUDA <= 1000000 ~ 1,
                                             TOTAL_DEUDA > 1000000 & TOTAL_DEUDA <= 3000000 ~ 2,
                                             TOTAL_DEUDA <= 6000000 ~ 3), NA)) %>% select(IDENTIFICADOR, deuda_total) %>% filter(deuda_total>6000000) %>% view()

df %>% mutate(deuda_total = ifelse(!is.na(TOTAL_DEUDA), 
                                   case_when(TOTAL_DEUDA <= 1000000 ~ 1,
                                             TOTAL_DEUDA <= 3000000 ~ 2,
                                             TOTAL_DEUDA <= 6000000 ~ 3), NA))
df %>% mutate(deuda_total = case_when(TOTAL_DEUDA <= 1000000 ~ 1,
                                             TOTAL_DEUDA <= 3000000 ~ 2,
                                             TOTAL_DEUDA <= 6000000 ~ 3)) %>% filter(is.na(deuda_total))

df %>% filter(is.na(TOTAL_DEUDA))
glimpse(df)
#La función tabla de contingencia  recibe un data frame y la varible que quieres sacar las tablas de contingencia 
#NOTA: como solo nos interesa la deserción solo vi los porcentajes de deserción, es decir, en porcentaje de los que se matriculan  
#      en la variable sea cero 


TablaContingencia <- function(df, variable) {
  
  
  a <- df %>% 
    group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,SE_MATRICULA,  {{variable}} )%>% 
    summarise(N=n(), .groups ="drop" ) %>% 
    group_nest(SE_MATRICULA) %>% 
    .[1,2] %>% 
    .[[1]] %>% .[[1]] 
  
  
  b <- df %>% 
    group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE) %>% 
    summarise(TOTAL=n())
  
  
  c <- left_join(a,b) %>% mutate(Ptje_Desercion = round(N/TOTAL*100, 2)) %>% 
    group_nest(ANNIO_INGRESO) %>% 
    unnest(ANNIO_INGRESO) %>% 
    group_by(ANNIO_INGRESO) %>%
    mutate(R= map(data, ~{names(.)[names(.) == "Ptje_Desercion"] <-paste0('Ptje_Desercion_', ANNIO_INGRESO); .}))%>% .[,3] %>% 
    .[[1]] 
  
  d <- left_join(c[[1]] %>% select(!TOTAL& !N), c[[2]] %>% select(!N,!TOTAL)%>% select(!TOTAL& !N) ) %>% 
    left_join(c[[3]] %>% select(!TOTAL& !N)) %>% 
    left_join(c[[4]] %>% select(!TOTAL& !N))
  
  return(d)
  
}

#tabla de contingencia por año de matricula e ingreso de todos ,para la variable de sexo 
TablaContingencia(df,PERS_SEXO)
#tabla de contingencia por año de matricula e ingreso de enfermeria ,para la variable de sexo 
TablaContingencia(df %>% filter(CODUA==4111),PERS_SEXO)
#tabla de contingencia por año de matricula e ingreso de enfermeria ,para la variable de sexo 
TablaContingencia(df %>% filter(CODUA==3353),PERS_SEXO)

glimpse(df)
TablaContingencia(df,EDAD_POR_ANNIO_MATRICULA) %>% view()

# puedes exportar los datos asi, en file debes poner la ruta de donde quieres que quede tu archivo


# TablaContingencia(df,PERS_SEXO) %>% 
#   write_csv(.,file = "")   


#EXPLICACIÓN: las columnas corresponden al porcentaje de deserción respecto al año de ingreso,
#             las filas representan  en año de matricula , en este caso aparece dos veces cada año, porque esta el factor del sexo F y M

TablaContingencia(df,PERS_SEXO)
#  ANNIO_MATRICULA_INDICE PERS_SEXO Ptje_Desercion_2017 Ptje_Desercion_2018 Ptje_Desercion_2019 Ptje_Desercion_2020
# <dbl> <chr>                   <dbl>               <dbl>               <dbl>               <dbl>
# 1                   2018 F                        5.17               NA                  NA                  NA   
# 2                   2018 M                        4.02               NA                  NA                  NA   
# 3                   2019 F                        7.47                6.38               NA                  NA   
# 4                   2019 M                        8.62                5.67               NA                  NA   
# 5                   2020 F                        8.62                6.38                6.11               NA   
# 6                   2020 M                       13.2                 9.93                8.4                NA   
# 7                   2021 F                        8.62                9.22                9.16                5.56
# 8                   2021 M                       13.2                 9.93                9.92                4.63


df %>% glimpse()

TablaContingencia(df,PROM_ANUAL)


df %>% 
  group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,SE_MATRICULA,  PROM_ANUAL )%>% 
  summarise(N=n(), .groups ="drop" ) %>% 
  group_nest(SE_MATRICULA) %>% 
  .[2,2] %>% 
  .[[1]] %>% .[[1]] 

a <- df %>% 
  group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,SE_MATRICULA, PERS_SEXO  )%>% 
  summarise(N=n(), .groups ="drop" ) %>% 
  group_nest(SE_MATRICULA) %>% 
  .[1,2] %>% 
  .[[1]] %>% .[[1]] 


b <- df %>% 
  group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,PERS_SEXO) %>% 
  summarise(TOTAL=n())


c <- left_join(a,b) %>% mutate(Ptje_Desercion = round(N/TOTAL*100, 2)) %>% 
  group_nest(ANNIO_INGRESO) %>% 
  unnest(ANNIO_INGRESO) %>% 
  group_by(ANNIO_INGRESO) %>%
  mutate(R= map(data, ~{names(.)[names(.) == "Ptje_Desercion"] <-paste0('Ptje_Desercion_', ANNIO_INGRESO); .}))%>% .[,3] %>% 
  .[[1]] 

d <- left_join(c[[1]] %>% select(!TOTAL& !N), c[[2]] %>% select(!N,!TOTAL)%>% select(!TOTAL& !N) ) %>% 
  left_join(c[[3]] %>% select(!TOTAL& !N)) %>% 
  left_join(c[[4]] %>% select(!TOTAL& !N))


TablaContingencia2 <- function(df, variable, margin=1) {
  
  a <- df %>% 
    group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,SE_MATRICULA,  {{variable}} )%>% 
    summarise(N=n(), .groups ="drop" ) %>% 
    group_nest(SE_MATRICULA) %>% 
    .[2,2] %>% 
    .[[1]] %>% .[[1]] 

  
  
  ifelse(margin==1,
         b <- df %>% 
              group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE) %>% 
              summarise(TOTAL=n()),
         b <- df %>% 
              group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,{{variable}}) %>% 
              summarise(TOTAL=n()))
  
  c <- left_join(a,b) %>% mutate(Ptje_Matriculados = round(N/TOTAL*100, 2)) %>% 
    group_nest(ANNIO_INGRESO) %>% 
    unnest(ANNIO_INGRESO) %>% 
    group_by(ANNIO_INGRESO) %>%
    mutate(R= map(data, ~{names(.)[names(.) == "Ptje_Matriculados"] <-paste0('Ptje_Matriculados', ANNIO_INGRESO); .}))%>% .[,3] %>% 
    .[[1]] 
  
  d <- left_join(c[[1]] %>% select(!TOTAL& !N), c[[2]] %>% select(!N,!TOTAL)%>% select(!TOTAL& !N) ) %>% 
    left_join(c[[3]] %>% select(!TOTAL& !N)) %>% 
    left_join(c[[4]] %>% select(!TOTAL& !N))
  

  
  return(d)
  
}
TablaContingencia3 <- function(df, variable, margin=1) {
  
  a <- df %>% 
    group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,SE_MATRICULA,  {{variable}} )%>% 
    summarise(N=n(), .groups ="drop" ) %>% 
    group_nest(SE_MATRICULA) %>% 
    .[1,2] %>% 
    .[[1]] %>% .[[1]] 
  
  a1 <- df %>% 
    group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,SE_MATRICULA,  {{variable}} )%>% 
    summarise(N=n(), .groups ="drop" ) %>% 
    group_nest(SE_MATRICULA) %>% 
    .[2,2] %>% 
    .[[1]] %>% .[[1]] 
  
  
  
  ifelse(margin==1,
         b <- df %>% 
           group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE) %>% 
           summarise(TOTAL=n()),
         b <- df %>% 
           group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,{{variable}}) %>% 
           summarise(TOTAL=n()))
  
  c <- left_join(a,b) %>% mutate(Ptje_Desercion = round(N/TOTAL*100, 2)) %>% 
    group_nest(ANNIO_INGRESO) %>% 
    unnest(ANNIO_INGRESO) %>% 
    group_by(ANNIO_INGRESO) %>%
    mutate(R= map(data, ~{names(.)[names(.) == "Ptje_Desercion"] <-paste0('Ptje_Desercion_', ANNIO_INGRESO); .}))%>% .[,3] %>% 
    .[[1]] 
  
  d <- left_join(c[[1]] %>% select(!TOTAL& !N), c[[2]] %>% select(!N,!TOTAL)%>% select(!TOTAL& !N) ) %>% 
    left_join(c[[3]] %>% select(!TOTAL& !N)) %>% 
    left_join(c[[4]] %>% select(!TOTAL& !N))
  
  c1 <- left_join(a1,b) %>% mutate(Ptje_Matriculados = round(N/TOTAL*100, 2)) %>% 
    group_nest(ANNIO_INGRESO) %>% 
    unnest(ANNIO_INGRESO) %>% 
    group_by(ANNIO_INGRESO) %>%
    mutate(R= map(data, ~{names(.)[names(.) == "Ptje_Matriculados"] <-paste0('Ptje_Matriculados', ANNIO_INGRESO); .}))%>% .[,3] %>% 
    .[[1]] 
  
  d1 <- left_join(c1[[1]] %>% select(!TOTAL& !N), c1[[2]] %>% select(!N,!TOTAL)%>% select(!TOTAL& !N) ) %>% 
    left_join(c1[[3]] %>% select(!TOTAL& !N)) %>% 
    left_join(c1[[4]] %>% select(!TOTAL& !N)) %>% 
    left_join(c1[[5]] %>% select(!TOTAL& !N))
  
  e <- full_join(d,d1) %>% slice(9,10, 1:8)
  
  
  return(e)
  
}

TablaContingencia(df,PERS_SEXO)
TablaContingencia2(df,PERS_SEXO,1)

TablaContingencia2(df,PERS_SEXO,1)
k <- TablaContingencia3(df,PERS_SEXO,1) %>%select(c(1:3,7,4,8,5,9,10,11)) 
v <- TablaContingencia3(df,PERS_SEXO,2) %>%select(c(1:3,7,4,8,5,9,10,11)) 

TablaContingencia3(df%>% filter(CODUA==3353),PERS_SEXO,1) %>%select(c(1:3,7,4,8,5,9,10,11)) 
TablaContingencia3(df%>% filter(CODUA==3353),PERS_SEXO,2) %>%select(c(1:3,7,4,8,5,9,10,11)) 

TablaContingencia3(df%>% filter(CODUA==4111),PERS_SEXO,1) %>%select(c(1:3,7,4,8,5,9,10,11)) 
TablaContingencia3(df%>% filter(CODUA==4111),PERS_SEXO,2) %>%select(c(1:3,7,4,8,5,9,10,11)) 
