
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
glimpse(df)
df %>% filter(SE_MATRICULA==0 & ANNIO_INGRESO==ANNIO_MATRICULA_INDICE) %>% view()
3
5
6
7
12
14
15
20
21
22
23
24
prop.table()
function(df){
  df %>%
    map( function(x) table(x) )
}

df[c(1, 3, 5:7, 10, 12,14,15,   20:24)]%>%
  group_nest(ANNIO_INGRESO,ANNIO_MATRICULA_INDICE) %>% 
  map( function(df){
    df %>%
      map( function(x) table(x) )
    
  } )
df %>% 
  select(IDENTIFICADOR,COD_NACIONLD,SE_MATRICULA, ANNIO_INGRESO,ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_INGRESO,ANNIO_MATRICULA_INDICE,COD_NACIONLD) %>%
  summarise(tabla = table())


carr_4111 <- df %>% 
  filter(CODUA == 4111)
carr_3353 <- datos %>% 
cohorte_prom <- function(df, anho){
  df %>% 
    filter (ANNIO_INGRESO == anho) %>% 
    select( ANNIO_MATRICULA_INDICE,SE_MATRICULA, PERS_SEXO)%>% 
    table() %>% 
    prop.table(.,margin = 1)*100
}
cohorte_prom(carr_3353,2017)
cohorte_prom(carr_4111,2018)

df %>% 
  group_by(ANNIO_MATRICULA_INDICE,SE_MATRICULA, PERS_SEXO) %>% 
  summarise(N=n(), .groups ="drop" ) %>% 
  group_nest(ANNIO_MATRICULA_INDICE) %>% 
  .[1,2] %>% 
  .[[1]]
df %>% 
  group_by(ANNIO_MATRICULA_INDICE,SE_MATRICULA, PERS_SEXO) %>% 
  summarise(N=n(), .groups ="drop" ) %>% 
  group_nest(PERS_SEXO) %>% 
  .[1,2] %>% 
  .[[1]]

df %>% 
  group_by(ANNIO_MATRICULA_INDICE,SE_MATRICULA, PERS_SEXO) %>% 
  summarise(N=n(), .groups ="drop" ) %>% 
  group_nest(SE_MATRICULA) %>% 
  .[1,2] %>% 
  .[[1]]

a <- df %>% 
  group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,SE_MATRICULA, PERS_SEXO) %>% 
  summarise(N=n(), .groups ="drop" ) %>% 
  group_nest(SE_MATRICULA) %>% 
  .[1,2] %>% 
  .[[1]] %>% .[[1]] 

group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE, PERS_SEXO) %>% 
  summarise(n=sum(N, na.rm = TRUE))

b <- df %>% 
  group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE) %>% 
  summarise(TOTAL=n())

full_join(a,b) %>% mutate(Ptje_Desercion = round(N/TOTAL*100, 2)) %>% 
  group_nest(ANNIO_INGRESO) %>% .[1,2] %>% 
  .[[1]] %>% .[[1]] 

c2 <-full_join(a,b) %>% mutate(Ptje_Desercion = round(N/TOTAL*100, 2)) %>% 
  group_nest(ANNIO_INGRESO) %>% 
 unnest(ANNIO_INGRESO) %>% 
  group_by(ANNIO_INGRESO) %>%
  mutate(R= map(data, ~{names(.)[names(.) == "Ptje_Desercion"] <-paste0('Ptje_Desercion_', ANNIO_INGRESO); .}))%>% .[,3] %>% 
  .[[1]] 

c <- left_join(a,b) %>% mutate(Ptje_Desercion = round(N/TOTAL*100, 2)) %>% 
  group_nest(ANNIO_INGRESO) %>% 
  unnest(ANNIO_INGRESO) %>% 
  group_by(ANNIO_INGRESO) %>%
  mutate(R= map(data, ~{names(.)[names(.) == "Ptje_Desercion"] <-paste0('Ptje_Desercion_', ANNIO_INGRESO); .}))%>% .[,3] %>% 
  .[[1]] 
  
d <- left_join(c[[1]] %>% select(!TOTAL& !N), c[[2]] %>% select(!N,!TOTAL)%>% select(!TOTAL& !N) ) %>% 
  left_join(c[[3]] %>% select(!TOTAL& !N)) %>% 
  left_join(c[[4]] %>% select(!TOTAL& !N)) 

left_join(c2[[1]] %>% select(!TOTAL& !N), c2[[2]] %>% select(!N,!TOTAL)%>% select(!TOTAL& !N) ) %>% 
  left_join(c2[[3]] %>% select(!TOTAL& !N)) %>% 
  left_join(c2[[4]] %>% select(!TOTAL& !N)) 


d %>% filter(PERS_SEXO=="F")
d %>% filter(PERS_SEXO=="M")
df %>% group_nest(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE)

#ifelse(is.null(CODUA), df <- DF, df <- DF %>% filter(CODUA==CODUA))
funcion <- function(df, variable) {
   
   
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


funcion(df %>% filter(CODUA==4111),PERS_SEXO)
