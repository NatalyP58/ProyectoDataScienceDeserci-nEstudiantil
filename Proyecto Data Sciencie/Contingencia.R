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
df <- df %>% mutate( INGRESO_FAMILIAR = case_when(INGRESO_BRUTO_FAMILIAR <= 4 ~ 1,
                                                    INGRESO_BRUTO_FAMILIAR > 4 & INGRESO_BRUTO_FAMILIAR <= 8 ~ 2,
                                                    INGRESO_BRUTO_FAMILIAR <= 10 ~ 3))

TablaContin <- function(df, variable, margin=1) {
  
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

# TAbla de contigencia para tdos

TablaContin(df,PERS_SEXO,1) %>%select(c(1:3,7,4,8,5,9,10,11)) 
TablaContin(df,PERS_SEXO,2) %>%select(c(1:3,7,4,8,5,9,10,11)) 

# TAbla de contigencia para enfermeria

TablaContin(df%>% filter(CODUA==4111),PERS_SEXO,1) %>%select(c(1:3,7,4,8,5,9,10,11)) 
TablaContin(df%>% filter(CODUA==4111),PERS_SEXO,2) %>%select(c(1:3,7,4,8,5,9,10,11)) 

#tabla de contingencia para ingenieria

TablaContin(df%>% filter(CODUA==3353),PERS_SEXO,1) %>%select(c(1,2,7:11)) 
TablaContin(df%>% filter(CODUA==3353),PERS_SEXO,2) %>%select(c(1:3,7,4,8,5,9,10,11)) 

x <- TablaContin(df%>% filter(CODUA==3353),PERS_SEXO,1)

y <- TablaContin(df%>% filter(CODUA==3353),PERS_SEXO,1) %>%select(c(1:3,7,4,8,5,9,10,11))

TablaContin(df,SE_MATRICULA,1) %>%select(c(1:3,7,4,8,5,9,10,11)) %>% view()

a <- df %>% 
  group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,SE_MATRICULA)%>% 
  summarise(N=n(), .groups ="drop" ) %>% 
  group_nest(SE_MATRICULA) %>% 
  .[1,2] %>% 
  .[[1]] %>% .[[1]] 

a1 <- df %>% 
  group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,SE_MATRICULA )%>% 
  summarise(N=n(), .groups ="drop" ) %>% 
  group_nest(SE_MATRICULA) %>% 
  .[2,2] %>% 
  .[[1]] %>% .[[1]] 

b <- df %>% 
         group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE) %>% 
         summarise(TOTAL=n())
       
 b1 <- df %>% 
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

c1 <- left_join(a1,b1) %>% mutate(Ptje_Matriculados = round(N/TOTAL*100, 2)) %>% 
  group_nest(ANNIO_INGRESO) %>% 
  unnest(ANNIO_INGRESO) %>% 
  group_by(ANNIO_INGRESO) %>%
  mutate(R= map(data, ~{names(.)[names(.) == "Ptje_Matriculados"] <-paste0('Ptje_Matriculados', ANNIO_INGRESO); .}))%>% .[,3] %>% 
  .[[1]] 

d1 <- left_join(c1[[1]] %>% select(!TOTAL& !N), c1[[2]] %>% select(!N,!TOTAL)%>% select(!TOTAL& !N) ) %>% 
  left_join(c1[[3]] %>% select(!TOTAL& !N)) %>% 
  left_join(c1[[4]] %>% select(!TOTAL& !N)) %>% 
  left_join(c1[[5]] %>% select(!TOTAL& !N))

e <- full_join(d,d1) %>% slice(9,10, 1:8)%>%select(c(1,2,6,3,7,4,8,5,9,10))

d%>%select(c(1:3,7,4,8,5,9,10,11)) %>% view()

unique(df$PROMEDIO_ANUAL)
max(df$PROMEDIO_ANUAL, na.rm = TRUE)

df %>% 
  filter(!is.na(as.numeric(PROMEDIO_ANUAL)) & ANNIO_INGRESO==2019) %>% 
  ggplot(aes(x= as.numeric(PROMEDIO_ANUAL)))+
  geom_histogram(stat="count", bins = 3)

as.nu

ggplot(df, aes(x= as.numeric(PROMEDIO_ANUAL)))+ geom_histogram(aes(fill = ANNIO_MATRICULA_INDICE),stat="count",bins =-5)

  theme(axis.text.y = element_text(angle = 45,
                                   size = 10,
                                   color = "coral"))+
  stat_summary(fun= "mean", color = "red",size=1.5)
glimpse(df)


df %>% group_by(ANNIO_INGRESO, ANNIO_MATRICULA_INDICE,SE_MATRICULA) %>% table()

df %>% 
  filter (ANNIO_INGRESO ==2017) %>% 
  select( ANNIO_MATRICULA_INDICE,SE_MATRICULA)%>% 
  table() %>% 
  prop.table(.,margin = 2)*100


df %>% 
  select( ANNIO_MATRICULA_INDICE,ANNIO_INGRESO,SE_MATRICULA)%>% 
  table() %>% 
  prop.table(.,margin = 2)*100

glimpse(df)

TablaContin(df,INGRESO_FAMILIAR,1) %>%  slice(3:10, 1:2) %>% select(c(1:3,7,4,8,5,9,10,11)) %>% view()
a <- TablaContin(df,INGRESO_FAMILIAR,2) %>%  slice(3:10, 1:2) %>% select(c(1:6)) 
aE <- TablaContin(df%>% filter(CODUA==4111),INGRESO_FAMILIAR,2) %>%  slice(3:10, 1:2) %>% select(c(1:6))
aI <- TablaContin(df%>% filter(CODUA==3353),INGRESO_FAMILIAR,2) %>%  slice(3:10, 1:2) %>% select(c(1:6))


a %>% write_csv(., file = "DatosEstadisticos//a.csv")
aE %>% write_csv(., file = "DatosEstadisticos//aE.csv")
aI %>% write_csv(., file = "DatosEstadisticos//aI.csv")

