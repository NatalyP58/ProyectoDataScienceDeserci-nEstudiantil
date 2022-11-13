
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
datos <- read_csv2("diplomado_ds_proyecto_18_v2.csv",
                      na = "NULL")
glimpse(datos)


#----Limpieza de datos---------------------------------



datos <- datos %>% 
  mutate(FNACIMIENTO= as.Date(FNACIMIENTO, "%d-%m-%Y"),
         SE_MATRICULA= ifelse(SE_MATRICULA==0 & ANNIO_INGRESO==ANNIO_MATRICULA_INDICE, 1, SE_MATRICULA),
         PROM_NOTA_SEM1= round(PROM_NOTA_SEM1/10,2),
         PROM_NOTA_SEM2= round(PROM_NOTA_SEM2/10,2),
         PROMEDIO_ANUAL= round(PROMEDIO_ANUAL/10,2),
         PROMEDIO_PRIMER_ANNIO= round(PROMEDIO_PRIMER_ANNIO/10,2),
         PROMEDIO_ALUMNO_EN_CARRERA= round(PROMEDIO_ALUMNO_EN_CARRERA/10,2),
         PROM_NOTAS_E_MEDIA=case_when(PROM_NOTAS_E_MEDIA < 7 ~ PROM_NOTAS_E_MEDIA, 
                                      PROM_NOTAS_E_MEDIA <=100~ PROM_NOTAS_E_MEDIA/10,
                                      PROM_NOTAS_E_MEDIA > 100~ PROM_NOTAS_E_MEDIA/100),
         PROM_LENG_Y_MAT=case_when(PROM_LENG_Y_MAT < 1000  ~ PROM_LENG_Y_MAT, 
                                   PROM_LENG_Y_MAT >=1000~ PROM_LENG_Y_MAT/10),
         PROMEDIO_ANUAL= ifelse(is.na(PROM_NOTA_SEM1) & is.na(PROM_NOTA_SEM2) & PROMEDIO_ANUAL==0, NA,PROMEDIO_ANUAL),
         PROMEDIO_PRIMER_ANNIO_2 =ifelse(is.na(PROM_NOTA_SEM1) & ANNIO_INGRESO == ANNIO_MATRICULA_INDICE , PROM_NOTA_SEM2,
                                      ifelse(is.na(PROM_NOTA_SEM2) & ANNIO_INGRESO == ANNIO_MATRICULA_INDICE , PROM_NOTA_SEM1, PROMEDIO_PRIMER_ANNIO)))%>%
         select(!FECHA_CARGA_PLANILLA) 

datos <- full_join(datos %>% 
                     select(!PROMEDIO_PRIMER_ANNIO_2),
                   datos %>% 
                     filter(ANNIO_INGRESO==ANNIO_MATRICULA_INDICE) %>% 
                     select(IDENTIFICADOR, PROMEDIO_PRIMER_ANNIO_2)) %>% 
          mutate(PROMEDIO_PRIMER_ANNIO = PROMEDIO_PRIMER_ANNIO_2) %>% 
          select(!PROMEDIO_PRIMER_ANNIO_2) %>% 
          mutate(PROMEDIO_PRIMER_ANNIO= ifelse(is.na(PROM_NOTA_SEM1) & is.na(PROM_NOTA_SEM2) & PROMEDIO_PRIMER_ANNIO==0, NA,PROMEDIO_PRIMER_ANNIO))

datos <-  datos %>% mutate(EDAD_ACTUAL= floor(time_length(ymd(Sys.Date()) - datos$FNACIMIENTO, unit = "year")),
                           EDAD_POR_ANNIO_MATRICULA= as.numeric(ANNIO_MATRICULA_INDICE) - year(datos$FNACIMIENTO)) %>% 
                    mutate(EDAD_ACTUAL= ifelse(EDAD_ACTUAL<10 | EDAD_ACTUAL>80, NA, EDAD_ACTUAL),
                           EDAD_POR_ANNIO_MATRICULA= ifelse(EDAD_POR_ANNIO_MATRICULA<10 | EDAD_POR_ANNIO_MATRICULA>80, NA, EDAD_POR_ANNIO_MATRICULA)) 




datos <- datos[, c(1:4,76, 77, 5:75)]

datos %>% write_csv(., file = "DatosEstadisticos//Diplomado_DS_Proyecto.csv")
datos %>% write.xlsx(., file = "DatosEstadisticos//Diplomado_DS_Proyecto.xlsx")

glimpse(datos)
colnames(datos) 
df %>% group_by(IDENTIFICADOR)#654
 df%>% filter(CODUA==4111) %>% group_by(IDENTIFICADOR)#302
 df%>% filter(CODUA==3353) %>% group_by(IDENTIFICADOR)#352
#*****************************************************************************************
 


# crear funcin de resumen de datos 

r <- function(df){
  df %>% 
    summarise(EDAD_ACTUAL                        = mean(EDAD_ACTUAL, na.rm = TRUE),
              PROM_NOTAS_E_MEDIA                 = mean(PROM_NOTAS_E_MEDIA, na.rm = TRUE),
              PUNTAJE_NEM                        = mean(PUNTAJE_NEM, na.rm = TRUE),
              PUNTAJE_RANKING                    = mean(PUNTAJE_RANKING, na.rm = TRUE),
              PTJE_LENGUAJE                      = mean(PTJE_LENGUAJE, na.rm = TRUE),
              PTJE_MATEMATICA                    = mean(PTJE_MATEMATICA, na.rm = TRUE),
              PTJE_HIST_CS_SOCIALES              = mean(PTJE_HIST_CS_SOCIALES, na.rm = TRUE),
              PTJE_CIENCIAS                      = mean(PTJE_CIENCIAS, na.rm = TRUE),
              PROM_LENG_Y_MAT                    = mean(PROM_LENG_Y_MAT , na.rm = TRUE),
              PTJE_PONDERADO                     = mean(PTJE_PONDERADO, na.rm = TRUE),
              INGRESO_BRUTO_FAMILIAR             = mean(INGRESO_BRUTO_FAMILIAR, na.rm = TRUE),
              PROMEDIO_PRIMER_ANNIO              = mean(PROMEDIO_PRIMER_ANNIO, na.rm = TRUE),
              PROMEDIO_ALUMNO_EN_CARRERA         = mean(PROMEDIO_ALUMNO_EN_CARRERA, na.rm = TRUE),
              CANT_RAMOS_INSCRITOS_CARRERA       = mean(CANT_RAMOS_INSCRITOS_CARRERA, na.rm = TRUE),
              CANT_RAMOS_APROBADOS_CARRERA       = mean(CANT_RAMOS_APROBADOS_CARRERA, na.rm = TRUE),
              PORCENTAJE_AVANCE_CARRERA          = mean(PORCENTAJE_AVANCE_CARRERA, na.rm = TRUE),
              CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO  = mean(CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO, na.rm = TRUE),
              CANT_RETIROS_TEMPORALES            = mean(CANT_RETIROS_TEMPORALES, na.rm = TRUE),
              CANT_RAMOS_OP2                     = mean(CANT_RAMOS_OP2, na.rm = TRUE),
              CANT_RAMOS_OP3                     = mean(CANT_RAMOS_OP3, na.rm = TRUE),
              CANT_RAMOS_OP4                     = mean(CANT_RAMOS_OP4, na.rm = TRUE),
              .groups = "drop")
}





tabla_resumen <- function(df){
  df %>% 
    summarise(Estadisticos= c("promedio", "mediana", "des.Estandar", "min", "max", "1rd.qt", "3rd.qt", "RIC","asimetria","curtosis", "C.V", "NA"),
      
              EDAD_ACTUAL             = c(mean(r$EDAD_ACTUAL, na.rm = TRUE),
                                        median(r$EDAD_ACTUAL, na.rm = TRUE),
                                        sd(r$EDAD_ACTUAL, na.rm = TRUE),
                                        min(r$EDAD_ACTUAL, na.rm = TRUE),
                                        max(r$EDAD_ACTUAL, na.rm = TRUE),
                                        quantile(r$EDAD_ACTUAL,probs = 0.25, na.rm = TRUE),
                                        quantile(r$EDAD_ACTUAL,probs = 0.75, na.rm = TRUE),
                                        IQR(r$EDAD_ACTUAL, na.rm = TRUE),
                                        skew(r$EDAD_ACTUAL, na.rm = TRUE),
                                        kurtosi(r$EDAD_ACTUAL, na.rm = TRUE),
                                        sd(r$EDAD_ACTUAL, na.rm = TRUE)/mean(r$EDAD_ACTUAL, na.rm = TRUE),
                                        sum(!is.na(r$EDAD_ACTUAL))),
              
              PROM_NOTAS_E_MEDIA    = c(mean(r$PROM_NOTAS_E_MEDIA, na.rm = TRUE),
                                        median(r$PROM_NOTAS_E_MEDIA, na.rm = TRUE),
                                        sd(r$PROM_NOTAS_E_MEDIA, na.rm = TRUE),
                                        min(r$PROM_NOTAS_E_MEDIA, na.rm = TRUE),
                                        max(r$PROM_NOTAS_E_MEDIA, na.rm = TRUE),
                                        quantile(r$PROM_NOTAS_E_MEDIA,probs = 0.25, na.rm = TRUE),
                                        quantile(r$PROM_NOTAS_E_MEDIA,probs = 0.75, na.rm = TRUE),
                                        IQR(r$PROM_NOTAS_E_MEDIA, na.rm = TRUE),
                                        skew(r$PROM_NOTAS_E_MEDIA, na.rm = TRUE),
                                        kurtosi(r$PROM_NOTAS_E_MEDIA, na.rm = TRUE),
                                        sd(r$PROM_NOTAS_E_MEDIA, na.rm = TRUE)/mean(r$PROM_NOTAS_E_MEDIA, na.rm = TRUE),
                                        sum(!is.na(r$PROM_NOTAS_E_MEDIA))),

              PUNTAJE_NEM           = c(mean(r$PUNTAJE_NEM, na.rm = TRUE),
                                        median(r$PUNTAJE_NEM, na.rm = TRUE),
                                        sd(r$PUNTAJE_NEM, na.rm = TRUE),
                                        min(r$PUNTAJE_NEM, na.rm = TRUE),
                                        max(r$PUNTAJE_NEM, na.rm = TRUE),
                                        quantile(r$PUNTAJE_NEM,probs = 0.25, na.rm = TRUE),
                                        quantile(r$PUNTAJE_NEM,probs = 0.75, na.rm = TRUE),
                                        IQR(r$PUNTAJE_NEM, na.rm = TRUE),
                                        skew(r$PUNTAJE_NEM , na.rm = TRUE),
                                        kurtosi(r$PUNTAJE_NEM , na.rm = TRUE),
                                        sd(r$PUNTAJE_NEM, na.rm = TRUE)/mean(r$PUNTAJE_NEM, na.rm = TRUE),
                                        sum(!is.na(r$PUNTAJE_NEM))),
              
              PUNTAJE_RANKING       = c(mean(r$PUNTAJE_RANKING, na.rm = TRUE),
                                        median(r$PUNTAJE_RANKING , na.rm = TRUE),
                                        sd(r$PUNTAJE_RANKING , na.rm = TRUE),
                                        min(r$PUNTAJE_RANKING , na.rm = TRUE),
                                        max(r$PUNTAJE_RANKING , na.rm = TRUE),
                                        quantile(r$PUNTAJE_RANKING ,probs = 0.25, na.rm = TRUE),
                                        quantile(r$PUNTAJE_RANKING ,probs = 0.75, na.rm = TRUE),
                                        IQR(r$PUNTAJE_RANKING , na.rm = TRUE),
                                        skew(r$PUNTAJE_RANKING , na.rm = TRUE),
                                        kurtosi(r$PUNTAJE_RANKING , na.rm = TRUE),
                                        sd(r$PUNTAJE_RANKING, na.rm = TRUE)/mean(r$PUNTAJE_RANKING, na.rm = TRUE),
                                        sum(!is.na(r$PUNTAJE_RANKING))),
              
              PTJE_LENGUAJE         = c(mean(r$PTJE_LENGUAJE, na.rm = TRUE),
                                        median(r$PTJE_LENGUAJE, na.rm = TRUE),
                                        sd(r$PTJE_LENGUAJE, na.rm = TRUE),
                                        min(r$PTJE_LENGUAJE, na.rm = TRUE),
                                        max(r$PTJE_LENGUAJE, na.rm = TRUE),
                                        quantile(r$PTJE_LENGUAJE,probs = 0.25, na.rm = TRUE),
                                        quantile(r$PTJE_LENGUAJE,probs = 0.75, na.rm = TRUE),
                                        IQR(r$PTJE_LENGUAJE, na.rm = TRUE),
                                        skew(r$PTJE_LENGUAJE , na.rm = TRUE),
                                        kurtosi(r$PTJE_LENGUAJE , na.rm = TRUE),
                                        sd(r$PUNTAJE_NEM, na.rm = TRUE)/mean(r$PUNTAJE_NEM, na.rm = TRUE),
                                        sum(!is.na(r$PTJE_LENGUAJE))),
              
              PTJE_MATEMATICA       = c(mean(r$PTJE_MATEMATICA, na.rm = TRUE),
                                        median(r$PTJE_MATEMATICA, na.rm = TRUE),
                                        sd(r$PTJE_MATEMATICA, na.rm = TRUE),
                                        min(r$PTJE_MATEMATICA, na.rm = TRUE),
                                        max(r$PTJE_MATEMATICA, na.rm = TRUE),
                                        quantile(r$PTJE_MATEMATICA,probs = 0.25, na.rm = TRUE),
                                        quantile(r$PTJE_MATEMATICA,probs = 0.75, na.rm = TRUE),
                                        IQR(r$PTJE_MATEMATICA, na.rm = TRUE),
                                        skew(r$PTJE_MATEMATICA , na.rm = TRUE),
                                        kurtosi(r$PTJE_MATEMATICA , na.rm = TRUE),
                                        sd(r$PTJE_MATEMATICA , na.rm = TRUE)/mean(r$PTJE_MATEMATICA , na.rm = TRUE),
                                        sum(!is.na(r$ PTJE_MATEMATICA))),
              
              PTJE_HIST_CS_SOCIALES = c(mean(r$PTJE_HIST_CS_SOCIALES, na.rm = TRUE),
                                        median(r$PTJE_HIST_CS_SOCIALES, na.rm = TRUE),
                                        sd(r$PTJE_HIST_CS_SOCIALES, na.rm = TRUE),
                                        min(r$PTJE_HIST_CS_SOCIALES, na.rm = TRUE),
                                        max(r$PTJE_HIST_CS_SOCIALES, na.rm = TRUE),
                                        quantile(r$PTJE_HIST_CS_SOCIALES,probs = 0.25, na.rm = TRUE),
                                        quantile(r$PTJE_HIST_CS_SOCIALES,probs = 0.75, na.rm = TRUE),
                                        IQR(r$PTJE_HIST_CS_SOCIALES, na.rm = TRUE),
                                        skew(r$PTJE_HIST_CS_SOCIALES, na.rm = TRUE),
                                        kurtosi(r$PTJE_HIST_CS_SOCIALES, na.rm = TRUE),
                                        sd(r$PTJE_HIST_CS_SOCIALES, na.rm = TRUE)/mean(r$PTJE_HIST_CS_SOCIALES, na.rm = TRUE),
                                        sum(!is.na(r$PTJE_HIST_CS_SOCIALES))),
              
              PTJE_CIENCIAS         = c(mean(r$PTJE_CIENCIAS, na.rm = TRUE),
                                        median(r$PTJE_CIENCIAS, na.rm = TRUE),
                                        sd(r$PTJE_CIENCIAS, na.rm = TRUE),
                                        min(r$PTJE_CIENCIAS, na.rm = TRUE),
                                        max(r$PTJE_CIENCIAS, na.rm = TRUE),
                                        quantile(r$PTJE_CIENCIAS,probs = 0.25, na.rm = TRUE),
                                        quantile(r$PTJE_CIENCIAS,probs = 0.75, na.rm = TRUE),
                                        IQR(r$PTJE_CIENCIAS, na.rm = TRUE),
                                        skew(r$PTJE_CIENCIAS, na.rm = TRUE),
                                        kurtosi(r$PTJE_CIENCIAS, na.rm = TRUE),
                                        sd(r$PTJE_CIENCIAS, na.rm = TRUE)/mean(r$PTJE_CIENCIAS, na.rm = TRUE),
                                        sum(!is.na(r$PTJE_CIENCIAS))),
              
              PROM_LENG_Y_MAT       = c(mean(r$PROM_LENG_Y_MAT , na.rm = TRUE),
                                        median(r$PROM_LENG_Y_MAT, na.rm = TRUE),
                                        sd(r$PROM_LENG_Y_MAT, na.rm = TRUE),
                                        min(r$PROM_LENG_Y_MAT, na.rm = TRUE),
                                        max(r$PROM_LENG_Y_MAT, na.rm = TRUE),
                                        quantile(r$PROM_LENG_Y_MAT,probs = 0.25, na.rm = TRUE),
                                        quantile(r$PROM_LENG_Y_MAT,probs = 0.75, na.rm = TRUE),
                                        IQR(r$PROM_LENG_Y_MAT, na.rm = TRUE),
                                        skew(r$PROM_LENG_Y_MAT, na.rm = TRUE),
                                        kurtosi(r$PROM_LENG_Y_MAT, na.rm = TRUE),
                                        sd(r$PROM_LENG_Y_MAT , na.rm = TRUE)/mean(r$PROM_LENG_Y_MAT , na.rm = TRUE),
                                        sum(!is.na(r$PROM_LENG_Y_MAT))),
              
              PTJE_PONDERADO        = c(mean(r$PTJE_PONDERADO, na.rm = TRUE),
                                        median(r$PTJE_PONDERADO, na.rm = TRUE),
                                        sd(r$PTJE_PONDERADO, na.rm = TRUE),
                                        min(r$PTJE_PONDERADO, na.rm = TRUE),
                                        max(r$PTJE_PONDERADO, na.rm = TRUE),
                                        quantile(r$PTJE_PONDERADO,probs = 0.25, na.rm = TRUE),
                                        quantile(r$PTJE_PONDERADO,probs = 0.75, na.rm = TRUE),
                                        IQR(r$PTJE_PONDERADO, na.rm = TRUE),
                                        skew(r$PTJE_PONDERADO, na.rm = TRUE),
                                        kurtosi(r$PTJE_PONDERADO, na.rm = TRUE),
                                        sd(r$PTJE_PONDERADO , na.rm = TRUE)/mean(r$PTJE_PONDERADO , na.rm = TRUE),
                                        sum(!is.na(r$PTJE_PONDERADO))),
              
              INGRESO_BRUTO_FAMILIAR    = c(mean(r$INGRESO_BRUTO_FAMILIAR, na.rm = TRUE),
                                        median(r$INGRESO_BRUTO_FAMILIAR, na.rm = TRUE),
                                        sd(r$INGRESO_BRUTO_FAMILIAR, na.rm = TRUE),
                                        min(r$INGRESO_BRUTO_FAMILIAR, na.rm = TRUE),
                                        max(r$INGRESO_BRUTO_FAMILIAR, na.rm = TRUE),
                                        quantile(r$INGRESO_BRUTO_FAMILIAR,probs = 0.25, na.rm = TRUE),
                                        quantile(r$INGRESO_BRUTO_FAMILIAR,probs = 0.75, na.rm = TRUE),
                                        IQR(r$INGRESO_BRUTO_FAMILIAR, na.rm = TRUE),
                                        skew(r$INGRESO_BRUTO_FAMILIAR, na.rm = TRUE),
                                        kurtosi(r$INGRESO_BRUTO_FAMILIAR, na.rm = TRUE),
                                        sd(r$INGRESO_BRUTO_FAMILIAR , na.rm = TRUE)/mean(r$INGRESO_BRUTO_FAMILIAR , na.rm = TRUE),
                                        sum(!is.na(r$INGRESO_BRUTO_FAMILIAR))),
              
              PROMEDIO_PRIMER_ANNIO    = c(mean(r$PROMEDIO_PRIMER_ANNIO, na.rm = TRUE),
                                        median(r$PROMEDIO_PRIMER_ANNIO, na.rm = TRUE),
                                        sd(r$PROMEDIO_PRIMER_ANNIO, na.rm = TRUE),
                                        min(r$PROMEDIO_PRIMER_ANNIO, na.rm = TRUE),
                                        max(r$PROMEDIO_PRIMER_ANNIO, na.rm = TRUE),
                                        quantile(r$PROMEDIO_PRIMER_ANNIO,probs = 0.25, na.rm = TRUE),
                                        quantile(r$PROMEDIO_PRIMER_ANNIO,probs = 0.75, na.rm = TRUE),
                                        IQR(r$PROMEDIO_PRIMER_ANNIO, na.rm = TRUE),
                                        skew(r$PROMEDIO_PRIMER_ANNIO, na.rm = TRUE),
                                        kurtosi(r$PROMEDIO_PRIMER_ANNIO, na.rm = TRUE),
                                        sd(r$PROMEDIO_PRIMER_ANNIO , na.rm = TRUE)/mean(r$PROMEDIO_PRIMER_ANNIO , na.rm = TRUE),
                                       sum(!is.na(r$PROMEDIO_PRIMER_ANNIO))),
              
             PROMEDIO_ALUMNO_EN_CARRERA = c(mean(r$PROMEDIO_ALUMNO_EN_CARRERA, na.rm = TRUE),
                                        median(r$PROMEDIO_ALUMNO_EN_CARRERA, na.rm = TRUE),
                                        sd(r$PROMEDIO_ALUMNO_EN_CARRERA, na.rm = TRUE),
                                        min(r$PROMEDIO_ALUMNO_EN_CARRERA, na.rm = TRUE),
                                        max(r$PROMEDIO_ALUMNO_EN_CARRERA, na.rm = TRUE),
                                        quantile(r$PROMEDIO_ALUMNO_EN_CARRERA,probs = 0.25, na.rm = TRUE),
                                        quantile(r$PROMEDIO_ALUMNO_EN_CARRERA,probs = 0.75, na.rm = TRUE),
                                        IQR(r$PROMEDIO_ALUMNO_EN_CARRERA, na.rm = TRUE),
                                        skew(r$PROMEDIO_ALUMNO_EN_CARRERA, na.rm = TRUE),
                                        kurtosi(r$PROMEDIO_ALUMNO_EN_CARRERA, na.rm = TRUE),
                                        sd(r$PROMEDIO_ALUMNO_EN_CARRERA , na.rm = TRUE)/mean(r$PROMEDIO_ALUMNO_EN_CARRERA , na.rm = TRUE),
                                        sum(!is.na(r$PROMEDIO_ALUMNO_EN_CARRERA))),
              
             CANT_RAMOS_INSCRITOS_CARRERA = c(mean(r$CANT_RAMOS_INSCRITOS_CARRERA, na.rm = TRUE),
                                        median(r$CANT_RAMOS_INSCRITOS_CARRERA, na.rm = TRUE),
                                        sd(r$CANT_RAMOS_INSCRITOS_CARRERA, na.rm = TRUE),
                                        min(r$CANT_RAMOS_INSCRITOS_CARRERA, na.rm = TRUE),
                                        max(r$CANT_RAMOS_INSCRITOS_CARRERA, na.rm = TRUE),
                                        quantile(r$CANT_RAMOS_INSCRITOS_CARRERA,probs = 0.25, na.rm = TRUE),
                                        quantile(r$CANT_RAMOS_INSCRITOS_CARRERA,probs = 0.75, na.rm = TRUE),
                                        IQR(r$CANT_RAMOS_INSCRITOS_CARRERA, na.rm = TRUE),
                                        skew(r$CANT_RAMOS_INSCRITOS_CARRERA, na.rm = TRUE),
                                        kurtosi(r$CANT_RAMOS_INSCRITOS_CARRERA , na.rm = TRUE),
                                        sd(r$CANT_RAMOS_INSCRITOS_CARRERA  , na.rm = TRUE)/mean(r$CANT_RAMOS_INSCRITOS_CARRERA  , na.rm = TRUE),
                                        sum(!is.na(r$CANT_RAMOS_INSCRITOS_CARRERA ))),
              
              CANT_RAMOS_APROBADOS_CARRERA= c(mean(r$CANT_RAMOS_APROBADOS_CARRERA, na.rm = TRUE),
                                        median(r$CANT_RAMOS_APROBADOS_CARRERA, na.rm = TRUE),
                                        sd(r$CANT_RAMOS_APROBADOS_CARRERA, na.rm = TRUE),
                                        min(r$CANT_RAMOS_APROBADOS_CARRERA, na.rm = TRUE),
                                        max(r$CANT_RAMOS_APROBADOS_CARRERA, na.rm = TRUE),
                                        quantile(r$CANT_RAMOS_APROBADOS_CARRERA,probs = 0.25, na.rm = TRUE),
                                        quantile(r$CANT_RAMOS_APROBADOS_CARRERA,probs = 0.75, na.rm = TRUE),
                                        IQR(r$CANT_RAMOS_APROBADOS_CARRERA, na.rm = TRUE),
                                        skew(r$CANT_RAMOS_APROBADOS_CARRERA, na.rm = TRUE),
                                        kurtosi(r$CANT_RAMOS_APROBADOS_CARRERA, na.rm = TRUE),
                                        sd(r$CANT_RAMOS_APROBADOS_CARRERA , na.rm = TRUE)/mean(r$CANT_RAMOS_APROBADOS_CARRERA , na.rm = TRUE),
                                        sum(!is.na(r$CANT_RAMOS_APROBADOS_CARRERA))),
              
             PORCENTAJE_AVANCE_CARRERA  = c(mean(r$PORCENTAJE_AVANCE_CARRERA, na.rm = TRUE),
                                        median(r$PORCENTAJE_AVANCE_CARRERA, na.rm = TRUE),
                                        sd(r$PORCENTAJE_AVANCE_CARRERA, na.rm = TRUE),
                                        min(r$PORCENTAJE_AVANCE_CARRERA, na.rm = TRUE),
                                        max(r$PORCENTAJE_AVANCE_CARRERA, na.rm = TRUE),
                                        quantile(r$PORCENTAJE_AVANCE_CARRERA,probs = 0.25, na.rm = TRUE),
                                        quantile(r$PORCENTAJE_AVANCE_CARRERA,probs = 0.75, na.rm = TRUE),
                                        IQR(r$PORCENTAJE_AVANCE_CARRERA, na.rm = TRUE),
                                        skew(r$PORCENTAJE_AVANCE_CARRERA, na.rm = TRUE),
                                        kurtosi(r$PORCENTAJE_AVANCE_CARRERA, na.rm = TRUE),
                                        sd(r$PORCENTAJE_AVANCE_CARRERA , na.rm = TRUE)/mean(r$PORCENTAJE_AVANCE_CARRERA , na.rm = TRUE),
                                        sum(!is.na(r$PORCENTAJE_AVANCE_CARRERA))),
             
             PTJE_PONDERADO        = c(mean(r$PTJE_PONDERADO, na.rm = TRUE),
                                       median(r$PTJE_PONDERADO, na.rm = TRUE),
                                       sd(r$PTJE_PONDERADO, na.rm = TRUE),
                                       min(r$PTJE_PONDERADO, na.rm = TRUE),
                                       max(r$PTJE_PONDERADO, na.rm = TRUE),
                                       quantile(r$PTJE_PONDERADO,probs = 0.25, na.rm = TRUE),
                                       quantile(r$PTJE_PONDERADO,probs = 0.75, na.rm = TRUE),
                                       IQR(r$PTJE_PONDERADO, na.rm = TRUE),
                                       skew(r$PTJE_PONDERADO, na.rm = TRUE),
                                       kurtosi(r$PTJE_PONDERADO, na.rm = TRUE),
                                       sd(r$PTJE_PONDERADO , na.rm = TRUE)/mean(r$PTJE_PONDERADO , na.rm = TRUE),
                                       sum(!is.na(r$PTJE_PONDERADO))),
             
             
             CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO= c(mean(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO, na.rm = TRUE),
                                       median(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO, na.rm = TRUE),
                                       sd(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO, na.rm = TRUE),
                                       min(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO, na.rm = TRUE),
                                       max(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO, na.rm = TRUE),
                                       quantile(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO,probs = 0.25, na.rm = TRUE),
                                       quantile(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO,probs = 0.75, na.rm = TRUE),
                                       IQR(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO, na.rm = TRUE),
                                       skew(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO, na.rm = TRUE),
                                       kurtosi(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO, na.rm = TRUE),
                                       sd(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO , na.rm = TRUE)/mean(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO , na.rm = TRUE),
                                       sum(!is.na(r$CANT_DEUDAS_DISTINTAS_EN_EL_ANNIO))),
             
             CANT_RETIROS_TEMPORALES   = c(mean(r$CANT_RETIROS_TEMPORALES, na.rm = TRUE),
                                       median(r$CANT_RETIROS_TEMPORALES, na.rm = TRUE),
                                       sd(r$CANT_RETIROS_TEMPORALES, na.rm = TRUE),
                                       min(r$CANT_RETIROS_TEMPORALES, na.rm = TRUE),
                                       max(r$CANT_RETIROS_TEMPORALES, na.rm = TRUE),
                                       quantile(r$CANT_RETIROS_TEMPORALES,probs = 0.25, na.rm = TRUE),
                                       quantile(r$CANT_RETIROS_TEMPORALES,probs = 0.75, na.rm = TRUE),
                                       IQR(r$CANT_RETIROS_TEMPORALES, na.rm = TRUE),
                                       skew(r$CANT_RETIROS_TEMPORALES, na.rm = TRUE),
                                       kurtosi(r$CANT_RETIROS_TEMPORALES, na.rm = TRUE),
                                       sd(r$CANT_RETIROS_TEMPORALES , na.rm = TRUE)/mean(r$CANT_RETIROS_TEMPORALES , na.rm = TRUE),
                                       sum(!is.na(r$CANT_RETIROS_TEMPORALES))),
             
             
             CANT_RAMOS_OP2        = c(mean(r$CANT_RAMOS_OP2, na.rm = TRUE),
                                       median(r$CANT_RAMOS_OP2, na.rm = TRUE),
                                       sd(r$CANT_RAMOS_OP2, na.rm = TRUE),
                                       min(r$CANT_RAMOS_OP2, na.rm = TRUE),
                                       max(r$CANT_RAMOS_OP2, na.rm = TRUE),
                                       quantile(r$CANT_RAMOS_OP2,probs = 0.25, na.rm = TRUE),
                                       quantile(r$CANT_RAMOS_OP2,probs = 0.75, na.rm = TRUE),
                                       IQR(r$CANT_RAMOS_OP2, na.rm = TRUE),
                                       skew(r$CANT_RAMOS_OP2, na.rm = TRUE),
                                       kurtosi(r$CANT_RAMOS_OP2, na.rm = TRUE),
                                       sd(r$CANT_RAMOS_OP2 , na.rm = TRUE)/mean(r$CANT_RAMOS_OP2 , na.rm = TRUE),
                                       sum(!is.na(r$CANT_RAMOS_OP2))),
             
             CANT_RAMOS_OP3       = c(mean(r$CANT_RAMOS_OP3, na.rm = TRUE),
                                       median(r$CANT_RAMOS_OP3, na.rm = TRUE),
                                       sd(r$CANT_RAMOS_OP3, na.rm = TRUE),
                                       min(r$CANT_RAMOS_OP3, na.rm = TRUE),
                                       max(r$CANT_RAMOS_OP3, na.rm = TRUE),
                                       quantile(r$CANT_RAMOS_OP3,probs = 0.25, na.rm = TRUE),
                                       quantile(r$CANT_RAMOS_OP3,probs = 0.75, na.rm = TRUE),
                                       IQR(r$CANT_RAMOS_OP3, na.rm = TRUE),
                                       skew(r$CANT_RAMOS_OP3, na.rm = TRUE),
                                       kurtosi(r$CANT_RAMOS_OP3, na.rm = TRUE),
                                       sd(r$CANT_RAMOS_OP3, na.rm = TRUE)/mean(r$CANT_RAMOS_OP3 , na.rm = TRUE),
                                       sum(!is.na(r$CANT_RAMOS_OP3))),
             
             CANT_RAMOS_OP4      = c(mean(r$CANT_RAMOS_OP4, na.rm = TRUE),
                                      median(r$CANT_RAMOS_OP4, na.rm = TRUE),
                                      sd(r$CANT_RAMOS_OP4, na.rm = TRUE),
                                      min(r$CANT_RAMOS_OP4, na.rm = TRUE),
                                      max(r$CANT_RAMOS_OP4, na.rm = TRUE),
                                      quantile(r$CANT_RAMOS_OP4,probs = 0.25, na.rm = TRUE),
                                      quantile(r$CANT_RAMOS_OP4,probs = 0.75, na.rm = TRUE),
                                      IQR(r$CANT_RAMOS_OP4, na.rm = TRUE),
                                      skew(r$CANT_RAMOS_OP4, na.rm = TRUE),
                                      kurtosi(r$CANT_RAMOS_OP4, na.rm = TRUE),
                                      sd(r$CANT_RAMOS_OP4, na.rm = TRUE)/mean(r$CANT_RAMOS_OP4 , na.rm = TRUE),
                                      sum(!is.na(r$CANT_RAMOS_OP4))))
}
# Table de resumen con los estadisticos gernerales 

D <- datos %>% 
  group_nest(IDENTIFICADOR) %>% 
  mutate(r = map_dfr(data,r)) %>% 
  tabla_resumen()

write.xlsx(D,file = "DatosEstadisticos//EstadisticosGenerales.xlsx") 
write_csv(D,file = "DatosEstadisticos//EstadisticosGenerales.csv") 

D.Enfermeria <- datos %>% 
  filter(CARR_DESCRIPCION=="4111- ENFERMERIA" ) %>% 
  group_nest(IDENTIFICADOR) %>% 
  mutate(r = map_dfr(data,r)) %>% 
  tabla_resumen()

write.xlsx(D.Enfermeria,file = "DatosEstadisticos//EstadisticosGeneralesEnfermeria.xlsx") 
write_csv(D.Enfermeria,file = "DatosEstadisticos//EstadisticosGeneralesEnfermeria.csv") 
  
D.Ingenieria<- datos %>% 
  filter(CARR_DESCRIPCION=="3353- INGENIERIA COMERCIAL MENCION NEGOCIOS MINEROS" ) %>% 
  group_nest(IDENTIFICADOR) %>% 
  mutate(r = map_dfr(data,r)) %>% 
  tabla_resumen()


write.xlsx(D.Ingenieria,file = "DatosEstadisticos//EstadisticosGeneralesIngenieria.xlsx") 
write_csv(D.Ingenieria,file = "DatosEstadisticos//EstadisticosGeneralesIngenieria.csv") 
   


#****************************************************************************************************************

colnames(datos)
r2 <- function(df){
  df %>% 
    summarise( Estadisticos= c("promedio", "mediana", "des.Estandar", "min", "max", "1rd.qt", "3rd.qt", "RIC","asimetria","curtosis", "C.V", "NA"),
               
      EDAD_POR_ANNIO_MATRICULA  = c(mean(EDAD_POR_ANNIO_MATRICULA, na.rm = TRUE),
                                   median(EDAD_POR_ANNIO_MATRICULA, na.rm = TRUE),
                                   sd(EDAD_POR_ANNIO_MATRICULA, na.rm = TRUE),
                                   min(EDAD_POR_ANNIO_MATRICULA, na.rm = TRUE),
                                   max(EDAD_POR_ANNIO_MATRICULA, na.rm = TRUE),
                                   quantile(EDAD_POR_ANNIO_MATRICULA,probs = 0.25, na.rm = TRUE),
                                   quantile(EDAD_POR_ANNIO_MATRICULA,probs = 0.75, na.rm = TRUE),
                                   IQR(EDAD_POR_ANNIO_MATRICULA, na.rm = TRUE),
                                   skew(EDAD_POR_ANNIO_MATRICULA, na.rm = TRUE),
                                   kurtosi(EDAD_POR_ANNIO_MATRICULA, na.rm = TRUE),
                                   sd(EDAD_POR_ANNIO_MATRICULA, na.rm = TRUE)/mean(EDAD_POR_ANNIO_MATRICULA, na.rm = TRUE),
                                   sum(!is.na(EDAD_POR_ANNIO_MATRICULA))), 
               
               PROM_NOTA_SEM1  = c(mean(PROM_NOTA_SEM1, na.rm = TRUE),
                                             median(PROM_NOTA_SEM1, na.rm = TRUE),
                                             sd(PROM_NOTA_SEM1, na.rm = TRUE),
                                             min(PROM_NOTA_SEM1, na.rm = TRUE),
                                             max(PROM_NOTA_SEM1, na.rm = TRUE),
                                             quantile(PROM_NOTA_SEM1,probs = 0.25, na.rm = TRUE),
                                             quantile(PROM_NOTA_SEM1,probs = 0.75, na.rm = TRUE),
                                             IQR(PROM_NOTA_SEM1, na.rm = TRUE),
                                             skew(PROM_NOTA_SEM1, na.rm = TRUE),
                                             kurtosi(PROM_NOTA_SEM1, na.rm = TRUE),
                                             sd(PROM_NOTA_SEM1, na.rm = TRUE)/mean(PROM_NOTA_SEM1, na.rm = TRUE),
                                             sum(!is.na(PROM_NOTA_SEM1))),
              
              PROM_NOTA_SEM2    = c(mean(PROM_NOTA_SEM2, na.rm = TRUE),
                                    median(PROM_NOTA_SEM2, na.rm = TRUE),
                                    sd(PROM_NOTA_SEM2, na.rm = TRUE),
                                    min(PROM_NOTA_SEM2, na.rm = TRUE),
                                    max(PROM_NOTA_SEM2, na.rm = TRUE),
                                    quantile(PROM_NOTA_SEM2,probs = 0.25, na.rm = TRUE),
                                    quantile(PROM_NOTA_SEM2,probs = 0.75, na.rm = TRUE),
                                    IQR(PROM_NOTA_SEM2, na.rm = TRUE),
                                    skew(PROM_NOTA_SEM2, na.rm = TRUE),
                                    kurtosi(PROM_NOTA_SEM2, na.rm = TRUE),
                                    sd(PROM_NOTA_SEM2, na.rm = TRUE)/mean(PROM_NOTA_SEM2, na.rm = TRUE),
                                    sum(!is.na(PROM_NOTA_SEM2))),
      PROMEDIO_ANUAL       = c(mean(PROMEDIO_ANUAL, na.rm = TRUE),
                            median(PROMEDIO_ANUAL, na.rm = TRUE),
                            sd(PROMEDIO_ANUAL, na.rm = TRUE),
                            min(PROMEDIO_ANUAL, na.rm = TRUE),
                            max(PROMEDIO_ANUAL, na.rm = TRUE),
                            quantile(PROMEDIO_ANUAL,probs = 0.25, na.rm = TRUE),
                            quantile(PROMEDIO_ANUAL,probs = 0.75, na.rm = TRUE),
                            IQR(PROMEDIO_ANUAL, na.rm = TRUE),
                            skew(PROMEDIO_ANUAL, na.rm = TRUE),
                            kurtosi(PROMEDIO_ANUAL, na.rm = TRUE),
                            sd(PROMEDIO_ANUAL, na.rm = TRUE)/mean(PROMEDIO_ANUAL, na.rm = TRUE),
                            sum(!is.na(PROMEDIO_ANUAL))),
              
              NUMEROCURSOSINSCRITOS_S1   = c(mean(NUMEROCURSOSINSCRITOS_S1, na.rm = TRUE),
                                    median(NUMEROCURSOSINSCRITOS_S1, na.rm = TRUE),
                                    sd(NUMEROCURSOSINSCRITOS_S1, na.rm = TRUE),
                                    min(NUMEROCURSOSINSCRITOS_S1, na.rm = TRUE),
                                    max(NUMEROCURSOSINSCRITOS_S1, na.rm = TRUE),
                                    quantile(NUMEROCURSOSINSCRITOS_S1,probs = 0.25, na.rm = TRUE),
                                    quantile(NUMEROCURSOSINSCRITOS_S1,probs = 0.75, na.rm = TRUE),
                                    IQR(NUMEROCURSOSINSCRITOS_S1, na.rm = TRUE),
                                    skew(NUMEROCURSOSINSCRITOS_S1, na.rm = TRUE),
                                    kurtosi(NUMEROCURSOSINSCRITOS_S1, na.rm = TRUE),
                                    sd(NUMEROCURSOSINSCRITOS_S1, na.rm = TRUE)/mean(NUMEROCURSOSINSCRITOS_S1, na.rm = TRUE),
                                    sum(!is.na(NUMEROCURSOSINSCRITOS_S1))),
              
              NUMEROCURSOSAPROBADOS_S1    = c(mean(NUMEROCURSOSAPROBADOS_S1, na.rm = TRUE),
                                    median(NUMEROCURSOSAPROBADOS_S1, na.rm = TRUE),
                                    sd(NUMEROCURSOSAPROBADOS_S1, na.rm = TRUE),
                                    min(NUMEROCURSOSAPROBADOS_S1, na.rm = TRUE),
                                    max(NUMEROCURSOSAPROBADOS_S1, na.rm = TRUE),
                                    quantile(NUMEROCURSOSAPROBADOS_S1,probs = 0.25, na.rm = TRUE),
                                    quantile(NUMEROCURSOSAPROBADOS_S1,probs = 0.75, na.rm = TRUE),
                                    IQR(NUMEROCURSOSAPROBADOS_S1, na.rm = TRUE),
                                    skew(NUMEROCURSOSAPROBADOS_S1, na.rm = TRUE),
                                    kurtosi(NUMEROCURSOSAPROBADOS_S1, na.rm = TRUE),
                                    sd(NUMEROCURSOSAPROBADOS_S1, na.rm = TRUE)/mean(NUMEROCURSOSAPROBADOS_S1, na.rm = TRUE),
                                    sum(!is.na(NUMEROCURSOSAPROBADOS_S1))),
              
              NUMEROCURSOSINSCRITOS_S2    = c(mean(NUMEROCURSOSINSCRITOS_S2, na.rm = TRUE),
                                    median(NUMEROCURSOSINSCRITOS_S2, na.rm = TRUE),
                                    sd(NUMEROCURSOSINSCRITOS_S2, na.rm = TRUE),
                                    min(NUMEROCURSOSINSCRITOS_S2, na.rm = TRUE),
                                    max(NUMEROCURSOSINSCRITOS_S2, na.rm = TRUE),
                                    quantile(NUMEROCURSOSINSCRITOS_S2,probs = 0.25, na.rm = TRUE),
                                    quantile(NUMEROCURSOSINSCRITOS_S2,probs = 0.75, na.rm = TRUE),
                                    IQR(NUMEROCURSOSINSCRITOS_S2, na.rm = TRUE),
                                    skew(NUMEROCURSOSINSCRITOS_S2, na.rm = TRUE),
                                    kurtosi(NUMEROCURSOSINSCRITOS_S2, na.rm = TRUE),
                                    sd(NUMEROCURSOSINSCRITOS_S2, na.rm = TRUE)/mean(NUMEROCURSOSINSCRITOS_S2, na.rm = TRUE),
                                    sum(!is.na(NUMEROCURSOSINSCRITOS_S2))),
              
              NUMEROCURSOSAPROBADOS_S2    = c(mean(NUMEROCURSOSAPROBADOS_S2, na.rm = TRUE),
                                    median(NUMEROCURSOSAPROBADOS_S2, na.rm = TRUE),
                                    sd(NUMEROCURSOSAPROBADOS_S2, na.rm = TRUE),
                                    min(NUMEROCURSOSAPROBADOS_S2, na.rm = TRUE),
                                    max(NUMEROCURSOSAPROBADOS_S2, na.rm = TRUE),
                                    quantile(NUMEROCURSOSAPROBADOS_S2,probs = 0.25, na.rm = TRUE),
                                    quantile(NUMEROCURSOSAPROBADOS_S2,probs = 0.75, na.rm = TRUE),
                                    IQR(NUMEROCURSOSAPROBADOS_S2, na.rm = TRUE),
                                    skew(NUMEROCURSOSAPROBADOS_S2, na.rm = TRUE),
                                    kurtosi(NUMEROCURSOSAPROBADOS_S2, na.rm = TRUE),
                                    sd(NUMEROCURSOSAPROBADOS_S2, na.rm = TRUE)/mean(NUMEROCURSOSAPROBADOS_S2, na.rm = TRUE),
                                    sum(!is.na(NUMEROCURSOSAPROBADOS_S2))),
              
              DEUDA_ARA             = c(mean(DEUDA_ARA, na.rm = TRUE),
                                              median(DEUDA_ARA, na.rm = TRUE),
                                              sd(DEUDA_ARA, na.rm = TRUE),
                                              min(DEUDA_ARA, na.rm = TRUE),
                                              max(DEUDA_ARA, na.rm = TRUE),
                                              quantile(DEUDA_ARA,probs = 0.25, na.rm = TRUE),
                                              quantile(DEUDA_ARA,probs = 0.75, na.rm = TRUE),
                                              IQR(DEUDA_ARA, na.rm = TRUE),
                                              skew(DEUDA_ARA, na.rm = TRUE),
                                              kurtosi(DEUDA_ARA, na.rm = TRUE),
                                              sd(DEUDA_ARA, na.rm = TRUE)/mean(DEUDA_ARA, na.rm = TRUE),
                                              sum(!is.na(DEUDA_ARA))),
              
              DEUDA_COB    = c(mean(DEUDA_COB, na.rm = TRUE),
                                              median(DEUDA_COB, na.rm = TRUE),
                                              sd(DEUDA_COB, na.rm = TRUE),
                                              min(DEUDA_COB, na.rm = TRUE),
                                              max(DEUDA_COB, na.rm = TRUE),
                                              quantile(DEUDA_COB,probs = 0.25, na.rm = TRUE),
                                              quantile(DEUDA_COB,probs = 0.75, na.rm = TRUE),
                                              IQR(DEUDA_COB, na.rm = TRUE),
                                              skew(DEUDA_COB, na.rm = TRUE),
                                              kurtosi(DEUDA_COB, na.rm = TRUE),
                                              sd(DEUDA_COB, na.rm = TRUE)/mean(DEUDA_COB, na.rm = TRUE),
                                              sum(!is.na(DEUDA_COB))),
              
              DEUDA_CUA    = c(mean(DEUDA_CUA, na.rm = TRUE),
                                              median(DEUDA_CUA, na.rm = TRUE),
                                              sd(DEUDA_CUA, na.rm = TRUE),
                                              min(DEUDA_CUA, na.rm = TRUE),
                                              max(DEUDA_CUA, na.rm = TRUE),
                                              quantile(DEUDA_CUA,probs = 0.25, na.rm = TRUE),
                                              quantile(DEUDA_CUA,probs = 0.75, na.rm = TRUE),
                                              IQR(DEUDA_CUA, na.rm = TRUE),
                                              skew(DEUDA_CUA, na.rm = TRUE),
                                              kurtosi(DEUDA_CUA, na.rm = TRUE),
                                              sd(DEUDA_CUA, na.rm = TRUE)/mean(DEUDA_CUA, na.rm = TRUE),
                                              sum(!is.na(DEUDA_CUA))),
              
              DEUDA_DAA    = c(mean(DEUDA_DAA, na.rm = TRUE),
                                              median(DEUDA_DAA, na.rm = TRUE),
                                              sd(DEUDA_DAA, na.rm = TRUE),
                                              min(DEUDA_DAA, na.rm = TRUE),
                                              max(DEUDA_DAA, na.rm = TRUE),
                                              quantile(DEUDA_DAA,probs = 0.25, na.rm = TRUE),
                                              quantile(DEUDA_DAA,probs = 0.75, na.rm = TRUE),
                                              IQR(DEUDA_DAA, na.rm = TRUE),
                                              skew(DEUDA_DAA, na.rm = TRUE),
                                              kurtosi(DEUDA_DAA, na.rm = TRUE),
                                              sd(DEUDA_DAA, na.rm = TRUE)/mean(DEUDA_DAA, na.rm = TRUE),
                                              sum(!is.na(DEUDA_DAA))),
              
              DEUDA_MAT    = c(mean(DEUDA_MAT, na.rm = TRUE),
                                              median(DEUDA_MAT, na.rm = TRUE),
                                              sd(DEUDA_MAT, na.rm = TRUE),
                                              min(DEUDA_MAT, na.rm = TRUE),
                                              max(DEUDA_MAT, na.rm = TRUE),
                                              quantile(DEUDA_MAT,probs = 0.25, na.rm = TRUE),
                                              quantile(DEUDA_MAT,probs = 0.75, na.rm = TRUE),
                                              IQR(DEUDA_MAT, na.rm = TRUE),
                                              skew(DEUDA_MAT, na.rm = TRUE),
                                              kurtosi(DEUDA_MAT, na.rm = TRUE),
                                              sd(DEUDA_MAT, na.rm = TRUE)/mean(DEUDA_MAT, na.rm = TRUE),
                                              sum(!is.na(DEUDA_MAT))),
              
              TOTAL_DEUDA    = c(mean(TOTAL_DEUDA, na.rm = TRUE),
                                              median(TOTAL_DEUDA, na.rm = TRUE),
                                              sd(TOTAL_DEUDA, na.rm = TRUE),
                                              min(TOTAL_DEUDA, na.rm = TRUE),
                                              max(TOTAL_DEUDA, na.rm = TRUE),
                                              quantile(TOTAL_DEUDA,probs = 0.25, na.rm = TRUE),
                                              quantile(TOTAL_DEUDA,probs = 0.75, na.rm = TRUE),
                                              IQR(TOTAL_DEUDA, na.rm = TRUE),
                                              skew(TOTAL_DEUDA, na.rm = TRUE),
                                              kurtosi(TOTAL_DEUDA, na.rm = TRUE),
                                              sd(TOTAL_DEUDA, na.rm = TRUE)/mean(TOTAL_DEUDA, na.rm = TRUE),
                                              sum(!is.na(TOTAL_DEUDA))),
              
              CANT_COLACIONES    = c(mean(CANT_COLACIONES, na.rm = TRUE),
                                              median(CANT_COLACIONES, na.rm = TRUE),
                                              sd(CANT_COLACIONES, na.rm = TRUE),
                                              min(CANT_COLACIONES, na.rm = TRUE),
                                              max(CANT_COLACIONES, na.rm = TRUE),
                                              quantile(CANT_COLACIONES,probs = 0.25, na.rm = TRUE),
                                              quantile(CANT_COLACIONES,probs = 0.75, na.rm = TRUE),
                                              IQR(CANT_COLACIONES, na.rm = TRUE),
                                              skew(CANT_COLACIONES, na.rm = TRUE),
                                              kurtosi(CANT_COLACIONES, na.rm = TRUE),
                                              sd(CANT_COLACIONES, na.rm = TRUE)/mean(CANT_COLACIONES, na.rm = TRUE),
                                              sum(!is.na(CANT_COLACIONES))),
              
              CANT_ALMUERZOS    = c(mean(CANT_ALMUERZOS, na.rm = TRUE),
                                              median(CANT_ALMUERZOS, na.rm = TRUE),
                                              sd(CANT_ALMUERZOS, na.rm = TRUE),
                                              min(CANT_ALMUERZOS, na.rm = TRUE),
                                              max(CANT_ALMUERZOS, na.rm = TRUE),
                                              quantile(CANT_ALMUERZOS,probs = 0.25, na.rm = TRUE),
                                              quantile(CANT_ALMUERZOS,probs = 0.75, na.rm = TRUE),
                                              IQR(CANT_ALMUERZOS, na.rm = TRUE),
                                              skew(CANT_ALMUERZOS, na.rm = TRUE),
                                              kurtosi(CANT_ALMUERZOS, na.rm = TRUE),
                                              sd(CANT_ALMUERZOS, na.rm = TRUE)/mean(CANT_ALMUERZOS, na.rm = TRUE),
                                              sum(!is.na(CANT_ALMUERZOS))),
              
              CANT_CENAS    = c(mean(CANT_CENAS, na.rm = TRUE),
                                              median(CANT_CENAS, na.rm = TRUE),
                                              sd(CANT_CENAS, na.rm = TRUE),
                                              min(CANT_CENAS, na.rm = TRUE),
                                              max(CANT_CENAS, na.rm = TRUE),
                                              quantile(CANT_CENAS,probs = 0.25, na.rm = TRUE),
                                              quantile(CANT_CENAS,probs = 0.75, na.rm = TRUE),
                                              IQR(CANT_CENAS, na.rm = TRUE),
                                              skew(CANT_CENAS, na.rm = TRUE),
                                              kurtosi(CANT_CENAS, na.rm = TRUE),
                                              sd(CANT_CENAS, na.rm = TRUE)/mean(CANT_CENAS, na.rm = TRUE),
                                              sum(!is.na(CANT_CENAS))),
              .groups = "drop") 
}



#promedios por cada año sin filtrar carrera


########################Los puse con relacion a cada año el semestre
u <- datos %>% 
  group_nest(ANNIO_MATRICULA_INDICE)%>% 
  mutate(r = map(data, r2))%>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 




u <- left_join(u[[1]], u[[2]]) %>% 
  left_join(u[[3]]) %>% 
  left_join(u[[4]]) %>% 
  left_join(u[[5]]) 

view(u)

write.xlsx(u,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_Todos.xlsx") 
write_csv(u,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_Todos.csv") 

u.Ingenieria <- datos %>% 
  filter(CARR_DESCRIPCION=="3353- INGENIERIA COMERCIAL MENCION NEGOCIOS MINEROS" ) %>%
  group_nest(ANNIO_MATRICULA_INDICE)%>% 
  mutate(r = map(data, r2))%>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 




u.Ingenieria <- left_join(u.Ingenieria[[1]], u.Ingenieria[[2]]) %>% 
  left_join(u.Ingenieria[[3]]) %>% 
  left_join(u.Ingenieria[[4]]) %>% 
  left_join(u.Ingenieria[[5]]) 

write.xlsx(u.Ingenieria,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_Ingenieria.xlsx") 
write_csv(u.Ingenieria,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_Ingenieria.csv") 

u.Enfermeria <- datos %>% 
  filter(CARR_DESCRIPCION=="4111- ENFERMERIA" ) %>%
  group_nest(ANNIO_MATRICULA_INDICE)%>% 
  mutate(r = map(data, r2))%>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 





u.Enfermeria <- left_join(u.Enfermeria[[1]], u.Enfermeria[[2]]) %>% 
  left_join(u.Enfermeria[[3]]) %>% 
  left_join(u.Enfermeria[[4]]) %>% 
  left_join(u.Enfermeria[[5]]) 

write.xlsx(u.Enfermeria,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_Enfermeria.xlsx") 
write_csv(u.Enfermeria,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_Enfermeria.csv") 


#**********************************************************************************************

#promedios por cada año, pero separados los años de ingreso, sin filtrar carrera-----


v <- datos %>% 
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2017") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 

v <- left_join(v[[1]], v[[2]]) %>% 
  left_join(v[[3]]) %>% 
  left_join(v[[4]]) %>% 
  left_join(v[[5]]) 
view(v)

write.xlsx(v,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_&_INGRESO_2017.xlsx") 
write_csv(v,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_&_INGRESO_2017.csv") 

v2 <- datos %>% 
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2018") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 

v2 <- left_join(v2[[1]], v2[[2]]) %>% 
  left_join(v2[[3]]) %>% 
  left_join(v2[[4]]) 

write.xlsx(v2,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_&_INGRESO_2018.xlsx") 
write_csv(v2,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_&_INGRESO_2018.csv") 

v3 <- datos %>% 
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2019") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 

v3 <- left_join(v3[[1]], v3[[2]]) %>% 
  left_join(v3[[3]]) 
view(v3)

write.xlsx(v3,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_&_INGRESO_2019.xlsx") 
write_csv(v3,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_&_INGRESO_2019.csv")


v4 <- datos %>% 
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2020") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 

v4 <- left_join(v4[[1]], v4[[2]]) 
view(v4)
write.xlsx(v4,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_&_INGRESO_2020.xlsx") 
write_csv(v4,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_&_INGRESO_2020.csv")

v5 <- datos %>% 
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2021") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] %>% 
  .[[1]]

view(v5)

write.xlsx(v5,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_&_INGRESO_2021.xlsx") 
write_csv(v5,file = "DatosEstadisticos//EstadisticosSemestral_ANNO_MATRI_&_INGRESO_2021.csv")

#promedios por cada año, pero separados los años de ingreso, carrera de enfermeria-----

w <- datos %>%   
  filter(CARR_DESCRIPCION=="4111- ENFERMERIA" ) %>%
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2017") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 

w <- left_join(w[[1]], w[[2]]) %>% 
  left_join(w[[3]]) %>% 
  left_join(w[[4]]) %>% 
  left_join(w[[5]]) 
view(w)
write.xlsx(w,file = "DatosEstadisticos//EstadisticosSemestralEnfermeria_ANNO_MATRI_&_INGRESO_2017.xlsx") 
write_csv(w,file = "DatosEstadisticos//EstadisticosSemestralEnfermeria_ANNO_MATRI_&_INGRESO_2017.csv") 


w2 <- datos %>%   
  filter(CARR_DESCRIPCION=="4111- ENFERMERIA" ) %>%
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2018") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 

w2 <- left_join(w2[[1]], w2[[2]]) %>% 
  left_join(w2[[3]]) %>% 
  left_join(w2[[4]]) 
view(w2)

write.xlsx(w2,file = "DatosEstadisticos//EstadisticosSemestralEnfermeria_ANNO_MATRI_&_INGRESO_2018.xlsx") 
write_csv(w2,file = "DatosEstadisticos//EstadisticosSemestralEnfermeria_ANNO_MATRI_&_INGRESO_2018.csv") 

w3 <- datos %>%    
  filter(CARR_DESCRIPCION=="4111- ENFERMERIA" ) %>%
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2019") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 

w3 <- left_join(w3[[1]], w3[[2]]) %>% 
  left_join(w3[[3]]) 
view(w3)

write.xlsx(w3,file = "DatosEstadisticos//EstadisticosSemestralEnfermeria_ANNO_MATRI_&_INGRESO_2019.xlsx") 
write_csv(w3,file = "DatosEstadisticos//EstadisticosSemestralEnfermeria_ANNO_MATRI_&_INGRESO_2019.csv") 

w4 <- datos %>%    
  filter(CARR_DESCRIPCION=="4111- ENFERMERIA" ) %>%
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2020") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 

w4 <- left_join(w4[[1]], w4[[2]]) 
write.xlsx(w4,file = "DatosEstadisticos//EstadisticosSemestralEnfermeria_ANNO_MATRI_&_INGRESO_2020.xlsx") 
write_csv(w4,file = "DatosEstadisticos//EstadisticosSemestralEnfermeria_ANNO_MATRI_&_INGRESO_2020.csv") 
view(w4)

w5 <- datos %>%    
  filter(CARR_DESCRIPCION=="4111- ENFERMERIA" ) %>%
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2021") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] %>% 
  .[[1]]

view(w5)

write.xlsx(w5,file = "DatosEstadisticos//EstadisticosSemestralEnfermeria_ANNO_MATRI_&_INGRESO_2021.xlsx") 
write_csv(w5,file = "DatosEstadisticos//EstadisticosSemestralEnfermeria_ANNO_MATRI_&_INGRESO_2021.csv") 
#promedios por cada año, pero separados los años de ingreso, carrera de INgenieria-----

x <- datos %>%   
  filter(CARR_DESCRIPCION=="3353- INGENIERIA COMERCIAL MENCION NEGOCIOS MINEROS" ) %>%
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2017") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 

x <- left_join(x[[1]], x[[2]]) %>% 
  left_join(x[[3]]) %>% 
  left_join(x[[4]]) %>% 
  left_join(x[[5]]) 
view(x)


write.xlsx(x,file = "DatosEstadisticos//EstadisticosSemestralIngenieria_ANNO_MATRI_&_INGRESO_2017.xlsx") 
write_csv(x,file = "DatosEstadisticos//EstadisticosSemestralIngenieria_ANNO_MATRI_&_INGRESO_2017.csv") 

x2 <- datos %>%   
  filter(CARR_DESCRIPCION=="3353- INGENIERIA COMERCIAL MENCION NEGOCIOS MINEROS" ) %>%
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2018") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 

x2 <- left_join(x2[[1]], x2[[2]]) %>% 
  left_join(x2[[3]]) %>% 
  left_join(x2[[4]]) 
view(x2)
write.xlsx(x2,file = "DatosEstadisticos//EstadisticosSemestralIngenieria_ANNO_MATRI_&_INGRESO_2018.xlsx") 
write_csv(x2,file = "DatosEstadisticos//EstadisticosSemestralIngenieria_ANNO_MATRI_&_INGRESO_2018.csv") 


x3 <- datos %>%    
  filter(CARR_DESCRIPCION=="3353- INGENIERIA COMERCIAL MENCION NEGOCIOS MINEROS" ) %>%
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2019") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 

x3 <- left_join(x3[[1]], x3[[2]]) %>% 
  left_join(x3[[3]]) 
view(x3)

write.xlsx(x3,file = "DatosEstadisticos//EstadisticosSemestralIngenieria_ANNO_MATRI_&_INGRESO_2019.xlsx") 
write_csv(x3,file = "DatosEstadisticos//EstadisticosSemestralIngenieria_ANNO_MATRI_&_INGRESO_2019.csv")


x4 <- datos %>%    
  filter(CARR_DESCRIPCION=="3353- INGENIERIA COMERCIAL MENCION NEGOCIOS MINEROS" ) %>%
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2020") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] 

x4 <- left_join(x4[[1]], x4[[2]]) 
view(x4)
write.xlsx(x4,file = "DatosEstadisticos//EstadisticosSemestralIngenieria_ANNO_MATRI_&_INGRESO_2020.xlsx") 
write_csv(x4,file = "DatosEstadisticos//EstadisticosSemestralIngenieria_ANNO_MATRI_&_INGRESO_2020.csv")

x5 <- datos %>%    
  filter(CARR_DESCRIPCION=="3353- INGENIERIA COMERCIAL MENCION NEGOCIOS MINEROS" ) %>%
  group_nest(ANNIO_MATRICULA_INDICE, ANNIO_INGRESO)%>% 
  mutate(r = map(data, r2)) %>% 
  filter(ANNIO_INGRESO=="2021") %>% 
  unnest(ANNIO_MATRICULA_INDICE) %>% 
  group_by(ANNIO_MATRICULA_INDICE) %>%
  mutate(R= map(r, ~{c(names(.)[names(.) == "EDAD_POR_ANNIO_MATRICULA"] <-c(paste0('EDAD_POR_ANNIO_MATRICULA_', ANNIO_MATRICULA_INDICE)),                       
                       names(.)[names(.) == "PROM_NOTA_SEM1"] <-c(paste0('PROM_NOTA_SEM1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "PROM_NOTA_SEM2"] <-c(paste0('PROM_NOTA_SEM2_', ANNIO_MATRICULA_INDICE)),                        
                       names(.)[names(.) == "PROMEDIO_ANUAL"] <-c(paste0('PROMEDIO_ANUAL_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S1"] <-c(paste0('NUMEROCURSOSINSCRITOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S1"] <-c(paste0('NUMEROCURSOSAPROBADOS_S1_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSINSCRITOS_S2"] <-c(paste0('NUMEROCURSOSINSCRITOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "NUMEROCURSOSAPROBADOS_S2"] <-c(paste0('NUMEROCURSOSAPROBADOS_S2_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_ARA"] <-c(paste0('DEUDA_ARA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_COB"] <-c(paste0('DEUDA_COB_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_CUA"] <-c(paste0('DEUDA_CUA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_DAA"] <-c(paste0('DEUDA_DAA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "DEUDA_MAT"] <-c(paste0('DEUDA_MAT_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "TOTAL_DEUDA"] <-c(paste0('TOTAL_DEUDA_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_COLACIONES"] <-c(paste0('CANT_COLACIONES_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_ALMUERZOS"] <-c(paste0('CANT_ALMUERZOS_', ANNIO_MATRICULA_INDICE)),
                       names(.)[names(.) == "CANT_CENAS"] <-c(paste0('CANT_CENAS_', ANNIO_MATRICULA_INDICE))); .}))%>% 
  select(R) %>%
  .[,2] %>% 
  .[[1]] %>% 
  .[[1]]

view(x5)

write.xlsx(x5,file = "DatosEstadisticos//EstadisticosSemestralIngenieria_ANNO_MATRI_&_INGRESO_2021.xlsx") 
write_csv(x5,file = "DatosEstadisticos//EstadisticosSemestralIngenieria_ANNO_MATRI_&_INGRESO_2021.csv")



















