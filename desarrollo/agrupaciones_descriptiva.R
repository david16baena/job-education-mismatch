setwd("C:/Users/david/Desktop/proyectos/job-education-mismatch/data")
library("dplyr")
library("tidyr")
library("Hmisc")

stata <- read.csv("datos_para_agrupar.csv")      

names(stata)

mean_group <- function(x) stata%>% group_by(sit_o)  %>% 
  summarise(var = wtd.mean(get(eval(x)), fex)) %>%
  ungroup() %>% 
  summarise(mean = mean(var, na.rm = TRUE),
            quant0 = quantile(var, 0, na.rm = TRUE),
            quant1 = quantile(var, 0.25, na.rm = TRUE),
            quant2 = quantile(var, 0.5, na.rm = TRUE),
            quant3 = quantile(var, 0.75, na.rm = TRUE),
            quant4 = quantile(var, 1, na.rm = TRUE))

sum_group <- function(x) stata%>% group_by(sit_o)  %>% 
  summarise(var = sum(get(eval(x))*fex)) %>%
  ungroup() %>% 
  summarise(mean = mean(var, na.rm = TRUE),
            quant0 = quantile(var, 0, na.rm = TRUE),
            quant1 = quantile(var, 0.25, na.rm = TRUE),
            quant2 = quantile(var, 0.5, na.rm = TRUE),
            quant3 = quantile(var, 0.75, na.rm = TRUE),
            quant4 = quantile(var, 1, na.rm = TRUE))

mean_group("mismatch")
mean_group("accesiblidad_nivel")
mean_group("accesiblidad_privada_nivel")
mean_group("accesiblidad")
mean_group("acc")
sum_group("empleado")
sum_group("carro0")
sum_group("carro1")
sum_group("carro2")
sum_group("overeduc")
sum_group("undereduc")

wtd.mean(stata$overeduc, stata$fex)
sum(stata$undereduc*stata$fex, na.rm = TRUE)
