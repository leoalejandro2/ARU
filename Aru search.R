#install.packages("oaxaca")
#install.packages("UpSetR")
#install.packages("sjlabelled")
library("oaxaca")
library("haven")
library("dplyr")
library(ggplot2)
library(survey)
library(srvyr)
library(stringr)
library(tidyr)
library(UpSetR)
library(sjlabelled)

############################################################################################

eh24 = read_sav("database/EH/EH2024/EH2024_Persona.sav")
eh24dis = read_sav("database/EH/EH2024/EH2024_Discriminacion.sav")
eds23h = read_sav("database/EDSA/EDSA2023/EDSA2023_Hogar.sav")
eds23p = read_sav("database/EDSA/EDSA2023/EDSA2023_Peso_talla_hemo.sav")
eds23v = read_sav("database/EDSA/EDSA2023/EDSA2023_Vivienda.sav")
eds23m = read_sav("database/EDSA/EDSA2023/EDSA2023_Mujer.sav")

eds23h %>% filter(hs01_0004a<=59) %>% nrow()
eds23p %>% nrow()
eds23h$hs01_0004a

eds23p %>% select(hs05_0095:hs05_0098,hs06_0120:hs06_0122, ponderador_mpt, 
                  ponderador_mhm, imc_m, categimc_m,categaimc_m) %>% 
  mutate(imcy = hs05_0095/((hs05_0096/100)**2)) %>% View()

hist(eds23p$imc_m)



get_label(eds23p) %>% View()

a %>% View()

eh24dis$s09a_01k

eh24$s01a_03


eh24dis %>% nrow()

aux1 = eh24dis %>%
  mutate(discriminacion =ifelse(rowSums(across(s09a_01a:s09a_01l, ~ .x %in% c(1,3)))>0,1,0))




table(aux1$discriminacion)/nrow(aux1)
table(aux1$s09a_01a)/nrow(aux1)
table(aux1$s09a_01b)/nrow(aux1)
table(aux1$s09a_01c)/nrow(aux1)
table(aux1$s09a_01d)/nrow(aux1)
table(aux1$s09a_01e)/nrow(aux1)
table(aux1$s09a_01f)/nrow(aux1)
table(aux1$s09a_01g)/nrow(aux1)
table(aux1$s09a_01h)/nrow(aux1)
table(aux1$s09a_01i)/nrow(aux1)
table(aux1$s09a_01j)/nrow(aux1)
table(aux1$s09a_01k)/nrow(aux1)
table(aux1$s09a_01l)/nrow(aux1)

eh24dis %>% group_by(s09a_01a) %>% summarise(total = n()/nrow(eh24dis))

mean(eh24dis$ponderador)

hist(eh24dis$ponderador)
