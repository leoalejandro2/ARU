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

edsahijos <- read_sav("database/EDSA/EDSA2023/EDSA2023_HistorialHijos.sav")
edsapari <- read_sav("database/EDSA/EDSA2023/EDSA2023_HistorialParidad.sav")
edsa = read_sav("database/EDSA/EDSA2023/EDSA2023_Hogar.sav")
edsah = read_sav("database/EDSA/EDSA2023/EDSA2023_Hombre.sav")
edsam = read_sav("database/EDSA/EDSA2023/EDSA2023_Mujer.sav")
edsamc <- read_sav("database/EDSA/EDSA2023/EDSA2023_MujerCalendario.sav")
edsap = read_sav("database/EDSA/EDSA2023/EDSA2023_Peso_talla_hemo.sav")
edsapi <- read_sav("database/EDSA/EDSA2023/EDSA2023_PrimeraInfancia.sav")
edsaV = read_sav("database/EDSA/EDSA2023/EDSA2023_Vivienda.sav")

edsap %>% get_label()
table(edsa$afilsegsal)

edsaph = edsap %>% select(folio, nro, upm, estrato, hs05_0103, hs05_0104, hs05_0105, hs05_0106,
                          ponderador_vpt, area, altitud, imc_h, categimc_h, categaimc_h)



edsagh = edsah %>% right_join(edsaph, by = c("folio", "nro", "upm","estrato"))

edsagh %>% get_label() %>% View()

edsagh %>% 
  filter(vs01_0101a >= 6, vs01_0101a <= 59) %>% 
  pull(imc_h) %>% 
  hist()

edsagh %>% 
  filter(vs01_0101a >= 6, vs01_0101a <= 59) %>% 
  with(plot(vs01_0101a, imc_h))

edsagh %>% 
  filter(vs01_0101a >= 6, vs01_0101a <= 59) %>% 
  with(plot(vs01_0124, imc_h))




###############################################################################################

edsapm = edsap %>% select(folio, nro, upm, estrato, hs05_0095, hs05_0096, hs05_0097, hs05_0098,
                          hs06_0119, hs06_0120, hs06_0121, hs06_0122, ponderador_mpt, 
                          ponderador_mhm, area, altitud, imc_m, categimc_m, categaimc_m)








eds23h %>% filter(hs01_0004a<=59) %>% nrow()
eds23p %>% nrow()
eds23h$hs01_0004a

eds23p %>% select(hs05_0095:hs05_0098,hs06_0120:hs06_0122, ponderador_mpt, 
                  ponderador_mhm, imc_m, categimc_m,categaimc_m) %>% 
  mutate(imcy = hs05_0095/((hs05_0096/100)**2)) %>% View()

hist(eds23p$imc_m)



get_label(eds23p)

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




