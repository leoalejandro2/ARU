library("oaxaca")
library("haven")
library("dplyr")
library(ggplot2)
library(survey)
library(srvyr)
library(stringr)
library(tidyr)
library(tseries)
library(writexl)
library(pheatmap)
library(UpSetR)
library(sjlabelled)

eh23 = read_sav("database/EH/EH2023/EH2023_Discriminacion.sav")
eh18 = read_sav("database/EH/EH2018/EH2018_Discriminacion.sav")

# edsa <- read_sav("database/EDSA/EDSA2016/EDSA16_HOGAR.sav")



edsa = read_sav("database/EDSA/EDSA2023/EDSA2023_Hogar.sav")
edsaV = read_sav("database/EDSA/EDSA2023/EDSA2023_Vivienda.sav")
edsah = read_sav("database/EDSA/EDSA2023/EDSA2023_Hombre.sav")
edsam = read_sav("database/EDSA/EDSA2023/EDSA2023_Mujer.sav")


### 
edsaV %>% get_label()
edsa %>% filter((hs03_0033 == 1)) %>% get_labels()

edsa %>% pull(afilsegsal) %>% table()
  
edsa %>% get_label()
  
get_label(edsa)
#establecimiento o servicio de salud
edsa %>% filter(hs03_0033==1,
                hs03_0035_A==1,   # puesto de salud
                hs03_0035_B==1,   # centro de salud ambulatorio
                hs03_0035_C==1,   # centro de salud con internacion
                hs03_0035_D==1,   # centro de salud integral
                hs03_0035_E==1,   # hospital de segundo nivel
                hs03_0035_F==1,   # hospital de tercer nivel
                hs03_0035_G==1,   # hospital especializado
                hs03_0035_H==1,   # caja nacional de salud
                hs03_0035_I==1,   # caja de la banca privada
                hs03_0035_J==1,   # caja petrolera
                hs03_0035_K==1,   # caja de la banca estatal
                hs03_0035_L==1,   # cordes
                hs03_0035_M==1,   # caja de caminos
                hs03_0035_N==1,   # cossmil/FFAA
                hs03_0035_O==1,   # seguro universitario
                hs03_0035_P==1,   # organismos privados 1,2,3
                hs03_0035_Q==1,   # iglesias 1, 2, 3
                hs03_0035_R==1,   # promotor de la salud/RPS/otro agente
                hs03_0035_S==1,   # visita domiciliaria
                hs03_0035_T==1,   # Farmacia
                hs03_0035_U==1,   # Medicina tradicional
                hs03_0035_V==1,   # No acudio a ningun centro de salud
                hs03_0035_X==1,   # otro lugar
                hs03_0035_Z==1,   # no sabe
                hs03_0035_X_cod==1,)    # especificar otro lugar






edsa %>% get_label()

edsa %>% filter(hs03_0033==1) %>% group_by(hs01_0010,hs03_0035_V) %>% count() %>% 
  group_by(hs01_0010) %>% mutate(n = n / sum(n))

# -------------------------------
# Disponibilidad
# -------------------------------
edsa %>% select(hs03_0035_A:hs03_0035_Z) %>% get_label()
edsa %>% select(hs03_0039_A:hs03_0039_Z) %>% get_label()

# Cantidad de servicios disponibles en relación con 
# la cantidad y tipo de necesidades de la población

edsa %>% filter(hs03_0033==1)


aux1 = edsa %>% filter(hs03_0033 == 1, hs03_0035_V == 1, hs03_0039_A==1 | hs03_0039_B==1 |
                         hs03_0039_E == 1 | hs03_0039_G == 1 | hs03_0039_H == 1 |
                         hs03_0039_I == 1 | hs03_0039_M == 1 | hs03_0039_X_cod %in% c('R','U','V'))



# -------------------------------
# Accesibilidad
# -------------------------------
edsa %>% select(hs03_0035_A:hs03_0035_Z)
edsa %>% select(hs03_0039_A:hs03_0039_Z)



# Localización de los servicios de salud y de los usuarios,
# incluyendo recursos de transporte, tiempo, distancia y costo


# -------------------------------
# Alojamiento (Accommodation)
# -------------------------------
edsa %>% select(hs03_0039_A:hs03_0039_Z)
edsa %>% select(hs03_b_0046:hs03_b_0048)

# Forma en que los servicios de salud están organizados
# para atender a los usuarios (horarios, turnos, tiempos de espera)


# -------------------------------
# Asequibilidad
# -------------------------------
edsa %>% select(hs03_0029_A:hs03_0029_X_cod)
edsa %>% select(hs03_0039_A:hs03_0039_Z)


edsa %>% filter(hs03_0033 == 1, hs03_0039_J==1)


# Relación entre el costo de los servicios y la capacidad
# de pago de los usuarios
# Incluye:
# - Existencia de seguro médico
# - Percepción del costo
# - Gastos en atención de salud


# -------------------------------
# Aceptabilidad
# -------------------------------

## The acceptability dimension will be constructed using a summated rating scale derived from binary response variables

aux1 = edsa %>% filter(hs03_0033 == 1) %>% select(hs03_0037_A:hs03_0037_I)
aux1 %>% get_label()

aux1 %>% 

aux1 %>% mutate(across(hs03_0037_A:hs03_0037_I, 
                       ))

edsa %>% select(hs03_0037_A:hs03_0037_I)
edsa %>% select(hs03_0039_A:hs03_0039_Z) %>% get_label()

# Relación entre las actitudes de los usuarios y las 
# características del personal de salud (idioma, trato,
# respeto cultural, etc.)

edsa %>% filter(hs03_0033 == 1 , is.na(hs03_0035_V)) %>% 
  select(hs03_0037_A:hs03_0037_I) %>% nrow()


edsa %>% 
  filter(hs03_0033 == 1,
         !is.na(hs03_0037_A),
         !is.na(hs03_0037_H),
         hs03_0037_A != 999) %>% 
  select(hs03_0037_A:hs03_0037_I) %>% summary()

edsa_accept <- edsa %>% 
  filter(hs03_0033 == 1,
         !is.na(hs03_0037_A),
         !is.na(hs03_0037_H),
         hs03_0037_A != 999) %>% 
  mutate(across(hs03_0037_A:hs03_0037_I,
                ~ case_when(
                  . == 1 ~ 1,
                  . == 2 ~ 0
                ))) %>% 
  mutate(acceptability = rowSums(across(hs03_0037_A:hs03_0037_I), 
                                 na.rm = TRUE))

edsa_accept <- edsa_accept %>% 
  mutate(acceptability_std = 
           (1 - ((acceptability - min(acceptability, na.rm = TRUE)) /
           (max(acceptability, na.rm = TRUE) - 
              min(acceptability, na.rm = TRUE)))))



edsa_accept %>% get_label()
edsa_accept %>% group_by(hs01_0010) %>% summarise(mean(acceptability_std))
edsa_accept$aestudio


plot(edsa_accept$aestudio, edsa_accept$acceptability_std)

hist(edsa_accept$acceptability_std, breaks = 8)





