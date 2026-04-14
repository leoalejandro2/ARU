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
edsa %>% filter((hs03_0033 == 1)) %>% get_labels()
edsa$
get_label(edsa)

# -------------------------------
# Disponibilidad
# -------------------------------
edsa %>% select(hs03_0035_A:hs03_0035_Z)
edsa %>% select(hs03_0039_A:hs03_0039_Z)

# Cantidad de servicios disponibles en relación con 
# la cantidad y tipo de necesidades de la población

edsa %>% select()


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



edsa_accept
hist(edsa_accept$acceptability_std, breaks = 8)





