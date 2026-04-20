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
library(svyVGAM)



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

## se estan filtrando a las personas que han tenido un problema de salud en los ultimos 3 meses, 
## ademas de que se esta excluyendo a las personas que reportan que no fueron a un centro de salud debido a que no lo
## consideraban grave o que no saben por que no fueron


aux1 <- edsa %>% 
  filter(hs03_0033 == 1, !(hs03_0039_X_cod== 'Q'), is.na(hs03_0039_Z)) %>% 
  mutate(subPublico = rowSums(across(hs03_0035_A:hs03_0035_G) == 1, na.rm = TRUE)) 

aux1 = aux1 %>% mutate(subsseguridads = rowSums(across(hs03_0035_H:hs03_0035_O) == 1,na.rm = TRUE))

aux1 = aux1 %>% mutate(subprivado = rowSums(across(hs03_0035_P:hs03_0035_Q) == 1,na.rm = TRUE))

aux1 = aux1 %>% mutate(otros = rowSums(across(hs03_0035_R:hs03_0035_U, hs03_0035_X, hs03_0035_Z) == 1,na.rm = TRUE))

aux1 = aux1 %>% mutate(nofue = rowSums(across(hs03_0035_V) == 1,na.rm = TRUE))

aux1$hs03_0039_X_cod %>% table()



aux2 = edsa %>% 
  filter(hs03_0033 == 1, !(hs03_0039_X_cod== 'Q'), is.na(hs03_0039_Z)) %>% 
  mutate(formal = rowSums(across(hs03_0035_A:hs03_0035_Q) == 1, na.rm = TRUE)) 

aux2 = aux2 %>% mutate(informal = rowSums(across(c(hs03_0035_R:hs03_0035_U, hs03_0035_X, hs03_0035_Z)) == 1,na.rm = TRUE))

aux2 = aux2 %>% mutate(nofue = rowSums(across(hs03_0035_V) == 1,na.rm = TRUE))


aux2 %>% group_by(formal, informal , nofue) %>% count()

aux2 = aux2 %>% filter(!(formal==0 & informal==0 & nofue==0)) %>% 
  mutate(servicio = ifelse(formal>=1,"formal",ifelse(informal>=1,"alternativa","no fue")))

aux2 %>% group_by( servicio) %>% count()

aux2 %>% group_by(hs01_0003, servicio) %>% count() %>% group_by(hs01_0003) %>% summarise(n = n /sum(n))

design = svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~factorexph,
  data = (aux2)
)

modelo <- svy_vglm(servicio ~ hs01_0004a + hs01_0003 + hs01_0010,
                   design = design,
                   family = multinomial())

summary(modelo)



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

aux3 = edsa %>% filter(hs03_0033 == 1) %>% 
  mutate(formal = rowSums(across(hs03_0035_A:hs03_0035_Q) == 1,na.rm = TRUE)) %>% filter(formal >= 1)


aux3 = aux3 %>% filter(!(hs03_0037_A==999)) 

aux3 = aux3 %>% mutate(acept = rowSums(across(hs03_0037_A:hs03_0037_I) == 1, na.rm = TRUE)) %>% 
  mutate(acept= )


aux3 <- aux3 %>% 
  mutate(
    acept = rowSums(across(hs03_0037_A:hs03_0037_I) == 1, na.rm = TRUE),
    acept_std = (acept - min(acept, na.rm = TRUE)) /
      (max(acept, na.rm = TRUE) - min(acept, na.rm = TRUE)),
    acept_std = 1 - acept_std
  )

aux3 %>% group_by(hs01_0010) %>% summarise(mean(acept_std))


  
  
  
  
  