library("oaxaca")
library("haven")
library("dplyr")

eh23 = read_sav("database/EH/EH2023/EH2023_Discriminacion.sav")
eh18 = read_sav("database/EH/EH2018/EH2018_Discriminacion.sav")


edsa = read_sav("database/EDSA/EDSA2023/EDSA2023_Hogar.sav")
edsaV = read_sav("database/EDSA/EDSA2023/EDSA2023_Vivienda.sav")
edsah = read_sav("database/EDSA/EDSA2023/EDSA2023_Hombre.sav")
edsam = read_sav("database/EDSA/EDSA2023/EDSA2023_Mujer.sav")


### 

# Disponibilidad
edsa %>% select(hs03_0035_A:hs03_0035_Z)

edsa %>% filter(hs03_0033 ==1) %>% select(hs03_0035_A:hs03_0035_Z) %>% View()

edsa %>% select(hs03_0039_A:hs03_0039_Z)
# Accesibilidad 
edsa %>% select(hs03_0035_A:hs03_0035_Z)
edsa %>% select(hs03_0039_A:hs03_0039_Z)
# Alojamiento
edsa %>% select(hs03_0039_A:hs03_0039_Z)
edsa %>% select(hs03_b_0046:hs03_b_0048)
# Asequibilidad 
edsa %>% select(hs03_0029_A:hs03_0029_X_cod)
edsa %>% select(hs03_0039_A:hs03_0039_Z)
# Aceptabilidad
edsa %>% select(hs03_0037_A:hs03_0037_I)
edsa %>% select(hs03_0039_A:hs03_0039_Z) View()









