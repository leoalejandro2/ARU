library(haven)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(stringr)
library(tidyr)
library(tseries)
library(writexl)


eh24 = read_sav("database/EH/EH2024/EH2024_Persona.sav")
eh24s = read_sav("database/EH/EH2024/EH2024_Seguridad_Alimentaria.sav")
eh24d = read_sav("database/EH/EH2024/EH2024_Discriminacion.sav")


eh24d %>% group_by(s09a_02) %>% count()

sum(eh24d$ponderador)

eh24d %>% group_by(s09a_05_8) %>% summarise(n = sum(ponderador))


aux1 = eh24d %>% 
  mutate(R = if_else(
    if_any(s09a_01a:s09a_01l, ~ . %in% c(1, 3)),
    "si",
    "no"
  )) 

aux1 %>% 
  group_by(R) %>% 
  summarise(n = sum(ponderador)) %>% mutate(n = n/sum(n)*100)

aux1 %>% filter(R=="si") %>% group_by(s09a_02) %>% summarise(n = sum(ponderador)) %>% 
  mutate(n = n / sum(n)*100)

aux1 %>% filter(R=="si", s09a_02 == 2) %>% select(s09a_05_1:s09a_05_8) 

aux1 %>% get_label()




######################################################
edsa16h = read_sav("database/EDSA/EDSA2016/EDSA16_HOGAR.sav")



edsah <- read_sav("database/EDSA/EDSA2023/EDSA2023_Hogar.sav")
edsape <- read_sav("database/EDSA/EDSA2023/EDSA2023_Peso_talla_hemo.sav")

edsav = read_sav("database/EDSA/EDSA2023/EDSA2023_Hombre.sav")
edsam = read_sav("database/EDSA/EDSA2023/EDSA2023_Mujer.sav")

edsadm = read_sav("database/EDSA/EDSA2023/EDSA2023_HistorialHijos.sav")


aux2 = edsadm %>% group_by(folio,nro) %>% count() %>% left_join(edsah, by = c("folio","nro"))

aux2$hs01_0004a

ggplot(aux2, aes(x = factor(hs01_0004a), y = n)) +
  geom_boxplot()

edsah$hs01_0003
hist(edsah$hs01_0004a)
hist(edsa16h$hs03_0003_1)
edsa16h$hs03_0005

edsah %>% 
  mutate(
    grupo_edad = cut(hs01_0004a, 
                     breaks = seq(0, 80, 5), 
                     right = FALSE),
    sexo = factor(hs01_0003, labels = c("Hombres", "Mujeres"))
  ) %>% 
  group_by(grupo_edad, sexo) %>% 
  summarise(n = n()) %>% 
  mutate(n = ifelse(sexo == "Hombres", -n, n)) %>% 
  ggplot(aes(x = grupo_edad, y = n, fill = sexo)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Grupo de edad",
    y = "Población",
    title = "Pirámide poblacional"
  )
  
edsa16h %>% 
  mutate(
    grupo_edad = cut(hs03_0003_1, 
                     breaks = seq(0, 80, 5), 
                     right = FALSE),
    sexo = factor(hs03_0005, labels = c("Hombres", "Mujeres"))
  ) %>% 
  group_by(grupo_edad, sexo) %>% 
  summarise(n = n()) %>% 
  mutate(n = ifelse(sexo == "Hombres", -n, n)) %>% 
  ggplot(aes(x = grupo_edad, y = n, fill = sexo)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Grupo de edad",
    y = "Población",
    title = "Pirámide poblacional"
  )



edsah %>% get_label()

edsag = edsah %>% left_join(edsape, by = c("folio","nro","upm","estrato"))

edsah %>% filter(hs03_0033==1) %>% group_by(hs03_0034_T) %>% 
  count()


edsah %>% filter(hs03_0033==1, hs03_0034_T == 1) %>% 
  group_by(hs03_0035_V) %>% count()

edsah %>% filter(hs03_a_0041==1,hs03_a_0042_A==1) %>% 
  group_by(hs03_a_0043_V) %>% count()



edsag %>% filter(hs01_0003==2) %>% group_by(categaimc_m,categimc_m, tip_anemia_m) %>% count() %>% View()

edsag$tip_anemia_m


edsag$categimc_m
edsag$hs03_0033
edsape %>% get_label()

