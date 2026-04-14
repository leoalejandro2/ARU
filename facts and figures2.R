library(haven)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(stringr)
library(tidyr)
library(tseries)
library(writexl)
library(tidyverse)
library(pheatmap)


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
edsa16hj = read_sav("database/EDSA/EDSA2016/EDSA16_MUJER_HISTORIA_NACIMIENTOS.sav")

edsah <- read_sav("database/EDSA/EDSA2023/EDSA2023_Hogar.sav")
edsape <- read_sav("database/EDSA/EDSA2023/EDSA2023_Peso_talla_hemo.sav")

edsav = read_sav("database/EDSA/EDSA2023/EDSA2023_Hombre.sav")
edsam = read_sav("database/EDSA/EDSA2023/EDSA2023_Mujer.sav")

edsadm = read_sav("database/EDSA/EDSA2023/EDSA2023_HistorialHijos.sav")

aux2 = edsadm %>% group_by(folio,nro) %>% count() %>% left_join(edsah, by = c("folio","nro"))

aux2 = aux2 %>% 
  mutate(gedad = case_when(
    hs01_0004a <= 20 ~ "0-20",
    hs01_0004a <= 30 ~ "21-30",
    hs01_0004a <= 40 ~ "31-40",
    TRUE ~ "41+"
  ))

ggplot(aux2, aes(x = factor(gedad), y = n)) +
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


###############################################################################################

eh24 = read_sav("database/EH/EH2024/EH2024_Discriminacion.sav")
eh23 = read_sav("database/EH/EH2023/EH2023_Discriminacion.sav")
eh22 = read_sav("database/EH/EH2022/EH2022_Discriminacion.sav")
eh21 = read_sav("database/EH/EH2021/EH2021_Discriminacion.sav")
eh19 = read_sav("database/EH/EH2019/EH2019_Discriminacion.sav")

eh24 %>% get_label()

eh24 %>% group_by(s09a_01k,s09a_01h) %>% summarise(n = sum(ponderador)) %>% pull(n) %>% sum()

eh24 %>% filter(s09a_02==2) %>% group_by(s09a_05_6) %>% summarise(n = sum(ponderador)) %>% 
  mutate(n = n/sum(n) *100)


(163+1.28+9.75)/12615 *100




aux1 = eh21 %>% mutate(dis = ifelse(rowSums(across(s10a_01a:s10a_01l, ~ . %in% c(1,3)),na.rm = TRUE)>0,"Si","No"))
aux2 = eh22 %>% mutate(dis = ifelse(rowSums(across(s09a_01a:s09a_01l, ~ . %in% c(1,3)),na.rm = TRUE)>0,"Si","No"))
aux3 = eh23 %>% mutate(dis = ifelse(rowSums(across(s09a_01a:s09a_01l, ~ . %in% c(1,3)),na.rm = TRUE)>0,"Si","No"))


aux4 = eh19 %>% mutate(dis = ifelse(rowSums(across(s12a_01a:s12a_01l, ~ . %in% c(1,3)),na.rm = TRUE)>0,"Si","No"))
aux5 = eh24 %>% mutate(dis = ifelse(rowSums(across(s09a_01a:s09a_01l, ~ . %in% c(1,3)),na.rm = TRUE)>0,"Si","No"))

aux4 %>% group_by(dis) %>% summarise(n = sum(Ponderador)) %>% mutate(n = n/sum(n)*100)

aux1 %>% group_by(dis) %>% summarise(n = sum(ponderador)) %>% mutate(n = n/sum(n)*100)

aux2 %>% group_by(dis) %>% summarise(n = sum(ponderador)) %>% mutate(n = n/sum(n)*100)

aux3 %>% group_by(dis) %>% summarise(n = sum(ponderador)) %>% mutate(n = n/sum(n)*100)

aux5 %>% group_by(dis) %>% summarise(n = sum(ponderador)) %>% mutate(n = n/sum(n)*100)


res <- bind_rows(
  aux4 %>% group_by(dis) %>% summarise(n = sum(Ponderador)) %>% mutate(base = "2019"),
  aux1 %>% group_by(dis) %>% summarise(n = sum(ponderador)) %>% mutate(base = "2021"),
  aux2 %>% group_by(dis) %>% summarise(n = sum(ponderador)) %>% mutate(base = "2022"),
  aux3 %>% group_by(dis) %>% summarise(n = sum(ponderador)) %>% mutate(base = "2023"),
  aux5 %>% group_by(dis) %>% summarise(n = sum(ponderador)) %>% mutate(base = "2024")
) %>% 
  group_by(base) %>% 
  mutate(porcentaje = n/sum(n)*100)

res

#write_xlsx(res,"tab.xlsx")
AUX5 %>% group_by()
aux5$s09a_02

res2 = aux5 %>% filter(dis=="Si") %>% group_by(s09a_02, s09a_04) %>% summarise(n = sum(ponderador))

#writes09a_02write_xlsx(res2, "tab.xlsx")
aux5$s09a_04

aux5$s09a_05_4
aux5$s09a_05_7
aux5$s09a_01a
aux5 %>% filter(s09a_02==2) %>% group_by(s09a_05_4) %>% summarise(n = sum(ponderador)) %>% 
  mutate(n = n / sum(n))

aux5 %>% group_by(s09a_01a) %>% summarise(n = sum(ponderador))

datos_matriz <- eh24 %>%
  select(s09a_01a:s09a_01l, ponderador) %>% # Ajusta 'factor_expansion' al nombre real de tu variable
  mutate(across(s09a_01a:s09a_01l, ~if_else(. %in% c(1, 3), 1, 0, missing = 0)))

# 2. Renombrar para que el gráfico sea legible
colnames(datos_matriz) <- c("Color Piel", "NPIOC", "Procedencia", "LGBTQ+", "Edad", 
                            "Sexo", "Idioma", "Vestimenta", "Discapacidad", 
                            "Religión", "Económica", "Otro", "peso")

# 3. Calcular la matriz de co-ocurrencia ponderada
# Extraemos solo las columnas de motivos
motivos <- datos_matriz %>% select(-peso) %>% as.matrix()
w <- datos_matriz$peso

# Calculamos la matriz: t(motivos) %*% (motivos * peso)
# Esto suma los pesos de cada intersección
matriz_ponderada <- t(motivos) %*% (motivos * w)


pheatmap(matriz_ponderada, 
         display_numbers = TRUE, 
         number_format = "%.0f",        # Muestra la población estimada en números enteros
         color = colorRampPalette(c("#ffffff", "#f28e2b", "#b07aa1"))(100), 
         main = "Interseccionalidad de la Discriminación (Datos Ponderados EH2024)",
         fontsize_number = 7,
         angle_col = 45)


eh24 %>% group_by(s09a_01a) %>% summarise(sum(ponderador))

# 1. Preparación de variables binarias (aplicando tu lógica: 1 y 3 son 'Si')
# Filtramos primero la sección que tiene datos (n = 15000)
datos_disc <- eh24 %>%
  filter(!is.na(s09a_01a)) %>% 
  select(s09a_01a:s09a_01l, ponderador) %>% # Usa el nombre de tu variable ponderadora
  mutate(across(s09a_01a:s09a_01l, ~if_else(. %in% c(1, 3), 1, 0, missing = 0)))

# 2. Renombrar columnas para claridad
colnames(datos_disc) <- c("Color Piel", "NPIOC", "Procedencia", "LGBTQ+", "Edad", 
                          "Sexo", "Idioma", "Vestimenta", "Discapacidad", 
                          "Religión", "Económica", "Otro", "w")

# 3. Calcular Matriz de Co-ocurrencia Ponderada
m_motivos <- as.matrix(datos_disc %>% select(-w))
pesos <- datos_disc$w

# Suma de pesos donde ocurren ambos motivos
matriz_co <- t(m_motivos) %*% (m_motivos * pesos)

pheatmap(matriz_co, 
         display_numbers = TRUE, 
         number_format = "%.0f", 
         color = colorRampPalette(c("white", "#deebf7", "#3182bd"))(100),
         main = "Heatmap: Co-ocurrencia Ponderada (n=15,000)",
         fontsize_number = 8)


# Normalizamos la matriz por la diagonal (total de cada motivo)
matriz_porcentaje <- sweep(matriz_co, 1, diag(matriz_co), "/") * 100

pheatmap(matriz_porcentaje, 
         display_numbers = TRUE, 
         number_format = "%.1f%%", 
         color = colorRampPalette(c("white", "#fee0d2", "#de2d26"))(100),
         main = "Interseccionalidad: % de solapamiento entre motivos")



