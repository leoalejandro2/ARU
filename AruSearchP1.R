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
library(modelsummary)
library(pscl)
library(car)
library(pROC)


eh23 = read_sav("database/EH/EH2023/EH2023_Discriminacion.sav")
eh18 = read_sav("database/EH/EH2018/EH2018_Discriminacion.sav")

# edsa <- read_sav("database/EDSA/EDSA2016/EDSA16_HOGAR.sav")



edsa = read_sav("database/EDSA/EDSA2023/EDSA2023_Hogar.sav")
edsaV = read_sav("database/EDSA/EDSA2023/EDSA2023_Vivienda.sav")
edsah = read_sav("database/EDSA/EDSA2023/EDSA2023_Hombre.sav")
edsam = read_sav("database/EDSA/EDSA2023/EDSA2023_Mujer.sav")
########################################################



edsah16 = read_sav("database/EDSA/EDSA2016/EDSA16_HOGAR.sav")
edsav16 = read_sav("database/EDSA/EDSA2016/EDSA16_HOMBRES.sav")
edsam16 = read_sav("database/EDSA/EDSA2016/EDSA16_MUJER_ANTECEDENTES.sav")

pe1 = read_sav("database/EH/EH2023/EH2023_Vivienda.sav")

eh23 = read_sav("database/EH/EH2023/EH2023_Persona.sav")
edsa$aestudio

eh23$ylab[is.na(eh23$ylab)] <- 1
ml1 = svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~factor,
  data = (eh23)
)

edsah$vs01_0148
edsam$ms08_0809

bd_eh = as_survey(ml1)

eh23$s04a_01
modelo <- svyglm(
  log(ylab) ~ s01a_03 + I(s01a_03^2) + as_label(s01a_02) + 
    as_label(area)  +aestudio  + as_label(s04a_01) ,
  design = ml1
)

model1 <- summary(modelo)
print(model1)


w      <- weights(modelo$survey.design, type = "sampling")
resids <- residuals(modelo, type = "working")
y      <- fitted(modelo) + resids

w_mean <- weighted.mean(y, w)

wRSS <- sum(w * resids^2)
wTSS <- sum(w * (y - w_mean)^2)

R2 <- 1 - wRSS / wTSS

n  <- length(y)
k  <- length(coef(modelo)) - 1
R2_adj <- 1 - (1 - R2) * (n - 1) / (n - k - 1)

cat(sprintf("Weighted R²:       %.4f\n", R2))
cat(sprintf("Weighted Adj. R²:  %.4f\n", R2_adj))


# Cargar librería necesaria para pruebas adicionales
library(survey)

# Prueba de Wald para el modelo global
regTermTest(modelo, ~ s01a_03 + s01a_02 + area + aestudio)

modelo$coefficients




# vs06_0635a
# vs06_0635b
# vs06_0635c
# vs06_0635X
# vs06_0635_1cod






edsav16 %>% group_by(vs06_0634) %>% summarise(n = sum(ponderadorv)) %>% mutate(n = n / sum(n))


edsam16 %>% group_by(ms05_0579) %>% summarise(n = sum(ponderadorm)) %>% mutate(n = n / sum(n))

edsam16 %>% group_by(ms05_0565, ms05_0570, ms05_0579) %>% 
  summarise(n = sum(ponderadorm)) %>% mutate(n = n / sum(n))

edsam16 %>% group_by(ms05_0565) %>% 
  summarise(n = sum(ponderadorm)) %>% mutate(n = n / sum(n))



edsam16 %>% nrow()


edsam16$ms05_0579




ax1 = edsa %>% mutate(
  seg = labelled(case_when(
    afilsegsal %in% c(1,2,3,5) ~ 1,
    afilsegsal == 6 ~ 0),labels = c(
      "Afiliado a algun seguro" = 1,
      "Sin afiliacion" = 0)),
  Atencion = labelled(case_when(
    hs03_0035_A==1 | hs03_0035_B==1 | hs03_0035_C==1 | hs03_0035_D==1 |
      hs03_0035_E==1 | hs03_0035_F==1 | hs03_0035_G==1 |
      hs03_0035_H==1 | hs03_0035_I==1 | hs03_0035_J==1 | hs03_0035_K==1 |
      hs03_0035_L==1 | hs03_0035_M==1 | hs03_0035_N==1 | hs03_0035_O==1 |
      hs03_0035_P==1 | hs03_0035_Q==1 ~ 1,
    TRUE ~ 0
  ),labels = c(
    "Atendido" = 1,
    "No Atendido" = 0
  )),
  AseguroSus = (case_when(
    hs03_0035_A==1 | hs03_0035_B==1 | hs03_0035_C==1 | hs03_0035_D==1 ~ "Centro de Salud"
  )),
  Ahospital23 = (case_when(
    hs03_0035_E==1 | hs03_0035_F==1 | hs03_0035_G==1 ~ "Hospital de 2 y 3 nivel"
  )), 
  AseguroCaja = (case_when(
    hs03_0035_H==1 | hs03_0035_I==1 | hs03_0035_J==1 | hs03_0035_K==1 |
      hs03_0035_L==1 | hs03_0035_M==1 | hs03_0035_N==1 | hs03_0035_O==1 ~ "Cajas de Salud"
  )),
  APrivado = (case_when(
    hs03_0035_P==1 | hs03_0035_Q==1 ~ "Privado"
  )),
  NoAcudio = (case_when(
    hs03_0035_R == 1 | hs03_0035_S == 1 | hs03_0035_T == 1 | hs03_0035_U == 1 | 
      hs03_0035_V == 1 | hs03_0035_X == 1 | hs03_0035_Z == 1 ~ "No acudio a establecimiento"
  )),
  atenCualquiera = case_when(
    (afilsegsal == 1 | afilsegsal == 2 | afilsegsal == 3 | afilsegsal == 5) & 
      (AseguroSus == "Centro de Salud" | Ahospital23== "Hospital de 2 y 3 nivel" | 
         AseguroCaja == "Cajas de Salud" | APrivado == "Privado") ~ "Cualquier proveedor",
    TRUE ~ "No atencion"),
  atenProvedor = case_when(
    ## SUS
    afilsegsal == 3 & (APrivado == "Privado")~ "Proveedor",
    afilsegsal == 2 & (AseguroCaja == "Cajas de Salud" | Ahospital23 == "Hospital de 2 y 3 nivel")~ "Proveedor",
    afilsegsal == 1 & (AseguroSus == "Centro de Salud" | Ahospital23== "Hospital de 2 y 3 nivel") ~ "Proveedor",
    TRUE ~ "No Proveedor")
) %>% left_join(edsaV, by = c("folio","upm","estrato","area","region","departamento")) 



ax1 %>% filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  group_by(afilsegsal,Atencion, atenCualquiera, atenProvedor) %>% 
  count() %>% View()


ax1 %>% filter(hs03_0033 == 1) %>% group_by(qriqueza) %>% count()


ax1$afilsegsal
ax1 %>% filter(hs03_0033 == 1) %>% group_by(Atencion) %>% count()
ax1 %>% filter(hs03_0033 == 1) %>% group_by(NoAcudio) %>% count()


ax1 %>% filter(hs03_0033 == 1) %>% group_by(afilsegsal) %>% count()

ax1 %>% filter(hs03_0033 == 1) %>% 
  group_by(AseguroSus, Ahospital23, AseguroCaja, APrivado,NoAcudio) %>% count() %>% View()


ax1 %>% filter(hs03_0033 == 1, afilsegsal != 4) %>%  group_by(seg, Atencion) %>% count()

ax1$afilsegsal





desg1 = svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~factorexph,
  data = (ax1)
)

bd_deg = as_survey(desg1)

## 

ax1$hs01_0004a
ax1$niv_ed_g %>% table()
ax1 %>% group_by(niv_ed_g, hs01_0004a) %>% count() %>% View()

ax1 %>% group_by(niv_ed_g) %>% count()

ax1$qriqueza 

### Seguro
bd_deg %>% filter(niv_ed_g != 99,afilsegsal != 4) %>% group_by(qriqueza,seg) %>% 
  summarise(n = survey_total() ) %>% mutate(prob = n/sum(n)) 

bd_deg %>% 
  filter(afilsegsal != 4) %>% 
  group_by(seg) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95) 
  ) %>% 
  mutate(
    prop = n / sum(n) * 100,
    prop_low = n_low / sum(n) * 100,
    prop_upp = n_upp / sum(n) * 100
  ) %>% View()

## atencion cualquiera
bd_deg %>% 
  filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  group_by(atenCualquiera) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95) 
  ) %>% 
  mutate(
    prop = n / sum(n) * 100,
    prop_low = n_low / sum(n) * 100,
    prop_upp = n_upp / sum(n) * 100
  ) %>% View()

## atencion proveedor afiliado
bd_deg %>% 
  filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  group_by( atenProvedor) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95) 
  ) %>% 
  mutate(
    prop = n / sum(n) * 100,
    prop_low = n_low / sum(n) * 100,
    prop_upp = n_upp / sum(n) * 100
  ) %>% View()


### problema de salud

bd_deg %>% filter(hs03_0033 %in% c(1,2)) %>% 
  group_by(hs03_0033) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95)
  ) %>% mutate(
    prob = n / sum(n) * 100,
    prob_low = n_low / sum(n) * 100,
    prob_upp = n_upp / sum(n) * 100
  ) %>% View()

bd_deg





