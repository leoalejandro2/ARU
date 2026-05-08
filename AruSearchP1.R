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
  ))
) 


ax1$afilsegsal
ax1 %>% filter(hs03_0033 == 1) %>% group_by(Atencion) %>% count()
ax1 %>% filter(hs03_0033 == 1) %>% group_by(NoAcudio) %>% count()


ax1 %>% filter(hs03_0033 == 1) %>% group_by(afilsegsal) %>% count()

ax1 %>% filter(hs03_0033 == 1) %>% 
  group_by(AseguroSus, Ahospital23, AseguroCaja, APrivado,NoAcudio) %>% count() %>% View()


ax1 %>% filter(hs03_0033 == 1, afilsegsal != 4) %>%  group_by(seg, Atencion) %>% count()


(482 + 658 + 1759 + 10818 - 482 - 658 - 1759) / (482 + 658 + 1759 + 10818)



desg1 = svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~factorexph,
  data = (ax1)
)

bd_deg = as_survey(desg1)



ax1$hs01_0003
bd_deg %>% filter(afilsegsal != 4) %>% group_by(area,seg) %>% 
  summarise(n = survey_total() ) %>% mutate(prob = n/sum(n)) 

qnorm(0.975)


bd_deg %>% 
  filter(afilsegsal != 4) %>% 
  group_by(hs01_0003, seg) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95) 
  ) %>% 
  mutate(
    # Para los intervalos de la proporción (proporción = n / sum(n))
    prop = n / sum(n),
    prop_low = n_low / sum(n),
    prop_upp = n_upp / sum(n)
  )



bd_deg %>% filter(hs03_0033 %in% c(1,2)) %>% 
  group_by(area, hs03_0033) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95)
  ) %>% mutate(
    prob = n / sum(n),
    prob_low = n_low / sum(n),
    prob_upp = n_upp / sum(n)
  )

bd_deg

bd_deg %>% filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  group_by(area, seg, Atencion) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95)
  ) %>% 
  group_by(area) %>% 
  mutate(
    prob = n / sum(n),
    prob_low = n_low / sum(n),
    prob_upp = n_upp / sum(n)
  )

bd_deg