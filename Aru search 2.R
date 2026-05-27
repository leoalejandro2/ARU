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

##################################################################3#############################
# -------------------------------
# Disponibilidad
# ---------------------------

## se estan filtrando a las personas que han tenido un problema de salud en los ultimos 3 meses, 
## ademas de que se esta excluyendo a las personas que reportan que no fueron a un centro de salud debido a que no lo
## consideraban grave o que no saben por que no fueron


## se excluyen a las personas menores a 16 anios
# personas que no buscaron algun centro de salud por que no lo consideraban grave
## solo a las personas que reportan haber persentado algun problema de salud
## Personas que no saben a donde los llevaron



##########################################################################################
bd1 = edsa %>% filter(hs01_0007>0 & hs01_0007<90) %>% group_by(folio, hs01_0007) %>% count() %>% 
  mutate(nro=hs01_0007)

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
  atenCualquiera111 = case_when(
    (afilsegsal == 1 | afilsegsal == 2 | afilsegsal == 3 | afilsegsal == 5) & 
      (AseguroSus == "Centro de Salud" | Ahospital23== "Hospital de 2 y 3 nivel" | 
         AseguroCaja == "Cajas de Salud" | APrivado == "Privado") ~ 1,
    TRUE ~ 0),
  atenProvedor = case_when(
    ## SUS
    afilsegsal == 3 & (APrivado == "Privado")~ "Proveedor",
    afilsegsal == 2 & (AseguroCaja == "Cajas de Salud" | Ahospital23 == "Hospital de 2 y 3 nivel")~ "Proveedor",
    afilsegsal == 1 & (AseguroSus == "Centro de Salud" | Ahospital23== "Hospital de 2 y 3 nivel") ~ "Proveedor",
    TRUE ~ "No Proveedor")
)
aux2 = ax1 %>% filter(hs01_0004a >= 0,  #edad
                hs03_0033 == 1, # problema de salud en los ultimos 3 meses 1=si
                # is.na(edsa$hs03_0039_Z)  # se excluyen a los que no saben donde fueron llevados
                ) %>% 
  mutate(
    SectorPublico = rowSums(across(hs03_0035_A:hs03_0035_O) == 1, na.rm = TRUE),
    SectorPrivado = rowSums(across(hs03_0035_P:hs03_0035_Q) == 1, na.rm = TRUE),
    atencionAlt = rowSums(across(c(hs03_0035_R:hs03_0035_U, hs03_0035_X)) == 1, na.rm = TRUE),
    noFue = rowSums(across(hs03_0035_V) == 1, na.rm = TRUE),
    noSabe = rowSums(across(hs03_0035_Z) == 1, na.rm = TRUE)
  ) %>% 
  left_join(edsaV, by = c("folio","upm","estrato","area","region","departamento")) %>% 
  left_join(bd1, by = c("folio","nro"))

aux2 = aux2 %>% 
  filter(!(SectorPublico ==0 & SectorPrivado ==0 & atencionAlt ==0 & noFue ==0)) %>% 
  mutate(servicio = case_when(
    SectorPublico >= 1 | SectorPrivado >= 1 ~ "Acceso a establecimiento de Salud",
    atencionAlt >= 1 ~ "Acceso a atencion alternativa",
    TRUE ~ "No accedió a atención"
  ),
  accesoS = case_when(
    SectorPublico >= 1 | SectorPrivado >= 1 ~ "Acceso a establecimiento de Salud",
    TRUE ~ "No accedió a atención"
  ),
  atenAltenativa = case_when(
    atencionAlt >= 1  ~ "Busco atencion alternativa",
    TRUE ~ "No busco atencion alternativa"
  ))
######################################################################################
######################################################################################
aux2 = aux2 %>% 
  filter(!(SectorPublico ==0 & SectorPrivado ==0 & atencionAlt ==0 & noFue ==0)) %>%
  mutate(
    
    # Variable 3 categorías
    servicio = labelled(
      case_when(
        SectorPublico >= 1 | SectorPrivado >= 1 ~ 1,
        atencionAlt >= 1 ~ 2,
        TRUE ~ 3
      ),
      labels = c(
        "Acceso a establecimiento de Salud" = 1,
        "Acceso a atencion alternativa" = 2,
        "No accedió a atención" = 3
      )
    ),
    
    # Variable binaria acceso formal
    accesoS = labelled(
      case_when(
        SectorPublico >= 1 | SectorPrivado >= 1 ~ 1,
        TRUE ~ 0
      ),
      labels = c(
        "No accedió a atención" = 0,
        "Acceso a establecimiento de Salud" = 1
      )
    ),
    
    # Variable binaria atención alternativa
    atenAltenativa = labelled(
      case_when(
        atencionAlt >= 1 ~ 1,
        TRUE ~ 0
      ),
      labels = c(
        "No busco atencion alternativa" = 0,
        "Busco atencion alternativa" = 1
      )
    ),
    cuidador = labelled(
      case_when(
        n>0 ~ 1,
        TRUE ~ 0
      ), labels = c(
        "cuidador" = 1,
        "no cuidador" = 0
      ) 
    ),
    tipo_salud = case_when(
      
      # 1. Lesiones (alta prioridad)
      hs03_0034_D == 1 | hs03_0034_E == 1 ~ "Lesiones",
      
      # 2. Enfermedades infecciosas
      hs03_0034_A == 1 | hs03_0034_B == 1 | hs03_0034_C == 1 |
        hs03_0034_K == 1 | hs03_0034_L == 1 | hs03_0034_M == 1 |
        hs03_0034_N == 1 | hs03_0034_P == 1 | hs03_0034_Q == 1 |
        hs03_0034_S == 1 ~ "Infecciosas",
      
      # 3. Crónicas / no transmisibles
      hs03_0034_G == 1 | hs03_0034_H == 1 | hs03_0034_I == 1 |
        hs03_0034_J == 1 | hs03_0034_R == 1 | hs03_0034_T == 1 ~ "Cronicas",
      
      # 4. Otros
      hs03_0034_F == 1 | hs03_0034_O == 1 | hs03_0034_X == 1 ~ "Otros",
      
      TRUE ~ NA_character_
    ),
    inf_A = ifelse(hs03_0034_A==1 | hs03_0034_B==1 | hs03_0034_C==1 |
                     hs03_0034_K==1 | hs03_0034_L==1 | hs03_0034_M==1 |
                     hs03_0034_N==1 | hs03_0034_P==1 | hs03_0034_Q==1 |
                     hs03_0034_S==1, 1, 0),
    
    lesion_A = ifelse(hs03_0034_D==1 | hs03_0034_E==1, 1, 0),
    
    mental_A = ifelse(hs03_0034_T==1, 1, 0),
    
    cronica_A = ifelse(hs03_0034_G==1 | hs03_0034_H==1 | hs03_0034_I==1 |
                         hs03_0034_J==1 | hs03_0034_R==1, 1, 0),
    icd = substr(hs03_0034_X_cod, 1, 1),
    
    inf_X = ifelse(icd %in% c("A","B"), 1, 0),
    lesion_X = ifelse(icd %in% c("S","T","V","W","Y"), 1, 0),
    mental_X = ifelse(icd == "F", 1, 0),
    cronica_X = ifelse(icd %in% c("I","E","G","K","N","H"), 1, 0),
    
    infecciosa = ifelse(inf_A==1 | inf_X==1, 1, 0),
    lesion     = ifelse(lesion_A==1 | lesion_X==1, 1, 0),
    mental     = ifelse(mental_A==1 | mental_X==1, 1, 0),
    cronica    = ifelse(cronica_A==1 | cronica_X==1, 1, 0),
    
    
    # Enfermedades infecciosas
    infecciosas = labelled(case_when(
      hs03_0034_X_cod %in% c("A01", "A02", "A06", "B00", "B01", "B02", "B03", "B17") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0) #
    ),
    # enfermedades de la sangre, tumores o trastornos endocrinos/metabólicos
    Sangre_metabolico = labelled(case_when(
      hs03_0034_X_cod %in% c("D36", "D48", "D64", "D75",
                             "E14", "E34", "E66", "E80") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0)
    ),
    mental = labelled(case_when(
      hs03_0034_X_cod %in% c("F03","F20" ,"F48", "F50") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0)
    ),
    ## problema de salud correspondiente a enfermedades crónicas de distintos sistemas del organismo
    sistemaN = labelled(case_when(
      hs03_0034_X_cod %in% c("G40", "G43", "G44", "G51", "G64",
                             "I00", "I10", "I52", "I70", "I72", "I82", "I84", "I86", "I89", "I95",
                             "J00", "J06", "J11", "J18", "J30", "J34", "J40", "J45", "J98",
                             "K36", "K38", "K46", "K65", "K76", "K82", "K92",
                             "M10", "M13", "M19", "M25", "M51", "M79", "M85", "M86", "M99",
                             "N39", "N42", "N50", "N64", "N94", "N95") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0)
    ),
    NnormalR = labelled(case_when(
      hs03_0034_X_cod %in% c("Q02", "R04", "R05", "R07", "R11", "R41", "R45", "R50", "R52", "R58", "R73") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0)
    ),
    lesiones = labelled(case_when(
      hs03_0034_X_cod %in% c("T07", "T30", "W54", "W57", "W64", "Y98") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0)
    ),
    atencionE = labelled(case_when(
      hs03_0034_X_cod %in% c("O06", "O14", "O83",
                             "Z13", "Z21", "Z30", "Z34", "Z35", "Z39", "Z51", "Z88") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0)
    ),
    
    # INFECCIOSAS (A–T + X) infecciones (encuesta + ICD)
    infecciosa_f = labelled(
      ifelse(
        hs03_0034_A==1 | hs03_0034_B==1 | hs03_0034_C==1 |
          hs03_0034_K==1 | hs03_0034_L==1 | hs03_0034_M==1 |
          hs03_0034_N==1 | hs03_0034_P==1 | hs03_0034_Q==1 |
          hs03_0034_S==1 | infecciosas==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    ),# infecciosa_f + Sangre_metabolico + cronica_f + mental_f + lesiones_f + sintomas_f + atencion_f
    
    
    # CRÓNICAS ORGÁNICAS  enfermedades crónicas reales
    cronica_f = labelled(
      ifelse(
        hs03_0034_G==1 | hs03_0034_H==1 | hs03_0034_I==1 |
          hs03_0034_J==1 | hs03_0034_R==1 |
          Sangre_metabolico==1 | sistemaN==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    ),
    
    # SALUD MENTAL   salud mental
    mental_f = labelled(
      ifelse(
        hs03_0034_T==1 | mental==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    ),
    
    # LESIONES / VIOLENCIA   accidentes/violencia
    lesiones_f = labelled(
      ifelse(
        hs03_0034_D==1 | hs03_0034_E==1 | lesiones==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    ),
    
    # SÍNTOMAS / NO ESPECÍFICOS     problemas vagos/no diagnosticados
    sintomas_f = labelled(
      ifelse(
        hs03_0034_F==1 | hs03_0034_O==1 | NnormalR==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    ),
    
    # USO DEL SISTEMA / EVENTOS
    atencion_f = labelled(
      ifelse(atencionE==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    ),
    
    # atencion en 2022
    tradicional2022 = labelled(
      ifelse(hs03_0030==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    ),
    estable2022 = labelled(
      ifelse(hs03_0031==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    )
  )


aux3 = aux2 %>% 
  mutate(
    area = as_label(area),
    atenAltenativa = as_label(atenAltenativa),
    sex = as_label(hs01_0003),
    niv_edu = ifelse(niv_ed_g==99,NA,niv_ed_g),
    niv_edu = as_label(labelled(niv_edu, labels = c(
      "Ninguno" = 0,
      "Primaria" = 1,
      "Secundaria" = 2,
      "Superior" = 3
    ))),
    seguro = as_label(labelled(case_when(
      afilsegsal == 1 ~ 1,
      afilsegsal == 2 ~ 2,
      afilsegsal == 3 ~ 3,
      afilsegsal %in% c(4,5,6) ~ 4
    ),labels = c(
      "SUS" = 1,
      "Cajas de Salud" = 2,
      "Seguro Privado" = 3,
      "Sin seguro/ No sabe" =4
   ))),
   qriquez = as_label(qriqueza),
   puebloind = as_label(hs01_0010),
   edad = hs01_0004a,
   
   redad2 = case_when(
     hs01_0004a <= 1 ~ "<= 1",
     hs01_0004a <= 14 ~ "1-14",
     hs01_0004a <= 24 ~ "15-24",
     hs01_0004a <= 44 ~ "25-44",
     hs01_0004a <= 64 ~ "45-64",
     TRUE ~ ">= 65"
   ),
   redad = case_when(
     hs01_0004a < 6 ~ "<= 5",
     hs01_0004a < 18 ~ "6-17",
     hs01_0004a < 30 ~ "18-29",
     hs01_0004a < 45 ~ "30-44",
     hs01_0004a < 60 ~ "45-59",
     TRUE ~ ">= 60"
   ),
   cuidador = as_label(cuidador),
   idiomaN = as_label(idiomaninez),
   thogar = as_label(tipohogar),
   educa = as_label(niv_ed_g),
   reg = as_label(region),
   naturalista2022 = as_label(tradicional2022),
   csalud2022 = as_label(estable2022),
   enfCronica = as_label(hs03_a_0041)
  )

aux3$seguro1 = relevel(as.factor(aux3$seguro), ref = "Sin seguro/ No sabe") 


aux4 = aux3 %>% filter(area=='2. Rural')
aux3 %>% filter(area=='2. Rural')


design = svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~factorexph,
  data = (aux4)
)


#modelo <- svy_vglm(servicio ~ area + atenAltenativa + sex + niv_edu + seguro,
#                   design = design,
#                   family = multinomial())

## 1. Urbana 2. Rural
aux3$seguro %>% table()
  


modelo <- svyglm(atenCualquiera111 ~  qriquez + redad2 + sex + 
                   seguro1 + naturalista2022 + csalud2022 + atenAltenativa +
                   infecciosa_f + Sangre_metabolico + cronica_f + mental_f +
                   lesiones_f + sintomas_f + atencion_f,
                   design = design,
                   family = quasibinomial())


modelo <- svyglm(
  accesoS ~ qriquez + redad2 + sex + 
    seguro1 + naturalista2022 + csalud2022 + atenAltenativa +
    infecciosa_f + Sangre_metabolico + cronica_f + mental_f +
    lesiones_f + sintomas_f + atencion_f,
  
  design = design,
  family = quasibinomial()
)
summary(modelo)

#qriquez2. SEGUNDO QUINTIL                  0.3163
#qriquez3. QUINTIL INTERMEDIO               0.1084
#qriquez4. CUARTO QUINTIL                   0.2859
#qriquez5. QUINTIL SUPERIOR                 0.2581


#qriquez2. SEGUNDO QUINTIL                 0.46164
#qriquez3. QUINTIL INTERMEDIO              0.33552
#qriquez4. CUARTO QUINTIL                  1.34290
#qriquez5. QUINTIL SUPERIOR                0.53396


library(marginaleffects)
library(tidyverse)

# Calcular AME con marginaleffects (funciona con svyglm)
ame <- avg_slopes(modelo)

ame |>
  as_tibble() |>
  select(term, contrast, estimate, std.error, statistic, p.value, conf.low, conf.high) 


# Ver todas las filas
ame |>
  as_tibble() |>
  select(term, contrast, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) |>
  print(n = 30) %>% View()



aux4$pred <- predict(modelo, type = "response")
aux4$pred_bin <- ifelse(aux4$pred > 0.5, 1, 0)

table(aux4$pred_bin, aux4$accesoS)

vif(modelo)
pR2(modelo)

roc <- roc(aux4$accesoS, aux4$pred)
plot(roc)
auc(roc)

modelo_null <- svyglm(
  accesoS ~ 1,
  design = design,
  family = quasibinomial()
)

# Pseudo R2 McFadden
pseudo_r2 <- 1 - (logLik(modelo) / logLik(modelo_null))

pseudo_r2

pseudo_r2_adj <- 1 - ((logLik(modelo) - length(coef(modelo))) / logLik(modelo_null))

pseudo_r2_adj

#############################################################################################################

modelo_logit <- glm(accesoS ~ area + edad + puebloind + sex + seguro + qriquez  + 
                      naturalista2022 + csalud2022 + atenAltenativa +enfCronica +
                      infecciosa_f + Sangre_metabolico + cronica_f + mental_f + 
                      lesiones_f + sintomas_f + atencion_f, 
                    data = aux3, family = "binomial")

# Ver el resumen del modelo
summary(modelo_logit)

library(lmtest)
lrtest(modelo_logit)

library(pscl)
pR2(modelo_logit)["McFadden"]


library(ResourceSelection)
hoslem.test(modelo_logit$y, fitted(modelo_logit))

predicciones <- ifelse(predict(modelo_logit, type = "response") > 0.5, 1, 0)
tabla_confusion <- table(Predicho = predicciones, Real = aux3$accesoS)
print(tabla_confusion)

# Precisión global
sum(diag(tabla_confusion)) / sum(tabla_confusion)


library(pROC)
roc_obj <- roc(aux3$accesoS, fitted(modelo_logit))
plot(roc_obj, main = "Curva ROC")
auc(roc_obj)

library(car)
vif(modelo_logit) # Valores > 5 o 10 sugieren problemas




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

############################################################################################
edsa %>% get_label()

aux3 = edsa %>% mutate(subPublico = rowSums(across(hs03_0035_A:hs03_0035_G) == 1, na.rm = TRUE)) %>% 
  filter(subPublico>=1, hs01_0004a >= 16, hs01_0010!=3) %>% 
  mutate(
    acept = rowSums(across(hs03_0037_A:hs03_0037_I) == 1, na.rm = TRUE),
    acept_std = (acept - min(acept, na.rm = TRUE)) /
      (max(acept, na.rm = TRUE) - min(acept, na.rm = TRUE)),
    acept_std = 1 - acept_std,
    Establecimiento = as_label(labelled(case_when(
      
      hs03_0035_E == 1 | hs03_0035_F == 1 | hs03_0035_G == 1 ~ 1,
      hs03_0035_P == 1 | hs03_0035_Q == 1 ~ 4,
      hs03_0035_H == 1 | hs03_0035_I == 1 | hs03_0035_J == 1 | hs03_0035_K == 1 |
      hs03_0035_L == 1 | hs03_0035_M == 1 | hs03_0035_N == 1 | hs03_0035_O == 1 ~ 2,
      hs03_0035_A == 1 | hs03_0035_B == 1 | hs03_0035_C == 1 | hs03_0035_D == 1 ~ 1
    ),labels = c(
      "Primer nivel público"=1,"Cajas de salud"=2,
      "Hospital público"=3,"Privado"=4)
    )),
    PrimerNivel = ifelse(hs03_0035_A == 1 |  # puesto de salud
                           hs03_0035_B == 1 |  # centro ambulatorio
                           hs03_0035_C == 1 |  # centro con internación
                           hs03_0035_D == 1,1),
    HospitalSTEsp = ifelse(hs03_0035_E == 1 |  # segundo nivel
                             hs03_0035_F == 1 |  # tercer nivel
                             hs03_0035_G == 1,1,0),
    CajasS = ifelse(hs03_0035_H == 1 |
                      hs03_0035_I == 1 |
                      hs03_0035_J == 1 |
                      hs03_0035_K == 1 |
                      hs03_0035_L == 1 |
                      hs03_0035_M == 1 |
                      hs03_0035_N == 1 |
                      hs03_0035_O == 1,1,0),
    Privado = ifelse(hs03_0035_P == 1 | hs03_0035_Q == 1,1,0),
    PrimerNivel = !is.na(PrimerNivel),
    HospitalSTEsp = !is.na(HospitalSTEsp),
    CajasS = ! is.na(CajasS),
    Privado = !is.na(Privado),
    edad = hs01_0004a,
    seguro = as_label(labelled(case_when(
      afilsegsal == 1 ~ 1,
      afilsegsal == 2 ~ 2,
      afilsegsal == 3 ~ 3,
      afilsegsal %in% c(4,5,6) ~ 4
    ),labels = c(
      "SUS" = 1,
      "Cajas de Salud" = 2,
      "Seguro Privado" = 3,
      "Sin seguro/ No sabe" =4
    ))),
    area = as_label(area),
    puebloind = as_label(hs01_0010),
    grupo_indigena = ifelse(hs01_0010 == 1, 1, 0),
    idiomaN = as_label(idiomaninez),
    genero = as_label(hs01_0003),
    tipo_salud = case_when(
      
      # 1. Lesiones (alta prioridad)
      hs03_0034_D == 1 | hs03_0034_E == 1 ~ "Lesiones",
      
      # 2. Enfermedades infecciosas
      hs03_0034_A == 1 | hs03_0034_B == 1 | hs03_0034_C == 1 |
        hs03_0034_K == 1 | hs03_0034_L == 1 | hs03_0034_M == 1 |
        hs03_0034_N == 1 | hs03_0034_P == 1 | hs03_0034_Q == 1 |
        hs03_0034_S == 1 ~ "Infecciosas",
      
      # 3. Crónicas / no transmisibles
      hs03_0034_G == 1 | hs03_0034_H == 1 | hs03_0034_I == 1 |
        hs03_0034_J == 1 | hs03_0034_R == 1 | hs03_0034_T == 1 ~ "Cronicas",
      
      # 4. Otros
      hs03_0034_F == 1 | hs03_0034_O == 1 | hs03_0034_X == 1 ~ "Otros",
      
      TRUE ~ NA_character_
    ),
    inf_A = ifelse(hs03_0034_A==1 | hs03_0034_B==1 | hs03_0034_C==1 |
                     hs03_0034_K==1 | hs03_0034_L==1 | hs03_0034_M==1 |
                     hs03_0034_N==1 | hs03_0034_P==1 | hs03_0034_Q==1 |
                     hs03_0034_S==1, 1, 0),
    
    lesion_A = ifelse(hs03_0034_D==1 | hs03_0034_E==1, 1, 0),
    
    mental_A = ifelse(hs03_0034_T==1, 1, 0),
    
    cronica_A = ifelse(hs03_0034_G==1 | hs03_0034_H==1 | hs03_0034_I==1 |
                         hs03_0034_J==1 | hs03_0034_R==1, 1, 0),
    icd = substr(hs03_0034_X_cod, 1, 1),
    
    inf_X = ifelse(icd %in% c("A","B"), 1, 0),
    lesion_X = ifelse(icd %in% c("S","T","V","W","Y"), 1, 0),
    mental_X = ifelse(icd == "F", 1, 0),
    cronica_X = ifelse(icd %in% c("I","E","G","K","N","H"), 1, 0),
    
    infecciosa = ifelse(inf_A==1 | inf_X==1, 1, 0),
    lesion     = ifelse(lesion_A==1 | lesion_X==1, 1, 0),
    mental     = ifelse(mental_A==1 | mental_X==1, 1, 0),
    cronica    = ifelse(cronica_A==1 | cronica_X==1, 1, 0),
    
    # Enfermedades infecciosas
    infecciosas = labelled(case_when(
      hs03_0034_X_cod %in% c("A01", "A02", "A06", "B00", "B01", "B02", "B03", "B17") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0) #
    ),
    # enfermedades de la sangre, tumores o trastornos endocrinos/metabólicos
    Sangre_metabolico = labelled(case_when(
      hs03_0034_X_cod %in% c("D36", "D48", "D64", "D75",
                             "E14", "E34", "E66", "E80") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0)
    ),
    mental = labelled(case_when(
      hs03_0034_X_cod %in% c("F03","F20" ,"F48", "F50") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0)
    ),
    ## problema de salud correspondiente a enfermedades crónicas de distintos sistemas del organismo
    sistemaN = labelled(case_when(
      hs03_0034_X_cod %in% c("G40", "G43", "G44", "G51", "G64",
                             "I00", "I10", "I52", "I70", "I72", "I82", "I84", "I86", "I89", "I95",
                             "J00", "J06", "J11", "J18", "J30", "J34", "J40", "J45", "J98",
                             "K36", "K38", "K46", "K65", "K76", "K82", "K92",
                             "M10", "M13", "M19", "M25", "M51", "M79", "M85", "M86", "M99",
                             "N39", "N42", "N50", "N64", "N94", "N95") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0)
    ),
    NnormalR = labelled(case_when(
      hs03_0034_X_cod %in% c("Q02", "R04", "R05", "R07", "R11", "R41", "R45", "R50", "R52", "R58", "R73") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0)
    ),
    lesiones = labelled(case_when(
      hs03_0034_X_cod %in% c("T07", "T30", "W54", "W57", "W64", "Y98") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0)
    ),
    atencionE = labelled(case_when(
      hs03_0034_X_cod %in% c("O06", "O14", "O83",
                             "Z13", "Z21", "Z30", "Z34", "Z35", "Z39", "Z51", "Z88") ~ 1,
      TRUE ~ 0 ),
      labels = c("Si" = 1,
                 "No" = 0)
    ),
    
    # INFECCIOSAS (A–T + X)
    infecciosa_f = labelled(
      ifelse(
        hs03_0034_A==1 | hs03_0034_B==1 | hs03_0034_C==1 |
          hs03_0034_K==1 | hs03_0034_L==1 | hs03_0034_M==1 |
          hs03_0034_N==1 | hs03_0034_P==1 | hs03_0034_Q==1 |
          hs03_0034_S==1 | infecciosas==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    ),# infecciosa_f + Sangre_metabolico + cronica_f + mental_f + lesiones_f + sintomas_f + atencion_f
    
    # CRÓNICAS ORGÁNICAS
    cronica_f = labelled(
      ifelse(
        hs03_0034_G==1 | hs03_0034_H==1 | hs03_0034_I==1 |
          hs03_0034_J==1 | hs03_0034_R==1 |
          Sangre_metabolico==1 | sistemaN==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    ),
    
    # SALUD MENTAL
    mental_f = labelled(
      ifelse(
        hs03_0034_T==1 | mental==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    ),
    
    # LESIONES / VIOLENCIA
    lesiones_f = labelled(
      ifelse(
        hs03_0034_D==1 | hs03_0034_E==1 | lesiones==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    ),
    
    # SÍNTOMAS / NO ESPECÍFICOS
    sintomas_f = labelled(
      ifelse(
        hs03_0034_F==1 | hs03_0034_O==1 | NnormalR==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    ),
    
    # USO DEL SISTEMA / EVENTOS
    atencion_f = labelled(
      ifelse(atencionE==1, 1, 0),
      labels = c("No"=0,"Si"=1)
    )
    
  )

vars <- aux3 %>% 
  select(hs03_0034_A:hs03_0034_T, hs03_0034_X) %>% 
  mutate(across(everything(), ~ ifelse(. == 1, 1, 0)))


pca <- prcomp(vars, scale. = TRUE)

# Extraer 5 componentes
scores <- as.data.frame(pca$x[,1:5])

names(scores) <- paste0("comp", 1:5)

aux3 <- bind_cols(aux3, scores)

aux3 <- aux3 %>% 
  rowwise() %>% 
  mutate(
    categoria_acp = which.max(c_across(comp1:comp5))
  ) %>% 
  ungroup()

aux3 <- aux3 %>% 
  mutate(
    categoria_acp = factor(categoria_acp,
                           levels = 1:5,
                           labels = c(
                             "Problemas de salud comunes",
                             "Problemas respiratorios y sensoriales",
                             "Lesiones y violencia",
                             "Enfermedades transmisibles",
                             "Salud mental"
                           )
    )
  )


aux3$categoria_acp

aux3 %>% count(categoria_acp)

round(pca$rotation[,1:5], 2)

ggplot(aux3, aes(x = acept_std)) +
  geom_histogram(
    fill = "#F16913", 
    color = "white", 
    bins = 9,
    alpha = 0.9
  ) +
  labs(
    title = "Distribución del índice de aceptabilidad",
    x = "Índice de aceptabilidad",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
  
design2 = svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~factorexph,
  data = (aux3)
)

modelo <- svyglm(
  acept_std ~  idiomaN + edad + Establecimiento +
    infecciosa_f + Sangre_metabolico + cronica_f + mental_f + lesiones_f + sintomas_f + atencion_f,

  design = design2,
  family = gaussian()
)
summary(modelo)  


modelo$survey.design


modelo_null <- svyglm(
  acept_std ~ 1,
  design = design2,
  family = gaussian()
)

# Pseudo R2 McFadden

modelo_lineal <- lm(acept_std ~  idiomaN + edad + seguro + area +
                      infecciosa_f + Sangre_metabolico + cronica_f + mental_f + lesiones_f + 
                      sintomas_f + atencion_f, data = aux3)

# Resumen completo
summary(modelo_lineal)


plot(modelo_lineal, which = 1)

# Gráfico Q-Q
plot(modelo_lineal, which = 2)

# Prueba de Shapiro-Wilk (p > 0.05 sugiere normalidad)
shapiro.test(residuals(modelo_lineal))

library(car)
durbinWatsonTest(modelo_lineal) # p > 0.05 sugiere que no hay autocorrelación

vif(modelo_lineal)


res_oaxaca <- oaxaca(acept_std ~ idiomaN + edad + Establecimiento + seguro + area +
                       infecciosa_f + Sangre_metabolico + cronica_f + mental_f + 
                       lesiones_f + sintomas_f + atencion_f | grupo_indigena, 
                     data = aux3, R = 100) # R = 100 para bootstrap
summary(res_oaxaca)


plot(res_oaxaca, components = c("endowments", "coefficients"))



