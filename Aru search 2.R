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
edsaV %>% get_label()
edsa %>% filter((hs03_0033 == 1)) %>% get_labels()

edsa %>% pull(afilsegsal) %>% table()
  
edsa %>% get_label()
  
get_label(edsa)
#establecimiento o servicio de salud

edsa$hs03_0033
edsa$hs03_0035_A
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
# ---------------------------

## se estan filtrando a las personas que han tenido un problema de salud en los ultimos 3 meses, 
## ademas de que se esta excluyendo a las personas que reportan que no fueron a un centro de salud debido a que no lo
## consideraban grave o que no saben por que no fueron


## se excluyen a las personas menores a 16 anios
# personas que no buscaron algun centro de salud por que no lo consideraban grave
## solo a las personas que reportan haber persentado algun problema de salud
## Personas que no saben a donde los llevaron


edsa$hs03_0034_X_cod %>% table()

edsa$hs03_0034_X %>% table()



edsa %>% get_label()
edsa %>% get_label()
edsa %>% select(hs03_0034_A:hs03_0034_X_cod) %>% get_label()
  
edsa$hs03_0034_X_cod %>% table()

edsa %>% filter(hs03_0034_X_cod=="D64") %>% pull(hs03_0034_X_cod) %>% get_labels()

edsa %>% mutate(
  infecciosaP = labelled(case_when(
    hs03_0034_X_cod %in% c("A01", "A02", "A06", "B00", "B01", "B02", "B03", "B17") ~ 1,
    TRUE ~ 0 ),
    labels = c("Si" = 1,
               "No" = 0)
  ),
  SangreEnf = labelled(case_when(
    hs03_0034_X_cod %in% c("D36", "D48", "D64", "D75") ~ 1,
    TRUE ~ 0 ),
    labels = c("Si" = 1,
               "No" = 0)
  ),
  metabolismoE = labelled(case_when(
    hs03_0034_X_cod %in% c("E14", "E34", "E66", "E80") ~ 1,
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
  sistemaN = labelled(case_when(
    hs03_0034_X_cod %in% c("G40", "G43", "G44", "G51", "G64") ~ 1,
    TRUE ~ 0 ),
    labels = c("Si" = 1,
               "No" = 0)
  ),
  sistemaC = labelled(case_when(
    hs03_0034_X_cod %in% c("I00", "I10", "I52", "I70", "I72", "I82", "I84", "I86", "I89", "I95") ~ 1,
    TRUE ~ 0 ),
    labels = c("Si" = 1,
               "No" = 0)
  ),
  sistemaR = labelled(case_when(
    hs03_0034_X_cod %in% c("J00", "J06", "J11", "J18", "J30", "J34", "J40", "J45", "J98") ~ 1,
    TRUE ~ 0 ),
    labels = c("Si" = 1,
               "No" = 0)
  ),
  sistemaD = labelled(case_when(
    hs03_0034_X_cod %in% c("K36", "K38", "K46", "K65", "K76", "K82", "K92") ~ 1,
    TRUE ~ 0 ),
    labels = c("Si" = 1,
               "No" = 0)
  ),
  sistemaM = labelled(case_when(
    hs03_0034_X_cod %in% c("M10", "M13", "M19", "M25", "M51", "M79", "M85", "M86", "M99") ~ 1,
    TRUE ~ 0 ),
    labels = c("Si" = 1,
               "No" = 0)
  ),
  sistemaG = labelled(case_when(
    hs03_0034_X_cod %in% c("N39", "N42", "N50", "N64", "N94", "N95") ~ 1,
    TRUE ~ 0 ),
    labels = c("Si" = 1,
               "No" = 0)
  ),
  embarazo = labelled(case_when(
    hs03_0034_X_cod %in% c("O06", "O14", "O83") ~ 1,
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
  atencion = labelled(case_when(
    hs03_0034_X_cod %in% c("Z13", "Z21", "Z30", "Z34", "Z35", "Z39", "Z51", "Z88") ~ 1,
    TRUE ~ 0 ),
    labels = c("Si" = 1,
               "No" = 0)
  )
)
  
edsa %>% mutate(
  
  # Enfermedades infecciosas
  infecciosas = labelled(case_when(
    hs03_0034_X_cod %in% c("A01", "A02", "A06", "B00", "B01", "B02", "B03", "B17") ~ 1,
    TRUE ~ 0 ),
    labels = c("Si" = 1,
               "No" = 0)
  ),
  # enfermedades de la sangre, tumores o trastornos endocrinos/metabólicos
  Sangre_metabolico = labelled(case_when(
    hs03_0034_X_cod %in% c("D36", "D48", "D64", "D75",
                           "E14", "E34", "E66", "E80") ~ 1,
    TRUE ~ 0 ),
    labels = c("Si" = 1,
               "No" = 0)
  ),
  
  # enfermedades mentales
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
  )
)




bd1 = edsa %>% filter(hs01_0007>0 & hs01_0007<90) %>% group_by(folio, hs01_0007) %>% count() %>% 
  mutate(nro=hs01_0007)

aux2 = edsa %>% 
  filter(hs01_0004a >= 16, hs03_0033 == 1, !(hs03_0039_X_cod== 'Q'), is.na(hs03_0039_Z),
         hs01_0010!=3) %>% 
  mutate(formal = rowSums(across(hs03_0035_A:hs03_0035_Q) == 1, na.rm = TRUE),
         informal = rowSums(across(c(hs03_0035_R:hs03_0035_U, hs03_0035_X, hs03_0035_Z)) == 1,na.rm = TRUE),
         nofue = rowSums(across(hs03_0035_V) == 1,na.rm = TRUE)) %>% 
  left_join(edsaV, by = c("folio","upm","estrato","area","region","departamento")) %>% 
  left_join(bd1, by = c("folio","nro"))
##########################################################################################
##########################################################################################
aux2 %>% group_by(formal, informal , nofue) %>% count() %>% summarise()


aux2 = aux2 %>% 
  filter(!(formal==0 & informal==0 & nofue==0)) %>% 
  mutate(servicio = case_when(
    formal >= 1 ~ "Acceso a servicios de salud",
    informal >= 1 ~ "Atención alternativa",
    TRUE ~ "No accedió a atención"
  ),
  accesoS = case_when(
    formal >= 1 ~ "Acceso a servicios de salud",
    TRUE ~ "No accedió a atención"
  ),
  atenAltenativa = case_when(
    informal >= 1 ~ "Busco atencion alternativa",
    TRUE ~ "No busco atencion alternativa"
  ))
######################################################################################
designdesc = svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~factorexph,
  data = (aux2)
)

bd1 = as_survey(designdesc)

tabla <- bd1 %>% 
  group_by(accesoS) %>% 
  summarise(n = survey_total()) %>% 
  mutate(porc = n/sum(n)*100)


ggplot(tabla, aes(x = accesoS, y = porc, fill = accesoS)) +
  geom_col(width = 0.5, color = "white") +
  geom_text(aes(label = paste0(round(porc,1), "%")),
            vjust = -0.5,
            size = 4) +
  scale_fill_manual(values = c(
    "#D94801",  # naranja fuerte
    "#F16913",  # naranja medio
    "#FDAE6B"   # naranja claro
  )) +
  labs(
    x = "",
    y = "Porcentaje",
    title = "Acceso a servicios de salud",
    fill = "Servicios de salud"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 12),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  ylim(0,100)
######################################################################################



aux2 = aux2 %>% 
  filter(!(formal==0 & informal==0 & nofue==0)) %>% 
  mutate(
    
    # Variable 3 categorías
    servicio = labelled(
      case_when(
        formal >= 1 ~ 1,
        informal >= 1 ~ 2,
        TRUE ~ 3
      ),
      labels = c(
        "Acceso a servicios de salud" = 1,
        "Atención alternativa" = 2,
        "No accedió a atención" = 3
      )
    ),
    
    # Variable binaria acceso formal
    accesoS = labelled(
      case_when(
        formal >= 1 ~ 1,
        TRUE ~ 0
      ),
      labels = c(
        "No accedió a atención" = 0,
        "Acceso a servicios de salud" = 1
      )
    ),
    
    # Variable binaria atención alternativa
    atenAltenativa = labelled(
      case_when(
        informal >= 1 ~ 1,
        TRUE ~ 0
      ),
      labels = c(
        "No buscó atención alternativa" = 0,
        "Buscó atención alternativa" = 1
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
    )
  )


aux2 %>% group_by( servicio) %>% count()

aux2 %>% group_by(accesoS) %>% count()

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
   cuidador = as_label(cuidador),
   idiomaN = as_label(idiomaninez),
   thogar = as_label(tipohogar),
   educa = as_label(niv_ed_g),
   reg = as_label(region)
  )

aux2 %>% group_by(hs01_0010) %>% summarise(mean(accesoS))

aux2 %>% group_by(hs01_0010, servicio) %>% summarise(n = n()) %>% mutate(n = n/sum(n))

design = svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~factorexph,
  data = (aux3)
)

#modelo <- svy_vglm(servicio ~ area + atenAltenativa + sex + niv_edu + seguro,
#                   design = design,
#                   family = multinomial())

modelo <- svyglm(accesoS ~ area + edad + puebloind + sex + seguro + qriquez + idiomaN + 
                   # atenAltenativa +
                   infecciosa_f + Sangre_metabolico + cronica_f + mental_f + lesiones_f + sintomas_f + atencion_f,
                   design = design,
                   family = quasibinomial())

summary(modelo)
library(modelsummary)
library(pscl)
library(car)
library(pROC)




regTermTest(modelo, ~ area + edad + I(edad^2) + puebloind + sex + seguro + qriquez + idiomaN + niv_edu +
              # atenAltenativa +
              infecciosas + Sangre_metabolico + mental + sistemaN + NnormalR +lesiones +atencionE + 
              inf_A + lesion_A + mental_A + cronica_A)

aux3$pred <- predict(modelo, type = "response")
aux3$pred_bin <- ifelse(aux3$pred > 0.5, 1, 0)

table(aux3$pred_bin, aux3$accesoS)

vif(modelo)
pR2(modelo)

roc <- roc(aux3$accesoS, aux3$pred)
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

modelo_logit <- glm(accesoS ~ area + edad + puebloind + sex + seguro + qriquez + idiomaN + 
                      atenAltenativa +
                      infecciosa_f + Sangre_metabolico + cronica_f + mental_f + lesiones_f + 
                      sintomas_f + atencion_f, data = aux3, family = "binomial")

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
  acept_std ~  idiomaN + edad + Establecimiento + seguro + area +
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

modelo_lineal <- lm(acept_std ~  idiomaN + edad + Establecimiento + seguro + area +
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



