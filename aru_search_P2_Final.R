rm(list = ls())

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
library(marginaleffects)

edsa  = read_sav("database/EDSA/EDSA2023/EDSA2023_Hogar.sav")
edsaV = read_sav("database/EDSA/EDSA2023/EDSA2023_Vivienda.sav")
edsah = read_sav("database/EDSA/EDSA2023/EDSA2023_Hombre.sav")
edsam = read_sav("database/EDSA/EDSA2023/EDSA2023_Mujer.sav")

##########################################################################################
# 1. TRANSFORMACIÓN DE VARIABLES EN LA BASE COMPLETA (SIN FILTRAR AÚN)
##########################################################################################

bd_completa = edsa %>% 
  left_join(edsaV, by = c("folio","upm","estrato","area","region","departamento")) %>% 
  mutate(
    seguro = labelled(case_when(
      afilsegsal %in% c(1,2,3,5) ~ 1,
      afilsegsal == 6 ~ 0,
      TRUE ~ NA_real_), labels = c(
        "Afiliado a algun seguro" = 1,
        "Sin afiliacion" = 0)),
    
    atencion = labelled(case_when( 
      (hs03_0033 == 1) & (hs03_0035_A==1 | hs03_0035_B==1 | hs03_0035_C==1 | hs03_0035_D==1 |
                            hs03_0035_E==1 | hs03_0035_F==1 | hs03_0035_G==1 | hs03_0035_H==1 | 
                            hs03_0035_I==1 | hs03_0035_J==1 | hs03_0035_K==1 | hs03_0035_L==1 | 
                            hs03_0035_M==1 | hs03_0035_N==1 | hs03_0035_O==1 | hs03_0035_P==1 | 
                            hs03_0035_Q==1) ~ 1,
      (hs03_0033 == 1) & (hs03_0035_R==1 | hs03_0035_S==1 | hs03_0035_T==1 | hs03_0035_U==1 | 
                            hs03_0035_V==1 | hs03_0035_X==1 | hs03_0035_Z==1) ~ 0,
      TRUE ~ NA_real_), labels = c(
        "Atendido" = 1,
        "No Atendido" = 0)),
    
    aseguro_sus = (case_when(
      (hs03_0033 == 1) & (hs03_0035_A==1 | hs03_0035_B==1 | hs03_0035_C==1 | hs03_0035_D==1) ~ "Centro de Salud"
    )),
    ahospital23 = (case_when(
      (hs03_0033 == 1) & (hs03_0035_E==1 | hs03_0035_F==1 | hs03_0035_G==1 )~ "Hospital de 2 y 3 nivel"
    )), 
    aseguro_caja = (case_when(
      (hs03_0033 == 1) & (hs03_0035_H==1 | hs03_0035_I==1 | hs03_0035_J==1 | hs03_0035_K==1 |
                            hs03_0035_L==1 | hs03_0035_M==1 | hs03_0035_N==1 | hs03_0035_O==1) ~ "Cajas de Salud"
    )),
    aprivado = (case_when(
      (hs03_0033 == 1) & (hs03_0035_P==1 | hs03_0035_Q==1)~ "Privado"
    )),
    no_acudio = (case_when(
      (hs03_0033 == 1) & (hs03_0035_R == 1 | hs03_0035_S == 1 | hs03_0035_T == 1 | hs03_0035_U == 1 | 
                            hs03_0035_V == 1 | hs03_0035_X == 1 | hs03_0035_Z == 1) ~ "No acudio a establecimiento"
    )),
    aten_cualquiera = case_when(
      (hs03_0033 == 1) & ((afilsegsal == 1 | afilsegsal == 2 | afilsegsal == 3 | afilsegsal == 5) & 
                            (aseguro_sus == "Centro de Salud" | ahospital23== "Hospital de 2 y 3 nivel" | 
                               aseguro_caja == "Cajas de Salud" | aprivado == "Privado")) ~ "Cualquier proveedor",
      (hs03_0033 == 1) ~ "No atencion",
      TRUE ~ NA_character_),
    aten_cualquiera111 = case_when(
      (hs03_0033 == 1) & ((afilsegsal == 1 | afilsegsal == 2 | afilsegsal == 3 | afilsegsal == 5) & 
                            (aseguro_sus == "Centro de Salud" | ahospital23== "Hospital de 2 y 3 nivel" | 
                               aseguro_caja == "Cajas de Salud" | aprivado == "Privado")) ~ 1,
      (hs03_0033 == 1) ~ 0,
      TRUE ~ NA_real_),
    aten_provedor = case_when(
      (hs03_0033 == 1) & (afilsegsal == 3 & (aprivado == "Privado"))~ "Proveedor",
      (hs03_0033 == 1) & (afilsegsal == 2 & (aseguro_caja == "Cajas de Salud" | ahospital23 == "Hospital de 2 y 3 nivel"))~ "Proveedor",
      (hs03_0033 == 1) & (afilsegsal == 1 & (aseguro_sus == "Centro de Salud" | ahospital23== "Hospital de 2 y 3 nivel")) ~ "Proveedor",
      (hs03_0033 == 1) ~ "No Proveedor",
      TRUE ~ NA_character_),
    
    # Agrupaciones de tipo de proveedor
    SectorPublico = rowSums(across(hs03_0035_A:hs03_0035_O) == 1, na.rm = TRUE),
    SectorPrivado = rowSums(across(hs03_0035_P:hs03_0035_Q) == 1, na.rm = TRUE),
    atencionAlt   = rowSums(across(c(hs03_0035_R:hs03_0035_U, hs03_0035_X)) == 1, na.rm = TRUE),
    noFue         = rowSums(across(hs03_0035_V) == 1, na.rm = TRUE),
    noSabe        = rowSums(across(hs03_0035_Z) == 1, na.rm = TRUE),
    
    # Acceso y servicios
    servicio = labelled(
      case_when(
        (SectorPublico >= 1 | SectorPrivado >= 1) & noFue == 0 ~ 1,
        atencionAlt >= 1  ~ 2,
        noFue == 1 ~ 3,
        TRUE ~ NA_real_
      ),
      labels = c("Acceso a establecimiento de Salud" = 1, "Acceso a atencion alternativa" = 2, "No accedió a atención" = 3)
    ),
    
    accesoS = labelled(
      case_when(
        (SectorPublico >= 1 | SectorPrivado >= 1) & noFue == 0 ~ 1,
        atencionAlt >= 1 | noFue >= 1 ~ 0,
        TRUE ~ NA_real_
      ),
      labels = c("No accedió a atención" = 0, "Acceso a establecimiento de Salud" = 1)
    ),
    
    atenAltenativa = labelled(
      case_when(
        atencionAlt >= 1 ~ 1,
        TRUE ~ 0
      ),
      labels = c("No busco atencion alternativa" = 0, "Busco atencion alternativa" = 1)
    ),
    
    tipo_salud = case_when(
      hs03_0034_D == 1 | hs03_0034_E == 1 ~ "Lesiones",
      hs03_0034_A == 1 | hs03_0034_B == 1 | hs03_0034_C == 1 |
        hs03_0034_K == 1 | hs03_0034_L == 1 | hs03_0034_M == 1 |
        hs03_0034_N == 1 | hs03_0034_P == 1 | hs03_0034_Q == 1 |
        hs03_0034_S == 1 ~ "Infecciosas",
      hs03_0034_G == 1 | hs03_0034_H == 1 | hs03_0034_I == 1 |
        hs03_0034_J == 1 | hs03_0034_R == 1 | hs03_0034_T == 1 ~ "Cronicas",
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
    
    infecciosas = labelled(case_when(
      hs03_0034_X_cod %in% c("A01", "A02", "A06", "B00", "B01", "B02", "B03", "B17") ~ 1,
      TRUE ~ 0 ), labels = c("Si" = 1, "No" = 0)),
    
    Sangre_metabolico = labelled(case_when(
      hs03_0034_X_cod %in% c("D36", "D48", "D64", "D75", "E14", "E34", "E66", "E80") ~ 1,
      TRUE ~ 0 ), labels = c("Si" = 1, "No" = 0)),
    
    mental = labelled(case_when(
      hs03_0034_X_cod %in% c("F03","F20" ,"F48", "F50") ~ 1,
      TRUE ~ 0 ), labels = c("Si" = 1, "No" = 0)),
    
    sistemaN = labelled(case_when(
      hs03_0034_X_cod %in% c("G40", "G43", "G44", "G51", "G64",
                             "I00", "I10", "I52", "I70", "I72", "I82", "I84", "I86", "I89", "I95",
                             "J00", "J06", "J11", "J18", "J30", "J34", "J40", "J45", "J98",
                             "K36", "K38", "K46", "K65", "K76", "K82", "K92",
                             "M10", "M13", "M19", "M25", "M51", "M79", "M85", "M86", "M99",
                             "N39", "N42", "N50", "N64", "N94", "N95") ~ 1,
      TRUE ~ 0 ), labels = c("Si" = 1, "No" = 0)),
    
    NnormalR = labelled(case_when(
      hs03_0034_X_cod %in% c("Q02", "R04", "R05", "R07", "R11", "R41", "R45", "R50", "R52", "R58", "R73") ~ 1,
      TRUE ~ 0 ), labels = c("Si" = 1, "No" = 0)),
    
    lesiones = labelled(case_when(
      hs03_0034_X_cod %in% c("T07", "T30", "W54", "W57", "W64", "Y98") ~ 1,
      TRUE ~ 0 ), labels = c("Si" = 1, "No" = 0)),
    
    atencionE = labelled(case_when(
      hs03_0034_X_cod %in% c("O06", "O14", "O83",
                             "Z13", "Z21", "Z30", "Z34", "Z35", "Z39", "Z51", "Z88") ~ 1,
      TRUE ~ 0 ), labels = c("Si" = 1, "No" = 0)),
    
    infecciosa_f = labelled(
      ifelse(hs03_0034_A==1 | hs03_0034_B==1 | hs03_0034_C==1 |
               hs03_0034_K==1 | hs03_0034_L==1 | hs03_0034_M==1 |
               hs03_0034_N==1 | hs03_0034_P==1 | hs03_0034_Q==1 |
               hs03_0034_S==1 | infecciosas==1, 1, 0), labels = c("No"=0,"Si"=1)),
    
    cronica_f = labelled(
      ifelse(hs03_0034_G==1 | hs03_0034_H==1 | hs03_0034_I==1 |
               hs03_0034_J==1 | hs03_0034_R==1 |
               Sangre_metabolico==1 | sistemaN==1, 1, 0), labels = c("No"=0,"Si"=1)),
    
    mental_f = labelled(
      ifelse(hs03_0034_T==1 | mental==1, 1, 0), labels = c("No"=0,"Si"=1)),
    
    lesiones_f = labelled(
      ifelse(hs03_0034_D==1 | hs03_0034_E==1 | lesiones==1, 1, 0), labels = c("No"=0,"Si"=1)),
    
    sintomas_f = labelled(
      ifelse(hs03_0034_F==1 | hs03_0034_O==1 | NnormalR==1, 1, 0), labels = c("No"=0,"Si"=1)),
    
    atencion_f = labelled(
      ifelse(atencionE==1, 1, 0), labels = c("No"=0,"Si"=1)),
    
    tradicional2022 = labelled(
      ifelse(hs03_0030==1, 1, 0), labels = c("No"=0,"Si"=1)),
    
    estable2022 = labelled(
      ifelse(hs03_0031==1, 1, 0), labels = c("No"=0,"Si"=1)),
    
    # Sociodemográficas
    area_lab = as_label(area),
    atenAltenativa_lab = as_label(atenAltenativa),
    sex = as_label(hs01_0003),
    niv_edu = ifelse(niv_ed_g==99 , NA, niv_ed_g),
    niv_edu = as_label(labelled(niv_edu, labels = c("Ninguno" = 0, "Primaria" = 1, "Secundaria" = 2, "Superior" = 3))),
    seguro = as_label(labelled(case_when(
      afilsegsal == 1 ~ 1,
      afilsegsal == 2 ~ 2,
      afilsegsal == 3 ~ 3,
      afilsegsal %in% c(5,6) ~ 4,
      TRUE ~ NA_real_
    ), labels = c("SUS" = 1, "Cajas de Salud" = 2, "Seguro Privado" = 3, "Sin seguro" = 4))),
    qriquez = as_label(qriqueza),
    puebloind = as_label(hs01_0010),
    edad = hs01_0004a,
    redad3 = case_when(
      hs01_0004a >= 1  & hs01_0004a < 2  ~ "<= 1",
      hs01_0004a >= 2  & hs01_0004a < 15 ~ "2-14",
      hs01_0004a >= 15 & hs01_0004a < 25 ~ "15-24",
      hs01_0004a >= 25 & hs01_0004a < 45 ~ "25-44",
      hs01_0004a >= 45 & hs01_0004a < 65 ~ "45-64",
      TRUE ~ ">= 65"
    ),
    idiomaN = as_label(idiomaninez),
    thogar = as_label(tipohogar),
    educa = as_label(niv_ed_g),
    reg = as_label(region),
    naturalista2022 = as_label(tradicional2022),
    csalud2022 = as_label(estable2022),
    calidad_vida = as_label(labelled(case_when(
      cviv == -1 ~ -1,
      cviv == 0 ~ 0,
      cviv == 1 ~ 1,
      TRUE ~ NA_real_
    ), labels = c("CALIDAD BAJA" = -1, "CALIDAD MEDIA" = 0, "CALIDAD ALTA" = 1)))
  )

bd_completa$seguro1 = relevel(as.factor(bd_completa$seguro), ref = "Sin seguro")

##########################################################################################
# 2. DEFINICIÓN DEL DISEÑO MUESTRAL COMPLETO
##########################################################################################

design_completo <- svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~factorexph,
  nest = TRUE,
  data = bd_completa
)

##########################################################################################
# 3. FILTRADO POR SUBPOBLACIÓN (DOMINIO) CON subset()
##########################################################################################
bd_completa$area_lab
# '1. Urbana'  '2. Rural' 
design_subpop <- subset(
  design_completo,
  hs03_0033 == 1 &                                  # Tuvo problema de salud
    area_lab == '2. Rural'  &                          # Área rural
    !(SectorPublico == 0 & SectorPrivado == 0 & atencionAlt == 0 & noFue == 0) & # Filtro de casos inconsistentes
    !is.na(accesoS) & !is.na(sex) & !is.na(redad3) & !is.na(seguro1) &
    !is.na(csalud2022) & !is.na(naturalista2022) & !is.na(qriquez) &
    !is.na(infecciosa_f) & !is.na(Sangre_metabolico) & !is.na(cronica_f) &
    !is.na(mental_f) & !is.na(lesiones_f) & !is.na(sintomas_f) & !is.na(atencion_f)
)

##########################################################################################
# 4. MODELO LOGÍSTICO Y EFECTOS MARGINALES EN LA SUBPOBLACIÓN
##########################################################################################

modelo <- svyglm( 
  accesoS ~ sex + redad3 + seguro1 + csalud2022 + naturalista2022 +
    qriquez + infecciosa_f + Sangre_metabolico + cronica_f + mental_f +
    lesiones_f + sintomas_f + atencion_f,
  design = design_subpop,
  family = quasibinomial()
)

summary(modelo)

# Efectos Marginales Promedio (AME)
ame <- avg_slopes(modelo)

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
  )  %>% View()

##########################################################################################
# 5. EVALUACIÓN Y DIAGNÓSTICO DEL MODELO
##########################################################################################

# Extraer el subconjunto de datos filtrado para métricas de clasificación (ROC/AUC)
df_subpop <- model.frame(modelo)
df_subpop$pred <- predict(modelo, type = "response")
df_subpop$pred_bin <- ifelse(df_subpop$pred > 0.5, 1, 0)

# Matriz de confusión
table(Predicho = df_subpop$pred_bin, Observado = df_subpop$accesoS)

# Multicolinealidad (VIF)
vif(modelo)

# Curva ROC y AUC
roc_obj <- roc(df_subpop$accesoS, df_subpop$pred)
plot(roc_obj)
auc(roc_obj)

guardar_curva_roc <- function(roc_obj, titulo, auc_val, color_linea, nombre_archivo) {
  
  # Generar el gráfico con ggplot2 vía pROC
  p <- ggroc(roc_obj, legacy.axes = TRUE, size = 1.2, color = color_linea) +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                 color = "grey50", linetype = "dashed", size = 0.8) +
    annotate("text", x = 0.65, y = 0.25, 
             label = paste0("AUC = ", sprintf("%.4f", auc_val)), 
             size = 4.5, fontface = "bold", color = "black") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02)) +
    labs(
      title = titulo,
      x = "1 - Especificidad (Tasa de Falsos Positivos)",
      y = "Sensibilidad (Tasa de Verdaderos Positivos)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
      axis.title = element_text(face = "bold", size = 11),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8)
    )
  
  # Guardar en PDF vectorial (Ideal para LaTeX)
  ggsave(filename = paste0(nombre_archivo, ".pdf"), plot = p, width = 6, height = 5, device = "pdf")
  # Guardar en PNG 300 DPI
  ggsave(filename = paste0(nombre_archivo, ".png"), plot = p, width = 6, height = 5, dpi = 300)
  
  return(p)
}

guardar_curva_roc(roc_obj, "Curva ROC - Área Rural", auc(roc_obj), "#1F77B4", "Graficos/ROC_Rural")

# Pseudo R2 McFadden ajustado al diseño muestral
modelo_null <- svyglm(
  accesoS ~ 1,
  design = design_subpop,
  family = quasibinomial()
)

pseudo_r2 <- 1 - (logLik(modelo) / logLik(modelo_null))
pseudo_r2

pseudo_r2_adj <- 1 - ((logLik(modelo) - length(coef(modelo))) / logLik(modelo_null))
pseudo_r2_adj
