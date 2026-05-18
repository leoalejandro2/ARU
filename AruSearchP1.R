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

#
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

eh23V = read_sav("database/EH/EH2023/EH2023_Vivienda.sav")

bd2 = eh23 %>% mutate(genero1 = as_label(s01a_02),
                      area1= as_label(area),
                      aestudio1 = aestudio,
                      relacion1 = case_when(
                        s01a_05 == 1 ~ "Jefe/a de hogar",
                        s01a_05 == 2 ~ "Esposo/a o conviviente",
                        s01a_05 == 3 ~ "Hijo o entenado",
                        s01a_05 == 4 ~ "Yerno o nuera",
                        s01a_05 == 5 ~ "Hermano o cuñado",
                        s01a_05 == 6 ~ "Padres",
                        s01a_05 == 7 ~ "Suegros",
                        s01a_05 == 8 ~ "Nietos",
                        s01a_05 == 9 ~ "Otro Pariente",
                        s01a_05 == 10 ~ "No Pariente",
                        s01a_05 == 11 ~ "Empleado del hogar",
                        s01a_05 == 12 ~ "Pariente del empleado"
                      ),
                      redad = case_when(
                        s01a_03 < 6 ~ "<= 5",
                        s01a_03 < 18 ~ "6-17",
                        s01a_03 < 30 ~ "18-29",
                        s01a_03 < 45 ~ "30-44",
                        s01a_03 < 60 ~ "45-59",
                        TRUE ~ ">= 60"
                        )
                      )

bd2$aestudio1[is.na(eh23$aestudio)] = 0
bd2$ylab[is.na(eh23$ylab)] <- 1

bd3 = edsa %>% mutate(genero2 = as_label(hs01_0003),
                      area2 = as_label(area),
                      aestudio2 = aestudio,
                      relacion2 = case_when(
                        hs01_0005 == 1 ~ "Jefe/a de hogar",
                        hs01_0005 == 2 ~ "Esposo/a o conviviente",
                        (hs01_0005 == 3 | hs01_0005 == 4) ~ "Hijo o entenado",
                        hs01_0005 == 5 ~ "Yerno o nuera",
                        hs01_0005 == 6 ~ "Hermano o cuñado",
                        hs01_0005 == 7 ~ "Padres",
                        hs01_0005 == 8 ~ "Suegros",
                        hs01_0005 == 9 ~ "Nietos",
                        hs01_0005 == 10 ~ "Otro Pariente",
                        hs01_0005 == 11 ~ "No Pariente",
                        hs01_0005 == 12 ~ "Empleado del hogar",
                        hs01_0005 == 13 ~ "Pariente del empleado"
                      ),
                      redad = case_when(
                        hs01_0004a < 6 ~ "<= 5",
                        hs01_0004a < 18 ~ "6-17",
                        hs01_0004a < 30 ~ "18-29",
                        hs01_0004a < 45 ~ "30-44",
                        hs01_0004a < 60 ~ "45-59",
                        TRUE ~ ">= 60"
                        )
                      )

bd3$aestudio2[is.na(bd3$aestudio2)] = 0

ml1 = svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~factor,
  data = (bd2)
)

bd_eh = as_survey(ml1)

modelo <- svyglm(
  log(ylab) ~ genero1 + area1  + aestudio1  + relacion1 + redad,
  design = ml1
)

model1 <- summary(modelo)
print(model1)


# Build a prediction-ready version of bd3 with variable names matching the model
bd3_pred <- bd3 |>
  mutate(
    genero1   = genero2,
    area1     = factor(
      case_when(
        area2 == "1. Urbana" ~ "Urbana",
        area2 == "2. Rural"  ~ "Rural"
      ),
      levels = levels(bd2$area1)
    ),
    aestudio1 = aestudio2,
    relacion1 = factor(relacion2, levels = levels(factor(bd2$relacion1))),
    redad     = factor(redad, levels = levels(factor(bd2$redad)))
  )

# Estimate log(ylab) using model coefficients
bd3$log_ylab_est <- predict(modelo, newdata = bd3_pred, type = "response")

cat("Summary of estimated log(ylab) for bd3:\n")
summary(bd3$log_ylab_est)



cat("\nRows with NA predictions:", sum(is.na(bd3$log_ylab_est)), "\n")





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

############################################################################################################
# vs06_0635a
# vs06_0635b
# vs06_0635c
# vs06_0635X
# vs06_0635_1cod


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


ax1 <- ax1 |>
  left_join(
    bd3 |> select(folio, nro, log_ylab_est),
    by = c("folio", "nro")
  )

cat("log_ylab_est added to ax1\n")
summary(ax1$log_ylab_est)



desg1 = svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~factorexph,
  data = (ax1)
)

bd_deg = as_survey(desg1)

## 

### Seguro
bd_deg %>% filter(niv_ed_g != 99,afilsegsal != 4) %>% group_by(qriqueza,seg) %>% 
  summarise(n = survey_total() ) %>% mutate(prob = n/sum(n)) 

res1 = bd_deg %>% 
  filter(afilsegsal != 4) %>% 
  group_by(seg) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95) 
  ) %>% 
  mutate(
    prop = n / sum(n) * 100,
    prop_low = n_low / sum(n) * 100,
    prop_upp = n_upp / sum(n) * 100
  ) %>% select(seg ,prop, prop_low, prop_upp)

## atencion cualquiera
res2 = bd_deg %>% 
  filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  group_by(atenCualquiera) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95) 
  ) %>% 
  mutate(
    prop = n / sum(n) * 100,
    prop_low = n_low / sum(n) * 100,
    prop_upp = n_upp / sum(n) * 100
  ) %>% select(atenCualquiera, prop, prop_low, prop_upp)

## atencion proveedor afiliado
res3 = bd_deg %>% 
  filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  group_by(atenProvedor) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95) 
  ) %>% 
  mutate(
    prop = n / sum(n) * 100,
    prop_low = n_low / sum(n) * 100,
    prop_upp = n_upp / sum(n) * 100
  ) %>% select(atenProvedor, prop, prop_low, prop_upp)


### problema de salud

res4 = bd_deg %>% filter(hs03_0033 %in% c(1,2)) %>% 
  group_by(hs03_0033) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95)
  ) %>% mutate(
    prop = n / sum(n) * 100,
    prop_low = n_low / sum(n) * 100,
    prop_upp = n_upp / sum(n) * 100
  ) %>% select(hs03_0033, prop, prop_low, prop_upp)

library(tidyverse)

# Combine all results into one data frame
combined <- bind_rows(
  res1 |> rename(category = seg) |> mutate(
    category = as.character(category),
    group = "Afiliación a seguro"
  ),
  res2 |> rename(category = atenCualquiera) |> mutate(
    group = "Atención cualquier proveedor"
  ),
  res3 |> rename(category = atenProvedor) |> mutate(
    group = "Atención proveedor"
  ),
  res4 |> rename(category = hs03_0033) |> mutate(
    category = as.character(category),
    group = "hs03_0033"
  )
)

combined

combined <- bind_rows(
  res1 |>
    mutate(category = sjlabelled::as_label(seg)) |>
    select(category, prop, prop_low, prop_upp) |>
    mutate(group = "Afiliación\na seguro"),
  res2 |>
    rename(category = atenCualquiera) |>
    select(category, prop, prop_low, prop_upp) |>
    mutate(group = "Atención\ncualquier proveedor"),
  res3 |>
    rename(category = atenProvedor) |>
    select(category, prop, prop_low, prop_upp) |>
    mutate(group = "Atención\nproveedor"),
  res4 |>
    mutate(category = sjlabelled::as_label(hs03_0033)) |>
    select(category, prop, prop_low, prop_upp) |>
    mutate(group = "Problema de\nsalud (3 meses)")
)

combined

combined <- combined |>
  mutate(tipo = if_else(
    category %in% c("Afiliado a algun seguro", "Cualquier proveedor", "Proveedor", "1. SI"),
    "Sí", "No"
  ))
###############################################

ggplot(combined, aes(y = group, x = prop, fill = tipo)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(
    aes(label = paste0(round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3.5, color = "white", fontface = "bold"
  ) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = c("Sí" = "#8B4500", "No" = "#FF7F24")) +
  labs(x = "Proporción (%)", y = NULL, fill = NULL) +
  theme_classic(base_size = 12) +
  theme(
    axis.title        = element_text(size = 11, face = "bold"),
    axis.text         = element_text(face = "bold", color = "black"),
    legend.position   = "right",
    legend.justification = "center",
    legend.key.width  = unit(0.6, "cm"),
    legend.key.height = unit(0.4, "cm"),
    plot.margin       = margin(10, 15, 10, 10)
  )

#########################################################################
#######################################################################
library(rineq)

ci_data1 = ax1 %>% filter(afilsegsal != 4) %>% 
  select(log_ylab_est, factorexph, seg)

as_numeric(ci_data1$log_ylab_est)

result_ci = ci(
  ineqvar = as.numeric(ci_data1$log_ylab_est),
  weights  = ci_data1$factorexph,
  outcome  = as.numeric(ci_data1$seg)
)


library(ggplot2)
library(dplyr)

# Build concentration curve data from result_ci
cc_data <- tibble(
  rank    = result_ci$fractional_rank,
  outcome = result_ci$outcome,
  weight  = result_ci$ineqvar  # not used directly
) |>
  arrange(rank) |>
  mutate(
    cum_pop     = rank,                                          # cumulative population share
    cum_outcome = cumsum(outcome) / sum(outcome)                 # cumulative outcome share
  )

# Add the (0,0) origin point
cc_data <- bind_rows(tibble(cum_pop = 0, cum_outcome = 0), cc_data)

ci_val <- round(result_ci$concentration_index, 4)

ggplot(cc_data, aes(x = cum_pop, y = cum_outcome)) +
  geom_line(color = "#2c7bb6", linewidth = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  annotate(
    "text", x = 0.75, y = 0.2,
    label = paste0("CI = ", ci_val),
    size = 4, hjust = 0
  ) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0, 0)) +
  labs(
    x = "Cumulative share of population\n(ranked by income, poorest to richest)",
    y = "Cumulative share of outcome"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(size = 11),
    plot.margin = margin(10, 15, 10, 10)
  )




result_e <- ci(
  ineqvar = as.numeric(ci_data1$log_ylab_est),
  weights  = ci_data1$factorexph,
  outcome  = as.numeric(ci_data1$seg),
  type     = "CIc"
)

result_w <- ci(
  ineqvar = as.numeric(ci_data1$log_ylab_est),
  weights  = ci_data1$factorexph,
  outcome  = as.numeric(ci_data1$seg),
  type     = "CIw"
)

summary(result_ci)
summary(result_e)
summary(result_w)

sqrt(result_w$variance)

###########################################################3

ci_data2 = ax1 %>% filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  select(log_ylab_est, factorexph, atenCualquiera) %>% 
  mutate(
    atencualquer = ifelse(atenCualquiera == "Cualquier proveedor", 1, 0))
ci_data2  

result2_ci = ci(
  ineqvar = as.numeric(ci_data2$log_ylab_est),
  weights  = ci_data2$factorexph,
  outcome  = as.numeric(ci_data2$atencualquer)
)

result2_e <- ci(
  ineqvar = as.numeric(ci_data2$log_ylab_est),
  weights  = ci_data2$factorexph,
  outcome  = as.numeric(ci_data2$atencualquer),
  type     = "CIc"
)

result2_w <- ci(
  ineqvar = as.numeric(ci_data2$log_ylab_est),
  weights  = ci_data2$factorexph,
  outcome  = as.numeric(ci_data2$atencualquer),
  type     = "CIw"
)

summary(result2_ci)
summary(result2_e)
summary(result2_w)

######################################################3
ax1$area


ci_data3 = ax1 %>% filter(area==1) %>% filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  select(log_ylab_est, factorexph, atenProvedor) %>% 
  mutate(
    atenprov = ifelse(atenProvedor == "Proveedor", 1, 0))
ci_data3

result3_ci = ci(
  ineqvar = as.numeric(ci_data3$log_ylab_est),
  weights  = ci_data3$factorexph,
  outcome  = as.numeric(ci_data3$atenprov)
)

result3_e <- ci(
  ineqvar = as.numeric(ci_data3$log_ylab_est),
  weights  = ci_data3$factorexph,
  outcome  = as.numeric(ci_data3$atenprov),
  type     = "CIc"
)

result3_w <- ci(
  ineqvar = as.numeric(ci_data3$log_ylab_est),
  weights  = ci_data3$factorexph,
  outcome  = as.numeric(ci_data3$atenprov),
  type     = "CIw"
)

summary(result3_ci)
summary(result3_e)
summary(result3_w)




####################################################
####################################################
##########################

#######################################################################
library(rineq)
ax1$qriqueza
ci_data1 = ax1 %>% filter(afilsegsal != 4) %>% 
  select(log_ylab_est,qriqueza, factorexph, seg)

as_numeric(ci_data1$qriqueza)

result4_ci = ci(
  ineqvar = as.numeric(ci_data1$qriqueza),
  weights  = ci_data1$factorexph,
  outcome  = as.numeric(ci_data1$seg)
)


result4_e <- ci(
  ineqvar = as.numeric(ci_data1$qriqueza),
  weights  = ci_data1$factorexph,
  outcome  = as.numeric(ci_data1$seg),
  type     = "CIc"
)

result4_w <- ci(
  ineqvar = as.numeric(ci_data1$qriqueza),
  weights  = ci_data1$factorexph,
  outcome  = as.numeric(ci_data1$seg),
  type     = "CIw"
)


# Build concentration curve data from result_ci
cc_data <- tibble(
  rank    = result4_ci$fractional_rank,
  outcome = result4_ci$outcome,
  weight  = result4_ci$ineqvar  # not used directly
) |>
  arrange(rank) |>
  mutate(
    cum_pop     = rank,                                          # cumulative population share
    cum_outcome = cumsum(outcome) / sum(outcome)                 # cumulative outcome share
  )

# Add the (0,0) origin point
cc_data <- bind_rows(tibble(cum_pop = 0, cum_outcome = 0), cc_data)

ci_val <- round(result_ci$concentration_index, 4)

ggplot(cc_data, aes(x = cum_pop, y = cum_outcome)) +
  # Línea de concentración con grosor óptimo y suavizado
  geom_line(color = "#2c7bb6", linewidth = 1.1, linejoin = "round", linecap = "round") + 
  # Línea de igualdad
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  
  # Texto de la Curva en negrita
  annotate("text", x = 0.30, y = 0.75, label = "Curva de concentración",
           color = "#2c7bb6", size = 4, hjust = 0, fontface = "bold") +
  
  # Texto de la Línea de igualdad en negrita
  annotate("text", x = 0.55, y = 0.45, label = "Línea de igualdad",
           color = "gray40", size = 4, hjust = 0, fontface = "bold") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), expand = c(0, 0)) +
  labs(
    x = "Proporción acumulada de la población\n(ordenada por quintiles de riqueza, del más pobre al más rico)",
    y = "Proporción acumulada \n del acceso potencial"
  ) +
  theme_classic(base_size = 12) +
  theme(
    # Títulos de los ejes en negrita
    axis.title = element_text(size = 11, face = "bold"),
    # Texto de los ejes (los números/porcentajes) en negrita
    axis.text = element_text(face = "bold", color = "black"),
    plot.margin = margin(10, 15, 10, 10)
  )


summary(result4_ci)
summary(result4_e)
summary(result4_w)

ax1$hs01_0003

ci_data1 = ax1 %>% filter(afilsegsal != 4) %>% 
  select(log_ylab_est,qriqueza, factorexph, seg, area, hs01_0003)

result4_w <- ci(
  ineqvar = as.numeric(ci_data1$qriqueza),
  weights  = ci_data1$factorexph,
  outcome  = as.numeric(ci_data1$seg),
  type     = "CIw"
)


summary(result4_w)

sqrt(result4_w$variance)



###########################################################3

ci_data2 = ax1 %>% filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  select(log_ylab_est,qriqueza , factorexph, atenCualquiera) %>% 
  mutate(
    atencualquer = ifelse(atenCualquiera == "Cualquier proveedor", 1, 0))
ci_data2  

result5_ci = ci(
  ineqvar = as.numeric(ci_data2$qriqueza),
  weights  = ci_data2$factorexph,
  outcome  = as.numeric(ci_data2$atencualquer)
)

result5_e <- ci(
  ineqvar = as.numeric(ci_data2$qriqueza),
  weights  = ci_data2$factorexph,
  outcome  = as.numeric(ci_data2$atencualquer),
  type     = "CIc"
)

result5_w <- ci(
  ineqvar = as.numeric(ci_data2$qriqueza),
  weights  = ci_data2$factorexph,
  outcome  = as.numeric(ci_data2$atencualquer),
  type     = "CIw"
)

summary(result5_ci)
summary(result5_e)
summary(result5_w)

ax1$hs01_0003
ci_data2 = ax1 %>% filter(hs03_0033 == 1, afilsegsal != 4, area == 2) %>% 
  select(log_ylab_est,qriqueza , factorexph, atenCualquiera, area, hs01_0003) %>% 
  mutate(
    atencualquer = ifelse(atenCualquiera == "Cualquier proveedor", 1, 0))

result5_w <- ci(
  ineqvar = as.numeric(ci_data2$qriqueza),
  weights  = ci_data2$factorexph,
  outcome  = as.numeric(ci_data2$atencualquer),
  type     = "CIw"
)

summary(result5_w)
sqrt(result5_w$variance)

######################################################3
ax1$area


ci_data3 = ax1 %>% filter(area==1) %>% filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  select(log_ylab_est,qriqueza , factorexph, atenProvedor) %>% 
  mutate(
    atenprov = ifelse(atenProvedor == "Proveedor", 1, 0))
ci_data3

result6_ci = ci(
  ineqvar = as.numeric(ci_data3$qriqueza),
  weights  = ci_data3$factorexph,
  outcome  = as.numeric(ci_data3$atenprov)
)

result6_e <- ci(
  ineqvar = as.numeric(ci_data3$qriqueza),
  weights  = ci_data3$factorexph,
  outcome  = as.numeric(ci_data3$atenprov),
  type     = "CIc"
)

result6_w <- ci(
  ineqvar = as.numeric(ci_data3$qriqueza),
  weights  = ci_data3$factorexph,
  outcome  = as.numeric(ci_data3$atenprov),
  type     = "CIw"
)

summary(result6_ci)
summary(result6_e)
summary(result6_w)


ci_data3 = ax1  %>% filter(hs03_0033 == 1, afilsegsal != 4, area == 2) %>% 
  select(log_ylab_est,qriqueza , factorexph, atenProvedor, area, hs01_0003) %>% 
  mutate(
    atenprov = ifelse(atenProvedor == "Proveedor", 1, 0))

result6_w <- ci(
  ineqvar = as.numeric(ci_data3$qriqueza),
  weights  = ci_data3$factorexph,
  outcome  = as.numeric(ci_data3$atenprov),
  type     = "CIw"
)

summary(result6_w)

sqrt(result6_w$variance)





