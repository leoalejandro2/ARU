rm(list = ls())
library(tidyverse)
library(oaxaca)
library(haven)
library(dplyr)
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
library(rineq)

edsa = read_sav("database/EDSA/EDSA2023/EDSA2023_Hogar.sav")
edsaV = read_sav("database/EDSA/EDSA2023/EDSA2023_Vivienda.sav")
edsah = read_sav("database/EDSA/EDSA2023/EDSA2023_Hombre.sav")
edsam = read_sav("database/EDSA/EDSA2023/EDSA2023_Mujer.sav")
########################################################
edsa$afilsegsal |> table()

bd_edsa = edsa %>% mutate(
  seguro = labelled(case_when(
    afilsegsal %in% c(1,2,3,5) ~ 1,
    afilsegsal == 6 ~ 0,
    TRUE ~ NA_real_),labels = c(
      "Afiliado a algun seguro" = 1,
      "Sin afiliacion" = 0)),
  atencion = labelled(case_when(
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
  aseguro_sus = (case_when(
    hs03_0035_A==1 | hs03_0035_B==1 | hs03_0035_C==1 | hs03_0035_D==1 ~ "Centro de Salud"
  )),
  ahospital23 = (case_when(
    hs03_0035_E==1 | hs03_0035_F==1 | hs03_0035_G==1 ~ "Hospital de 2 y 3 nivel"
  )), 
  aseguro_caja = (case_when(
    hs03_0035_H==1 | hs03_0035_I==1 | hs03_0035_J==1 | hs03_0035_K==1 |
      hs03_0035_L==1 | hs03_0035_M==1 | hs03_0035_N==1 | hs03_0035_O==1 ~ "Cajas de Salud"
  )),
  aprivado = (case_when(
    hs03_0035_P==1 | hs03_0035_Q==1 ~ "Privado"
  )),
  no_acudio = (case_when(
    hs03_0035_R == 1 | hs03_0035_S == 1 | hs03_0035_T == 1 | hs03_0035_U == 1 | 
      hs03_0035_V == 1 | hs03_0035_X == 1 | hs03_0035_Z == 1 ~ "No acudio a establecimiento"
  )),
  aten_cualquiera = case_when(
    (afilsegsal == 1 | afilsegsal == 2 | afilsegsal == 3 | afilsegsal == 5) & 
      (aseguro_sus == "Centro de Salud" | ahospital23== "Hospital de 2 y 3 nivel" | 
         aseguro_caja == "Cajas de Salud" | aprivado == "Privado") ~ "Cualquier proveedor",
    TRUE ~ "No atencion"),
  aten_provedor = case_when(
    ## SUS
    afilsegsal == 3 & (aprivado == "Privado")~ "Proveedor",
    afilsegsal == 2 & (aseguro_caja == "Cajas de Salud" | ahospital23 == "Hospital de 2 y 3 nivel")~ "Proveedor",
    afilsegsal == 1 & (aseguro_sus == "Centro de Salud" | ahospital23== "Hospital de 2 y 3 nivel") ~ "Proveedor",
    TRUE ~ "No Proveedor")
) %>% left_join(edsaV, by = c("folio","upm","estrato","area","region","departamento")) 

desg1 = svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~factorexph,
  data = (bd_edsa)
)

edsa_survey = as_survey(desg1)

### Seguro
edsa_survey %>% filter(niv_ed_g != 99,afilsegsal != 4) %>% group_by(qriqueza,seguro) %>% 
  summarise(n = survey_total() ) %>% mutate(prob = n/sum(n))

res1 = edsa_survey %>% 
  filter(afilsegsal != 4) %>% 
  group_by(seguro) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95) 
  ) %>% 
  mutate(
    prop = n / sum(n) * 100,
    prop_low = n_low / sum(n) * 100,
    prop_upp = n_upp / sum(n) * 100
  ) %>% select(seguro ,prop, prop_low, prop_upp)

## atencion cualquiera
res2 = edsa_survey %>% 
  filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  group_by(aten_cualquiera) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95) 
  ) %>% 
  mutate(
    prop = n / sum(n) * 100,
    prop_low = n_low / sum(n) * 100,
    prop_upp = n_upp / sum(n) * 100
  ) %>% select(aten_cualquiera, prop, prop_low, prop_upp)

## atencion proveedor afiliado
res3 = edsa_survey %>% 
  filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  group_by(aten_provedor) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95) 
  ) %>% 
  mutate(
    prop = n / sum(n) * 100,
    prop_low = n_low / sum(n) * 100,
    prop_upp = n_upp / sum(n) * 100
  ) %>% select(aten_provedor, prop, prop_low, prop_upp)

### problema de salud
res4 = edsa_survey %>% filter(hs03_0033 %in% c(1,2)) %>% 
  group_by(hs03_0033) %>% 
  summarise(
    n = survey_total(vartype = "ci", level = 0.95)
  ) %>% mutate(
    prop = n / sum(n) * 100,
    prop_low = n_low / sum(n) * 100,
    prop_upp = n_upp / sum(n) * 100
  ) %>% select(hs03_0033, prop, prop_low, prop_upp)



# Combine all results into one data frame
combined <- bind_rows(
  res1 |> rename(category = seguro) |> mutate(
    category = as.character(category),
    group = "Afiliación a seguro"
  ),
  res2 |> rename(category = aten_cualquiera) |> mutate(
    group = "Atención cualquier proveedor"
  ),
  res3 |> rename(category = aten_provedor) |> mutate(
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
    mutate(category = sjlabelled::as_label(seguro)) |>
    select(category, prop, prop_low, prop_upp) |>
    mutate(group = "Afiliación\na seguro"),
  res2 |>
    rename(category = aten_cualquiera) |>
    select(category, prop, prop_low, prop_upp) |>
    mutate(group = "Atención\ncualquier proveedor"),
  res3 |>
    rename(category = aten_provedor) |>
    select(category, prop, prop_low, prop_upp) |>
    mutate(group = "Atención\nproveedor"),
  res4 |>
    mutate(category = sjlabelled::as_label(hs03_0033)) |>
    select(category, prop, prop_low, prop_upp) |>
    mutate(group = "Problema de\nsalud (3 meses)")
)

combined <- combined |>
  mutate(tipo = if_else(
    category %in% c("Afiliado a algun seguro", "Cualquier proveedor", "Proveedor", "1. SI"),
    "Si", "No"
  ))

combined
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
  scale_fill_manual(values = c("Si" = "#8B4500", "No" = "#FF7F24")) +
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

#######################################################################
ci_data1 = bd_edsa %>% filter(afilsegsal != 4) %>% 
  select(qriqueza, factorexph, seguro)

result4_ci = ci(
  ineqvar = as.numeric(ci_data1$qriqueza),
  weights  = ci_data1$factorexph,
  outcome  = as.numeric(ci_data1$seguro)
)


result4_e <- ci(
  ineqvar = as.numeric(ci_data1$qriqueza),
  weights  = ci_data1$factorexph,
  outcome  = as.numeric(ci_data1$seguro),
  type     = "CIc"
)

result4_w <- ci(
  ineqvar = as.numeric(ci_data1$qriqueza),
  weights  = ci_data1$factorexph,
  outcome  = as.numeric(ci_data1$seguro),
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

ci_val <- round(result4_ci$concentration_index, 4)

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

bd_edsa$seguro

##########################################################################3
bd_edsa$aten_cualquiera
ci_data1 = bd_edsa %>% filter(afilsegsal != 4) %>% 
  select(qriqueza, factorexph, seguro, area, hs01_0003)

result4_w <- ci(
  ineqvar = as.numeric(ci_data1$qriqueza),
  weights  = ci_data1$factorexph,
  outcome  = as.numeric(ci_data1$seguro),
  type     = "CIw"
)


summary(result4_w)
sqrt(result4_w$variance)

###########################################################3
ci_data2 = bd_edsa %>% filter(hs03_0033 == 1,afilsegsal != 4) %>% 
  select(qriqueza , factorexph, aten_cualquiera, area, hs01_0003) %>% 
  mutate(
    atencualquer = ifelse(aten_cualquiera == "Cualquier proveedor", 1, 0))

result5_w <- ci(
  ineqvar = as.numeric(ci_data2$qriqueza),
  weights  = ci_data2$factorexph,
  outcome  = as.numeric(ci_data2$atencualquer),
  type     = "CIw"
)

summary(result5_w)
sqrt(result5_w$variance)

######################################################3

ci_data3 = bd_edsa  %>% filter(hs03_0033 == 1, afilsegsal != 4) %>% 
  select(qriqueza , factorexph, aten_provedor, area, hs01_0003) %>% 
  mutate(
    atenprov = ifelse(aten_provedor == "Proveedor", 1, 0))

result6_w <- ci(
  ineqvar = as.numeric(ci_data3$qriqueza),
  weights  = ci_data3$factorexph,
  outcome  = as.numeric(ci_data3$atenprov),
  type     = "CIw"
)

summary(result6_w)
sqrt(result6_w$variance)





