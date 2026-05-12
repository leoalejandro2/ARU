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

edsaV$hs04_0060 
edsaV$hs04_0061
edsaV$hs04_0081_12 

edsaV %>% select(hs04_0060, hs04_0061, hs04_0081_12) %>% summary()

eh23V$s06a_02
eh23V$s06a_01
eh23V$s06a_19
edsa$tipohogar
eh23$yhog

eh23V %>% select(folio, s06a_02, s06a_01, s06a_19) %>% summary()

edsa %>% select(hs01_0004a, hs01_0003, area, aestudio, hs01_0005) %>% summary()
edsa$hs01_0005
eh23 %>% filter() %>% select(ylab, s01a_03, s01a_02, area, aestudio, s01a_05) %>% summary()

eh23$ylab

edsa$hs02_0022

eh23$niv_ed_g[is.na(eh23$niv_ed_g)]

as_label(s01a_02) + as_label(area)  +aestudio  +as_label(s01a_05) + redad



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


#TXZQG

# eh23 = eh23 %>% left_join(eh23V %>% select(folio, s06a_02, s06a_01, s06a_19), by = "folio")



eh23 %>% group_by(s04a_01,ylab) %>% count() %>% View()

edsa$aestudio

edsah$vs01_0148 
edsah$vs01_0157 

edsah %>% group_by(vs01_0148) %>% count() %>% summarise(n = n/sum(n))

edsah %>% nrow()

edsam$ms08_0809

edsah$vs01_0149

eh23$s04a_01
eh23 %>% pull(aestudio) %>% summary()



eh23$ylab[is.na(eh23$ylab)] <- 1

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

##########################################################
library(dplyr)

# Work with non-missing seg observations (afilsegsal != 4 → seg is NA)
ax1 %>%  filter(afilsegsal != 4) %>% 
  pull(seg) %>% summary()

ax1 %>% arrange(log_ylab_est) %>% select(seg, log_ylab_est)

ci_data <- ax1 %>% 
  filter(!is.na(seg)) %>% 
  mutate(
    seg_num = as.numeric(seg),
    w       = factorexph
  ) |>
  arrange(log_ylab_est) |>
  mutate(
    # Weighted fractional rank
    cum_w  = cumsum(w),
    r      = (cum_w - 0.5 * w) / sum(w)
  )



ci_data$seg_num
ci_data$factorexph
ci_data$r

# Survey-weighted mean of seg
mu <- weighted.mean(ci_data$seg_num, ci_data$w)

# Weighted covariance between seg and rank
wcov <- cov.wt(
  cbind(ci_data$seg_num, ci_data$r),
  wt = ci_data$w,
  method = "ML"
)$cov[1, 2]

# Standard CI
CI <- 2 * wcov / mu

# Erreygers CI (preferred for binary)
CI_E <- 4 * mu * CI

# Wagstaff CI
CI_W <- CI / (1 - mu)

cat(sprintf("Prevalence (μ):       %.4f\n", mu))
cat(sprintf("Standard CI:          %.4f\n", CI))
cat(sprintf("Erreygers CI:         %.4f\n", CI_E))
cat(sprintf("Wagstaff CI:          %.4f\n", CI_W))
cat(sprintf("Theoretical bounds:   [%.4f, %.4f]\n", mu - 1, 1 - mu))




compute_ci <- function(df) {
  df <- df |>
    filter(!is.na(seg)) |>
    mutate(seg_num = as.numeric(seg), w = factorexph) |>
    arrange(log_ylab_est) |>
    mutate(
      cum_w = cumsum(w),
      r     = (cum_w - 0.5 * w) / sum(w)
    )
  
  mu   <- weighted.mean(df$seg_num, df$w)
  wcov <- cov.wt(cbind(df$seg_num, df$r), wt = df$w, method = "ML")$cov[1, 2]
  CI   <- 2 * wcov / mu
  
  tibble(
    n        = nrow(df),
    mu       = mu,
    CI       = CI,
    CI_E     = 4 * mu * CI,
    CI_W     = CI / (1 - mu),
    bound_lo = mu - 1,
    bound_hi = 1 - mu
  )
}

ax1 |>
  mutate(area_label = as_label(area)) |>
  group_by(area_label) |>
  group_modify(~ compute_ci(.x)) |>
  bind_rows(
    compute_ci(ax1) |> mutate(area_label = "Total") |> select(area_label, everything())
  )

###########################################################


compute_ci_unweighted <- function(df) {
  df <- df |>
    filter(!is.na(seg)) |>
    mutate(
      seg_num = as.numeric(seg),
      w       = 1  # equal weights
    ) |>
    arrange(log_ylab_est) |>
    mutate(
      n   = n(),
      r   = (row_number() - 0.5) / n  # simplified fractional rank
    )
  
  mu   <- mean(df$seg_num)
  wcov <- cov(df$seg_num, df$r)
  CI   <- 2 * wcov / mu
  
  tibble(mu = mu, CI = CI, CI_E = 4 * mu * CI, CI_W = CI / (1 - mu))
}

cat("--- Weighted (factorexph) ---\n")
cat(sprintf("CI: %.4f | CI_E: %.4f | CI_W: %.4f\n", CI, CI_E, CI_W))

cat("\n--- Unweighted ---\n")
result_uw <- compute_ci_unweighted(ax1)
cat(sprintf("CI: %.4f | CI_E: %.4f | CI_W: %.4f\n", result_uw$CI, result_uw$CI_E, result_uw$CI_W))


#############################################################


ci_aten <- ax1 |>
  filter(hs03_0033 == 1, afilsegsal != 4) |>
  mutate(
    y_bin = if_else(atenCualquiera == "Cualquier proveedor", 1, 0),
    w     = factorexph
  ) |>
  arrange(log_ylab_est) |>
  mutate(
    cum_w = cumsum(w),
    r     = (cum_w - 0.5 * w) / sum(w)
  )

mu_a   <- weighted.mean(ci_aten$y_bin, ci_aten$w)
wcov_a <- cov.wt(cbind(ci_aten$y_bin, ci_aten$r), wt = ci_aten$w, method = "ML")$cov[1, 2]
CI_a   <- 2 * wcov_a / mu_a
CI_E_a <- 4 * mu_a * CI_a
CI_W_a <- CI_a / (1 - mu_a)

cat(sprintf("Prevalence (μ):       %.4f\n", mu_a))
cat(sprintf("Standard CI:          %.4f\n", CI_a))
cat(sprintf("Erreygers CI:         %.4f\n", CI_E_a))
cat(sprintf("Wagstaff CI:          %.4f\n", CI_W_a))
cat(sprintf("Theoretical bounds:   [%.4f, %.4f]\n", mu_a - 1, 1 - mu_a))


#########################################


compute_ci <- function(df, y_var, ses_var, weight_var, filter_expr = NULL) {
  if (!is.null(filter_expr)) df <- df |> filter({{ filter_expr }})
  
  df <- df |>
    filter(!is.na({{ y_var }})) |>
    mutate(
      y_bin = as.numeric({{ y_var }}),
      w     = {{ weight_var }}
    ) |>
    arrange({{ ses_var }}) |>
    mutate(
      cum_w = cumsum(w),
      r     = (cum_w - 0.5 * w) / sum(w)
    )
  
  mu   <- weighted.mean(df$y_bin, df$w)
  wcov <- cov.wt(cbind(df$y_bin, df$r), wt = df$w, method = "ML")$cov[1, 2]
  CI   <- 2 * wcov / mu
  
  tibble(
    mu   = mu,
    CI   = CI,
    CI_E = 4 * mu * CI,
    CI_W = CI / (1 - mu)
  )
}

compute_ci(ax1, seg,           log_ylab_est, factorexph)
compute_ci(ax1, atenCualquiera, log_ylab_est, factorexph, hs03_0033 == 1 & afilsegsal != 4)







