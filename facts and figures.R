#install.packages("survey")
#install.packages("srvyr")
library(haven)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
emp21 <- read_sav("database/empleo/ECE_1T2022.sav")
emp22 <- read_sav("database/empleo/ECE_2T2022.sav")
emp23 <- read_sav("database/empleo/ECE_3T2022.sav")
emp24 <- read_sav("database/empleo/ECE_4T2022.sav")
emp31 <- read_sav("database/empleo/ECE_1T2023.sav")
emp32 <- read_sav("database/empleo/ECE_2T2023.sav")
emp33 <- read_sav("database/empleo/ECE_3T2023.sav")
emp34 <- read_sav("database/empleo/ECE_4T2023.sav")
emp41 <- read_sav("database/empleo/ECE_1T2024.sav")
emp42 <- read_sav("database/empleo/ECE_2T2024.sav")
emp43 <- read_sav("database/empleo/ECE_3T2024.sav")
emp44 <- read_sav("database/empleo/ECE_4T2024.sav")
emp51 <- read_sav("database/empleo/ECE_1T2025.sav")
emp52 <- read_sav("database/empleo/ECE_2T2025.sav")
save(emp21,emp22,emp23,emp24,emp31,emp32,emp33,emp34,emp41,emp42,emp43,emp44,emp51,emp52,
     file= "database/empleo/emp.RData")
load("database/empleo/emp.RData")

# factores que afectan al participacion laboral     
# factores sociales, edicacion, caracteristicas de salud
names(emp41)
aux1=emp41 %>% select(depto:s1_14esp,pead,peadces,peadasp,s2_08b_a,s2_08b_b,estrato,upm,fact_mes,fact_trim)

aux1$mesb=ifelse(aux1$s2_08b_b==2,aux1$s2_08b_a*0.230137,ifelse(aux1$s2_08b_b==8,aux1$s2_08b_a*12,aux1$s2_08b_a))

aux1$mesagrup = cut(aux1$mesb, 
                    breaks = c(0,1,3,6,12,24,Inf),
                    labels = c("< 1 mes", "1-3 meses", "3-6 meses", "6-12 meses", "12-24 meses", "> 24 meses"))





aux1 %>% names()
aux1 %>% filter(pead==1) %>% nrow()
aux1 %>% filter(!is.na(s2_08b_a)) %>% nrow()

deg1 = svydesign(id = ~upm,
          strata = ~estrato,
          weights = ~fact_trim,
          data = aux1)

aux1_deg = as_survey(deg1)

distribucion = aux1_deg %>% filter(pead==1,!is.na(mesagrup)) %>% group_by(mesagrup) %>% summarise(n = survey_total())

distribucion = distribucion %>% mutate(p=n/sum(n))
distribucion = distribucion %>% mutate(F = cumsum(p))
distribucion

plot(distribucion$F)

hist(distribucion$p,breaks = 10)



26/4.33



