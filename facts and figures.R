#install.packages("survey")
#install.packages("srvyr")
library(haven)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(stringr)
library(tidyr)
library(tseries)
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

#####################################################################
tiem_des=NULL
bases = list(emp21,emp22,emp23,emp24,
              emp31,emp32,emp33,emp34,
              emp41,emp42,emp43,emp44,
              emp51,emp52)
for (i in 1:length(bases)) {
  aux1 <- bases[[i]]
  
  aux1$mesb=ifelse(aux1$s2_08b_b==2,aux1$s2_08b_a*0.230137,ifelse(aux1$s2_08b_b==8,aux1$s2_08b_a*12,aux1$s2_08b_a))
  
  aux1$mesagrup = cut(aux1$mesb, 
                      breaks = c(0,1,3,6,12,24,Inf),
                      labels = c("< 1 mes", "1-3 meses", "3-6 meses", "6-12 meses", "12-24 meses", "> 24 meses"))
  
  aux1$mesagrup2 = cut(aux1$mesb, 
                       breaks = c(0,3,6,12,Inf),
                       labels = c("< 3 mes", "3-6 meses", "6-12 meses", "> 12 meses"))
  
  deg1 = svydesign(id = ~upm,
                   strata = ~estrato,
                   weights = ~fact_trim,
                   data = aux1)
  
  aux1_deg = as_survey(deg1)
  options(survey.lonely.psu = "adjust")

  #aux1_deg %>% filter(pet==1) %>% group_by(pea,pead) %>% summarise(n=survey_total()) %>% mutate(p=n/sum(n))
  
  #aux1_deg %>% filter(pet==1, pea==1, pead==1) %>% group_by(s2_04,s2_05) %>% 
  #  summarise(n=survey_total()) %>% mutate(p=n/sum(n)*100) 
  
  tab1 = aux1_deg %>% filter(pet==1, pea==1, pead==1,s2_05==1, !is.na(mesagrup2)) %>% 
    group_by(mesagrup2) %>% summarise(n=survey_total()) %>% mutate(p=n/sum(n))
  
  tiem_des = cbind(tiem_des,tab1$p)
}

tiem_des= as.data.frame(t(tiem_des))
names(tiem_des) = c("menor3m","de3a6m","de6a12m","mayor12m")

tiem_des$anio = c(sort(rep(c(2022,2023,2024),4)),2025,2025)
tiem_des$trim = c((rep(c(1,2,3,4),3)),1,2)

tiem_des = tiem_des %>% select(anio,trim,menor3m,de3a6m,de6a12m,mayor12m)
tiem_des

data_long <- tiem_des %>%
  pivot_longer(cols = menor3m:mayor12m,
               names_to = "duracion",
               values_to = "prop")

ggplot(data_long,
       aes(x = interaction(anio,trim),
           y = prop,
           fill = duracion)) +
  geom_area() +
  labs(x="Trimestre",
       y="Proporción",
       fill="Duración del desempleo")

ggplot(data_long,
       aes(x=interaction(anio,trim),
           y=prop,
           color=duracion,
           group=duracion))+
  geom_line(size=1)+
  geom_point()

ggplot(data_long,
       aes(x=interaction(anio,trim),
           y=prop,
           fill=duracion))+
  geom_col(position="fill")

ts(tiem_des$mayor12m, start = c(2022,1), frequency = 4)



##########################################################################################################
aux1 <- bases[[5]]

aux1$mesb=ifelse(aux1$s2_08b_b==2,aux1$s2_08b_a*0.230137,ifelse(aux1$s2_08b_b==8,aux1$s2_08b_a*12,aux1$s2_08b_a))

aux1$mesagrup = cut(aux1$mesb, 
                    breaks = c(0,1,3,6,12,24,Inf),
                    labels = c("< 1 mes", "1-3 meses", "3-6 meses", "6-12 meses", "12-24 meses", "> 24 meses"))

aux1$mesagrup2 = cut(aux1$mesb, 
                     breaks = c(0,3,6,12,Inf),
                     labels = c("< 3 mes", "3-6 meses", "6-12 meses", "> 12 meses"))

deg1 = svydesign(id = ~upm,
                 strata = ~estrato,
                 weights = ~fact_trim,
                 data = aux1)

aux1_deg = as_survey(deg1)
options(survey.lonely.psu = "adjust")



#aux1_deg %>% filter(pet==1) %>% group_by(pea,pead) %>% summarise(n=survey_total()) %>% mutate(p=n/sum(n))

#aux1_deg %>% filter(pet==1, pea==1, pead==1,s2_05==1) %>% group_by(s2_04,s2_05,mesagrup2) %>% 
#  summarise(n=survey_total()) %>% mutate(p=n/sum(n)*100) 
#aux1%>% filter(pet==1, pea==1, pead==1,s2_05==1, is.na(mesagrup2)) %>% View()

tab1 = aux1_deg %>% filter(pet==1, pea==1, pead==1,s2_05==1, !is.na(mesagrup2)) %>% 
  group_by(mesagrup2) %>% summarise(n=survey_total()) %>% mutate(p=n/sum(n))
tab1
#######################################################################################################################

aux1 = emp41
aux2 = emp42
aux3 = emp43
aux4 = emp44


tab2 = aux1 %>% filter(id_per_panel %in% aux2$id_per_panel) %>% left_join(aux2, by = "id_per_panel")

tab2 %>% filter(pet.x==1, pea.x==1, pead.x==1) %>% group_by(s2_08a.x,s2_05.y) %>% count() 

tab2 %>% filter(pet.x==1, pea.x==1, pead.x==1) %>% count(s2_08a.x,pead.y) %>% 
  pivot_wider(
    names_from = pead.y,
    values_from = n,
    values_fill = 0
  ) %>% arrange(desc(`0`)) %>% 
  mutate(
    total = rowSums(across(c(`0`,`1`,`NA`)), na.rm = TRUE),
    across(c(`0`,`1`,`NA`), ~ . / total * 100)
  ) %>% 
  select(-total)


# factores que afectan al participacion laboral     
# factores sociales, edicacion, caracteristicas de salud
names(emp41)
aux1=emp41 %>% select(depto:s1_14esp,pet,pea,pead,peadces,peadasp,s2_05,s2_08a,s2_08b_a,s2_08b_b,estrato,upm,fact_mes,fact_trim)

aux1$mesb=ifelse(aux1$s2_08b_b==2,aux1$s2_08b_a*0.230137,ifelse(aux1$s2_08b_b==8,aux1$s2_08b_a*12,aux1$s2_08b_a))

aux1$mesagrup = cut(aux1$mesb, 
                    breaks = c(0,1,3,6,12,24,Inf),
                    labels = c("< 1 mes", "1-3 meses", "3-6 meses", "6-12 meses", "12-24 meses", "> 24 meses"))

aux1 %>% filter(s2_05==1) %>% group_by(s2_08a) %>% count()
table(emp41$s2_08e)

#########################################################################################################3
emp24 %>% filter(id_per_panel %in% emp21$id_per_panel) 
emp21 %>% filter(id_per_panel %in% emp24$id_per_panel) 

emp21 %>% filter(s1_09==1,s1_13 %in%c(1,3),s2_01==2) %>% group_by(pead,s2_05) %>% count() %>% View()
emp21 %>% group_by(pet,pea,pei,pead,s2_05) %>% count()
emp21$pea

### estrategia de busqueda 


#############################################################################################
aux1 %>% names()
aux1 %>% filter(pead==1) %>% nrow()
aux1 %>% filter(!is.na(s2_08b_a)) %>% nrow()

deg1 = svydesign(id = ~upm,
          strata = ~estrato,
          weights = ~fact_trim,
          data = aux1)

aux1_deg = as_survey(deg1)
options(survey.lonely.psu = "adjust")

tab1 = aux1_deg %>% filter(peadces==1) %>% group_by(s2_05) %>% summarise(n=survey_total())
tab1 %>% mutate(busca = n/sum(n))

distribucion = aux1_deg %>% filter(pead==1,!is.na(mesagrup)) %>% group_by(mesagrup) %>% summarise(n = survey_total())

distribucion = distribucion %>% mutate(p=n/sum(n))
distribucion = distribucion %>% mutate(F = cumsum(p))
sum(distribucion$p[5:6])

plot(distribucion$F)

hist(distribucion$p,breaks = 10)

aux1_deg %>% group_by(pead) %>% summarise(n = survey_total())

aux1_deg %>% group_by(s2_05) %>% summarise(n = survey_total()) %>% View()

aux1_deg %>% group_by(s2_08a) %>% summarise(n=survey_total()) %>% View()


########
tab1 = aux1_deg %>% filter(s2_05==1) %>% group_by(s2_08a) %>%
  summarise(n=survey_total()) %>% arrange(desc(n)) %>% mutate(id=row_number())
tab1 = tab1 %>% mutate(p=(n/sum(n)*100))
tab1$s2_08a=as.character(as_factor(tab1$s2_08a))
tab1$s2_08a[tab1$s2_08a %in% tab1$s2_08a[(nrow(tab1)-4):nrow(tab1)]]= "0. Otros"

ntab1 = tab1 %>% group_by(s2_08a) %>% summarise(p=sum(p)) %>% arrange(desc(p))


ntab1$s2_08a = gsub("^[0-9]+\\.\\s*", "", ntab1$s2_08a)


ntab1$s2_08a <- str_wrap(ntab1$s2_08a, width = 30)

ggplot(ntab1, aes(x = reorder(s2_08a, -p), y = p, fill = s2_08a)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(p,1)), vjust = -0.5, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0,0.15))) +
  labs(
    x = "",
    y = "Porcentaje",
    fill = ""
  ) +
  scale_fill_manual(values = c(
    "chocolate4",
    "chocolate3",
    "chocolate2",
    "chocolate1",
    "chocolate"
  )) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 11),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width  = unit(0.35,"cm"),
    legend.key.height = unit(0.35,"cm"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE,
      override.aes = list(size = 4)
    )
  )

#######################################################################################

auxt1 = emp41 %>% filter(id_per_panel %in% emp42$id_per_panel) %>% 
  select(id_per_panel,depto:s1_14esp,pet,pea,pead,peadces,peadasp,s2_08b_a,s2_08b_b,s2_05,s2_08a,estrato,upm,fact_mes,fact_trim) 

auxt2 = emp42 %>% 
  select(id_per_panel,depto:s1_14esp,pet,pea,pead,peadces,peadasp,s2_08b_a,s2_08b_b,s2_05,s2_08a,estrato,upm,fact_mes,fact_trim)

auxt = auxt1 %>% left_join(auxt2, by="id_per_panel")


auxt %>% filter(pet.x==1) %>% group_by(s2_05.x,s2_05.y) %>% count()

auxt %>% select(fact_trim.x,fact_trim.x)

auxt %>% filter(s2_05.x==1,pea.y==1,pead.y==0) %>% group_by(s2_08a.x) %>% count()

auxt %>% filter(s2_05.x==1,s2_05.y==1) %>% group_by(s2_08a.x,s2_08a.y) %>% count()


tabla_cruzada <- auxt %>% 
  filter(s2_05.x == 1, s2_05.y == 1) %>% 
  count(s2_08a.x, s2_08a.y) %>% 
  pivot_wider(
    names_from = s2_08a.y,
    values_from = n,
    values_fill = 0
  )



