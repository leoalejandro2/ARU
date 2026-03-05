library(haven)
library(dplyr)
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

# factores que afectan al participacion laboral     
# factores sociales, edicacion, caracteristicas de salud
View(emp21)


table(emp21$s2_08b_a)
table(emp21$s2_08b_b)


nrow(emp21)
emp$s1_03a

emp21 %>% group_by(s1_03a) %>% count() %>% View()

emp21 %>% group_by(s1_07a) %>% count() %>% View()

m=emp44 %>% filter(s1_07a>=71) %>% nrow() 
n=emp44 %>% filter(s1_07a>=71, s2_09==2) %>% nrow() 
n/m*100

m=emp44 %>% filter(s1_03a>=19) %>% nrow() 
n=emp44 %>% filter(s1_03a>=19,s2_09==2) %>% nrow() 
n/m*100

emp21 %>% names()
emp21$s2_08b_b



emp21 %>% filter(s2_05==1) %>% group_by(s2_08b_b,s2_08b_a) %>% count() %>% View()

emp21 %>% filter(s2_05==1) %>% group_by(s2_08b_a) %>% count()

