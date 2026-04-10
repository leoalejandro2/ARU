library(haven)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(stringr)
library(tidyr)
library(tseries)
library(writexl)
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

emp41 %>% get_label()


<<<<<<< HEAD
emp41 %>% select(cob_op,caeb_op)

hist(emp41$tothrs_ef)
hist(emp41$tothrs)
hist(emp41$phrs)

boxplot(emp41$phrs)
emp41$
emp41$pead
emp41$peadces
emp41$peadasp

emp42 %>% 
  filter(id_per_panel %in% (emp41 %>% filter(pead == 1) %>% pull(id_per_panel)))




=======
edsah <- read_sav("database/EDSA/EDSA2023/EDSA2023_Hogar.sav")
edsape <- read_sav("database/EDSA/EDSA2023/EDSA2023_Peso_talla_hemo.sav")

edsag = edsah %>% left_join(edsape, by = c("folio","nro"))

edsah %>% filter(hs03_0033==1) %>% group_by(hs03_0034_T) %>% 
  count()

edsah %>% filter(hs03_0033==1, hs03_0034_T == 1) %>% 
  group_by(hs03_0035_V) %>% count()



edsah %>% filter(hs03_a_0041==1,hs03_a_0042_I==1) %>% 
  group_by(hs03_a_0043_V) %>% count()


edsag %>% group_by(tip_anemia_m, hs03_0033) %>% count()

>>>>>>> d17fb2a1c62df0f413faf199438bf04916dd5c66

