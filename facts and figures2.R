library(haven)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(stringr)
library(tidyr)
library(tseries)
library(writexl)


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


