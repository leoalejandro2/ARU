library(haven)
library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(stringr)
library(tidyr)
library(tseries)
library(writexl)


eh24 = read_sav("database/EH/EH2024/EH2024_Persona.sav")

table(eh24$phrs)
table(eh24$shrs)


eh24 %>% summarise(hl = s04b_15*s04b_16aa ) %>% table()

table(eh24$s04b_15*eh24$s04b_16aa)

