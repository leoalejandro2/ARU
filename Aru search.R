install.packages("oaxaca")
install.packages("UpSetR")
library("oaxaca")
library("haven")
library("dplyr")
library(ggplot2)
library(survey)
library(srvyr)
library(stringr)
library(tidyr)
library(UpSetR)
eh24 <- read_sav("database/EH/EH2024/EH2024_Persona.sav")


eh24 %>% View()
eh24$estrato
eh24$s03a_02a

folio=(eh24 %>% group_by(folio) %>% count())$folio

estrato=(eh24 %>% group_by(estrato) %>% count())$estrato

upm=(eh24 %>% group_by(upm) %>% count())$upm
#######################################################################################

deg1 = svydesign(id = ~upm,
                 strata = ~estrato,
                 weights = ~factor,
                 data = eh24)

eh24d = as_survey(deg1)
options(survey.lonely.psu = "adjust")

eh24d %>% filter(area==1,niv_ed_g==4) %>% group_by(s01a_02,s01a_09) %>% 
  summarise(a=survey_mean(aestudio,na.rm = TRUE), y = survey_mean(ylab,na.rm=TRUE)) 


eh24 %>% filter(area==1, niv_ed_g==4, cob_op == 4) %>% 
  group_by(s01a_09) %>% summarise(a=mean(aestudio, na.rm=TRUE,),y=mean(ylab,na.rm=TRUE)) 


eh24$cob_op
eh24$s01a_02
eh24$cob_op
eh24$cob_op
eh24$depto
eh24$s03a_02a>=72
eh24$niv_ed_g
###########################################################################################
eh24 %>% names()
eh24$s01a_09


eh24$ind <- ifelse(eh24$s01a_09 == 1, 1, 0)

oaxaca(formula = ylab ~ aestudio | ind, data = eh24)


table(eh24$s01b_11a)

table(eh24$s01b_11d)

eh24 %>% filter(area==1,niv_ed_g==4) %>% group_by(s01a_09, s01a_02) %>% 
  summarise(a = mean(aestudio,na.rm=TRUE),y=mean(ylab,na.rm=TRUE))

############################################################################################

eh24 = read_sav("database/EH/EH2024/EH2024_Persona.sav")
eh24dis <- read_sav("database/EH/EH2024/EH2024_Discriminacion.sav")
eh24$s01a_03


eh24dis %>% nrow()

aux1 = eh24dis %>%
  mutate(discriminacion =ifelse(rowSums(across(s09a_01a:s09a_01l, ~ .x %in% c(1,3)))>0,1,0))

aux1 %>% group_by(discriminacion = ) 


table(aux1$discriminacion)/nrow(aux1)


eh24dis %>% group_by(s09a_01a) %>% summarise(total = n()/nrow(eh24dis))

mean(eh24dis$ponderador)

hist(eh24dis$ponderador)
