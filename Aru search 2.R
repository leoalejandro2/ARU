library("oaxaca")
library("haven")
library("dplyr")


edsa <- read_sav("database/EDSA/EDSA2023/EDSA2023_Hogar.sav")

edsah = read_sav("database/EDSA/EDSA2023/EDSA2023_Hombre.sav")
edsam = read_sav("database/EDSA/EDSA2023/EDSA2023_Mujer.sav")



table(edsa$hs03_0037_H)
edsa %>% nrow()



