
library(readxl)
library(dplyr)




carbon <- read_excel("database/BOL.xlsx", sheet = "Country carbon data")

treel <- read_excel("database/BOL.xlsx", sheet = "Country tree cover loss")

drivers <- read_excel("database/BOL.xlsx", sheet = "Country drivers")


sub1 <- read_excel("database/BOL.xlsx", sheet = "Subnational 1 tree cover loss")


sub2 <- read_excel("database/BOL.xlsx", sheet = "Subnational 1 drivers")


sub3 <- read_excel("database/BOL.xlsx", sheet = "Subnational 1 carbon data")

sub1 %>% filter(threshold==30)  %>% select(subnational1 , tc_loss_ha_2025)

sub1$tc_loss_ha_2025

sub3 %>% filter(umd_tree_cover_density_2000__threshold==30) %>% select(subnational1,gfw_forest_carbon_gross_emissions_2025__Mg_CO2e)

sub3$gfw_forest_carbon_gross_emissions_2025__Mg_CO2e

carbon %>% filter(umd_tree_cover_density_2000__threshold==30) %>% 
  select(gfw_forest_carbon_gross_emissions_2010__Mg_CO2e:gfw_forest_carbon_gross_emissions_2025__Mg_CO2e) %>% View()


library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

carbon %>% 
  # 1. Tu filtro inicial
  filter(umd_tree_cover_density_2000__threshold == 30) %>% 
  
  # 2. Seleccionar las columnas de interés
  select(gfw_forest_carbon_gross_emissions_2010__Mg_CO2e:gfw_forest_carbon_gross_emissions_2025__Mg_CO2e) %>% 
  
  # 3. Calcular el total (o promedio) por año de todas las filas/regiones
  summarise(across(everything(), \ (x) sum(x, na.rm = TRUE))) %>% 
  
  # 4. Pivotar de formato ancho a largo
  pivot_longer(
    cols = everything(),
    names_to = "anio",
    values_to = "emisiones"
  ) %>% 
  
  # 5. Limpiar el nombre de la columna para quedarnos solo con el año (número)
  mutate(anio = as.numeric(str_extract(anio, "\\d{4}"))) %>% 
  
  # 6. Crear el gráfico de serie de tiempo
  ggplot(aes(x = anio, y = emisiones)) +
  geom_line(color = "#2ca25f", size = 1) +
  geom_point(color = "#006d2c", size = 2) +
  scale_x_continuous(breaks = seq(2010, 2025, by = 2)) +
  theme_minimal() +
  labs(
    title = "Evolución de las Emisiones Brutas de Carbono Forestal (2010-2025)",
    subtitle = "Filtro: Umbral de densidad de cobertura arbórea = 30%",
    x = "Año",
    y = "Emisiones Totales (Mg CO2e)",
    caption = "Fuente: Global Forest Watch"
  )




