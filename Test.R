
library(tidyverse)
library(sf)

Regiones <- read_rds("Regiones.rds")
Infectados <- read_rds("Infectados.rds") %>% mutate(region = str_replace(region, "Araucania", "Araucanía")) %>% mutate(region = str_replace(region, "Aysen", "Aysén")) %>% mutate(region = str_replace(region, "Biobio", "Biobío"))

for(i in 1:nrow(Infectados)){
  try({Temp <- Regiones %>% dplyr::filter(str_detect(Region , pattern = Infectados$region[i])) %>% pull(Region)
  Infectados$region[i] <- Temp})
}

