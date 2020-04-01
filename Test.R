library(tidyverse)
library(sf)
library(lubridate)

Regiones <- read_rds("Regiones.rds")  %>% st_transform(crs = 4326) %>% dplyr::filter(Region != "Zona sin demarcar")
Regiones <- Regiones %>% lwgeom::st_make_valid() %>% st_crop(xmin = -80, ymin = -55.98, xmax = -66.41821, ymax = -17.4984)
saveRDS(Regiones, "Regiones.rds")


Results <- read_rds("Results.rds")
Results <- bind_rows(Results)  %>% mutate(Fecha = dmy("17-03-2020") + Time)

Results3 <- Results %>% dplyr::filter(Time %in% c(25, 50, 75, 100)) %>% dplyr::select(Region, Prop_I, Fecha) %>% pivot_wider(names_from = Fecha, values_from = Prop_I)
Regiones <- read_rds("Regiones.rds") %>% st_transform(crs = 4326) %>% dplyr::filter(Region != "Zona sin demarcar") %>% dplyr::select(-Infectados, -Prevalencia, -Poblacion) %>% full_join(Results3) %>% dplyr::select(-Region)


plot(Regiones, axes = T)