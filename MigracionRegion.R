

library(tidyverse)
library(sf)


Viajes <- read_rds("Viajes_Comuna.rds") %>% ungroup() %>% rename(origen = comuna_origen, destino = comuna_destino) %>% mutate(origen= str_replace(origen, "Los Álamos", "Los Alamos")) %>% 
  mutate(origen= str_replace(origen, "Los Ángeles", "Los Angeles")) %>% 
  mutate(origen= str_replace(origen, "Marchihue", "Marchigüe")) %>% 
  mutate(origen= str_replace(origen, "Paiguano", "Paihuano")) %>% mutate(origen = str_to_lower(origen)) %>% mutate(destino= str_replace(destino, "Los Álamos", "Los Alamos")) %>% 
  mutate(destino= str_replace(destino, "Los Ángeles", "Los Angeles")) %>% 
  mutate(destino= str_replace(destino, "Marchihue", "Marchigüe")) %>% 
  mutate(destino= str_replace(destino, "Paiguano", "Paihuano")) %>% mutate(destino = str_to_lower(destino))


Comunas <- read_rds("Comunas.rds") %>% as.data.frame() %>% dplyr::select(Region, Comuna, Poblacion)



for(i in 1:nrow(Comunas)){
  Comuna <- Comunas[i,] %>% pull(Comuna)
  Region <- Comunas[i,] %>% pull(Region)
  Viajes$origen <- ifelse(Viajes$origen == Comuna, Region, Viajes$origen)
  Viajes$destino <- ifelse(Viajes$destino == Comuna, Region, Viajes$destino)
  
}


Viajes <- Viajes %>% group_by(origen, destino) %>% summarise(n_personas = sum(n)) %>% ungroup()  %>% arrange(desc(n_personas))

saveRDS(Viajes, "Viajes_Regiones.rds")

MatrixRegion <- matrix(0, ncol = 17, nrow = 17)

colnames(MatrixRegion) <- sort(unique(Comunas$Region))
rownames(MatrixRegion) <- sort(unique(Comunas$Region))


Regiones <- read_rds("Comunas.rds") %>% as.data.frame()%>% group_by(Region) %>% summarise(Poblacion = sum(Poblacion))


for(i in sort(unique(Comunas$Region))){
  for(j in sort(unique(Comunas$Region))){
    if(i == j){
      0
    }
    else{
      try(MatrixRegion[i,j] <- Viajes %>% dplyr::filter(origen == j, destino == i) %>% mutate(Tasa = n_personas/(Regiones %>% dplyr::filter(Region == destino) %>%  pull(Poblacion))) %>% pull(Tasa))
    }
  }
  message(paste(i, "of", length(sort(unique(Comunas$Region)))))
}



saveRDS(MatrixRegion, "MatrixRegion.rds")
