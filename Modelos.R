library(tidyverse)
library(sf)
library(lubridate)

Regiones <- read_rds("Regiones.rds") %>% dplyr::filter(Region != "Zona sin demarcar")
Viajes_Regiones <- read_rds("Viajes_Regiones.rds") %>% dplyr::filter(n_personas > 1) %>% mutate(n_personas = round(n_personas))

beta = 0.3
gamma = 0.07
Times = 100
alpha = 1

df_out <- Regiones %>% as.data.frame() %>% select(Region, Poblacion, Infectados) %>% mutate(Infectados = ifelse(is.na(Infectados), 0, Infectados),Suceptibles = Poblacion -Infectados , Recuperados = 0) %>% dplyr::select(Region, Suceptibles, Infectados, Recuperados) %>% mutate(Total = Suceptibles + Infectados + Recuperados, Time = 1)


Results <- list()

Results[[1]] <- df_out %>% mutate(Prop_S = Suceptibles/Total, Prop_I = Infectados/Total, Prop_R = Recuperados/Total)

for(i in 2:Times){
  Results[[i]] <- Results[[i -1]] %>% mutate(Prop_S = Suceptibles/Total, Prop_I = Infectados/Total, Prop_R = Recuperados/Total)
  for(j in 1:nrow(df_out)){
  temp <- Results[[i]][j,]
  ##Modulo inmigracion emigración
  Inmig <- Viajes_Regiones %>% dplyr::filter(destino == temp$Region[1]) %>% rename(Region = origen) %>% left_join(Results[[i]]) %>% mutate(Suceptibles = round(n_personas*Prop_S), Infectados = round(n_personas*Prop_I),Recuperados = round(n_personas*Prop_R)) %>% group_by(destino) %>% summarise_at(c("Suceptibles", "Infectados", "Recuperados"), sum) %>% ungroup()
  Emig <- Viajes_Regiones %>% dplyr::filter(origen == temp$Region[1]) %>% rename(Region = destino) %>% left_join(Results[[i]]) %>% mutate(Suceptibles = round(n_personas*Prop_S), Infectados = round(n_personas*Prop_I),Recuperados = round(n_personas*Prop_R)) %>% group_by(origen) %>% summarise_at(c("Suceptibles", "Infectados", "Recuperados"), sum)  %>% ungroup()
  Results[[i]][j,] <- temp %>% mutate(Suceptibles = Suceptibles + Inmig$Suceptibles - Emig$Suceptibles, Infectados = Infectados + Inmig$Infectados - Emig$Infectados, Recuperados = Recuperados + Inmig$Recuperados - Inmig$Recuperados, Total = Suceptibles + Infectados + Recuperados) %>% 
    #Modulo contagio y recuperacion
    mutate(Suceptibles = Suceptibles - round((Suceptibles*beta*alpha*Infectados)/Total), Infectados = Infectados + round((Suceptibles*beta*alpha*Infectados)/Total) - round(Infectados*gamma), Recuperados = Recuperados + round(Infectados*gamma), Total = Suceptibles + Infectados + Recuperados, Time = i)
  }
  saveRDS(Results, "Results_beta0_3_alpha1_gama_0_07.rds")
  message(i)
}

Results <- bind_rows(Results) 

Results2 <- Results %>% dplyr::select(Region, Time, Prop_I, Prop_S, Prop_R) %>% pivot_longer(starts_with("Prop"), names_to = "Grupo", values_to = "Proporcion") %>% dplyr::filter(Grupo != "Prop_S") %>% mutate(Fecha = dmy("17-03-2020") + Time)

#ggplot(Results, aes(x = Time, y = Prop_I)) + geom_line() + facet_wrap(~Region) + theme_classic()

ggplot(Results2, aes(x = Fecha, y =Proporcion)) + geom_line(aes(color = Grupo)) + facet_wrap(~Region) + theme_classic()


Maxs <- Results2 %>% dplyr::filter(Grupo == "Prop_I") %>% dplyr::group_split(Region) %>% purrr::map(~dplyr::filter(.x, Proporcion == max(Proporcion))) %>% reduce(bind_rows)
