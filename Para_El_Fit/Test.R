
#####

library(tidyverse)
library(sf)
library(lubridate)

### Elegir fecha inicial de simulacion

Inicio = dmy("09-03-2020")



Nombres <- c("Región de Arica y Parinacota", "Región de Tarapacá", "Región de Antofagasta", 
             "Región de Magallanes y Antártica Chilena", "Región de Aysén del Gral.Ibañez del Campo", 
             "Región de Atacama", "Región de Coquimbo", "Región de Valparaíso", 
             "Región Metropolitana de Santiago", "Región de Los Lagos", 
             "Región de Los Ríos", "Región de La Araucanía", "Región del Bío-Bío", 
             "Región de Ñuble", "Región del Maule", "Región del Libertador Bernardo O'Higgins"
)

githubURL <- ("https://raw.githubusercontent.com/itoledor/coronavirus/master/data/covid19_chile.rds")
download.file(githubURL,"covid19_chile.rds", method="curl")
Covid19_Chile <- readRDS("covid19_chile.rds") %>% dplyr::select(region, fecha, casos_totales) %>% dplyr::filter(!is.na(region), region != "Total")
Covid19_Chile$Region <- NA

for(i in 1:nrow(Covid19_Chile)){
  try(Covid19_Chile$Region[i] <- Nombres[str_detect(Nombres, pattern = Covid19_Chile$region[i])])
  print(i)
}

Covid19_Chile$Region <- ifelse(Covid19_Chile$region == "O’Higgins", "Región del Libertador Bernardo O'Higgins", Covid19_Chile$Region)
Covid19_Chile$Region <- ifelse(Covid19_Chile$region == "Biobío", "Región del Bío-Bío" , Covid19_Chile$Region)

Covid19_Chile <- Covid19_Chile %>% rename(Fecha = fecha) %>% dplyr::select(-region) %>% dplyr::filter(Fecha == Inicio) %>% rename(Infectados = casos_totales) %>% dplyr::select(-Fecha) %>% ungroup()

Regiones <- read_rds("Regiones2.rds") %>% dplyr::select(-Infectados) %>% left_join(Covid19_Chile) %>% ungroup()


Viajes_Regiones <- read_rds("Viajes_Regiones.rds")


#obtenemos todos los nombres de las regiones
Nombres <- Regiones$Region


#Generamos una lista de Probabilidades
Probs <-list()

#Hacemos el proceso que teníamos arriba para todas las regiones


for(i in 1:length(Nombres)){
  Probs[[i]] <- Viajes_Regiones %>% dplyr::filter(origen ==	Nombres[i]) %>% mutate(p = n_personas/sum(n_personas)) %>% dplyr::select(destino, p)
  colnames(Probs[[i]])[2] <- Nombres[i]
}

#Luego unimos todos estos dataframes con un full_join y remplazamos los na por 0 en caso de = origen destino
Probs <- Probs %>% reduce(full_join) %>% mutate_if(is.numeric, list(~replace_na(., 0)))


df_out <- Regiones %>% as.data.frame() %>% #Transformamos el sf en un data.frame lo hace más rápido
  select(Region, Poblacion, Adult, Over_65, Under_25, Infectados) %>%  #Seleccionamos las columnas necesarioas
  mutate(Infectados_Adult = round((Adult/Poblacion)*Infectados), Infectados_Over_65 = round((Over_65/Poblacion)*Infectados),Infectados_Under_25 = round((Under_25/Poblacion)*Infectados), Infectados_Adult = Infectados_Adult + (Infectados - (Infectados_Adult + Infectados_Under_25 + Infectados_Over_65))) %>%  #Calculamos los infectados por grupo etareo
  mutate(Suceptibles_Adult = Adult - Infectados_Adult, Suceptibles_Under_25 = Under_25 - Infectados_Under_25, Suceptibles_Over_65 = Over_65 - Infectados_Over_65, Expuestos_Adult = 0, Expuestos_Under_25 = 0, Expuestos_Over_65 = 0, Asintomaticos_Adult = 0, Asintomaticos_Under_25 = 0, Asintomaticos_Over_65 = 0, UCI_Adult = 0, UCI_Under_25 = 0, UCI_Over_65 = 0, Muerto_Adult = 0, Muerto_Under_25 = 0, Muerto_Over_65 = 0, Recuperados_Adult = 0, Recuperados_Under_25 = 0, Recuperados_Over_65 = 0, Cuarentenados_Adult = 0, Cuarentenados_Under_25 = 0, Cuarentenados_Over_65 = 0) %>%  #Agregamos el resto de las etapas
  mutate(Time = 1)


Under_25 <- df_out %>% dplyr::select(Region, Poblacion, contains("Under_25"), Time) %>% rename(Generacion = Under_25)

Adult <- df_out %>% dplyr::select(Region, Poblacion, contains("Adult"), Time) %>% rename(Generacion = Adult)

Over_65 <- df_out %>% dplyr::select(Region, Poblacion, contains("Over_65"), Time) %>% rename(Generacion = Over_65)

#Cambiamos los nombres de las columnas para que todos tengas cosas iguales

colnames(Under_25) <- str_remove_all(string = colnames(Under_25), pattern = "_Under_25")

colnames(Adult) <- str_remove_all(string = colnames(Adult), pattern = "_Adult")

colnames(Over_65) <- str_remove_all(string = colnames(Over_65), pattern = "_Over_65")

### Lo agregamos todo a una lista llamada df_out con los nombres de las edades


df_out <- list(Under_25, Adult, Over_65)

names(df_out) <- c("Under_25", "Adult", "Over_65")


###############
###############
#Parametros####
################

#Infectividad de asintomaticos
betaA = 0.06

#Infectividad de Infectados
betaI = 0.06

#numero de contactos promedio
K_g = c(11.8, 13.3, 6.6)

#Tasa latente
Eta = 1/2.34

#Tasa de infeccion asintomática
Alpha_g = c(1/5.06, 1/2.86, 1/2.86)

#Tasa de escape
Mu_g = c(1, 1/3.2, 1/3.2)

#Fracción de casos que requieren UCI
Gamma_g = c(0.002,0.05,0.36)

#Fatalidad en la UCI
Omega_g = 0.42

#Tasa de mortalidad

Psi_g = 1/7

# Tasa de salida de la UCI

Chi_g = 0.1

# Factor de densidad

Epsilon = 0.01

#Factor de movilidad

p_G = c(0.1, 1, 0.1)

###Tamaño promedio de hogar

Sigma = 2.5

## Tasa de confinamiento

K_0 = 0.35


## Matriz de contactos entre generaciones


C_G_H <- matrix(data = c(0.5980, 0.3849, 0.0171, 0.2440, 0.7210, 0.0350, 0.1919, 0.5705, 0.2376), nrow = 3, ncol = 3, byrow = T)



Mat  <- matrix(rep(0,(length(Nombres)*length(Nombres))), nrow = length(Nombres), ncol = length(Nombres))

colnames(Mat) <- Nombres
rownames(Mat) <- Nombres

Func <- function(x, epsilon = 0.01){
  y <- 1 + (1- exp(-epsilon*x))
  return(y)
}


k_g_c <- (1 - K_0)*K_g + K_0*(Sigma - 1)


#Para cada generacion
for(x in 1:3){
  #creamos la columna a llenar 
  df_out[[x]]$n_i_g_eff <- NA
  #Para cada region
  for(R in 1:nrow(df_out[[x]])){
    n_i_g_eff <- vector()
    for(i in 1:length(Nombres)){
      n_i_g_eff[i] <- ((1 - p_G[x])*ifelse(Nombres[i] == df_out[[x]][R,]$Region, 1, 0) + p_G[x]*Probs %>% dplyr::filter(destino == df_out[[x]][R,]$Region) %>% pull(Nombres[i]))*(df_out[[x]] %>% dplyr::filter(Region == Nombres[i]) %>% pull(Generacion))
    }
    df_out[[x]][R,]$n_i_g_eff <- sum(n_i_g_eff) 
  }
}


n_i_eff <- data.frame(Region = df_out$Adult$Region, n_i_eff = df_out[[1]]$n_i_g_eff + df_out[[2]]$n_i_g_eff+ df_out[[3]]$n_i_g_eff)

z_g <- vector()

for(x in 1:length(df_out)){
  z_g[x] <- (df_out[[x]] %>% summarise(N = sum(Generacion)) %>% pull(N))/sum(Func(n_i_eff$n_i_eff/(df_out[[1]]$Suceptibles + df_out[[2]]$Suceptibles + df_out[[3]]$Suceptibles))*df_out[[x]]$n_i_g_eff)
}

##########################################
###########################################
####Loop debiera partir desde acá##########
###########################################
###########################################

Results <- list()

Results[[1]] <- df_out %>% bind_rows() %>% dplyr::select(-Poblacion, -n_i_g_eff) %>% rename(Poblacion = Generacion) %>% group_by(Region) %>% summarise_if(is.numeric, sum) %>% mutate(Time = Time/3)

Dias <- 30

Dia_imp <- 30

for(d in 2:Dias){
  
  N_I_h_j_i <- list(Mat, Mat, Mat)
  names(N_I_h_j_i) <- c("Under_25", "Adult", "Over_65")
  
  N_A_h_j_i <- list(Mat, Mat, Mat)
  names(N_A_h_j_i) <- c("Under_25", "Adult", "Over_65")
  
  
  for(x in 1:length(N_I_h_j_i)){
    for(i in 1:length(Nombres)){
      for(j in 1:length(Nombres)){
        N_I_h_j_i[[x]][j,i] <- (df_out[[x]] %>% dplyr::filter(Region == Nombres[j]) %>% pull(Generacion))*((df_out[[x]] %>% dplyr::filter(Region == Nombres[j]) %>% pull(Infectados))/(df_out[[x]] %>% dplyr::filter(Region == Nombres[j]) %>% pull(Generacion)))*((1-p_G[x])*0 + p_G[x]*Probs %>% dplyr::filter(destino == Nombres[i]) %>% pull(Nombres[j]))
      }
    }
  }
  
  for(x in 1:length(N_A_h_j_i)){
    for(i in 1:length(Nombres)){
      for(j in 1:length(Nombres)){
        N_A_h_j_i[[x]][j,i] <- (df_out[[x]] %>% dplyr::filter(Region == Nombres[j]) %>% pull(Generacion))*((df_out[[x]] %>% dplyr::filter(Region == Nombres[j]) %>% pull(Asintomaticos))/(df_out[[x]] %>% dplyr::filter(Region == Nombres[j]) %>% pull(Generacion)))*((1-p_G[x])*0 + p_G[x]*Probs %>% dplyr::filter(destino == Nombres[i]) %>% pull(Nombres[j]))
      }
    }
  }
  

  
  for(g in 1:3){
    df_out[[g]]$P_G <- NA
    Temp2 <- list()
    for(h in 1:3){
      temp <- list()
      for(j in 1:16){
        temp[[j]] <- (1 - betaA)^(z_g[g]*K_g[g]*Func(x = n_i_eff$n_i_eff/(df_out[[1]]$Suceptibles + df_out[[2]]$Suceptibles + df_out[[3]]$Suceptibles))*C_G_H[g,h]*(N_A_h_j_i[[h]][j,]/df_out[[h]]$n_i_g_eff))*(1 - betaI)^(z_g[g]*K_g[g]*Func(x = n_i_eff$n_i_eff/(df_out[[1]]$Suceptibles + df_out[[2]]$Suceptibles + df_out[[3]]$Suceptibles))*C_G_H[g,h]*(N_I_h_j_i[[h]][j,]/df_out[[h]]$n_i_g_eff))
      }
      Temp2[[h]] <- reduce(temp, `*`)
    }
    
    df_out[[g]]$P_G <- 1 - reduce(Temp2, `*`)
  }
  
  for(g in 1:3){
    df_out[[g]]$PI = NA
    for(i in 1:nrow(Regiones)){
      df_out[[g]]$PI[i] <- p_G[g]*(df_out[[g]] %>% dplyr::filter(Region == Nombres[i]) %>% pull(P_G)) + p_G[g]*sum((Probs %>% dplyr::filter(destino == Nombres[i]) %>% select_if(is.numeric) %>% reduce(c))*df_out[[g]]$P_G)
    }
  }
  
  
  ###
  
  if (d == Dia_imp){
    Num <- list()
    Denom <- list()
    for(g in 1:3){
      Num[[g]] <- ((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion) + (df_out[[g]]$Recuperados/df_out[[g]]$Generacion))*df_out[[g]]$Generacion
      Denom [[g]]<- sum(df_out[[g]]$Generacion)
    }
     CH_i_tc <- reduce(Num, `+`)/reduce(Denom, `+`)^Sigma
     p_G <- (1 - K_0)*p_G
     
     #Para cada generacion
     for(x in 1:3){
       #creamos la columna a llenar 
       df_out[[x]]$n_i_g_eff <- NA
       #Para cada region
       for(R in 1:nrow(df_out[[x]])){
         n_i_g_eff <- vector()
         for(i in 1:length(Nombres)){
           n_i_g_eff[i] <- ((1 - p_G[x])*ifelse(Nombres[i] == df_out[[x]][R,]$Region, 1, 0) + p_G[x]*Probs %>% dplyr::filter(destino == df_out[[x]][R,]$Region) %>% pull(Nombres[i]))*(df_out[[x]] %>% dplyr::filter(Region == Nombres[i]) %>% pull(Generacion))
         }
         df_out[[x]][R,]$n_i_g_eff <- sum(n_i_g_eff) 
       }
     }
     
     
     n_i_eff <- data.frame(Region = df_out$Adult$Region, n_i_eff = df_out[[1]]$n_i_g_eff + df_out[[2]]$n_i_g_eff+ df_out[[3]]$n_i_g_eff)
     
     z_g <- vector()
     
     for(x in 1:length(df_out)){
       z_g[x] <- (df_out[[x]] %>% summarise(N = sum(Generacion)) %>% pull(N))/sum(Func(n_i_eff$n_i_eff/(df_out[[1]]$Suceptibles + df_out[[2]]$Suceptibles + df_out[[3]]$Suceptibles))*df_out[[x]]$n_i_g_eff)
     }
     
     
     print("Empiezan medidas")
  }
  
  
  Temp1 <- df_out
  for(g in 1:3){
    if(d != Dia_imp){
      Temp1[[g]]$Suceptibles <- df_out[[g]]$Generacion*((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*(1-df_out[[g]]$PI))
      Temp1[[g]]$Expuestos <- df_out[[g]]$Generacion*(((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*df_out[[g]]$PI) + (1 - Eta)*(df_out[[g]]$Expuestos/df_out[[g]]$Generacion))
    }
    if(d == Dia_imp){
      Temp1[[g]]$Suceptibles <- df_out[[g]]$Generacion*((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*(1-df_out[[g]]$PI)*(1 - K_0*CH_i_tc))
      Temp1[[g]]$Expuestos <- df_out[[g]]$Generacion*(((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*df_out[[g]]$PI) + (1 - Eta)*(df_out[[g]]$Expuestos/df_out[[g]]$Generacion))
      Temp1[[g]]$Cuarentenados <- df_out[[g]]$Generacion*((df_out[[g]]$Suceptibles/df_out[[g]]$Generacion)*(1 - K_0*CH_i_tc)) 
    }
    
    Temp1[[g]]$Asintomaticos <- df_out[[g]]$Generacion*((Eta*(df_out[[g]]$Expuestos/df_out[[g]]$Generacion)) + (1 - Alpha_g[g])*(df_out[[g]]$Asintomaticos/df_out[[g]]$Generacion))
    Temp1[[g]]$Infectados <- df_out[[g]]$Generacion*((Alpha_g[g]*(df_out[[g]]$Asintomaticos/df_out[[g]]$Generacion)) + (1 - Mu_g[g])*(df_out[[g]]$Infectados/df_out[[g]]$Generacion))
    Temp1[[g]]$UCI <-  df_out[[g]]$Generacion*(Mu_g[g]*Gamma_g[g]*(df_out[[g]]$Infectados/df_out[[g]]$Generacion) + Omega_g*(1 - Psi_g)*(df_out[[g]]$UCI/df_out[[g]]$Generacion) + (1 - Omega_g)*(1 - Chi_g)*(df_out[[g]]$UCI/df_out[[g]]$Generacion))
    Temp1[[g]]$Muerto <-  df_out[[g]]$Generacion*(Omega_g*Psi_g*(df_out[[g]]$UCI/df_out[[g]]$Generacion) + (df_out[[g]]$Muerto/df_out[[g]]$Generacion))
    Temp1[[g]]$Recuperados <-  df_out[[g]]$Generacion*(Mu_g[g]*(1 - Gamma_g[g])*(df_out[[g]]$Infectados/df_out[[g]]$Generacion) + (1 - Omega_g)*Chi_g*(df_out[[g]]$UCI/df_out[[g]]$Generacion) + (df_out[[g]]$Recuperados/df_out[[g]]$Generacion))
  }
  
  df_out <- Temp1
  Results[[d]] <- df_out %>% bind_rows() %>% dplyr::select(-Poblacion, -n_i_g_eff, -P_G, -PI) %>% rename(Poblacion = Generacion) %>% group_by(Region) %>% summarise_if(is.numeric, sum) %>% mutate(Time = d)
  print(paste("Día", d, "de", Dias, "Listos!", Sys.time()))
  
}

Results <- bind_rows(Results) %>% mutate(Fecha = Inicio + (Time-1))

saveRDS(Results, "Results.rds")
#Results <- readRDS("Results.rds")

Maxs <- Results %>% group_by(Region) %>%  dplyr::filter(UCI == max(UCI)) %>% mutate(Prop_UCI = UCI/Poblacion) %>% dplyr::select(Region, UCI, Prop_UCI, Fecha)  %>% arrange(Fecha)


#ggplot(Results, aes(x = Fecha, y = UCI)) + geom_line() + facet_wrap(~Region, scales = "free_y") + theme_bw() + geom_vline(xintercept = dmy("14-03-2020") + Dia_imp, color = "red")

Dia_imp <- Inicio + Dia_imp


Results2 <- Results %>% dplyr::select(-Time, -Poblacion, -Suceptibles, -Recuperados) %>% pivot_longer(cols = c("Infectados", "Expuestos", 
                                                                                    "Asintomaticos", "UCI", "Muerto"), names_to = "Grupo", values_to = "Numero")  
ggplot(Results2, aes(x = Fecha, y = Numero)) + geom_area(aes(fill = Grupo)) + facet_wrap(~Region, scales = "free_y") + theme_classic()  + geom_vline(xintercept = Dia_imp)


githubURL <- ("https://raw.githubusercontent.com/itoledor/coronavirus/master/data/covid19_chile.rds")
download.file(githubURL,"covid19_chile.rds", method="curl")
Covid19_Chile <- readRDS("covid19_chile.rds") %>% dplyr::select(region, fecha, casos_totales) %>% dplyr::filter(!is.na(region), region != "Total")
Covid19_Chile$Region <- NA

for(i in 1:nrow(Covid19_Chile)){
  try(Covid19_Chile$Region[i] <- Nombres[str_detect(Nombres, pattern = Covid19_Chile$region[i])])
  print(i)
}

Covid19_Chile$Region <- ifelse(Covid19_Chile$region == "O’Higgins", "Región del Libertador Bernardo O'Higgins", Covid19_Chile$Region)
Covid19_Chile$Region <- ifelse(Covid19_Chile$region == "Biobío", "Región del Bío-Bío" , Covid19_Chile$Region)

Covid19_Chile <- Covid19_Chile %>% rename(Fecha = fecha) %>% dplyr::select(-region) %>% left_join(Results) %>% dplyr::select(Region, Fecha, casos_totales, Infectados) %>% rename(Observado = casos_totales, Modelado = Infectados) 

RMSE <- Covid19_Chile %>% dplyr::filter(!is.na(Modelado)) %>% mutate(sqer = (Modelado - Observado)^2) %>% summarise(rmse = sqrt(mean(sqer))) %>% pull(rmse)

RMSE_Region <- Covid19_Chile %>% dplyr::filter(!is.na(Modelado)) %>% mutate(sqer = (Modelado - Observado)^2) %>% group_by(Region)%>% summarise(rmse = sqrt(mean(sqer)))

Covid19_Chile <-Covid19_Chile %>% pivot_longer(cols = c(Observado, Modelado))

ggplot(Covid19_Chile, aes(x = Fecha, y = value)) + geom_point(aes(color = name)) + geom_line(aes(color = name)) + facet_wrap(~Region, scales = "free_y")
