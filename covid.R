require(dplyr)

# S son suceptibles
# I son infectados
# R son recuperados

# expo es la tasa de exposicion
# out son la tasas de migracion por ejemplo out12 es de la ciudad 2 a la 1
# beta es la tasa de contagio
# gamma es la tasa de recuperacion

beta = 1
gamma = 0.1
out12= 0.2 
out13= 0.1
out21 = 0
out23 = 0
out31 = 0.3
out32 = 0

outs = c((1-out21- out31), out12, out13,out21, (1-out12-out32), out23, out31, out32, (1-out13-out23))  
time = 50

conMat = matrix(data=outs, nrow = 3) #crea matriz de conectividad con las tasas de migracion de suceptibles
colnames(conMat) =  rownames(conMat) = c("city1", "city2", "city3")

#initial pop sizes
city1 = c(10, 1, 0)
city2 = c(10, 1, 0)
city3 = c(10, 3, 0)

df_out = matrix(data=0, ncol = 3*ncol(conMat), nrow=(time+1))
df_out[1,] <- c(city1, city2, city3)
colnames(df_out) = c(paste0(rep(c("S", "I", "R"), time=3), rep(1:3, each=3)))


for (t in 1:time) {
  S21out = df_out[t,1]*conMat[1,2]
  S31out = df_out[t,1]*conMat[1,3]
  S12out = df_out[t,4]*conMat[2,1]
  S32out = df_out[t,4]*conMat[2,3]
  S13out = df_out[t,7]*conMat[3,1]
  S23out = df_out[t,7]*conMat[3,2]
  cont1 = round(beta*(df_out[t,1]*conMat[1,1]-S21out- S31out + S12out +S13out)*df_out[t,2]/(sum(city1)-S21out- S31out + S12out +S13out))
  rec1 = round(gamma*df_out[t,2])
  cont2 = round(beta*(df_out[t,4]*conMat[2,2]-S12out- S32out + S21out +S23out)*df_out[t,5]/(sum(city2)-S12out- S32out + S21out +S23out))
  rec2 = round(gamma*df_out[t,5])
  cont3 = round(beta*(df_out[t,7]*conMat[3,3]-S23out- S13out + S32out +S31out)*df_out[t,8]/(sum(city3)-S23out- S13out + S32out +S31out))
  rec3 = round(gamma*df_out[t,8])
  
  df_out[(t+1),1] = df_out[t,1] - cont1 - S21out - S31out + S12out+ S13out #S1
  df_out[(t+1),2] = df_out[t,2] + cont1 - rec1  #I1 
  df_out[(t+1),3] = df_out[t,3] + rec1          #R1
  
  df_out[(t+1),4] = df_out[t,4] - cont2 - S12out - S32out + S21out+ S23out        #S2
  df_out[(t+1),5] = df_out[t,5] + cont2 - rec2  #I2 
  df_out[(t+1),6] = df_out[t,6] + rec2          #R2
  
  df_out[(t+1),7] = df_out[t,7] - cont3 - - S23out - S13out + S31out+ S32out       #S3
  df_out[(t+1),8] = df_out[t,8] + cont3 - rec3  #I3 
  df_out[(t+1),9] = df_out[t,9] + rec3          #R3
  df_out[df_out < 0] = 0
}

View
df_out
