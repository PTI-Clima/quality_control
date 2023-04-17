setwd("C:/Users/SERGIO/Documents/Github/Datos_qc") # Set working directory
library(dplyr)

# Sacamos cuatro estaciones porque la fase de comparación con vecinos requiere de la 
# comparación de la estación con tres cercanas


# HUMEDAD RELATIVA

hum_relativa22 <- as.data.frame(
  data.table::fread("~/Github/Datos_qc/data/904_update_2022_auto/HumedadDESEMON.csv",
                    encoding = "Latin-1", 
                    sep = ";"))

unique(hum_relativa22$NOMBRE) # Nombres de todas las estaciones

hum_relativa22_muestra = hum_relativa22[hum_relativa22$NOMBRE == "LLUC" | 
                                          hum_relativa22$NOMBRE == "SÓLLER" |
                                          hum_relativa22$NOMBRE == "BANYALBUFAR" |
                                          hum_relativa22$NOMBRE == "ANDRATX - SANT ELM" |
                                          hum_relativa22$NOMBRE == "CALVIÀ, ES CAPDELLÀ" |
                                          hum_relativa22$NOMBRE == "PALMA-PUERTO", ] 

hum_relativa22_muestra %>% # 12260 entradas correspondientes a 6 estaciones y 366 días
  count(MES,DIA) %>% 
  nrow()

write.csv(hum_relativa22_muestra, 
          "hum_relativa22_muestra.csv",
          sep = ",",
          row.names = F)

rm(hum_relativa22)
rm(hum_relativa22_muestra)

# TEMPERATURA

temp22 <- as.data.frame(
  data.table::fread("~/Github/Datos_qc/data/904_update_2022_auto/TermoDESEMON.csv",
                    encoding = "Latin-1", 
                    sep = ";"))

unique(temp22$NOMBRE)

temp22_muestra = temp22[temp22$NOMBRE == "LLUC" |  
                          temp22$NOMBRE == "SÓLLER" |
                          temp22$NOMBRE == "BANYALBUFAR" |
                          temp22$NOMBRE == "ANDRATX - SANT ELM" |
                          temp22$NOMBRE == "CALVIA-EST.DEPURADORA S.PONSA" |
                          temp22$NOMBRE == "PALMA-PUERTO", ] 

temp22_muestra %>% # 6083 entradas correspondientes a 6 estaciones y 62 meses
  count(MES,AÑO) %>% 
  nrow()

write.csv(temp22_muestra, 
          "temp22_muestra.csv",
          sep = ",",
          row.names = F)
rm(temp22)
rm(temp22_muestra)


# INSOLACIÓN

ins_22 <- as.data.frame(
  data.table::fread("~/Github/Datos_qc/data/904_update_2022_auto/InsolacionDESEMON.csv",
                    encoding = "Latin-1", 
                    sep = ";"))

unique(ins_22$NOMBRE) # Sacamos los nombres de todas las estaciones y seleccionamos cuatro estaciones cercanas

ins22_muestra <- ins_22[ins_22$NOMBRE == "PALMA-PUERTO" | 
                          ins_22$NOMBRE == "PALMA DE MALLORCA/SON SAN JUAN" |
                          ins_22$NOMBRE == "PORTO COLOM" |
                          ins_22$NOMBRE == "SA POBLA SA CANOVA" |
                          ins_22$NOMBRE == "MENORCA/AEROPUERTO" |
                          ins_22$NOMBRE == "IBIZA/ES CODOLÁ", ] 

ins22_muestra %>% # 10328 entradas correspondientes a 6 estaciones y 1886 días
  count(AÑO,MES,DIA) %>% 
  nrow()

write.csv(ins22_muestra, 
          "ins22_muestra.csv",
          sep = ",",
          row.names = F)
rm(ins_22)
rm(ins22_muestra)

# VIENTO

viento_22 <- as.data.frame(
  data.table::fread("~/Github/Datos_qc/data/904_update_2022_auto/VientoDESEMON.csv",
                    encoding = "Latin-1", 
                    sep = ";"))

unique(viento_22$NOMBRE)

viento22_muestra = viento_22[viento_22$NOMBRE == "LLUC" |  
                               viento_22$NOMBRE == "SÓLLER" |
                               viento_22$NOMBRE == "BANYALBUFAR" |
                               viento_22$NOMBRE == "ANDRATX - SANT ELM" |
                               viento_22$NOMBRE == "CALVIÀ, ES CAPDELLÀ" |
                               viento_22$NOMBRE == "PALMA-PUERTO", ] 

viento22_muestra %>% # 9486 entradas correspondientes a 6 estaciones y 1886 días
  count(AÑO, MES, DIA) %>% 
  nrow()

write.csv(viento22_muestra, 
          "viento22_muestra.csv",
          sep = ",",
          row.names = F)

rm(viento_22)
rm(viento22_muestra)

# PRESION

presion_22 <- as.data.frame(
  data.table::fread("~/Github/Datos_qc/data/904_update_2022_auto/PresionDESEMON.csv",
                    encoding = "Latin-1", 
                    sep = ";"))

unique(presion_22$NOMBRE)

presion22_muestra = presion_22[presion_22$NOMBRE == "ANDRATX - SANT ELM" |  
                                 presion_22$NOMBRE == "PALMA-PUERTO" |
                                 presion_22$NOMBRE == "SON BONET, AEROPUERTO" |
                                 presion_22$NOMBRE == "PALMA DE MALLORCA/SON SAN JUAN" |
                                 presion_22$NOMBRE == "SANTANYÍ" |
                                 presion_22$NOMBRE == "PORTO COLOM", ] 

presion22_muestra %>% # 9355 entradas correspondientes a 4 estaciones y 1886 días
  count(AÑO, MES, DIA) %>% 
  nrow()

write.csv(presion22_muestra, 
          "presion22_muestra.csv",
          sep = ",",
          row.names = F)
rm(presion22_muestra)
rm(presion_22)

