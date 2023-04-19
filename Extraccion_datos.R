setwd("C:/Users/SERGIO/Documents/Github/Datos_qc") # Set working directory
library(dplyr)

# Sacamos 6 estaciones porque la fase de comparación con vecinos requiere de la 
# comparación de la estación con otras 5 cercanas


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

presion22_muestra %>% # 9355 entradas correspondientes a 6 estaciones y 1886 días
  count(AÑO, MES, DIA) %>% 
  nrow()

write.csv(presion22_muestra, 
          "presion22_muestra.csv",
          sep = ",",
          row.names = F)
rm(presion22_muestra)
rm(presion_22)

# PLUVIO

pluvio_22 <- as.data.frame(
  data.table::fread("~/Github/Datos_qc/data/904_update_2022_auto/PluvioDESEMON.csv",
                    encoding = "Latin-1", 
                    sep = ";"))

unique(pluvio_22$NOMBRE)

pluvio22_muestra = pluvio_22[pluvio_22$NOMBRE == "POLLENSA      TORRE ARIANT" |  
                                 pluvio_22$NOMBRE == "PORT DE SOLLER SA TALAIA" |
                                 pluvio_22$NOMBRE == "SÓLLER" |
                                 pluvio_22$NOMBRE == "FORNALUTX (ES MARROIG)" |
                                 pluvio_22$NOMBRE == "SOLLER (CONVENTO)" |
                                 pluvio_22$NOMBRE == "SOLLER (SA VINYASSA)" |
                                 pluvio_22$NOMBRE == "PALMA-PUERTO" |
                                 pluvio_22$NOMBRE == "LLUC", ] 
pluvio22_muestra %>% # 3814 entradas correspondientes a 8 estaciones y 62 días
  count(AÑO, MES) %>% 
  nrow()

write.csv(pluvio22_muestra, 
          "pluvio22_muestra.csv",
          sep = ",",
          row.names = F)
rm(pluvio22_muestra)
rm(pluvio_22)

# RADIACIÓN

radiacion_22 <- as.data.frame(
  data.table::fread("~/Github/Datos_qc/data/904_update_2022_auto/RadiacionDESEMON.csv",
                    encoding = "Latin-1", 
                    sep = ";"))

unique(radiacion_22$NOMBRE)

rad22_muestra = radiacion_22[radiacion_22$NOMBRE == "LANZAROTE/AEROPUERTO" |  
                               radiacion_22$NOMBRE == "FUERTEVENTURA/AEROPUERTO" |
                               radiacion_22$NOMBRE == "TENERIFE/SUR" |
                               radiacion_22$NOMBRE == "IZAÑA" |
                               radiacion_22$NOMBRE == "TENERIFE/LOS RODEOS" |
                               radiacion_22$NOMBRE == "STA.CRUZ DE TENERIFE" |
                               radiacion_22$NOMBRE == "SAN BARTOLOME TIRAJANA-C.INSULAR TURISMO", ]

rad22_muestra %>% # 14196 entradas correspondientes a 8 estaciones y 1857 días
  count(AÑO, MES, DIA) %>% 
  nrow()

write.csv(rad22_muestra, 
          "rad22_muestra.csv",
          sep = ",",
          row.names = F)

#----------------- PRUEBA DE LOS CONTROLES INTERNOS: REPETICIÓN Y CEROS ------------- #

hum_relativa22 <- as.data.frame(
  data.table::fread("~/Github/quality_control/hum_relativa22_muestra.csv",
                    encoding = "Latin-1", 
                    sep = ","))

# Convertimos los valores de la estación de LLUC en una repetición del mismo valor (85)

hum_relativa22$HU13[hum_relativa22$NOMBRE == "LLUC"] <- 85

# Introducimos ceros sustituyendo al número 91 en HU07 y HU18
hum_relativa22 %>% 
  filter(HU07 == 91) %>% 
  count() # hay 211 datos a esas horas con el índice a 91

hum_relativa22 %>% 
  filter(HU18 == 87) %>% 
  count() # hay 265 datos a esas horas con el índice a 91

hum_relativa22$HU07[hum_relativa22$HU07 == 91] <- 0
hum_relativa22$HU18[hum_relativa22$HU18 == 87] <- 0

write.csv(hum_relativa22, 
          "hum_muestra_controles_internos.csv",
          sep = ";",
          row.names = F,
          )


#----------------- PRUEBA DE LOS CONTROLES INTERNOS: CEROS ------------- #
# Convertimos los valores de temperatura máxima de tres
# horas en 0.

temp22_muestra$TMAX1[temp22_muestra$TMAX1 == 311] <- 0
temp22_muestra$TMAX13[temp22_muestra$TMAX13 == 100] <- 0
temp22_muestra$TMAX18[temp22_muestra$TMAX18 == 127] <- 0

write.csv(temp22_muestra, 
          "temp_muestra_controles_internos.csv",
          sep = ";",
          row.names = F,
)

# ----------- INSOLACIÓN MUESTRA ALEATORIA --------------- #
set.seed(123)
muestra <- sample(1:nrow(ins_22), size = 10000, replace = F)
df <- ins_22[muestra,]
row.names(df) <- NULL
write.csv(df, 
          "insolacion_muestra_aleatoria.csv",
          row.names = F,
)

# ---------- HUMEDAD MUESTRA ALEATORIA ------------------- # 
set.seed(123)
muestra <- sample(1:nrow(hum_relativa22), size = 10000, replace = F)
df <- hum_relativa22[muestra,]
row.names(df) <- NULL
write.csv(df, 
          "hr_muestra_aleatoria.csv",
          row.names = F,
)
write.table(hum_relativa22, file = "hr_muestra_aleatoria2.csv", 
            sep = ";", col.names = TRUE, row.names = FALSE)

# ---------- TEMPERATURA MUESTRA ALEATORIA ------------------- # 
set.seed(123)
muestra <- sample(1:nrow(temp22), size = 10000, replace = F)
df <- temp22[muestra,]
row.names(df) <- NULL
write.csv(df, 
          "temp_muestra_aleatoria.csv",
          row.names = F,
)

# ----------------- VIENTO MUESTRA ALEATORIA -------------------- #

set.seed(123)
muestra <- sample(1:nrow(viento_22), size = 10000, replace = F)
df <- viento_22[muestra,]
row.names(df) <- NULL
write.csv(df, 
          "viento_muestra_aleatoria.csv",
          row.names = F,
)

# --------------- PRESIÓN MUESTRA ALEATORIA ------------------- # 
set.seed(123)
muestra <- sample(1:nrow(presion_22), size = 10000, replace = F)
df <- presion_22[muestra,]
row.names(df) <- NULL
write.csv(df, 
          "presion_muestra_aleatoria.csv",
          row.names = F,
)

# --------------- PLUVIO MUESTRA ALEATORIA ---------------- #
set.seed(123)
muestra <- sample(1:nrow(pluvio_22), size = 10000, replace = F)
df <- pluvio_22[muestra,]
row.names(df) <- NULL
write.csv(df, 
          "pluvio_muestra_aleatoria.csv",
          row.names = F,
)

# ---------------- RADIACIÓN MUESTRA ALEATORIA ----------------- #
set.seed(123)
muestra <- sample(1:nrow(radiacion_22), size = 10000, replace = F)
df <- radiacion_22[muestra,]
row.names(df) <- NULL
write.csv(df, 
          "radiacion_muestra_aleatoria.csv",
          row.names = F,
)

hum_relativa22 <- as.data.frame(
  data.table::fread("~/Github/datos_qc/hr_muestra_aleatoria2.csv",
                    encoding = "Latin-1", 
                    sep = ";"))

