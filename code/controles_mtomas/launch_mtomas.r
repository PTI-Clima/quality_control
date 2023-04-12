# Author: Fergus Reig Gracia <http://fergusreig.es/>; Environmental Hydrology, Climate and Human Activity Interactions, Geoenvironmental Processes, IPE, CSIC <http://www.ipe.csic.es/hidrologia-ambiental/>
# Version: 1.0

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/> <http://www.gnu.org/licenses/gpl.txt/>.
#####################################################################

# No mostrar warnings al compilar
Sys.setenv("PKG_CXXFLAGS" = "-w")

suppressPackageStartupMessages(library(compiler))
compilePKGS(TRUE)
enableJIT(3)

# Eliminar error "reached elapsed time limit" "llegado el tiempo límite"
setTimeLimit(cpu = Inf,
             elapsed = Inf,
             transient = FALSE)
setSessionTimeLimit(cpu = Inf, elapsed = Inf)
options(error = quote(dump.frames("testdump", TRUE)))

setwd("/mnt/dostb3/descargas/daily_correction") # MODIFICAR: ELIMINAR NECESIDAD DE DIRECTORIO

##################################################################################

route.filesR = "code"

C_TMAX <- "tmax" #temperatura máxima
C_TMIN <- "tmin" #temperatura mínima
C_W <- "w" #velocidad viento
C_HR <- "hr" #humedad relativa
C_PR <- "pr" #precipitación
C_IN <- "in" #insolación
C_TD <- "td" #temperatura de rocío
C_RA <- "ra" #radiacion
C_P <- "p" #presion

# Otros nombres, por código asimilado de mtomas
C_T <- "t"  #temperatura
C_R <- "r" #radiación
C_MAX <- "max" #temperatura máxima
C_MIN <- "min" #temperatura mínima
C_INS <- "ins" #insolación
C_PP <- "pp" #precipitación

DS_ALL <- "all"
DS_TRI <- "tri"

C_AEMET = "AEMET"
C_SIAR = "SIAR"

##################################################################################
###UNICA CLASE QUE LLAMA A LOS FICHEROS DE controles_mtomas###
suppressPackageStartupMessages(library(R6))
ControlesMtomas <- R6Class('ControlesMtomas', )

#' Revisión de los datos diarios de un tipo, código de mtomas
#'
#' @param type tipo de datos (tmin, pr...)
#' @export
#' @examples
ControlesMtomas$controles = function(type){
  source(file.path(route.filesR, "controles_mtomas", "Controles.R"))
  launchControles(type)
}

##################################################################################

#' Cambio de unidades de las variables en las que es necesario
#'
#' @param series matriz de datos
#' @param type tipo de datos (tmin, pr...)
#'
#' @return matriz de datos
#' @export
#'
#' @examples
correctUnits = function(series, type) {
  if (type == C_IN | type == C_PR) {
    series <- series / 10
  } else{
    if (type == C_W) { #Entrada en km/h a la altura de 10 y salida en m/s a la altura de 2 metros
      series <- 0.75 * ((series * 1000) / 3600)
      # series <- series/10
    } else{
      if (type == C_TMIN || type == C_TMAX) {
        series <- series / 10
      }
    }
  }
  return(series)
}

#' Directorio principal de los ficheros en función de la fecha que se calcula (todo el periodo, un año o un día)
#'
#' @param year.new una fecha, un día o NA (todo el periodo)
#'
#' @return directorio
#' @export
#'
#' @examples
year.new.dir.calc = function(year.new) {
  if (!is.na(year.new)) {
    if(nchar(year.new) <= 4){ # year
      year.new.dir = year.new
    }else{ # day
      year.new.dir = "new_all"
    }
  } else{ # NA
    year.new.dir = "new_all"
  }
  return(year.new.dir)
}

#' crea una sequencia entre from y to, solo si from es menor
#'
#' @param from valor
#' @param to valor
#'
#' @return valor
#' @export
#'
#' @examples
seqf = function(from = 1, to = 1) {
  if (to < from) {
    return(NULL)
  } else{
    return(c(from:to))
  }
}

# 1. Leer los nuevos datos (para mtomas)

#' Lee la salida de los algoritmos de correción de mtomas y lo pasa al formato y las unidades necesarias para el resto de cálculos
#'
#' @param name.type Tipo (w, tmax...)
#'
#' @return None
#' @export
#'
#' @examples
prepare.data = function(name.type) {
  # Nombre del fichero que guarda las coordenadas coords_NAMES5.RData (función distancias de Funciones_QC.R)
  NAMES5 = c()
  NAMES5[C_TMAX] = C_T
  NAMES5[C_TMIN] = C_T
  NAMES5[C_HR] = C_HR
  NAMES5[C_W] = C_W
  NAMES5[C_IN] = C_INS
  NAMES5[C_PR] = C_PP
  NAMES5[C_RA] = C_RA #radiacion
  NAMES5[C_P] = C_P #presion
  NAMES5[C_R] = C_RA

  fileOk = file.path(dataOutFiles, paste0(name.type, "_ok.rds"))
  if(!file.exists(fileOk)){
    return(NA)
  }
  all.data = readRDS(fileOk)

  date = paste(all.data[, "MES"], all.data[, "DIA"], all.data[, "YEAR"], sep = "/")
  date = chron(
    date,
    format = c(dates = "m/d/y", times = "h:m:s"),
    out.format = c(dates = "d/m/yy", times = "h:m:s")
  )
  date.aux = seq.dates(from = date[1], to = date[length(date)])
  if (sum(date.aux != date)) {
    printMutex$print("Error, faltan o sobran fechas")
  }
  date = as.character(date)
  data.daily = all.data[, -c(1:3)]
  rownames(data.daily) = date
  colnames(data.daily) = colnames(all.data[4:dim(all.data)[2]])

  load.data(paste0("coords_", NAMES5[name.type], ".RData"))

  coor.names = c("INDICATIVO",
                 "NOMBRE",
                 "ALTITUD",
                 "C_X",
                 "C_Y",
                 "LONGITUD",
                 "LATITUD",
                 "NOM_PROV")
  coor = coords[, coor.names]
  coor = transform(coor, INDICATIVO = as.character(INDICATIVO))
  coor = transform(coor, NOMBRE = as.character(NOMBRE))
  coor = transform(coor, ALTITUD = as.character(ALTITUD))
  coor = transform(coor, C_X = as.character(C_X))
  coor = transform(coor, C_Y = as.character(C_Y))
  coor = transform(coor, LONGITUD = as.character(LONGITUD))
  coor = transform(coor, LATITUD = as.character(LATITUD))
  coor = transform(coor, NOM_PROV = as.character(NOM_PROV))

  folder = file.path(as.character(year.new.dir.calc(year.new)), data_source)
  dir.create(folder, showWarnings = FALSE)
  dir.create(file.path(folder, "data_sort"), showWarnings = FALSE)
  dir.create(file.path(folder, "data_coor"), showWarnings = FALSE)
  data.daily = correctUnits(data.daily, name.type)

  # Eliminar 'de los nombres para evitar problemas en lecturas (Se pierden datos en lectura sin avisar según como se lea)
  coor[, "NOMBRE"] = gsub("'", " ", as.character(coor[, "NOMBRE"]))

  write.table(
    data.daily,
    file = file.path(folder, "data_sort", paste0(name.type, ".csv")), sep = ";")
  write.table(
    coor,
    file = file.path(
      folder,
      "data_coor",
      paste0(name.type, ".csv")
    ),
    sep = ";",
    row.names = FALSE
  )
}

#' Revisión y cambio al formato correcto de los datos diarios de un tipo, código de mtomas
#'
#' @param type tipo de datos (tmin, pr...)
#' @export
#' @examples
#' readOriData(type)
readOriData = function(type) {
  #Cargamos de nuevo las funciones y variables para no tener que pasarlas a Snowfall
  ControlesMtomas$controles(type = type)

  if (type == C_T) {
    prepare.data(name.type = C_TMAX)
    prepare.data(name.type = C_TMIN)
  } else {
    prepare.data(name.type = type)
  }
}

#' Revisión de los datos diarios, código de mtomas
#'
#' @return None
#' @export
#' @examples
#' controlesmtomas()
controlesmtomas = function() {
  NAMES = c(C_T, C_HR, C_W, C_IN, C_PR, C_R)
  type = NAMES[length(NAMES)]
  for (type in NAMES) {
    print(type)
    readOriData(type)
  }
}

# 1. Leer los nuevos datos
##################################################################################

data_source = C_AEMET
controlesmtomas()
data_source = C_SIAR
controlesmtomas()
