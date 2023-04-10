# Author: Miguel Tomas Burguera <http://www.eead.csic.es/home/staffinfo?Id=459>; Erosion, and Soil and Water Evaluation , EEAD, CSIC <http://www.eead.csic.es>. Fergus Reig Gracia <http://fergusreig.es/>; Environmental Hydrology, Climate and Human Activity Interactions, Geoenvironmental Processes, IPE, CSIC <http://www.ipe.csic.es/hidrologia-ambiental/>
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

# Definir carpetas
# setwd("/mnt/dostb2/fuendetodos/DATOS/desemon_update_year/")

libraryf(Rcpp)
libraryf(compiler)

# Definir variables sobre las que trabajar y nombres de ficheros de "Control"
if (!exists("NAMES")) {
  NAMES = c()
  NAMES[C_T] = "Temp"
  NAMES[C_HR] = "HR"
  NAMES[C_W] = "W"
  NAMES[C_IN] = "Insolacion"
  NAMES[C_PR] = "lluvia"
  NAMES[C_R] = "Radiacion"
}

# Definir variables sobre las que trabajar y nombres de ficheros de "Unificar_metadatos"
NAMES2 = c()
NAMES2[C_T] = "T"
NAMES2[C_HR] = "HR"
NAMES2[C_W] = "W"
NAMES2[C_IN] = "SD"
NAMES2[C_PR] = NA
NAMES2[C_R] = "Ra"

# Elegir una variable
if (!exists("type")) {
  type = NAMES[6]
}

# Elegir año con el que trabajar, NA para trabajar con todos
if (!exists("year.new")) {
  year.new = NA
} #2016

# Ruta en la que está el código, en mi caso tengo todos los scripts en la misma carpeta
route.filesR = "code"
codeFiles = file.path(route.filesR, "controles_mtomas")

sourcef(file.path(codeFiles, 'Funciones_QC.R'))
sourcef(file.path(codeFiles, 'Funciones_QC_2.R'))
sourcef(file.path(codeFiles, 'Funciones_QC_1.R'))

# Ruta en la que están los datos de entrada
dataFiles = "data"

# Ruta en la que colocar las salidas
if (is.na(year.new)) {
  year.new.folder = "new_all"
} else{
  year.new.folder = year.new
}

if (!exists("data_source")) {
  data_source = C_AEMET #C_SIAR
}

if(data_source==C_AEMET){
  dataOutFiles = file.path(year.new.folder, "out_files")
}else{
  dataOutFiles = file.path(year.new.folder, paste("out_files", data_source, sep="_"))
}

#' Envuelve la función save de R, añadiendo una ruta a los ficheros a guardar; crea el directorio si es necesario
#'
#' @param ... parámetros que se pasarán a save
#' @param file nombre del fichero
#'
#' @return None
#' @export
#'
#' @examples
save.data = function(..., file) {
  newUrl = file.path(dataOutFiles, file)
  dir.create(dirname(newUrl),
             showWarnings = FALSE,
             recursive = TRUE)
  envir = parent.frame()
  save(..., envir = envir, file = newUrl)
}

#' Envuelve la función load de R, añadiendo una ruta a los ficheros a guardar
#'
#' @param file nombre del fichero
#'
#' @return None
#' @export
#'
#' @examples
load.data = function(file) {
  newUrl = file.path(dataOutFiles, file)
  envir = parent.frame()
  load(newUrl, envir = envir)
}

# Compilamos desde R: gcc simple.c -O2 -o simple
if (!exists("main_deteccion_duplicados")) {
  sourceCpp(file.path(codeFiles, "simple.cpp"))
}

#' Función que llama al código escrito por el gran mtomas
#'
#' @param type tipo
#'
#' @return None
#' @export
#'
#' @examples
launchControles = function(type) {
  sourcef(file.path(codeFiles, paste0('Controles_', type, '.R')))
  controles(var = type)
}
