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

#' Tratamiento de datos de ins, final de la revisión de datos diarios
#'
#' @param ins datos
#' @param metadatos_originales_ins metadatos_originales
#'
#' @return
#' @export
#'
contol_ins <- function(ins, metadatos_originales_ins) {
  ###########################################################
  ########### REFORMATEAMOS DATOS A FORMATO ANCHO ###########
  ###########################################################
  ins <- reformat(ins)
  
  save.data(ins, file = 'Duplicados/ins_sin_duplicados_formato_ancho.RData')
  
  # reformateamos los metadatos
  load.data('Duplicados/metadatos_duplicados_combinados_ins.RData')
  metadatos_duplicados_combinados <-
    reformat(as.data.frame(metadatos_duplicados_combinados))
  save.data(metadatos_duplicados_combinados, file = 'Duplicados/metadatos_duplicados_combinados_ins_formato_ancho.RData')
  rm(metadatos_duplicados_combinados)
  
  load.data('Originales/ins_meta.RData')
  metadatos_originales_ins_ancho <-
    reformat(as.data.frame(metadatos_originales_ins))
  save.data(metadatos_originales_ins_ancho, file = 'Originales/ins_meta.RData')
  rm(metadatos_originales_ins)
  
  ##################################################################
  ########### DETECTAMOS Y ELIMINAMOS DATOS CONSECUTIVOS ###########
  ##################################################################
  load.data(file = 'Duplicados/ins_sin_duplicados_formato_ancho.RData')
  
  detectar_consecutivos(ins, metadatos_originales_ins_ancho, var = C_INS)
  metadatos_consecutivos_ins <-
    max_mobil(ins,
              metadatos_originales_ins_ancho,
              C = 25,
              D = 20)
  rm(metadatos_consecutivos_ins)
  
  load.data(file = 'Consecutivos/Paso 1/Consecutivos_ins.RData')
  ins <- eliminar_datos(ins, metadatos_consecutivos)
  
  load.data(file = 'Consecutivos/Paso 2/Consecutivos_ins.RData')
  ins <- eliminar_datos(ins, metadatos_consecutivos)
  rm(metadatos_consecutivos)
  
  ##################################
  ########### ABERRANTES ###########
  ##################################
  metadatos_aberrantes <-
    deteccion_aberrantes(ins, metadatos_originales_ins_ancho, var = C_INS)
  rm(metadatos_originales_ins_ancho)
  ins <- eliminar_datos(ins, metadatos_aberrantes)
  rm(metadatos_aberrantes)
  save.data(ins, file = 'Aberrantes/Paso 1/ins_sin_aberrantes.RData')
  
  saveRDS(ins, file.path(dataOutFiles, "in_ok.rds"))
}

#' Preparación de los metadatos de ins para su guardado
#'
#' @return None
#' @export
#'
metadata_ins = function() {
  rename_metadata(C_INS)
  
  load.data(file = 'Consecutivos/Paso 1/Consecutivos_ins.RData')
  metadatos_consecutivos_0 <- metadatos_consecutivos
  
  load.data(file = 'Consecutivos/Paso 2/Consecutivos_ins.RData')
  load.data(file = 'Aberrantes/metadatos_aberrantes_ins.RData')
  metadatos_final_resto <-
    metadatos_consecutivos_0 + metadatos_consecutivos + metadatos_aberrantes
  save.data(metadatos_final_resto, file = 'Metadatos_finales/metadatos_resto_ins.RData')
  
  delete_metadata()
}
