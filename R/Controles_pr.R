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

#' Tratamiento de datos de pp, final de la revisión de datos diarios
#'
#' @param Pre
#' @param metadatos_originales_pre
#' @param dist
#'
#' @return
#'
#'
contol_pp <- function(Pre, metadatos_originales_pre, dist) {
  ## Transformamos los -3 a 0 y eliminamos los -4 y su lluvia posterior
  Pre[Pre == -3] <- 0
  datos_Pre <- special_character(Pre)
  metadatos_menos_4 <- datos_Pre$meta
  save.data(metadatos_menos_4, file = 'metadatos_menos_4_eliminados.RData')
  rm(metadatos_menos_4)
  Pre <- datos_Pre$data
  rm(datos_Pre)
  save.data(Pre, file = 'Originales/pp_sin_menos_4.RData')
  
  meses_duplicados(datos = Pre,
                   metadatos = metadatos_originales_pre,
                   dist = dist,
                   var = C_PP,
                   pasada = 2, data_source = data_source)
  
  decenas_duplicadas(Pre,
                     metadatos_originales_pre,
                     var = C_PP,
                     pasada = 2, data_source = data_source)
  
  intradecenas_duplicadas(Pre,
                          metadatos_originales_pre,
                          var = C_PP,
                          pasada = 2, data_source = data_source)
  
  offset_duplicated_rain(Pre, metadatos_originales_pre, pasada = 2)
  
  ## este control solo tiene una pasada
  n_dias_duplicados(data=Pre, meta=metadatos_originales_pre)
  
  agrupar_metadatos_duplicados(metadatos_originales_pre,
                               var = C_PP,
                               pasada = 2)
  
  ###################################
  #### ELIMINAR DATOS DUPLICADOS ####
  ###################################
  load.data(file = 'Duplicados/metadatos_duplicados_combinados_pp_pasada2.RData')
  Pre[metadatos_duplicados_combinados == 2] <- NA
  Pre[, 1:3] <- metadatos_duplicados_combinados[, 1:3]
  save.data(Pre, file = 'Duplicados/pp_sin_duplicados.RData')
  
  ######################################
  #### REFORMATEAR DATOS Y METADATOS####
  ######################################
  
  Pre <- reformat(Pre)
  metadatos_duplicados_combinados_formato_ancho <-
    reformat(metadatos_duplicados_combinados)
  rm(metadatos_duplicados_combinados)
  metadatos_originales_pre <- reformat(metadatos_originales_pre)
  
  save.data(metadatos_duplicados_combinados_formato_ancho, file = 'Duplicados/metadatos_duplicados_final_ancho.RData')
  save.data(metadatos_originales_pre, file = 'Originales/metadatos_originales_formato_ancho.RData')
  save.data(Pre, file = 'Duplicados/pp_formato_ancho.RData')
  rm(metadatos_duplicados_combinados_formato_ancho)
  #########################################
  #### DETECCION DE DATOS CONSECUTIVOS ####
  #########################################
  ## Se permiten 7 días de lluvia idéntica inferior a 1mm
  ## Se permiten 4 días de lluvia idéntica inferior a 10mm
  ## Se permiten 2 días de lluvia idéntica inferior a 50mm
  ## No se permiten dias consecutivos con lluvia idéntica por encima de 50mm
  # load.data('Originales/metadatos_originales_formato_ancho.RData')
  # load.data('Duplicados/pp_formato_ancho.RData')
  detectar_consecutivos(Pre, metadatos_originales_pre, var = C_PP)
  rm(metadatos_originales_pre)
  
  load.data(file = 'Consecutivos/Paso 1/Consecutivos_pp.RData')
  Pre <- eliminar_datos(Pre, metadatos_consecutivos)
  rm(metadatos_consecutivos)
  
  saveRDS(Pre, file.path(dataOutFiles, "pr_ok.rds"))
}
