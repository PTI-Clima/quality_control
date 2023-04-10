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

#' Tratamiento de datos de t, una parte
#'
#' @param min datos
#' @param namevar nombre variable
#'
#' @return
#' @export
#'
#' @examples
contol_temp15 = function(min, namevar) {
  ###########################################################
  ########### REFORMATEAMOS DATOS A FORMATO ANCHO ###########
  ###########################################################
  
  ## reformateamos los datos
  min <- reformat(datos=min)
  
  save.data(min,
            file = paste0(
              'Duplicados/',
              namevar,
              '_sin_duplicados_formato_ancho.RData'
            ))
  
  ## reformateamos los metadatos
  load.data(paste0(
    'Duplicados/metadatos_duplicados_combinados_',
    namevar,
    '.RData'
  ))
  metadatos_duplicados_combinados <-
    reformat(metadatos_duplicados_combinados)
  save.data(
    metadatos_duplicados_combinados,
    file = paste0(
      'Duplicados/metadatos_duplicados_combinados_',
      namevar,
      '_formato_ancho.RData'
    )
  )
  rm(metadatos_duplicados_combinados)
  
  load.data(paste0('Originales/', namevar, '_meta.RData'))
  
  metadatos_originales_min_ancho <-
    reformat(metadatos_originales_min)
  rm(metadatos_originales_min)
  
  save.data(
    metadatos_originales_min_ancho,
    file = paste0('Originales/meta_', namevar, '_ancho.RData')
  )
  
  #########################################################
  #### SEGUNDO GRUPO DE CONTROLES DE MALA CODIFICACION ####
  #### 1.- DIAS CONSECUTIVOS                           ####
  #### 2.- FALSOS CEROS                                ####
  #### 3.- MESES MAL CODIFICADOS                       ####
  
  ##############################################################
  ########### DETECTAMOS DIAS CON DATOS CONSECUTIVOS ###########
  ##############################################################
  
  ## metadatos_consecutivos valen '13' si se detectó consecutivo
  detectar_consecutivos(min, metadatos_originales_min_ancho, var = namevar)
  
  ################################################################
  ################### DETECCION DE FALSOS 0's  ###################
  ################################################################
  zeros_aislados(min, metadatos_originales_min_ancho, var = namevar)
  
  return(list(value = min, metadatos = metadatos_originales_min_ancho))
}

#' Tratamiento de datos de t, una parte
#'
#' @param min datos
#' @param metadatos_originales_min_ancho metadatos_originales
#' @param namevar nombre variable
#'
#' @return
#' @export
#'
#' @examples
contol_temp2 = function(min,
                        metadatos_originales_min_ancho,
                        namevar) {
  #########################################################
  ########### DETECTAMOS  MESES MAL CODIFICADOS ###########
  #########################################################
  mala_codificacion_temperatura(min, metadatos_originales_min_ancho, var =
                                  namevar)
  
  ############################################
  ########### ELIMINACION DE DATOS ###########
  ############################################
  
  ## ELIMINACION DE CONSECUTIVOS
  load.data(file = paste0('Consecutivos/Paso 1/Consecutivos_', namevar, '.RData'))
  min <- eliminar_datos(min, metadatos_consecutivos)
  rm(metadatos_consecutivos)
  
  ## ELIMINACION DE FALSOS 0's
  load.data(file = paste0('Zeros/Paso 2/Zeros_comunes_', namevar, '.RData'))
  min <- eliminar_datos(min, metadatos_zeros)
  rm(metadatos_zeros)
  
  ## ELIMINACION DE MALA CODIFICACION
  load.data(file = paste0('Mala_codificacion/Mala_codificacion_', namevar, '.RData'))
  min <- eliminar_datos(min, metadatos_mala_codificacion)
  rm(metadatos_mala_codificacion)
  
  ###########################################################
  ################### CONTROLES EXTREMOS  ###################
  ###########################################################
  ## detectamos valores superiores a 50ºC y también valores inferiores a -35ºC
  deteccion_aberrantes(min, metadatos_originales_min_ancho, var = namevar)
  return(min)
}

#' Tratamiento de datos de t, final de la revisión de datos diarios
#'
#' @param min datos
#' @param namevar nombre variable
#'
#' @return
#' @export
#'
#' @examples
contol_temp3 = function(min, namevar) {
  load.data(file = paste0(
    'Aberrantes/Paso 1/metadatos_aberrantes_',
    namevar,
    '.RData'
  ))
  min <- eliminar_datos(min, metadatos_aberrantes)
  rm(metadatos_aberrantes)
  
  load.data(file = paste0('Aberrantes/Paso 2/metadatos_', namevar, '.RData'))
  min <- eliminar_datos(min, get(paste0("metadatos_", namevar)))
  
  load.data(file = paste0('Aberrantes/Paso 3/metadatos_dr_350_', namevar, '.RData'))
  min <- eliminar_datos(min, get(paste0("metadatos_", namevar)))
  
  saveRDS(min, file.path(dataOutFiles, paste0(C_T, namevar, "_ok.rds")))
  return(min)
}

#' Preparación de los metadatos de temp para su guardado
#'
#' @return None
#' @export
#'
#' @examples
metadata_temp = function(namevar) {
  ## namevar
  rename_metadata(namevar)
  
  ## construimos archivos finales de metadatos
  load.data(file = paste0('Consecutivos/Paso 1/Consecutivos_', namevar, '.RData'))
  load.data(file = paste0('Zeros/Paso 2/Zeros_comunes_', namevar, '.RData'))
  load.data(file = paste0('Mala_codificacion/Mala_codificacion_', namevar, '.RData'))
  load.data(file = paste0(
    'Aberrantes/Paso 1/metadatos_aberrantes_',
    namevar,
    '.RData'
  ))
  load.data(file = paste0('Aberrantes/Paso 2/metadatos_', namevar, '.RData'))
  metadatos_final_resto <-
    metadatos_consecutivos[rownames(get(paste0('metadatos_', namevar))), colnames(get(paste0('metadatos_', namevar)))] + metadatos_zeros[rownames(get(paste0('metadatos_', namevar))), colnames(get(paste0('metadatos_', namevar)))] + metadatos_mala_codificacion[rownames(get(paste0('metadatos_', namevar))), colnames(get(paste0('metadatos_', namevar)))] + metadatos_aberrantes[rownames(get(paste0('metadatos_', namevar))), colnames(get(paste0('metadatos_', namevar)))] + get(paste0('metadatos_', namevar))
  metadatos_final_resto[, 1:3] <- metadatos_consecutivos[, 1:3]
  load.data(file = paste0('Aberrantes/Paso 3/metadatos_dr_350_', namevar, '.RData'))
  metadatos_final_resto <-
    metadatos_final_resto + get(paste0('metadatos_', namevar))
  save.data(
    metadatos_final_resto,
    file = paste0('Metadatos_finales/metadatos_resto_', namevar, '.RData')
  )
  
  delete_metadata()
}
