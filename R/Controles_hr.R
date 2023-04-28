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

#' Tratamiento de datos de rh, final de la revisión de datos diarios
#'
#' @param RH datos
#' @param metadatos_originales_RH metadatos_originales
#'
#' @return None
#' @export
#'
contol_hr <- function(RH, metadatos_originales_RH) {
  ###########################################################
  ########### REFORMATEAMOS DATOS A FORMATO ANCHO ###########
  ###########################################################
  load.data(file = 'Duplicados/hr_sin_duplicados.RData')
  if (data_source != C_SIAR) {
    RH_00 <- RH[, c(1, 2, 3, grep('^hr00', colnames(RH)))]
    RH_07 <- RH[, c(1, 2, 3, grep('^hr07', colnames(RH)))]
    RH_13 <- RH[, c(1, 2, 3, grep('^hr13', colnames(RH)))]
    RH_18 <- RH[, c(1, 2, 3, grep('^hr18', colnames(RH)))]
    
    ## reformateamos los datos
    RH_00 <- reformat(RH_00)
    RH_07 <- reformat(RH_07)
    RH_13 <- reformat(RH_13)
    RH_18 <- reformat(RH_18)
    
    ## creamos 4 archivos diferentes: RH_00; RH_07; RH_13; RH_18
    save.data(RH_00, file = 'Duplicados/RH_00_sin_duplicados_formato_ancho.RData')
    save.data(RH_07, file = 'Duplicados/RH_07_sin_duplicados_formato_ancho.RData')
    save.data(RH_13, file = 'Duplicados/RH_13_sin_duplicados_formato_ancho.RData')
    save.data(RH_18, file = 'Duplicados/RH_18_sin_duplicados_formato_ancho.RData')
    
    RH_diario <- from_subdaily_to_daily(RH_07, RH_13, RH_18)
  } else{
    RH_diario <- reformat(RH)
  }
  save.data(RH_diario, file = 'Duplicados/hr_diario_sin_duplicados_formato_ancho.RData')
  
  ## reformateamos los metadatos
  
  load.data('Duplicados/metadatos_duplicados_combinados_hr.RData')
  if (data_source != C_SIAR) {
    metadatos_duplicados_combinados_diario <-
      metadatos_duplicados_combinados[, c(1, 2, 3, grep('^hr07', colnames(metadatos_duplicados_combinados)))]
  } else{
    metadatos_duplicados_combinados_diario <-
      metadatos_duplicados_combinados
  }
  metadatos_duplicados_combinados_diario <-
    reformat(metadatos_duplicados_combinados_diario)
  w <-
    match(colnames(RH_diario),
          colnames(metadatos_duplicados_combinados_diario))
  metadatos_duplicados_combinados_diario <-
    metadatos_duplicados_combinados_diario[, w]
  save.data(metadatos_duplicados_combinados_diario, file = 'Duplicados/metadatos_duplicados_combinados_hr_formato_ancho.RData')
  
  load.data(file = 'Originales/hr_meta.RData')
  if (data_source != C_SIAR) {
    metadatos_originales_RH_formato_ancho <- metadatos_originales_RH[, c(1, 2, 3, grep('^hr07', colnames(metadatos_originales_RH)))]
    metadatos_originales_RH_formato_ancho[, colnames(metadatos_originales_RH)[grep('^hr07', colnames(metadatos_originales_RH))]] <-
      (metadatos_originales_RH[, c(grep('^hr07', colnames(metadatos_originales_RH)))] + metadatos_originales_RH[, c(grep('^hr13', colnames(metadatos_originales_RH)))] + metadatos_originales_RH[, c(grep('^hr18', colnames(metadatos_originales_RH)))]) / 3
  } else{
    metadatos_originales_RH_formato_ancho <- metadatos_originales_RH
  }
  metadatos_originales_RH_formato_ancho$INDICATIVO <-
    metadatos_originales_RH$INDICATIVO
  metadatos_originales_RH_formato_ancho <-
    reformat(metadatos_originales_RH_formato_ancho)
  w <-
    match(colnames(RH_diario),
          colnames(metadatos_originales_RH_formato_ancho))
  metadatos_originales_RH_formato_ancho <-
    metadatos_originales_RH_formato_ancho[, w]
  save.data(metadatos_originales_RH_formato_ancho, file = 'Originales/metadatos_originales_hr_formato_ancho.RData')
  rm(metadatos_originales_RH)
  
  ##################################################################
  ########### DETECTAMOS Y ELIMINAMOS DATOS CONSECUTIVOS ###########
  ##################################################################
  
  ## metadatos_consecutivos valen '0' si el dato es original, '1' si se detectó duplicado pero no se eliminó, '2' si
  ## se eliminó duplicado, '3' si se ha detectado y eliminado consecutivo
  if (data_source != C_SIAR) {
    metadatos_consecutivos_07 <-
      detectar_consecutivos(RH_07,
                            metadatos_originales_RH_formato_ancho,
                            var = C_HR,
                            vart = '07')
    metadatos_consecutivos_13 <-
      detectar_consecutivos(RH_13,
                            metadatos_originales_RH_formato_ancho,
                            var = C_HR,
                            vart = '13')
    metadatos_consecutivos_18 <-
      detectar_consecutivos(RH_18,
                            metadatos_originales_RH_formato_ancho,
                            var = C_HR,
                            vart = '18')
  }
  metadatos_consecutivos_dia <-
    detectar_consecutivos(
      RH_diario,
      metadatos_originales_RH_formato_ancho,
      var = C_HR,
      vart = 'diario'
    )
  
  metadatos_consecutivos_RHdia <-
    metadatos_duplicados_combinados_diario
  if (data_source != C_SIAR) {
    metadatos_consecutivos_RHdia[metadatos_consecutivos_07 == 13 &
                                   metadatos_consecutivos_13 == 13 &
                                   metadatos_consecutivos_18 == 13 &
                                   metadatos_consecutivos_dia == 13] <-
      13
    save.data(
      metadatos_consecutivos_RHdia,
      file = paste('Consecutivos/Paso 1/Consecutivos_hr.RData', sep = '')
    )
    rm(metadatos_consecutivos_07)
    rm(metadatos_consecutivos_13)
    rm(metadatos_consecutivos_18)
    rm(metadatos_consecutivos_dia)
  }
  rm(metadatos_duplicados_combinados_diario)
  
  ############################################################################
  ########### DETECTAMOS Y ELIMINAMOS PERIODOS CON POCA VARIACION ############
  ###########################################################################
  if (data_source != C_SIAR) {
    load.data(file = 'Duplicados/RH_07_sin_duplicados_formato_ancho.RData')
    load.data(file = 'Duplicados/RH_13_sin_duplicados_formato_ancho.RData')
    load.data(file = 'Duplicados/RH_18_sin_duplicados_formato_ancho.RData')
    
    metadatos_poca_variacion_95 <-
      poca_variacion_bucle(RH_07,
                           RH_13,
                           RH_18,
                           C = 60,
                           E = 3,
                           G = T)
    metadatos_poca_variacion <-
      poca_variacion_bucle(
        x_07 = RH_07,
        x_13 = RH_13,
        x_18 = RH_18,
        C = 21,
        E = 3,
        G = F
      )
    metadatos_poca_variacion[metadatos_poca_variacion_95 == 15] <-
      15
    save.data(
      metadatos_poca_variacion,
      file = paste('Consecutivos/Paso 2/Poca_variacion_hr.RData', sep = '')
    )
    rm(metadatos_poca_variacion_95)
  }
  RH_diario <-
    eliminar_datos(RH_diario, metadatos_consecutivos_RHdia)
  rm(metadatos_consecutivos_RHdia)
  if (data_source != C_SIAR) {
    RH_diario <- eliminar_datos(RH_diario, metadatos_poca_variacion)
    rm(metadatos_poca_variacion)
  }
  save.data(RH_diario, file = 'Consecutivos/hr_diario_sin_consecutivos.RData')
  
  ####################################
  ########## 100s entre NAs ##########
  ####################################
  
  metadatos_100_nas <-
    cien_entre_nas(RH_diario, metadatos_originales_RH_formato_ancho)
  save.data(metadatos_100_nas, file = 'Consecutivos/Paso 3/100_nas.RData')
  
  RH_diario <- eliminar_datos(RH_diario, metadatos_100_nas)
  rm(metadatos_100_nas)
  
  ###########################################################
  ################### CONTROLES EXTREMOS  ###################
  ###########################################################
  if (data_source != C_SIAR) {
    metadatos_aberrantes_00 <-
      deteccion_aberrantes(RH_00,
                           metadatos_originales_RH_formato_ancho,
                           var = C_HR,
                           vart = '00')
    metadatos_aberrantes_07 <-
      deteccion_aberrantes(RH_07,
                           metadatos_originales_RH_formato_ancho,
                           var = C_HR,
                           vart = '07')
    metadatos_aberrantes_13 <-
      deteccion_aberrantes(RH_13,
                           metadatos_originales_RH_formato_ancho,
                           var = C_HR,
                           vart = '13')
    metadatos_aberrantes_18 <-
      deteccion_aberrantes(RH_18,
                           metadatos_originales_RH_formato_ancho,
                           var = C_HR,
                           vart = '18')
  }
  metadatos_aberrantes <- metadatos_originales_RH_formato_ancho
  rm(metadatos_originales_RH_formato_ancho)
  if (data_source != C_SIAR) {
    metadatos_aberrantes[metadatos_aberrantes_00 == 21 |
                           metadatos_aberrantes_07 == 21 |
                           metadatos_aberrantes_13 == 21 |
                           metadatos_aberrantes_18 == 21] <- 21
    metadatos_aberrantes[metadatos_aberrantes_00 == 22 |
                           metadatos_aberrantes_07 == 22 |
                           metadatos_aberrantes_13 == 22 |
                           metadatos_aberrantes_18 == 22] <- 22
    rm(metadatos_aberrantes_00)
    rm(metadatos_aberrantes_07)
    rm(metadatos_aberrantes_13)
    rm(metadatos_aberrantes_18)
  }
  save.data(
    metadatos_aberrantes,
    file = ('Aberrantes/Paso 1/metadatos_aberrantes_diarios.RData')
  )
  RH_diario <- eliminar_datos(RH_diario, metadatos_aberrantes)
  rm(metadatos_aberrantes)
  save.data(RH_diario, file = 'Aberrantes/Paso 1/hr_diario_sin_aberrantes.RData')
  
  saveRDS(RH_diario, file = file.path(dataOutFiles, 'hr_ok.rds'))  
}

#' Preparación de los metadatos de hr para su guardado
#'
#' @return None
#' @export
#'
metadata_hr = function() {
  rename_metadata(C_HR)
  
  if (data_source != C_SIAR) {
    load.data(file = 'Consecutivos/Paso 2/Poca_variacion_hr.RData')
    load.data(file = 'Consecutivos/Paso 1/Consecutivos_hr.RData')
  }
  load.data(file = 'Consecutivos/Paso 3/100_nas.RData')
  load.data(file = 'Aberrantes/Paso 1/metadatos_aberrantes_diarios.RData')
  
  # metadatos_final_resto <- metadatos_consecutivos + metadatos_poca_variacion + metadatos_100_nas + metadatos_aberrantes #CORREGIDO_MTOMAS
  # metadatos_final_resto[,1:3] <- metadatos_consecutivos[,1:3]
  # save.data(metadatos_final_resto,file='Metadatos_finales/metadatos_resto_hr.RData')
  
  delete_metadata()
}
