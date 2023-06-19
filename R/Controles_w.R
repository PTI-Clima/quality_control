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

#' Tratamiento de datos de W, final de la revisión de datos diarios.
#' En primer lugar se agrega de datos sub-diarios (lecturas a las 00, 07,
#' 13 y 18 horas) a datos diarios. Luego se detectan datos repetidos,
#' datos aberrantes, y los propios controles de calidad de AEMET.
#'
#' @param U10 datos
#' @param metadatos_originales_U10 metadatos_originales
#'
#' @return
#'
#'
contol_w <- function(U10, metadatos_originales_U10) {
  ###########################################################
  ########### REFORMATEAMOS DATOS A FORMATO ANCHO ###########
  ###########################################################
  if (data_source != C_SIAR) {
    U_00 <- U10[, c(1, 2, 3, grep('^w00', colnames(U10)))]
    U_07 <- U10[, c(1, 2, 3, grep('^w07', colnames(U10)))]
    U_13 <- U10[, c(1, 2, 3, grep('^w13', colnames(U10)))]
    U_18 <- U10[, c(1, 2, 3, grep('^w18', colnames(U10)))]
    
    ## reformateamos los datos
    U_00 <- reformat(U_00)
    U_07 <- reformat(U_07)
    U_13 <- reformat(U_13)
    U_18 <- reformat(U_18)
    
    ## creamos 4 archivos diferentes: RH_00; RH_07; RH_13; RH_18
    save.data(U_00, file = 'Duplicados/U_00_sin_duplicados_formato_ancho.RData')
    save.data(U_07, file = 'Duplicados/U_07_sin_duplicados_formato_ancho.RData')
    save.data(U_13, file = 'Duplicados/U_13_sin_duplicados_formato_ancho.RData')
    save.data(U_18, file = 'Duplicados/U_18_sin_duplicados_formato_ancho.RData')
    
    U10_diario <- from_subdaily_to_daily(U_07, U_13, U_18)
  } else{
    U10_diario <- reformat(U10)
  }
  save.data(U10_diario, file = 'Duplicados/w_diario_sin_duplicados_formato_ancho.RData')
  
  ## reformateamos los metadatos
  
  load.data('Duplicados/metadatos_duplicados_combinados_w.RData')
  if (data_source != C_SIAR) {
    metadatos_duplicados_combinados_diario <-
      metadatos_duplicados_combinados[, c(1, 2, 3, grep('^w07', colnames(metadatos_duplicados_combinados)))]
  } else{
    metadatos_duplicados_combinados_diario <-
      metadatos_duplicados_combinados
  }
  metadatos_duplicados_combinados_diario <-
    reformat(metadatos_duplicados_combinados_diario)
  save.data(metadatos_duplicados_combinados_diario,
            file = 'Duplicados/metadatos_duplicados_combinados_w_formato_ancho.RData')
  
  load.data(file = 'Originales/w_meta.RData')
  if (data_source != C_SIAR) {
    metadatos_originales_U10_formato_ancho <- metadatos_originales_U10[, c(1, 2, 3, grep('^w07', colnames(metadatos_originales_U10)))]
    metadatos_originales_U10_formato_ancho[, colnames(metadatos_originales_U10)[grep('^w07', colnames(metadatos_originales_U10))]] <-
      (
        metadatos_originales_U10[, c(grep('^w07', colnames(metadatos_originales_U10)))] +
          metadatos_originales_U10[, c(grep('^w13', colnames(metadatos_originales_U10)))] +
          metadatos_originales_U10[, c(grep('^w18', colnames(metadatos_originales_U10)))]
      ) / 3
    # metadatos_originales_U10_formato_ancho <-
    #   (
    #     metadatos_originales_U10[, c(1, 2, 3, grep('^w07', colnames(metadatos_originales_U10)))] +
    #       metadatos_originales_U10[, c(1, 2, 3, grep('^w13', colnames(metadatos_originales_U10)))] +
    #       metadatos_originales_U10[, c(1, 2, 3, grep('^w18', colnames(metadatos_originales_U10)))]
    #   ) / 3
  } else{
    metadatos_originales_U10_formato_ancho <- metadatos_originales_U10
  }
  metadatos_originales_U10_formato_ancho$INDICATIVO <-
    metadatos_originales_U10$INDICATIVO
  metadatos_originales_U10_formato_ancho <-
    reformat(metadatos_originales_U10_formato_ancho)
  w <-
    match(colnames(U10_diario),
          colnames(metadatos_originales_U10_formato_ancho))
  metadatos_originales_U10_formato_ancho <-
    metadatos_originales_U10_formato_ancho[, w]
  save.data(metadatos_originales_U10_formato_ancho, file = 'Originales/metadatos_originales_w_formato_ancho.RData')
  rm(metadatos_originales_U10)
  
  ##################################################################
  ########### DETECTAMOS Y ELIMINAMOS DATOS CONSECUTIVOS ###########
  ##################################################################
  
  ## metadatos_consecutivos valen '0' si el dato es original, '1' si se detectó duplicado pero no se eliminó, '2' si
  ## se eliminó duplicado, '3' si se ha detectado y eliminado consecutivo
  if (data_source != C_SIAR) {
    metadatos_consecutivos_U07 <-
      detectar_consecutivos(U_07,
                            metadatos_originales_U10_formato_ancho,
                            var = C_W,
                            vart = '07')
    metadatos_consecutivos_U13 <-
      detectar_consecutivos(U_13,
                            metadatos_originales_U10_formato_ancho,
                            var = C_W,
                            vart = '13')
    metadatos_consecutivos_U18 <-
      detectar_consecutivos(U_18,
                            metadatos_originales_U10_formato_ancho,
                            var = C_W,
                            vart = '18')
  }
  metadatos_consecutivos_Udia <-
    detectar_consecutivos(
      U10_diario,
      metadatos_originales_U10_formato_ancho,
      var = C_W,
      vart = 'diario'
    )
  
  metadatos_consecutivos_U10 <-
    metadatos_originales_U10_formato_ancho
  if (data_source != C_SIAR) {
    metadatos_consecutivos_U10[metadatos_consecutivos_U07 == 13 &
                                 metadatos_consecutivos_U13 == 13 &
                                 metadatos_consecutivos_U18 == 13 &
                                 metadatos_consecutivos_Udia == 13] <-
      13
  }
  save.data(
    metadatos_consecutivos_U10,
    file = paste('Consecutivos/Paso 1/Consecutivos_w.RData', sep = '')
  )
  if (data_source != C_SIAR) {
    rm(metadatos_consecutivos_U07)
    rm(metadatos_consecutivos_U13)
    rm(metadatos_consecutivos_U18)
  }
  rm(metadatos_consecutivos_Udia)
  U10_diario <-
    eliminar_datos(U10_diario, metadatos_consecutivos_U10)
  save.data(U10_diario, file = 'Consecutivos/w_diario_sin_consecutivos.RData')
  rm(metadatos_consecutivos_U10)
  
  ########################################################
  ################### CONTROLES AEMET ###################
  ######################################################
  if (data_source != C_SIAR) {
    metadatos_aberrantes_00 <-
      deteccion_aberrantes(U_00,
                           metadatos_originales_U10_formato_ancho,
                           var = C_W,
                           vart = '00')
    metadatos_aberrantes_07 <-
      deteccion_aberrantes(U_07,
                           metadatos_originales_U10_formato_ancho,
                           var = C_W,
                           vart = '07')
    metadatos_aberrantes_13 <-
      deteccion_aberrantes(U_13,
                           metadatos_originales_U10_formato_ancho,
                           var = C_W,
                           vart = '13')
    metadatos_aberrantes_18 <-
      deteccion_aberrantes(U_18,
                           metadatos_originales_U10_formato_ancho,
                           var = C_W,
                           vart = '18')
  }
  metadatos_aberrantes <- metadatos_originales_U10_formato_ancho
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
    file = ('Aberrantes/Paso 1/w_metadatos_aberrantes_diarios.RData')
  )
  U10_diario <- eliminar_datos(U10_diario, metadatos_aberrantes)
  save.data(U10_diario, file = 'Aberrantes/Paso 1/w_diario_sin_aberrantes.RData')
  rm(metadatos_aberrantes)
  ##################################################################
  #### Deteccion y eliminacion de todos los datos diarios> 150  ####
  #### y todos los dias que tienen más de 1 valor de            ####
  #### 108/144/180                                              ####
  ##################################################################
  metadatos_aberrantes_propios <-
    metadatos_originales_U10_formato_ancho
  metadatos_aberrantes_propios[U10_diario > 150] <- 26
  if (data_source != C_SIAR) {
    metadatos_aberrantes_propios_07 <-
      metadatos_aberrantes_propios_13 <-
      metadatos_aberrantes_propios_18 <-
      metadatos_originales_U10_formato_ancho
    rm(metadatos_originales_U10_formato_ancho)
    metadatos_aberrantes_propios_07[U_07 == 108 |
                                      U_07 == 144 |
                                      U_07 == 180] <- 100
    metadatos_aberrantes_propios_13[U_13 == 108 |
                                      U_13 == 144 |
                                      U_13 == 180] <- 100
    metadatos_aberrantes_propios_18[U_18 == 108 |
                                      U_18 == 144 |
                                      U_18 == 180] <- 100
    
    metadatos_aberrantes_propios[(
      metadatos_aberrantes_propios_07 +
        metadatos_aberrantes_propios_13 +
        metadatos_aberrantes_propios_18
    ) >= 200] <- 27
    rm(metadatos_aberrantes_propios_07)
    rm(metadatos_aberrantes_propios_13)
    rm(metadatos_aberrantes_propios_18)
  }
  metadatos_aberrantes_propios[, 1:3] <- U10_diario[, 1:3]
  save.data(metadatos_aberrantes_propios, file = 'Aberrantes/Paso 2/w_metadatos_aberrantes_propios.RData')
  U10_diario <-
    eliminar_datos(U10_diario, metadatos_aberrantes_propios)
  rm(metadatos_aberrantes_propios)
  
  saveRDS(U10_diario, file.path(dataOutFiles, "w_ok.rds"))
}

#' Preparación de los metadatos de w para su guardado
#'
#' @return None
#'
#'
metadata_w = function() {
  rename_metadata(C_W)
  
  load.data(file = 'Consecutivos/Paso 1/Consecutivos_w.RData')
  load.data(file = 'Aberrantes/Paso 1/metadatos_aberrantes_diarios.RData')
  load.data(file = 'Aberrantes/Paso 2/metadatos_aberrantes_propios.RData')
  metadatos_final_resto <-
    metadatos_consecutivos_U10 + metadatos_aberrantes_propios
  save.data(metadatos_final_resto, file = 'Metadatos_finales/metadatos_resto_w.RData')
  
  delete_metadata()
}
