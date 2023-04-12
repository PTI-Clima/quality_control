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

#' Renombrado de metadatos
#'
#' @param namevar variable a tratar
#'
#' @return None
#' @export
#'
#' @examples
rename_metadata <- function(namevar) {
  ## cambiamos algunos archivos de directorio
  file.rename(file.path(
    dataOutFiles,
    paste0(
      'Duplicados/metadatos_duplicados_combinados_',
      namevar,
      '_formato_ancho.RData'
    )
  ),
  file.path(
    dataOutFiles,
    paste0(
      'Metadatos_finales/metadatos_duplicados_',
      namevar,
      '.RData'
    )
  ))

  file.rename(file.path(
    dataOutFiles,
    paste0(
      'Duplicados/Meses/Paso 1/Duplicados_a_dias_',
      namevar,
      '.RData'
    )
  ),
  file.path(
    dataOutFiles,
    paste0('Metadatos_finales/Meses_duplicados_', namevar, '.RData')
  ))

  file.rename(file.path(
    dataOutFiles,
    paste0(
      'Duplicados/Decenas/Paso 1/dec_duplicados_',
      namevar,
      '.RData'
    )
  ),
  file.path(
    dataOutFiles,
    paste0('Metadatos_finales/Decenas_duplicadas_', namevar, '.RData')
  ))

  file.rename(file.path(
    dataOutFiles,
    paste0('Duplicados/25_dias/duplicados_', namevar, '.RData')
  ),
  file.path(
    dataOutFiles,
    paste0('Metadatos_finales/25_dias_duplicados_', namevar, '.RData')
  ))
}

#' Borrado de metadatos no útiles
#'
#' @return None
#' @export
#'
#' @examples
delete_metadata <- function() {
  ## ELIMINAR LOS ARCHIVOS DE METADATOS QUE YA NO NECESITAMOS
  unlink('Duplicados/', recursive = T)
  unlink('Aberrantes/', recursive = T)
  unlink('Consecutivos/', recursive = T)
  unlink('Mala_codificacion/', recursive = T)
  unlink('Zeros/', recursive = T)
}

#' Tratamiento de los datos diarios leídos, 1º parte
#'
#' @param min datos diarios de la variable
#' @param namevar variable a tratar
#'
#' @return datos de la variable tratados y metadatos originales
#' @export
#'
#' @examples
contol_temp1 = function(min, dist, namevar) {
  min = delete_na(min)
  colnames(min)[2] <- 'YEAR'

  min = delete_no_dist(min, dist)

  ## guardamos los datos
  save.data(min, file = paste0('Originales/', namevar, '.RData'))

  ############################################################
  ########### CREAMOS ARCHIVO INICIAL DE METADATOS ###########
  ############################################################
  metadatos_originales_min <- crear_metadatos_originales(min)

  save.data(metadatos_originales_min,
            file = paste0('Originales/', namevar, '_meta.RData'))

  #####################################################
  ########### DETECCION DE MESES DUPLICADOS ###########
  #####################################################
  meses_duplicados(min,
                   metadatos_originales_min,
                   dist,
                   var = namevar,
                   data_source = data_source)
  decenas_duplicadas(datos=min, metadatos=metadatos_originales_min, var = namevar, data_source=data_source)
  intradecenas_duplicadas(datos=min, metadatos=metadatos_originales_min, var = namevar, data_source=data_source)
  if (namevar != C_PP) {
    deteccion_duplicados_25_dias(
      data = min,
      metadatos = metadatos_originales_min,
      var = namevar,
      dist = dist,
      data_source = data_source
    )

    #######################################################
    ########### ELIMINACION DE MESES DUPLICADOS ###########
    #######################################################

    load.data(
      file = paste0(
        'Duplicados/Meses/Paso 3/metadatos_meses_duplicados_',
        namevar,
        '.RData'
      )
    )
    min <- eliminar_datos(min, metadatos_meses_duplicados_final)
    rm(metadatos_meses_duplicados_final)

    ## Eliminamos los meses con decenas duplicadas
    load.data(
      file = paste0(
        'Duplicados/Decenas/Paso 2/metadatos_decenas_duplicadas_',
        namevar,
        '.RData'
      )
    )
    min <- eliminar_datos(min, metadatos_decenas_duplicadas_final)
    rm(metadatos_decenas_duplicadas_final)

    ## Eliminamos los meses con intradecenas duplicadas
    load.data(file = paste0(
      'Duplicados/Intradecenas/metadatos_intradecenas_',
      namevar,
      '.RData'
    ))
    min <- eliminar_datos(min, metadatos_intradecenas_duplicadas)
    rm(metadatos_intradecenas_duplicadas)

    ## Eliminamos los meses con mas de 25 dias duplicados
    load.data(file = paste0(
      'Duplicados/25_dias/metadatos_duplicados_final_',
      namevar,
      '.RData'
    ))
    min <- eliminar_datos(min, metadatos_duplicados_25_final)
    rm(metadatos_duplicados_25_final)

    min <- as.data.frame(min)
    save.data(min,
              file = paste0('Duplicados/', namevar, '_sin_duplicados.RData'))
  } else{
    offset_duplicated_rain(data=min, meta=metadatos_originales_min, pasada = 1)
  }

  agrupar_metadatos_duplicados(metadatos_originales_min, var = namevar)

  return(list(value = min, metadatos = metadatos_originales_min))
}

#' Control de datos de mtomas sobre las variable indicada
#'
#' @param var variable a tratar
#'
#' @return None
#' @export
#'
#' @examples
controles = function(var) {
  if (var == C_IN) {
    var = C_INS
  } else if (var == C_PR) {
    var = C_PP
  } else if (var == C_R) {
    var = C_RA
  }

  crear <-
    c(
      'Originales/',
      'Duplicados/Meses/Paso 1/',
      'Duplicados/Meses/Paso 2/',
      'Duplicados/Meses/Paso 3/',
      'Duplicados/Decenas/Paso 1/',
      'Duplicados/Decenas/Paso 2/',
      'Duplicados/Intradecenas/',
      'Duplicados/25_dias/',
      'Consecutivos/Paso 1/',
      'Zeros/Paso 1/',
      'Zeros/Paso 2/',
      'Mala_codificacion/',
      'Aberrantes/Paso 1/',
      'Aberrantes/Paso 2/',
      'Aberrantes/Paso 3',
      'Metadatos_finales/'
    )
  crear = file.path(dataOutFiles, crear)
  i = 1
  for (i in 1:length(crear)) {
    dir.create(crear[i], recursive = T, showWarnings = FALSE)
  }
  
  #################################################
  ################# LECTURA #######################
  #################################################

  ## leemos los datos de los txt de AEMET
  min_max <-
    lectura_datos(a = dataFiles,
                  var = var,
                  data_source = data_source)
  if(length(min_max)==1 && is.na(min_max)){
    return(NA)
  }

  if (var == C_T) {
    min_max <-
      list(min = list(value = min_max$min),
           max = list(value = min_max$max))
  }

  ## creamos el archivo de distancias.
  dist <- distancias(a = dataFiles,
                     var = var,
                     data_source = data_source)

  if (var == C_T) {
    min_max$min <- contol_temp1(min=min_max$min$value, dist=dist, namevar=C_MIN)
    min_max$max <- contol_temp1(min=min_max$max$value, dist, namevar=C_MAX)
    min_max$min <- contol_temp15(min=min_max$min$value, namevar=C_MIN)
    min_max$max <- contol_temp15(min=min_max$max$value, namevar=C_MAX)
    zeros_dobles(min_max$min$value, min_max$max$value)
  } else{
    min_max <- contol_temp1(min = min_max, dist = dist, namevar = var)
  }

  if (var == C_T) {
    min_max$min$value <-
      contol_temp2(min_max$min$value, min_max$min$metadatos, C_MIN)
    min_max$max$value <-
      contol_temp2(min_max$max$value, min_max$max$metadatos, C_MAX)

    ## detectamos dias en que la minima es superior a la maxima o igual
    min_sup_eq_max(
      min_max$min$value,
      min_max$max$value,
      min_max$min$metadatos,
      min_max$max$metadatos
    )

    ## detectamos dias en que la amplitud termica es superior a 35 grados
    daily_range(
      min_max$min$value,
      min_max$max$value,
      min_max$min$metadatos,
      min_max$max$metadatos,
      th_dr = 350
    )
    min_max$min$metadatos <- NULL
    min_max$max$metadatos <- NULL

    min_max$min$value <- contol_temp3(min_max$min$value, C_MIN)
    min_max$max$value <- contol_temp3(min_max$max$value, C_MAX)

    metadata_temp(C_MIN)
    metadata_temp(C_MAX)
  } else if (var == C_W) {
    contol_w(U10 = min_max$value,
             metadatos_originales_U10 = min_max$metadatos)
    metadata_w()
  } else if (var == C_HR) {
    contol_hr(RH = min_max$value,
              metadatos_originales_RH = min_max$metadatos)
    metadata_hr()
  } else if (var == C_INS) {
    contol_ins(min_max$value, min_max$metadatos)
    metadata_ins()
  } else if (var == C_PP) {
    contol_pp(Pre = min_max$value,
              metadatos_originales_pre = min_max$metadatos, dist = dist)
  } else if (var == C_RA) {
    contol_ra(Ra=min_max$value, metadatos_originales_ra=min_max$metadatos)
    metadata_ra()
  }
}
