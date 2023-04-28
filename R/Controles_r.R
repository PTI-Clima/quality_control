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

#' Tratamiento de datos de ra, final de la revisión de datos diarios
#'
#' @param Ra datos
#' @param metadatos_originales_ra metadatos_originales
#'
#' @return
#' @export
#'
contol_ra <- function(Ra, metadatos_originales_ra) {
  ###########################################################
  ########### REFORMATEAMOS DATOS A FORMATO ANCHO ###########
  ###########################################################
  Ra <- reformat(datos=Ra)
  
  save.data(Ra, file = 'Duplicados/ra_sin_duplicados_formato_ancho.RData')
  
  # reformateamos los metadatos
  load.data('Duplicados/metadatos_duplicados_combinados_ra.RData')
  metadatos_duplicados_combinados <-
    reformat(as.data.frame(metadatos_duplicados_combinados))
  save.data(metadatos_duplicados_combinados, file = 'Duplicados/metadatos_duplicados_combinados_ra_formato_ancho.RData')
  rm(metadatos_duplicados_combinados)
  
  load.data('Originales/ra_meta.RData')
  metadatos_originales_ra_ancho <-
    reformat(as.data.frame(metadatos_originales_ra))
  save.data(metadatos_originales_ra_ancho, file = 'Originales/ra_meta.RData')
  rm(metadatos_originales_ra)
  
  ##################################################################
  ########### DETECTAMOS Y ELIMINAMOS DATOS CONSECUTIVOS ###########
  ##################################################################
  load.data(file = 'Duplicados/ra_sin_duplicados_formato_ancho.RData')
  
  detectar_consecutivos(Ra, metadatos_originales_ra_ancho, var = C_RA)
  
  load.data(file = 'Consecutivos/Paso 1/Consecutivos_ra.RData')
  Ra <- eliminar_datos(Ra, metadatos_consecutivos)
  rm(metadatos_consecutivos)
  
  ##################################
  ########### ABERRANTES ###########
  ##################################
  metadatos_aberrantes <-
    deteccion_aberrantes(data = Ra, metadatos = metadatos_originales_ra_ancho, var = C_RA, vart = NULL)
  rm(metadatos_originales_ra_ancho)
  Ra <- eliminar_datos(Ra, metadatos_aberrantes)
  save.data(Ra, file = 'Aberrantes/Paso 1/ra_sin_aberrantes.RData')
  rm(metadatos_aberrantes)
  
  ## transformamos a MJ m²
  Ra[,-c(1:3)] <- Ra[,-c(1:3)] / 100 #CORREGIDO_MTOMAS
  
  saveRDS(Ra, file.path(dataOutFiles, "r_ok.rds"))
}

#' Preparación de los metadatos de ra para su guardado
#'
#' @return None
#' @export
#'
metadata_ra = function() {
  rename_metadata(C_RA)
  
  load.data(file = 'Consecutivos/Paso 1/Consecutivos_ra.RData')
  load.data(file = 'Aberrantes/metadatos_aberrantes_ra.RData')
  metadatos_final_resto <-
    metadatos_consecutivos + metadatos_aberrantes
  save.data(metadatos_final_resto, file = 'Metadatos_finales/metadatos_resto_ra.RData')
  
  delete_metadata()
}
