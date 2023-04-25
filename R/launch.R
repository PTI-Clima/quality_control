# Primera versión del fichero de funciones

#' Calculate the directory name for a given year or day value
#'
#' This function calculates the directory name for a given year or day value. If the value
#' is a year, the directory name will be the same as the year value. If the value is a day,
#' the directory name will be "new_all". If the value is missing or NA, the directory name
#' will also be "new_all".
#'
#' @param year.new A numeric or character value representing a year or day.
#'
#' @return A character string representing the directory name.
#'
#' @examples
#' year.new.dir.calc(2023)
#' year.new.dir.calc("2023-04-10")
#' year.new.dir.calc(NA)
#'
#' @keywords directory name, year, day
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


#' Prepare data for analysis
#'
#' This function prepares the data for analysis by reading in the processed data
#' from a file, correcting the units, and writing the data to new files with the
#' appropriate format for analysis.
#'
#' @param name.type character string indicating the type of data to prepare
#' @param dataOutFiles character string indicating the path to the directory where the output data will be stored
#' @param year.new integer indicating the year of the data
#' @param data_source character string indicating the source of the data
#'
#' @return None
#'
#' @importFrom lattice xyplot
#'
prepare.data <- function(name.type) {
  
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
  
  if (is.na(name.type)) {
    stop('Error: The climatological variable cannot be null')
  }
  
  if (!name.type %in% c(C_W, C_HR, C_PR, C_IN, C_R, C_T)) {
    stop(paste0('Error: The following climatological variable is not valid: ', name.type))
  }
  
  fileOk = file.path(dataOutFiles, paste0(name.type, "_ok.rds"))
  print(fileOk)
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
  
  folder = file.path(as.character(year.new.dir.calc(NA)), data_source)
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

#' qc.apply
#'
#' Apply quality control to meteorological data
#'
#' @param vars Character vector of variable types to apply QC to
#' @param input.folder Character string of the input data folder path
#' @param output.folder Character string of the output data folder path
#' @param data.source Character string indicating the data source (default is "AEMET")
#'
#' @return None
#' @import chron, lattice, Rcpp, reshape, reshape2, Hmisc
#'
#' @export
qc.apply <- function(vars, output.folder = "new_all", data.source = "AEMET") {
  
  # Parameters check ###########################################################
  
  if (any(is.na(vars))) {
    stop('Error: The climatological variable cannot be null')
  }
  
  if (!all(vars %in% c(C_W, C_HR, C_PR, C_IN, C_R, C_T))) {
    stop(paste0('Error: The following climatological variable is not valid: ', vars))
  }
  
  # Variables pre-proccessing ##################################################
  
  if (!exists(C_AEMET)){
    init.variables(output.folder = output.folder)
  }
  
  data_source <<- data.source

  # Launch mtomas code #########################################################

  for (type in vars) {
    
    print(paste0("Launching control for: ", type))

    controles(type) # this line actually launches the code

    if (type == "t") {
      prepare.data(name.type = "tmax")
      prepare.data(name.type = "tmin")
    } else {
      prepare.data(name.type = type)
    }

  }
  
}

#' init.variables
#'
#' This function initialize the necessary global variables to execute the
#' quality control code itself.
#'
#' @return None
#'
init.variables <- function(output.folder = "new_all", data.source = "AEMET") {
  
  C_TMAX <<- "tmax" #temperatura máxima
  C_TMIN <<- "tmin" #temperatura mínima
  C_W <<- "w" #velocidad viento
  C_HR <<- "hr" #humedad relativa
  C_PR <<- "pr" #precipitación
  C_IN <<- "in" #insolación
  C_TD <<- "td" #temperatura de rocío
  C_RA <<- "ra" #radiacion
  C_P <<- "p" #presion
  
  C_T <<- "t"  #temperatura
  C_R <<- "r" #radiación
  C_MAX <<- "max" #temperatura máxima
  C_MIN <<- "min" #temperatura mínima
  C_INS <<- "ins" #insolación
  C_PP <<- "pp" #precipitación
  
  DS_ALL <<- "all"
  DS_TRI <<- "tri"
  
  C_AEMET <<- "AEMET"
  C_SIAR <<- "SIAR"
  
  dataFiles <<- "data"
  
  if (data.source == "AEMET") {
    dataOutFiles <<- file.path(output.folder, "out_files")
  } else {
    dataOutFiles <<- file.path(output.folder, paste("out_files", data.source, sep="_"))
  }
  
}

#' Launch all quality controls
#'
#' This function launches all quality controls for the variables: wind speed, relative humidity,
#' precipitation, sunshine duration, radiation and temperature.
#' 
#' @return No return value.
#' @export
launch.all.controls <- function() {
  
  init.variables("AEMET")
  
  #vars = c(C_W, C_HR, C_PR, C_IN, C_R, C_T)
  vars = c(C_IN, C_R, C_T)
  
  qc.apply(vars)
  
}


#' Apply quality controls to specified variables.
#'
#' This function applies quality controls to the specified variables.
#'
#' @param vars A character vector containing the variable names to apply quality controls to.
#' @export
#' 
launch.controls <- function(vars) {
  
  init.variables("AEMET")
  
  qc.apply(vars)
  
}

#' Save data to a specified file
#'
#' This function saves data to a specified file in a given directory.
#' 
#' @param ... objects to be saved
#' @param file a character string specifying the file name
#' @param dataOutFiles a character string specifying the directory where the file should be saved
#' 
#' @return This function saves the data to the specified file.
#' 
save.data = function(..., file) {
  newUrl = file.path(dataOutFiles, file)
  dir.create(dirname(newUrl),
             showWarnings = FALSE,
             recursive = TRUE)
  envir = parent.frame()
  save(..., envir = envir, file = newUrl)
}

#' Load data from a file in a specified directory
#'
#' @param file Name of the file to load data from
#' @param dataOutFiles Directory where the file is located
#'
#' @return Objects loaded from the file
#'
load.data = function(file) {
  newUrl = file.path(dataOutFiles, file)
  envir = parent.frame()
  load(newUrl, envir = envir)
}

#' Correct units of a weather series
#'
#' This function corrects the units of a weather series based on its type.
#'
#' @param series a numeric vector containing the values of the weather series to be corrected
#' @param type a character string indicating the type of weather series, which can be one of "w" (wind speed), "hr" (relative humidity), "pr" (precipitation), "in" (insolation), "r" (radiation), "t" (temperature)
#' 
#' @return a numeric vector with the corrected values of the input series
#'
correctUnits = function(series,type){
  NAMES5 = c()
  NAMES5[C_TMAX] = C_T
  NAMES5[C_TMIN] = C_T
  NAMES5[C_HR] = C_HR
  NAMES5[C_W] = C_W
  NAMES5[C_IN] = C_IN
  NAMES5[C_PR] = C_PR
  NAMES5[C_R] = C_R # radiacion
  NAMES5[C_PR] = C_P # presion
  NAMES5[C_R] = C_RA
  if (!(type %in% names(NAMES5))){
    stop("Error: The Climatological variable is not valid")
  }
  else if (all(is.na(series)) == TRUE){
    return(NA)
  }
  else if (type == C_IN | type == C_PR |
           type == C_MIN | type == C_TMAX){
    series <- series / 10
  }
  else if (type == C_W){
    series <- 0.75 * ((series * 1000) / 3600)
    # series <- series / 10
  }
  return(series)
}
