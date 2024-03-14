#' ---
#' title: Functions used in the quality control process
#' author: Santiago Beguería, Miquel Tomas-Burguera
#' date: 2024-09-02
#' version: 0.0 (very preliminar).
#' abstract: Functions that are used in the QC process. This is a re-written
#' (and highly restructed) set of functions by S. Beguería, based on the
#' original QC code by M. Tomas. There might be substantial differences between
#' the two codes.
#' ---

require(tidyverse)
require(Hmisc)
require(ggpmisc)
require(patchwork)


# Read and format raw data ------------------------------------------------

# Functions for reading raw data files

#' Read raw meteorological data into long format
#'
#' This function reads meteorological data provided in raw text files by AEMET,
#' and re-formats the data to have a consistent structure with stations / dates
#' as rows and variables / values as columns (long format).
#'
#' @param path Path to a single data file, or to a directory containing
#' several data files.
#'
#' @details The function expects the data files to have a specific format. In
#' particular, the file names must end with extension ".csv.gz" (e.g., they
#' are compressed), and they contain one of the following chains within their
#' names: "Termo", "Pluvio", "Humedad", "Viento", "Presion", "Radiacion",
#' "Insolacion". Other file names will be ignored.
#'
#' If parameter `path` point to one specific file, only that file will be read.
#' If it points to a directory, all the files that abide to the above name
#' structure will be read. The function applies a recursive search, so other
#' directories within the `path` will also be searched for readable files.
#'
#' @return A data.frame containing the meteorological data in long format. Each
#' row represents one observation, and the columns are as follows:
#'
#' \describe{
#'   \item{INDICATIVO}{Station identifier}
#'   \item{AÑO}{Year of the observation}
#'   \item{MES}{Month of the observation}
#'   \item{DIA}{Day in the month of the observation}
#'   \item{VAR}{Variable observed}
#'   \item{VALUE}{Value of the observation}
#' }
#'
#' The following table contains a list of the variable acronyms, their meaning,
#' and their units:
#'
#' \describe{
#'   \item{TMAX}{Maximum daily temperature (x1/10 ºC)}
#'   \item{TMIN}{Minimum daily temperature (x1/10 ºC)}
#'   \item{P}{Daily cumulative precipitation (x1/10 mm)}
#'   \item{HU00}{Relative air humidity at 00 h UTC (%)}
#'   \item{HU07}{Relative air humidity at 07 h UTC (%)}
#'   \item{HU13}{Relative air humidity at 13 h UTC (%)}
#'   \item{HU18}{Relative air humidity at 18 h UTC (%)}
#'   \item{HUMAX}{Maximum daily relative air humidity (%)}
#'   \item{HUMIN}{Minimum daily relative air humidity (%)}
#'   \item{DIR_00}{Wind direction at 00 h UTC (x1/10 degrees; 0 = no wind)}
#'   \item{VEL_00}{Wind speed at 00 h UTC (km / h)}
#'   \item{DIR_07}{Wind direction at 07 h UTC (x1/10 degrees; 0 = no wind)}
#'   \item{VEL_07}{Wind speed at 07 h UTC (km / h)}
#'   \item{DIR_13}{Wind direction at 13 h UTC (x1/10 degrees; 0 = no wind)}
#'   \item{VEL_13}{Wind speed at 13 h UTC (km / h)}
#'   \item{DIR_18}{Wind direction at 18 h UTC (x1/10 degrees; 0 = no wind)}
#'   \item{VEL_18}{Wind speed at 18 h UTC (km / h)}
#'   \item{R_MAX_DIR}{Direction of the strongest daily wind gust (1/10 degrees)}
#'   \item{R_MAX_VEL}{Speed of the strongest daily wind gust (km / h)}
#'   \item{R_MAX_HOR}{Time of the strongest daily wind gust (hh:mm)}
#'   \item{REC24}{Cumulative wind distance between 00 and 24 h UTC (km)}
#'   \item{REC77}{Cumulative wind distance between 07 of the previous day and
#'   07 h UTC (km)}
#'   \item{PRES00}{Atmospheric pressure at station reference level at 00 h UTC
#'   (x1/10 hPa)}
#'   \item{PRES07}{Atmospheric pressure at station reference level at 07 h UTC
#'   (x1/10 hPa)}
#'   \item{PRES13}{Atmospheric pressure at station reference level at 13 h UTC
#'   (x1/10 hPa)}
#'   \item{PRES18}{Atmospheric pressure at station reference level at 18 h UTC
#'   (x1/10 hPa)}
#'   \item{PRESMAX}{Maximum daily atmospheric pressure at station reference
#'   level (x1/10 hPa)}
#'   \item{PRESMIN}{Minimum daily atmospheric pressure at station reference
#'   level (x1/10 hPa)}
#'   \item{RDIRDIA}{Radiación directa total diaria (x10 kJ m^-2)}
#'   \item{RDIFDIA}{Radiación difusa total diaria en plano horizontal
#'   (x10 kJ m^-2)}
#'   \item{RGLODIA}{Radiación global total diaria en plano horizontal
#'   (x10 kJ m^-2)}
#'   \item{TOTSOL}{Total daily insolation (x1/10 h)}
#' }
#'
#' @examples
#' # Read all files in path:
#' kk <- qc_read_long(path = "./data_raw")
#' # Read a single file:
#' kk <- qc_read_long(path = "./data_raw/PTI_Radiacion_Cuenca_0.csv.gz")
#'
#' @export
#'
# qc_read_to_long <- function(path, source = "AEMET") {
#   
#   # If source is not AEMET use the appropriate function
#   # TO DO: include all data sources into a single function
#   if (source == "SIAR") {
#     stop("Read SIAR data is not yet implemented.")
#   } else if (source != "AEMET") {
#     stop("Source must be one of 'AEMET', 'SIAR'.")
#   }
# 
#   # Get the list of files to read
#   if (grepl(".csv.gz", path)) {
#     files <- path
#   } else {
#     files <- list.files(
#       path = path,
#       pattern = paste0("^(\\w*)(", file_name, ")(\\w*)(.csv.gz)$"),
#       full.names = TRUE, recursive = TRUE
#     )
#     files <- files[!grepl("Description", files)]
#     files <- files[!grepl("Maestro", files)]
#   }
# 
#   # Read the files
#   DF <- NULL
#   for (f in files) {
#     # Read file f
#     dat <- read.table(
#       file = f, sep = ";", header = TRUE,
#       encoding = "latin1", quote = "", dec = ","
#     ) %>%
#       dplyr::select(!NOMBRE:LATITUD)
# 
#     # Specific data mangling according to variable
#     if (grepl("Termo", f)) {
#       dat <- rbind(
#         dat %>%
#           dplyr::select(INDICATIVO:MES, TMAX1:TMAX31) %>%
#           tidyr::pivot_longer(cols = TMAX1:TMAX31) %>%
#           dplyr::mutate(
#             name = gsub("TMAX", "", name),
#             VAR = "TMAX"
#           ) %>%
#           dplyr::rename(DIA = name, VALUE = value),
#         dat %>%
#           dplyr::select(INDICATIVO:MES, TMIN1:TMIN31) %>%
#           tidyr::pivot_longer(cols = TMIN1:TMIN31) %>%
#           dplyr::mutate(
#             name = gsub("TMIN", "", name),
#             VAR = "TMIN"
#           ) %>%
#           dplyr::rename(DIA = name, VALUE = value)
#       ) %>%
#         dplyr::select(INDICATIVO, AÑO, MES, DIA, VAR, VALUE) %>%
#         na.omit() %>%
#         as.data.frame()
#     } else if (grepl("Pluvio", f)) {
#       dat <- dat %>%
#         dplyr::select(INDICATIVO:MES, P1:P31) %>%
#         tidyr::pivot_longer(cols = P1:P31) %>%
#         dplyr::mutate(
#           name = gsub("P", "", name),
#           VAR = "P"
#         ) %>%
#         dplyr::rename(DIA = name, VALUE = value) %>%
#         dplyr::select(INDICATIVO, AÑO, MES, DIA, VAR, VALUE) %>%
#         na.omit() %>%
#         as.data.frame()
#     } else if (grepl("Humedad|Viento|Presion|Radiacion|Insolacion", f)) {
#       dat <- dat %>%
#         tidyr::pivot_longer(cols = !INDICATIVO:DIA) %>%
#         dplyr::rename(VAR = name, VALUE = value) %>%
#         na.omit() %>%
#         as.data.frame()
#     } else {
#       stop("The data read does not correspond to any of the expected variables.")
#     }
# 
#     # Append new data to DF object
#     DF <- rbind(DF, dat)
#   } # next file
# 
#   # Remove empty months
#   rowAny <- function(x) rowSums(x) > 0
#   DF <- DF %>%
#     dplyr::filter(rowAny(across(-c(1:3), ~ !is.na(.))))
# 
#   # Remove duplicates
#   #    DF <- DF[!duplicated(DF[, c("INDICATIVO", "AÑO", "MES")],
#   #                         fromLast = TRUE), ]
# 
#   # Return
#   return(DF)
# }


#' Read and preprocess meteorological data
#'
#' This function reads meteorological data provided in raw text files by AEMET,
#' and then it re-formats the data to have a _long_ structure in which each
#' row contains one month worth of data for a given station, month and year,
#' while columns contain observed values of one or more variables at each
#' day within said month.
#'
#' @param path Path to the raw data files.
#' @param type Character vector, indicating the file type to read. Possible
#' values are "Termo", "Pluvio", "Humedad", "Viento", "Insolacion",
#' "Radiacion", and "Presion".
#' @param source Data source. By default, "AEMET" is assumed. That's the only
#' source file type incorporated, as of now, so no other values are accepted.
#'
#' @return A data.frame structured as explained in the introduction. Note that
#' the columns might contain more than one variable, for instance `TMAX` and
#' `TMIN` in the case of temperature. The naming convention for the columns is
#' a prefix denoting the variable followed by a number that represents the day
#' within the month. There are 31 data columns for each month, and the months
#' with less than 31 days contain NA values on the extra days.
#'
#' @details The function expects the data files to have a specific format.
#' Column names and their structure vary depending on the variable of interest.
#' For instance, Precip and Termo data are stored in `long_month` format, where
#' each fow contains one month worth of data (with 31 columns for each
#' variable), while Insolacion is stored in `long_day` format, where each row
#' represents one day. The argument `type` is therefore required for the
#' function to know what data structure to expect.
#'
#' @export
qc_read_raw_data <- function(path, type, source = "AEMET") {
  
  # If source is not AEMET use the appropriate function
  # TO DO: include all data sources into a single function
  if (source == "SIAR") {
    stop("Read SIAR data is not yet implemented.")
  } else if (source != "AEMET") {
    stop("Source must be one of 'AEMET', 'SIAR'.")
  }
  
  # Check the variable type
  valid_types <- c("Termo", "Pluvio", "Humedad", "Viento", "Insolacion",
                   "Radiacion", "Presion")
  if (!(type %in% valid_types)) {
    paste("Type must be one of:", paste(valid_types, collapse = ', '))
  }
  
  # Get the list of text files for the variable of interest
  if (grepl(".csv.gz", path)) {
    files <- path
  } else {
    files <- list.files(
      path = path,
      pattern = paste0("^(\\w*)(", type, ")(\\w*)(.csv.gz)$"),
      full.names = TRUE, recursive = TRUE
    )
    files <- files[!grepl("Description", files)]
    files <- files[!grepl("Maestro", files)]
  }
  
  # Read the data
  DF <- NULL
  for (f in files) {
    DF <- rbind(
      DF,
      read.table(
        file = f, sep = ";", header = TRUE,
        encoding = "latin1", quote = "", dec = ","
      )
    )
  }

  # Remove unwanted columns
  if (type == "Pluvio") {
    DF <- DF %>%
      dplyr::select(-MARCA)
  }
  
  # Arrange data into long-month format (only for file types in long-day format)
  if (type %in% c("Humedad", "Viento", "Radiacion", "Presion")) {
    
    vals <- "00|07|13|18|MAX|MIN|REC24|REC77|RDIRDIA|RDIFDIA|RGLODIA"
    DF <- tidyr::pivot_wider(
      data = DF %>% dplyr::arrange(DIA),
      id_cols = INDICATIVO:LATITUD,
      names_from = DIA,
      names_sep = "",
      values_from = colnames(DF)[grep(vals, colnames(DF))]
    ) %>%
      as.data.frame()
    
  } else if (type %in% c("Insolacion")) {
    
    vals <- "TOTSOL"
    DF <- tidyr::pivot_wider(
      data = DF,
      id_cols = INDICATIVO:LATITUD,
      names_from = DIA,
      values_from = colnames(DF)[grep(vals, colnames(DF))],
      names_prefix = paste0(colnames(DF)[grep(vals, colnames(DF))], "")
    ) %>%
      as.data.frame()
    
  }
  
  # Return
  return(DF)
  
}




# qc_class basics ---------------------------------------------------------

# Classes

# qc: a class to store the data and quality-control metadata. It is an RC
# class, so it has fields treated by reference. Contains the following fields:
#   `var_name`: variable name (a character vector)
#   `stations`: station metadata (a DF)
#   `long`: contains the raw data and metadata, in long format (a DF)
#   `wide`: contains the raw data and metadata, in wide format (a DF)
# `qc()` is a generator function that instantiates a new `qc` objects, that is
# it behaves like `matrix()`, `data.frame()`, etc.
#
# Note for developers: during development, if we modify an already existing
# method (while debugging, for instance), any RC object already created will
# still be bond to the old version of the method. To update the class
# definition so the old object can access the new version, the easiest way is
# to copy the object, e. g.: my_object <- my_object$copy().
#
qc <- setRefClass(
  Class = "qc",
  fields = c(
    var_type = "character",
    var_name = "character",
    var_longname = "character",
    var_unit = "character",
    stations = "data.frame",
    long = "data.frame",
    wide = "data.frame")
)


# General methods - TO DO

# print
# summary
# head

qc$methods(
  
  # Populate the 'stations', 'long' and 'wide' items of the qc object from
  # raw data read from file.
  qc_add_data = function(data) {

    id_cols <- grep(
      "INDICATIVO|NOMBRE|ALTITUD|C_X|C_Y|NOM_PROV|LONGITUD|LATITUD",
      colnames(data)
    )
    data_cols <- grep(paste0(var_name, "\\d+"), colnames(data))

    # add stations data
    stations <<- data[id_cols] %>%
      unique() %>%
      dplyr::arrange(INDICATIVO) %>%
      as_tibble()
    
    # add data in long format
    long <<- dplyr::select(dat, INDICATIVO:MES, all_of(data_cols)) %>%
      as_tibble()
    
    # correct negative values in precipitation
    if (var_name == "P") {
      long <<- qc_negative_precip(long)
    }
    
    # add data in wide format
    wide <<- qc_long_to_wide(long)
  }
  
)


# Long format -------------------------------------------------------------

# These functions work on the data stored in long format, where each row
# contains a month worth of data. These are no qc methods, so they return
# objects.


## General functions ----

# Determine columns containing id information
qc_id_cols <- function(data) {
  id_cols <- which(colnames(data) %in% c(
    "INDICATIVO", "AÑO", "MES", "DIA", "NOMBRE", "ALTITUD",
    "C_X", "C_Y", "NOM_PROV", "LONGITUD", "LATITUD"
  ))
  return(id_cols)
}

# Determine columns containing data
qc_data_cols <- function(data) {
  data_cols <- setdiff(
    1:ncol(data),
    c(qc_id_cols(data), qc_meta_cols(data))
  )
  return(data_cols)
}

# Determine columns containing QC metadata
qc_meta_cols <- function(data) {
  meta_cols <- grep("minus3|minus4|ndays|nrecords|resol|dupl|malformed|empty|zero|misencoded|dupl*|remove",
                    colnames(data))
  return(meta_cols)
}

# Transpose data matrix from long (rows_are_months, RAM) to wide
# (rows_are_days, RAD) formats.
qc_long_to_wide <- function(data) {
  
  idc <- qc_id_cols(data)
  rc <- which(colnames(data) == 'resol')
  dc <- qc_data_cols(data)
  prefix <- gsub('1', '', colnames(data)[dc][1])
  
  # transpose the data: first to long...
  data_long <- tidyr::pivot_longer(
    data = data[, c(idc,dc,rc)],
    cols = all_of(dc),
    names_to = "DIA",
    names_prefix = prefix
  ) %>%
    dplyr::filter(!is.na(value)) %>% 
    dplyr::select(INDICATIVO, AÑO, MES, DIA, value) %>% 
    dplyr::mutate(DIA = as.integer(DIA)) %>%
    dplyr::arrange(INDICATIVO, AÑO, MES, DIA)
  
  # ... and then to wide again
  data_wide <- pivot_wider(
    data = data_long %>% 
      dplyr::select(INDICATIVO, AÑO, MES, DIA, value),
    names_from = "INDICATIVO"
  ) %>%
    dplyr::arrange(AÑO, MES, DIA) %>%
    tidyr::nest(id = all_of(1:3),
                data = all_of(4:ncol(.)))
  
  return(data_wide)
  
}

# For precipitation only: remove the -3 and -4 values (missing observations).
# Note that the values after any sequence of -3 has to be removed as well.
qc_negative_precip <- function(data) {
  
  dc <- qc_data_cols(data)
  corr <- data[,dc] %>% as.matrix()
  
  # set -3 values (non-measurable precipitation) to zero
  minus3 <- rowSums(data[,dc] == -3, na.rm = TRUE)
  w <- which(corr == -3)
  corr[w] <- 0
  
  # set -4 values (non-measured / accumulated) and the nex record to NA
  minus4 <- rowSums(data[,dc] == -4, na.rm = TRUE)
  w <- which(corr == -4)
  corr[w] <- NA
  corr[w+1] <- NA
  
  return(
    cbind(data[,-dc], corr, minus3, minus4) %>%
    as_tibble()
  )
  
}


## General metods ----

qc$methods(
  
  # Determine the number of days in month.
  qc_num_days = function() {
    dim <- paste(long$AÑO, long$MES, "01", sep = "-") %>%
      as.Date() %>%
      Hmisc::monthDays()
    long$ndays <<- dim
  },
  
  # Determine the number of records in month.
  qc_num_records = function() {
    dc <- qc_data_cols(long)
    rim <- apply(X = long[, dc],
                 MARGIN = 1,
                 FUN = function(x) sum(!is.na(x)))
    long$nrecords <<- rim
  },
  
  # Determine the data resolution in month.
  qc_resolution = function(thres = 10) {
    dc <- qc_data_cols(long)
    resol <- apply(long[,dc], 1, function(x) {
      if (sum(!is.na(x)) < thres) {
        return(NA)
      }
      if (sum(x %% 10, na.rm = TRUE) == 0) {
        return(1)
      }
      if (sum(x %% 5, na.rm = TRUE) == 0) {
        return(0.5)
      }
      if (var_name == "P" & sum(x %% 2, na.rm = TRUE) == 0) {
        return(0.2)
      }
      if (sum(x %% 1, na.rm = TRUE) == 0) {
        return(0.1)
      }
    }) %>% 
      unlist()
    long$resol <<- resol
  },

  ## Controls: encoding errors ----

  # Find malformed months (more data than days in the month). Returns a
  # logical vector, with TRUE meaning that the month has more data than days.
  qc_malformed_months = function() {
    
    long$malformed <<- long$nrecords > long$ndays
    
  },

  # Find empty months (all NA) - TRUE if empty
  qc_empty_months = function() {
    
    dc <- qc_data_cols(long)
    long$empty <<- rowSums(!is.na(long[,dc])) == 0
      
  },
  
  # Find months that consist on only zeros - TRUE if all zero
  qc_zero_months = function() {
    
    dc <- qc_data_cols(long)
    nzeros_in_month <- rowSums(long[,dc] == 0, na.rm = TRUE)
      
    # compare with number of zeros
    long$zero <<- nzeros_in_month == long$ndays

  },

  # Find misencoded months (are coded in ºC, instead of 1/10 ºC)
  #
  # Problem: There are months with temperature values that apparently are in
  # degrees, and not in tenths of a degree. As a result, abnormally low
  # variability is apparent, and (usually) very low max. temp values.
  #
  # Applied solution: These months are detected and removed. Reconstruction of
  # the data (e.g., multiplication x 10) has not been chosen because there are
  # very few cases and, furthermore, there are cases that are clearly misencoded,
  # but the reconstructed value does not match the month's climatology. Detection
  # is done as follows (depends on the variable):
  #
  #  Maximum temperature: maximum of the month is less than 4°C and, furthermore,
  #   the thermal amplitude of the maximum temperatures is less than 3°C.
  #
  #  Minimum temperature: all values of the month are between +2°C and -2°C and,
  #   furthermore, the thermal amplitude of the minimum temperatures is less than
  #   3°C.
  #
  # This test is only applied to complete months (or ndata > thres).
  #
  qc_misencoded_months = function(thres = NULL) {

    dc <- qc_data_cols(long)
      
    # determine complete months
    if (is.null(thres)) {
      thres <- long$ndays
    }
    is_complete <- rowSums(!is.na(long[,dc])) >= thres
      
    # determine misencoded
    mx <- apply(long[,dc], 1, function(x) {
      if (sum(!is.na(x)) > 2) {
        max(x) / 10
      } else {
        NA
      }
    }) %>%
      unlist()
    mn <- apply(long[,dc], 1, function(x) {
      if (sum(!is.na(x)) > 2) {
        min(x) / 10
      } else {
        NA
      }
    }) %>%
      unlist()
    low_range <- (mx - mn) <= 3
    if (var_name == 'TMAX') {
      is_misencoded <- low_range & mx < 4
    } else if (var_name == 'TMIN') {
      is_misencoded <- low_range & mx < 2 & mn > -2
    }
      
    long$misencoded <<- is_complete & is_misencoded
      
  },
  
  
  ## Controls: duplicated months ----

  # # Find and characterize duplicated months.
  # #
  # # Problem: There are complete months whose values, day by day, appear in
  # # another point in the database, whether within the series of the same
  # # station or within the series of another station. This is considered not
  # # possible
  # #
  # # Applied solution: All months that appear duplicated at another point in the
  # # database are detected and removed. Only duplicated data between stations
  # # less than 1km apart ("collocated" stations) are retained.
  # #
  # # This functions applies in a sequence the following: `qc_dupl_months_find()`,
  # # `qc_dupl_months_match()`, `qc_dupl_months_dist()`, 'qc_dupl_months_type()',
  # # and `qc_dupl_months_prob()`. As a result, the following new columns are
  # # added to `@long`: `dupl`, `dupl_match`, `dupl_dist`, `dupl_type`,
  # # and `dupl_prob`.
  # #
  # qc_dupl_months = function(start = 1, end = 31,
  #                            thres = NULL, verbose = TRUE) {
  #   
  #   if (verbose) print('Searching duplicated months...')
  #   .self$qc_dupl_months_find(start, end, thres)
  #   
  #   if (verbose) print('Matching the stations...')
  #   .self$qc_dupl_months_match()
  #   
  #   if (verbose) print('Computing distances...')
  #   .self$qc_dupl_months_dist()
  #   
  #   if (verbose) print('Characterizing types...')
  #   .self$qc_dupl_months_type()
  #   
  #   if (verbose) print('Computing probabilities...')
  #   .self$qc_dupl_months_prob()
  #   
  #   if (verbose) print('Done')
  #   
  # },
  
  # Find duplicated months.
  # Returns a logical vector indicating whether a given record (month) is
  # duplicated or not. Months with less data than thres will not be considered,
  # as well as months that consist only on zeros.
  qc_dupl_months_find = function(start = 1, end = 31, thres = NULL) {
    
    if (is.null(thres)) {
      thres <- long$ndays
    }
    
    data_cols <- qc_data_cols(long)[start:end]
    
    is_complete <- rowSums(!is.na(long[, data_cols])) >= thres
    is_duplicate <- duplicated(long[, data_cols]) |
      duplicated(long[, data_cols], fromLast = TRUE)
    
    # extra requirements for pcp: consider the monthly total and the number of wet days
    if (var_name == "P") {
      cum <- rowSums(long[, data_cols], na.rm = TRUE)
      wet <- rowSums(long[, data_cols] > 0, na.rm = TRUE)
      long$dupl <<- !long$zero & cum > 100 & wet > 3 & is_complete & is_duplicate
    } else {
      long$dupl <<- !long$zero & is_complete & is_duplicate
    }

    
  },
  
  # Find the rows that are duplicated.
  # Returns a list of the same length than nrow(data) indicating the
  # identifier(s) of the weather station(s) that is(are) duplicated (or NA if
  # the data are unique).
  #
  # TO DO: se puede reducir el tiempo de cálculo a la mitad, pues sólo hace
  # falta recorrer la mitad de los meses marcados, ya que se repiten por
  # parejas.
  #
  qc_dupl_months_match = function() {

    data_cols <- qc_data_cols(long)
    long$id <<- 1:nrow(long)

    match <- as.list(rep(NA, nrow(long)))
    for (i in which(long$dupl)) {
      if (!is.na(match[[i]])) next()
      kk <- dplyr::inner_join(
        x = long[-i,],
        y = long[i,],
        by = colnames(long[, data_cols]),
        suffix = c("", ".y")
      )
      if (nrow(kk) > 0) {
        # assign the match
        match[[i]] <- kk$id
        # the following doubles the process speed
        for (j in 1:length(kk$id)) {
          if (is.na(match[[kk$id[j]]])) {
            match[[kk$id[j]]] <- i
          } else {
            match[[kk$id[j]]] <- c(match[[kk$id[j]]], i)
          }
        }
      }
    }

    long <<- long %>% 
      dplyr::select(-id)
    
    long$dupl_match <<- match
    
  },
  
  # Find the distance of duplicated months.
  # Returns a numeric vector with the euclidean distance to the station that is
  # duplicated (or NA if there is no duplication).
  # TO DO: se puede reducir el tiempo de cálculo a la mitad, pues sólo hace
  # falta recorrer la mitad de los meses marcados, ya que se repiten por parejas.
  qc_dupl_months_dist = function() {
    
    dist <- as.list(rep(NA, length(long$dupl)))
    for (i in which(long$dupl)) {
      # determine stations: candidate and repeated
      ws <- which(long$INDICATIVO[i] == stations$INDICATIVO)
      wss <- lapply(long$INDICATIVO[long$dupl_match[[i]]],
                    function(x) which(stations$INDICATIVO == x)
                    ) %>%
        unlist()
      kk <- rbind(
        stations[ws, c("C_X", "C_Y")],
        stations[wss, c("C_X", "C_Y")]
      ) %>%
        dist() %>%
        as.matrix()
      dist[[i]] <- round(kk[1, -1] / 1000, 1)
    }
    
    long$dupl_dist <<- dist
    
  },

  # Determine the type of duplication.
  # Returns a numeric vector indicating the type of duplicate:
  # * 1: same station, same year, different month (syM)
  # * 2: same station, different year, same month (sYm)
  # * 3: same station, different year, different month (sYM)
  # * 4: different station, same year, same month (Sym)
  # * 5: different station, different year and/or different month (SYM)
  qc_dupl_months_type = function() {
    
    type <- as.list(rep(NA, nrow(long)))
    for (i in which(long$dupl)) {
      n <- length(long$dupl_match[i][[1]])
      for (j in 1:n) {
        k <- long$dupl_match[i][[1]][j]
        same_st <- long$INDICATIVO[i] == long$INDICATIVO[k]
        same_year <- long$AÑO[i] == long$AÑO[k]
        same_month <- long$MES[i] == long$MES[k]
        type[[i]][j] <- case_when(
          (same_st & same_year & !same_month) ~ 1,
          (same_st & !same_year & same_month) ~ 2,
          (same_st & !same_year & !same_month) ~ 3,
          (!same_st & same_year & same_month) ~ 4,
          (!same_st & !same_year & same_month) ~ 5,
          (!same_st & same_year & !same_month) ~ 5,
          (!same_st & !same_year & !same_month) ~ 5
        )
      }
    }
    
    long$dupl_type <<- type
    
  },
  
  # Determine the probability of the duplicated data.
  # Returns a vector giving the probability of the repeated data (PDF = p(x <= X)),
  # according to that station and month climatology (mean and sd).
  qc_dupl_months_prob = function() {
    
    data_cols <- qc_data_cols(long)
    prob <- as.list(rep(NA, length(long$dupl)))
    for (i in which(long$dupl)) {
      
      # find data for same station and month, excluding the target month
      wm <- which(long$INDICATIVO == long$INDICATIVO[i] &
                    long$MES == long$MES[i]) %>% 
        setdiff(., i)
      
      # compute empirical probability for target month (min, mean, max)
      if (length(wm) == 0) next()
      f <- ecdf(long[wm, data_cols] %>% unlist())
      p <- f(long[i, data_cols] %>% unlist()) %>% na.exclude()
      prob[[i]] <- c(min(p), mean(p), max(p)) %>%
        round(3)
    }
    
    long$dupl_prob <<- prob
    
  },
  
  ## Bad months handling ----

  # Mark the months that shall be removed, according to the following criteria:
  #
  # * The month is malformed.
  # * The month is empty.
  # * The month contains only zeros (not applied for `Precip`).
  # * The month is duplicated, except if it's the same year and month and the two
  #   stations are collocated (distance < 1 km).
  #   (No probability criteria used, at the moment; this is only for reporting.)
  qc_flag_month = function() {
    
    malf <- long$malformed
    empty <- long$empty
    if (var_name == "P") {
      zero <- rep(FALSE, nrow(long))
    } else {
      zero <- long$zero
    }
    dupl <- long$dupl
    match <- long$dupl_match
    dist <- long$dupl_dist
    type <- long$dupl_type
    
    rem <- rep(NA, length(long$dupl))
    for (i in which(malf | empty | zero | dupl)) {
      if (malf[i] | empty[i] | zero[i]) {
        rem[i] <- TRUE
      } else {
        dd <- dist[i][[1]]
        tt <- type[i][[1]]
        for (j in 1:length(dd)) {
          if (dupl[i] & tt[j] < 4) {
            # same station, flag
            rem[i] <- TRUE
          } else if (dupl[i] & tt[j] > 3 & dd[j] > 1) {
            # different station not collocated, flag
            rem[i] <- TRUE
          } else if (dupl[i] & tt[j] > 3 & dd[j] <= 1) {
            # different station collocated, do not flag
            rem[i] <- FALSE
          }
        }
      }
    }

    long$remove <<- rem
    
  },
  
  # Remove flagged months and add the quality-controlled data as item
  # `data_qc_1` on wide format.
  qc_clean_to_long_to_wide = function() {
    
    clean_wide <- long %>% 
      dplyr::filter(is.na(.$remove) | !.$remove) %>%
      qc_long_to_wide()

    wide <<- wide %>%
      tibble::add_column(data_qc_1 = clean_wide$data)

  }
  
)



# Wide format -------------------------------------------------------------

# These functions work on the data stored in wide format, where each row
# corresponds to one day and each column stores the data from one stations.
# Therefore, they operate on `qc@wide`.

qc$methods(

  ## General methods ---
  
  # Move one data column from long to wide data formats.
  qc_add_var_to_long = function(col_name) {
    
#    data <- long
    idc <- qc_id_cols(long)
    wc <- which(colnames(long) == col_name)
    dc <- qc_data_cols(long)
    prefix <- gsub('1', '', colnames(long)[dc][1])
    
    # transpose the data: first to long...
    data_long <- tidyr::pivot_longer(
      data = long[, c(idc,dc,wc)],
      cols = all_of(dc),
      names_to = "DIA",
      names_prefix = prefix
    ) %>%
      dplyr::filter(!is.na(value)) %>% 
      dplyr::select(INDICATIVO, AÑO, MES, DIA, resol) %>% 
      dplyr::mutate(DIA = as.integer(DIA)) %>%
      dplyr::arrange(INDICATIVO, AÑO, MES, DIA)
    
    # ... and then to wide again
    data_wide <- pivot_wider(
      data = data_long,
      names_from = "INDICATIVO",
      values_from = "resol"
    ) %>%
      dplyr::arrange(AÑO, MES, DIA) %>%
      tidyr::nest(id = all_of(1:3),
                  data = all_of(4:ncol(.)))
    
    wide <<- wide %>%
      tibble::add_column(resolution = data_wide$data)
    
  },
  
  ## Controls: sequences ----
  
  # Find stationary periods.
  # Returns a logical vector indicating whether a given record (month) belongs
  # to a stationary period or not. Sequences are flagged when there are a number
  # `n` = 7 (15) consecutive days with identical values, for data resolutions of
  # 0.5 / '0.1 (1) °C.
  qc_stationary_sequence = function() {
    
    resol <- wide$resolution[[1]]
    data <- wide$data_qc_1[[1]]
    thres <- ifelse(resol == 1, 15, 7) %>% 
      as_tibble()
    
    # Initialize an empty object with all zeros  
    static <- data %>%
      dplyr::mutate_all(function(x) x <- 0)
    
    # Now go station by station
    # TO DO: parallelize, or at least lapply()
    for (i in 1:ncol(data)) {
      seq <- rle(data[[i]])
      long_seq <- which(seq$lengths >= 7)
      if (length(long_seq) > 0) {
        for (j in 1:length(long_seq)) {
          nseq <- seq$lengths[long_seq[j]]
          nstart <- sum(seq$lengths[1:(long_seq[j]-1)]) + 1
          th <- max(thres[[i]][nstart:(nstart+nseq-1)])
          if (nseq >= th) {
            static[[i]][nstart:(nstart+nseq-1)] <- 1
          }
        }
      }
    }
    
    wide <<- wide %>%
      tibble::add_column(stationary = list(static))
    
    # TO DO: using sparse matrix to save a ton of memory  
    # require(Matrix)
    # B <- Matrix::Matrix(as.matrix(seq), sparse = TRUE)
    # kk <- obj@wide %>% tibble::add_column(stationary = list(B))
    
  },
  
  
  # Find repeated sequences.
  # Problem: Sequences of 10 or more days that are repeated in a different
  # position in the series of the same station. (Note the difference with the
  # previous control of duplicated months, which is carried out on the complete
  # database and involve complete months).
  #
  # Applied solution: Repeated sequences are detected, and they are removed if:
  #
  #   repetition of the sequence within the same month and year;
  #   repetition of the sequence between different months of the same year,
  #     as long as the days coincide;
  #   repetition of the sequence in consecutive months within the same year,
  #     even if the days do not coincide;
  #   repetition of the sequence between different years, as long as they belong
  #     to the same month and the same days.
  #
  # Returns a logical vector indicating whether a given record (month) belongs
  # to a repeated sequence or not.
  #
  # (These criteria have been applied because there are repeated sequences that
  # do not meet them, but it is very difficult to discern whether the data are
  # correct or not. *A priori*, the probability of repetition of sequences of 10
  # days should be very low, but when I visualized the data, I had doubts about 
  # its quality.)
  #
  # (This method allows for the detection of sequences of repeated values with a
  # duration exceeding 10 days. A repeated sequence of 11 days contains two
  # consecutive repeated sequences of 10 days, one from the first to the tenth
  # element, and another from the second to the eleventh. These sequences have
  # been treated with the same temporal restrictions as those of 10 days.)
  #
  # qc_repeated_sequence <- function(obj, thres = NULL) {
  #   
  #   
  #   obj@meta_long$duplicated <- is_duplicated
  #   
  #   return(obj)
  # },
  
  
  
  ## Controls: outliers ----
  
  # Find false zeros.
  # Problem: Erroneous encoding of missing data, where a temperature value of '0'
  # is entered into the database when it should be an NA (not available).
  #
  # Applied solution: Suspect zeros are found and flagged. The criteria to 
  # detect a suspect zero is the following: if for a specific month, once the
  # zero is removed, the minimum temperature is above 10°C, it is considered
  # that the 0 is false, and therefore the observation is flagged.
  #
  qc_false_zeros = function(thres = NULL) {
    
    id <- wide$id[[1]]
    data <- wide$data_qc_1[[1]]
    
    if (is.null(thres)) {
      thres <- 100
    }
    
    # Initialize an empty object with all zeros  
    false0 <- data %>%
      dplyr::mutate_all(function(x) x <- 0)
    
    # Now go station by station
    # TO DO: at least, move this to lapply()
    for (i in 1:ncol(data)) {
      wz <- which(data[[i]] == 0)
      if (length(wz) > 0) {
        for (j in 1:length(wz)) {
          # Miquel's method: flag as false zero if min of the month
          # once the zeros are removed is > 10 ºC
          wr <- which(id$AÑO == id$AÑO[wz[j]] & id$MES == id$MES[wz[j]])
          dat <- data[[i]][wr]
          dat <- dat[-which(is.na(dat) | dat == 0)]
          if (length(dat) == 0) next()
          if (min(dat) > thres) {
            false0[[i]][wz[j]] <- 1
          }
          # # Santiago's method: flag as false zero if the difference between
          # # that month's ECDF and the ECDF of zero is < 0.1
          # wr <- which(id$MES == id$MES[wz[j]])
          # dat <- data[[i]][wr]
          # dat <- dat[-which(is.na(dat) | dat == 0)]
          # if (length(dat) == 0) next()
          # f <- ecdf(dat)
          # # if (f(0) < 0.1) {
          # #   false0[[i]][wz[j]] <- 1
          # # }
          # wr <- which(id$AÑO == id$AÑO[wz[j]] & id$MES == id$MES[wz[j]])
          # dat <- data[[i]][wr]
          # dat <- dat[-which(is.na(dat))]
          # if (length(dat) == 0) next()
          # if (f(mean(dat)) - f(0) > 0.1) {
          #   false0[[i]][wz[j]] <- 1
          # }
        }
      }
    }
    table(colSums(false0))
    
    wide <<- wide %>%
      tibble::add_column(false_0 = list(false0))
    
    # OPTION using sparse matrix to save a ton of memory  
    # require(Matrix)
    # B <- Matrix::Matrix(as.matrix(seq), sparse = TRUE)    # Thanks to Aaron for pointing this out
    # kk <- obj@wide %>% tibble::add_column(stationary = list(B))
    
  },
  
  
  # Find outlier values.
  # Problem: Some values are out of the accepted range for Spain.
  #
  # Applied solution: All outliers are removed. The filters applied are:
  #
  #  Tmax > 47.6ºC (official record in Spain, La Rambla, Córdoba, Aug. 14 2021)
  #  Tmin < -30.0ºC (official record in Spain, Calamocha, Zaragoza, Dec. 17 1963)
  #
  # Argument `thres`, mandatory, is a pair of values that determine the minimum
  # and maximum accepted values of the observation, e.g. `thres = c(-Inf, 475)`.
  #
  qc_outliers = function(thres) {
    
    data <- wide$data_qc_1[[1]]
    
    # Initialize an empty object with all zeros  
    outliers <- data %>%
      dplyr::mutate_all(function(x) x <- 0)
    
    outliers[data < thres[1]] <- 1
    outliers[data > thres[2]] <- 1
    
    wide <<- wide %>%
      tibble::add_column(outliers = list(outliers))
    
    # OPTION using sparse matrix to save a ton of memory  
    # require(Matrix)
    # B <- Matrix::Matrix(as.matrix(A), sparse = TRUE)
    # kk <- obj@wide %>% tibble::add_column(stationary = list(B))
    
  },
  
  
  # Find suspicious values.
  # Problem: Slightly less stringent outlier detection (suspect errors), and 
  # too-large inter-day differences.
  # Applied solution: All cases are flagged. Filters:
  #
  #  Tmax > 45ºC
  #  Tmax < -10ºC
  #  Jump greater than 25ºC between consecutive days
  #
  # qc_suspicious = function() {
  #   
  # },
  
  
  # Compare tmax and tmin.
  #
  # Flag if:
  #  Tmin = Tmax = 0
  #  Tmin >= Tmax
  #  Tmax – Tmin > 35ºC
  #
  # (Note: In reality, Tmin being higher than Tmax may be due to the way readings
  # are taken. In synop stations, a nighttime temperature higher than daytime may
  # lead to this type of anomaly. We assume it's incorrect since we have no
  # information on it.)
  #
  # qc_tmax_tmin_compare = function() {
  #   
  # },
  
  # Compare with neighbours (spatial anomalies)
  # Problem: Anomalous records when compared with neighboring stations.
  # Applied solution: A 365-day moving percentile is calculated for each station
  # and compared with that of its 5 nearest neighbors. If there is a percentile
  # distance greater than 0.65 with more than two neighbors, the record is
  # flagged as suspicious. Flagged data are removed if they have also been
  # flagged in a previous control, or if they represent a temperature jump
  # compared to the previous and subsequent day higher than 5 ºC.
  #
  # TO DO: replace this control with a control based on SPDE regression (INLA)
  # and the OOB statistic (probability of observation given the remaining
  # observations and the model).
  #
  # qc_neighbours = function() {
  #   
  # },
  
  
  ## Bad days handling ----
  
  # Mark the days that shall be removed, according to the following criteria:
  #
  # * The day is part of a stationary sequence.
  # * The day has been flagged as a false zero.
  # * The day is an outlier.
  # * The day as been flagged in the comparison with other variables, or with
  #   neighbors.
  qc_flag_day = function() {
    
    flag <- wide$data_qc_1[[1]] %>%
      dplyr::mutate_all(function(x) x <- 0)
      
    if ("stationary" %in% names(wide)) {
      flag <- flag + wide$stationary[[1]]
    }
    
    if ("false_0" %in% names(wide)) {
      flag <- flag + wide$false_0[[1]]
    }
    
    if ("outliers" %in% names(wide)) {
      flag <- flag + wide$outliers[[1]]
    }
    
    flag <- flag > 0

    wide <<- wide %>%
      tibble::add_column(remove = list(flag %>% as_tibble()))
    
  },

  # Remove flagged days and create the second and definitive quality-controlled
  # data set as item `data_qc_2`.
  qc_clean_days = function() {
  
    w <- which(wide$remove[[1]] == 1)
    clean_data <- wide$data_qc_1[[1]] %>%
      as.matrix()
    clean_data[w] <- NA

    wide <<- wide %>%
      tibble::add_column(data_qc_2 = list(clean_data %>% as_tibble()))
  
  }

)


# # Returns a 'clean' data set: a d.f., in long format, once the flagged days
# # have been removed.
# qc_clean_day <- function(obj) {
#   
#   data <- obj@wide$data_qc_1[[1]]
#   
#   data <- data[-obj@wide$remove]
#   
#   return(data)
# }
# 
# qc_clean_day_then_export <- function(obj, file) {
#   
#   clean <- cbind(
#     data@wide$id,
#     qc_clean_day(obj)
#   ) %>% 
#     as.data.frame()
#   
#   write.csv(clean, file)
#   
# }


# Reporting ----

# Polygons of Spain and world's coastline, used for mapping
require(rnaturalearth)
coast <- rnaturalearth::ne_coastline(scale='medium', returnclass='sf')
spain <- rnaturalearth::ne_countries(country='spain', scale='medium', returnclass='sf')

# Basic maps
g_all <- ggplot(spain) +
  geom_sf(color='grey90', fill='grey90') +
  geom_sf(data=coast, color='grey60') +
  xlim(c(-18, 4)) +
  ylim(c(27, 44)) +
  theme_classic() +
  theme(axis.title=element_blank(),
        plot.caption=element_text(hjust=0),
        strip.background=element_blank())

g_pen <- ggplot(spain) +
  geom_sf(color='grey90', fill='grey90') +
  geom_sf(data=coast, color='grey60') +
  xlim(c(-9.2, 4)) +
  ylim(c(35, 44)) +
  theme_classic() +
  theme(axis.title=element_blank(),
        plot.caption=element_text(hjust=0),
        strip.background=element_blank())

g_can <- ggplot(spain) +
  geom_sf(color='grey90', fill='grey90') +
  geom_sf(data=coast, color='grey60') +
  xlim(c(-18, -13.5)) +
  ylim(c(27.5, 29.4)) +
  theme_classic() +
  theme(axis.title=element_blank(),
        plot.caption=element_text(hjust=0),
        strip.background=element_blank())

qc$methods(
  
  ## General methods ---
  
  # Produce a report of the quality control process for one given station.
  qc_report_station = function(s, report_dir) {
  
#    stations <- var$stations
#    long <- var$long
#    wide <- var$wide
    
    station <- stations[s,]
    
    # determine if that station exists in wide
    if (!(station$INDICATIVO %in% colnames(wide$data[[1]]))) return(NULL)
    
    is_Canary <- ifelse(station$LATITUD < 32, TRUE, FALSE)
    
    pdf(
      file = paste0(report_dir, "/", station$INDICATIVO, ".pdf"),
      width = 10
    )
    
    # Time series
    kk <- data.frame(
      dates = mapply(FUN = paste,
                     sep = "-",
                     wide$id[[1]]$AÑO, wide$id[[1]]$MES, wide$id[[1]]$DIA) %>% 
        as.Date.character(),
      ori = wide$data[[1]] %>% 
        dplyr::select(which(colnames(wide$data[[1]]) == station$INDICATIVO)),
      qcc = wide$data_qc_2[[1]] %>% 
        dplyr::select(which(colnames(wide$data_qc_2[[1]]) == station$INDICATIVO))
    ) %>% 
      dplyr::rename(ori = 2, qcc = 3)
    #   determine data removed  
    kk$rem <- ifelse(is.na(kk$qcc) & !is.na(kk$ori), kk$ori, NA)
    
    n <- sum(!is.na(kk$ori))
    m <- sum(!is.na(kk$rem))
    p <- signif(m / n * 100, 2)
    
    g1 <- ggplot(kk) +
      geom_point(aes(dates, ori), color = 'black', size = 0.5)
    if (m > 0) {
      g1 <- g1 +
        geom_point(aes(dates, rem), color = 'red', size = 0.5)
    }
    g1 <- g1 +
      theme_classic() +
      ggtitle(
        label = paste0(station$NOMBRE, " (", station$INDICATIVO, ")"),
        subtitle = paste0("Total daily records: ", n, ". Removed: ", m, " (", p, "%).")
      ) +
      xlab("Time") +
      ylab(paste0(cnfg$var$names[i], " (", cnfg$var$units, ")")) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10))
    
    # Location
    if (is_Canary) {
      
      g2 <- g_can +
        geom_point(data = station, aes(LONGITUD, LATITUD),
                   shape = 21, size = 2, color = "red") +
        ggtitle("Location") +
        theme(plot.title = element_text(hjust = 0.5, size = 12))
      
    } else {
      
      g2 <- g_pen +
        geom_point(data = station, aes(LONGITUD, LATITUD),
                   shape = 21, size = 2, color = "red") +
        geom_label(data = station, aes(LONGITUD, LATITUD, label = INDICATIVO),
                   color = "red", nudge_y = 0.25, nudge_x = 0.25, hjust = "outward") +
        ggtitle("Location") +
        theme(plot.title = element_text(hjust = 0.5, size = 12))
      
    }
    
    # Tables
    # Removed months
    w <- which(long$INDICATIVO == station$INDICATIVO)
    kk <- data.frame(
      Malformed = sum(long$malformed[w]),
      Badly_encoded = sum(long$misencoded[w], na.rm = TRUE),
      Empty = sum(long$empty[w]),
      All_zeros = sum(long$zero[w]),
      Duplicated = sum(long$dupl[w]),
      Removed = sum(long$remove[w], na.rm = TRUE)
    )
    t1 <- ggplot() +
      ggpp::annotate(geom = "table",
                     x = 1,
                     y = 1,
                     label = list(kk)) +
      ggtitle("QC controls, monthly level") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, size = 12))
    
    # Removed days
    w <- which(colnames(wide$outliers[[1]]) == station$INDICATIVO)
    kk <- data.frame(
      Stationary = sum(wide$stationary[[1]][[w]]),
      Sequences = sum(wide$sequence[[1]][[w]]),
      False_zeros = sum(wide$false_0[[1]][[w]]),
      Outliers = sum(wide$outliers[[1]][[w]]),
      Removed = sum(wide$remove[[1]][[w]])
    )
    t2 <- ggplot() +
      ggpp::annotate(geom = "table",
                     x = 1,
                     y = 1,
                     label = list(kk)) +
      ggtitle("QC controls, daily level") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, size = 12))
    
    # Main plots (first page)
    design <- "111
             111
             233
             244"
    print(g1 + g2 + t1 + t2 + plot_layout(design = design))
    
    # List of malformed months
    w <- which(long$INDICATIVO == station$INDICATIVO)
    nr <- sum(long$malformed[w])
    if (nr > 0) {
      dc <- qc_data_cols(long)
      kk <- long[w,][long$malformed[w],] %>%
        dplyr::select(INDICATIVO:MES) %>%
        dplyr::mutate(data = long[w,][long$malformed[w],] %>%
                        dplyr::select(all_of(dc)) %>%
                        t() %>%
                        as.data.frame() %>%
                        paste()
        )
      
      for (r in 1:ceiling(nr/30)) {
        wr_start <- (1 * (r-1) * 30) + 1
        wr_end <- min(wr_start + 29, nr)
        g <- ggplot() +
          ggpp::annotate(geom = "table",
                         x = 1,
                         y = 1,
                         label = list(kk[wr_start:wr_end,]),
                         size = 2) +
          ggtitle(paste0("Zero months ", ifelse(r > 1, r, ""))) +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5))
        print(g)
      }
    }
    
    # List of badly encoded months
    w <- which(long$INDICATIVO == station$INDICATIVO)
    nr <- sum(long$misencoded[w], na.rm = TRUE)
    if (nr > 0) {
      dc <- qc_data_cols(long)
      kk <- long[w,][long$misencoded[w],] %>%
        dplyr::select(INDICATIVO:MES) %>%
        dplyr::mutate(data = long[w,][long$misencoded[w],] %>%
                        dplyr::select(all_of(dc)) %>%
                        t() %>%
                        as.data.frame() %>%
                        paste()
        )
      
      for (r in 1:ceiling(nr/30)) {
        wr_start <- (1 * (r-1) * 30) + 1
        wr_end <- min(wr_start + 29, nr)
        g <- ggplot() +
          ggpp::annotate(geom = "table",
                         x = 1,
                         y = 1,
                         label = list(kk[wr_start:wr_end,]),
                         size = 2) +
          ggtitle(paste0("Badly encoded months ", ifelse(r > 1, r, ""))) +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5))
        print(g)
      }
    }
    
    # List of empty months
    w <- which(long$INDICATIVO == station$INDICATIVO)
    nr <- sum(long$empty[w])
    if (nr > 0) {
      dc <- qc_data_cols(long)
      kk <- long[w,][long$empty[w],] %>%
        dplyr::select(INDICATIVO:MES) %>%
        dplyr::mutate(data = long[w,][long$empty[w],] %>%
                        dplyr::select(all_of(dc)) %>%
                        t() %>%
                        as.data.frame() %>%
                        paste()
        )
      
      for (r in 1:ceiling(nr/30)) {
        wr_start <- (1 * (r-1) * 30) + 1
        wr_end <- min(wr_start + 29, nr)
        g <- ggplot() +
          ggpp::annotate(geom = "table",
                         x = 1,
                         y = 1,
                         label = list(kk[wr_start:wr_end,]),
                         size = 2) +
          ggtitle(paste0("Empty months ", ifelse(r > 1, r, ""))) +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5))
        print(g)
      }
    }
    
    # List of zero months
    w <- which(long$INDICATIVO == station$INDICATIVO)
    nr <- sum(long$zero[w])
    if (nr > 0) {
      dc <- qc_data_cols(long)
      kk <- long[w,][long$zero[w],] %>%
        dplyr::select(INDICATIVO:MES) %>%
        dplyr::mutate(data = long[w,][long$zero[w],] %>%
                        dplyr::select(all_of(dc)) %>%
                        t() %>%
                        as.data.frame() %>%
                        paste()
        )
      
      for (r in 1:ceiling(nr/30)) {
        wr_start <- (1 * (r-1) * 30) + 1
        wr_end <- min(wr_start + 29, nr)
        g <- ggplot() +
          ggpp::annotate(geom = "table",
                         x = 1,
                         y = 1,
                         label = list(kk[wr_start:wr_end,]),
                         size = 2) +
          ggtitle(paste0("Zero months ", ifelse(r > 1, r, ""))) +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5))
        print(g)
      }
    }
    
    # List of duplicated months
    w <- which(long$INDICATIVO == station$INDICATIVO)
    nr <- sum(long$dupl[w])
    if (nr > 0) {
      dc <- qc_data_cols(long)
      # ww <- apply(long[w,][long$dupl[w],'dupl_match'], 1,
      #             function(x) x[[1]])
      ww <- lapply(long[w,][long$dupl[w],'dupl_match'], 
                  function(x) x[[1]]) %>%
        unlist()
      kk <- cbind(
        long[w,][long$dupl[w],] %>% 
          dplyr::select(INDICATIVO:MES),
        long[ww,] %>% 
          dplyr::select(INDICATIVO:MES) %>% 
          dplyr::rename(IND.2 = INDICATIVO, AÑO.2 = AÑO, MES.2 = MES)
      ) %>% 
        dplyr::mutate(data = long[w,][long$dupl[w],] %>% 
                        dplyr::select(all_of(dc)) %>% 
                        t() %>% 
                        as.data.frame() %>% 
                        paste()
        )
      
      for (r in 1:ceiling(nr/30)) {
        wr_start <- (1 * (r-1) * 30) + 1
        wr_end <- min(wr_start + 29, nr)
        g <- ggplot() +
          ggpp::annotate(geom = "table",
                         x = 1,
                         y = 1,
                         label = list(kk[wr_start:wr_end,]),
                         size = 2) +
          ggtitle(paste0("Duplicated months ", ifelse(r > 1, r, ""))) +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5))
        print(g)
      }
    }
    
    # List of stationary sequences
    
    # List of repeated sequences
    
    # List of false zeros
    
    # List of outliers
    w <- which(colnames(wide$outliers[[1]]) == station$INDICATIVO)
    nr <- sum(wide$outliers[[1]][[w]])
    if (nr > 0) {
      #dc <- qc_data_cols(long)
      ww <- which(wide$outliers[[1]][[w]] == 1)
      kk <- cbind(
        data.frame(INDICATIVO = rep(station$INDICATIVO, length(ww))),
        wide$id[[1]][ww,],
        data.frame(data = wide$data_qc_1[[1]][ww,w])
      ) %>%
        dplyr::rename(data =5)
      
      for (r in 1:ceiling(nr/30)) {
        wr_start <- (1 * (r-1) * 30) + 1
        wr_end <- min(wr_start + 29, nr)
        g <- ggplot() +
          ggpp::annotate(geom = "table",
                         x = 1,
                         y = 1,
                         label = list(kk[wr_start:wr_end,])) +
          ggtitle(paste0("Out of range days ", ifelse(r > 1, r, ""))) +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5))
        print(g)
      }
    }
    
    dev.off()
    
  }
)