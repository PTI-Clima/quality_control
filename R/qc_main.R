#' ---
#' title: Quality control process for raw AEMET data files.
#' 
#' author: Santiago Beguería
#' 
#' date: 2024-26-02, 2024-04-03
#' 
#' version: 0.1 (pre-production)
#' 
#' abstract: This is the main file that performs the quality-control process.
#'  It reads or sources all other files (qc_config, qc_functions, qc_report).
#'  From the console, run: run e.g. `Rscript R/qc_main.R termo`, where 'termo'
#'  is a named configuration read from the configuration file `qc_config.yml`.
#'  As a result, the files and variables indicated in the configuration
#'  arguments are processed, and a number of output files are stored in the
#'  output directory (also determined in the configuration file). Output files
#'  include the complete `qc` object in an .RData file, (optional) the final
#'  data matrix in a .csv file, a gobal report in a .pdf file, and (optional)
#'  individual reports for each data series as .pdf files inside a
#'  corresponding directory.
#' ---


# Preamble ----------------------------------------------------------------

# Dependencies
library(tidyverse)
source("./R/qc_functions.R")

# Configuration arguments

# These are defined in `qc_config.yml`. The name of the configuration to use
# should be passed as inline argument when the script is called.

#   determine which configuration to use
configuration <- commandArgs(trailingOnly = TRUE)
if (length(configuration) == 0) {
  writeLines("No configuration name has been supplied. Using the default configuration.")
} else {
  writeLines(paste0("Using '", configuration, "' configuration."))
}

#   read arguments from config file
#cnfg <- config::get(file = "./R/qc_config.yml", config = 'insolacion')
cnfg <- config::get(file = "./R/qc_config.yml", config = configuration[1])



# Read data -----------------------------------------------------------------

# We start by reading the raw data files provided by AEMET. The data is stored
# in a structured data.frame, following a format that we call 'long'. In this
# format, each row contains one day or one month worth of data at a given
# station, while columns represent one or more variables. There are no empty
# rows (that is, non-observed days or months are *implicit*). The first data
# columns contain meta-data:
#
#   * INDICATIVO: stations ID code.
#   * AÑO: year of the observation.
#   * MES: month of the observation.
#   * DIA: day of the observation.
#   * NOMBRE: stations name.
#   * ALTITUD: stations elevation.
#   * C_X, C_Y: UTM coordinates.
#   * NOM_PROV: province.
#   * LONGITUD, LATITUD: geographic coordinates.
#
# This information is stored in the `stations` slot of the `qc` object, so
# there's no need to replicate the same data each time, when the qc_add_data()
# method is applied.
#
# The following columns contain the data, which can be either one or more
# variables. For instance, `Precip` files only contain one variable (`P`), while
# `Termo` files contain two variables (`TMAX` and `TMIN`).
#
# According to the file structure, there are two types of data files:
# 
#   * Long-monthly: each row contains one month of data. The naming convention
#     for the data columns is a prefix denoting the variable (e.g. `TMIN`,
#     `TMAX`), followed by a number that represents the day within the month
#     (e.g. `TMIN1`, `TMIN31`). There are 31 columns for each variable and 
#     month, and the months with less than 31 days contain NA values on the
#     extra days. File types `Termo` and `Pluvio` are structured in
#     long-monthly format. 
#   * Long-daily: each row contains one day of data. The column name consists
#     just on the prefix denoting the variable (e.g. `TOTSOL`). File types
#     `Humedad`, `Viento`, `Insolacion`, `Radiacion`, and `Presion` follow
#     this structure.
#
# The `type` argument indicates what type of data structure to expect. The
# qc_add_data() method stores the data into the qc object, in the `long` and
# `wide` slots.

if (cnfg$do$verbose) {
  writeLines("Reading data files")
}

dat <- qc_read_raw_data(
  path = cnfg$dir$input,
  type = cnfg$file$type
)

if (cnfg$do$trial) {
  if (cnfg$do$verbose) {
    writeLines("Trial mode: considering max. 50000 station / months.")
  }
  dat <- dat[1:min(nrow(dat), 50000),]
}


# Go through the variables in the file
for (var in 1:length(cnfg$var$names)) {
  
  if (cnfg$do$verbose) {
    writeLines(paste0("Processing variable ",  cnfg$var$names[var], ":"))
  }

  # We have defined a class (`qc`) to store both the data and metadata resulting
  # from a variety of test performed on the data. It is an RC class, so it has
  # fields treated by reference. Contains the following fields:
  #
  #   * `var_name`: variable name (a character vector)
  #   * `stations`: station metadata (a DF)
  #   * `long`: contains the raw data and metadata, in long format (a DF) where
  #     each row contains one month of data at a given station, and NA
  #     months are implicit (they don't exist).
  #   * `wide`: contains the raw data and metadata, in wide format (a DF) where
  #     rows are days and columns are stations, and NAs are explicit.
  #
  # All the `qc` methods work by reference, that is they modify the data in the
  # fields of the object, or add new ones.
  
  # Create and populate a qc object
  QC <- qc$new(var_type = cnfg$file$type[var],
               var_name = cnfg$var$names[var],
               var_longname = cnfg$var$longnames[var],
               var_unit = cnfg$var$units[var]
               )
  QC$qc_add_data(dat)



  # Controls in long format -------------------------------------------------

  if (cnfg$do$verbose) {
    writeLines("   Performing controls in long format")
  }
  
  # A number of controls are applied at the monthly scale: entire months are
  # checked and assessed, and removed if found wrong. There is a good reason
  # for this, since the data from the manual stations (the vast majority of
  # the dataset) are recorded daily and then sent to the Agency monthly to be
  # digitized. Therefore, there are certain errors that affect entire months.

  # We start by flagging entire months that contain malformed data:
  #   * more records than there are days in the month (`malformed```);
  #   * consist on only NA values (`empty`), or zeros (`zero`);
  #   * incorrect units / resolution (`misencoded`).

  # Then we search for duplicated months (`dupl`), we match which month is
  # duplicated with whom (`dupl_match`), and characterize the replication
  # (`dupl_dist`, `dupl_type`, `dupl_prob`).

  # Finally, we flag months that need to be removed from the dataset (`remove`).
  
  
  #   determine the number of days in each month
  QC$qc_num_days()

  #   determine the number of records in each month
  QC$qc_num_records()

  #   determine the data resolution in each month
  QC$qc_resolution()

  #   control: malformed months (incorrect number of days with data)
  QC$qc_malformed_months()

  #   control: empty months (all NA)
  QC$qc_empty_months()

  #   control: months that are all zero
  QC$qc_zero_months()

  #   control: misencoded months (in ºC, instead of 1/10 ºC)
  if (cnfg$file$type == 'Termo') {
    QC$qc_misencoded_months()
  }
  
  #   control: duplicated months
  QC$qc_dupl_months_find()
  QC$qc_dupl_months_match()
  QC$qc_dupl_months_dist()
  QC$qc_dupl_months_type()
  QC$qc_dupl_months_prob()
  
  #   control: duplicated dekads
  # TO DO: PROGRAMAR ESTE CONTROL; LOS METADATOS GUARDARLOS EN COLUMNAS DE NOMBRE 'dupl_1_10', 'dupl_match_1_10', ETC.
  # QC$qc_dupl_months_find(start = 1, end = 10)
  # QC$qc_dupl_months_find(start = 11, end = 20)
  # QC$qc_dupl_months_find(start = 21, end = 31)
  
  #   flag months to remove
  QC$qc_flag_month()

  # # Some examples of flagged months:
  # w <- which(QC$long$remove)
  # QC$long[w, ] %>% as.data.frame()
  # 
  # # Some examples of duplicated months, but not flagged:
  # w <- which(QC$long$remove == FALSE)
  # QC$long[w, ] %>% as.data.frame()



  # Controls in wide format -------------------------------------------------

  # A number of controls are done on a daily scale, using the complete data
  # series, and not month-by-month. Only the suspicious days are flagged and
  # removed. Therefore, we should first transpose the data to long format,
  # and then proceed with the controls.

  # We start by removing the months flagged in the previous step, to create
  # a first quality-controlled data set (`data_qc_1`).

  # Then we search for suspicious days:
  #   * stationary periods (sequences of days with the same exact values);
  #   * repeated sequences (NOT DONE SO FAR);
  #   * false zeros;
  #   * outliers (out-of-range values) and supicious values (not-so-much);
  #   * inter-variable comparisons (tmax / tmin);
  #   * compare with neighbours (spatial anomalies).

  # Finally, we flag days that need to be removed from the data set (`remove`),
  # and a second and definitive quality-controlled data set is created
  # (`data_qc_2`).
  
  if (cnfg$do$verbose) {
    writeLines("   Performing controls in wide format")
  }

  #   remove bad months and create first q-controlled data set (`data_qc_1`)
  QC$qc_clean_to_long_to_wide()

  # add required metadata to wide format (data resolution)
  QC$qc_add_var_to_long("resol")

  #   control: stationary periods
  if (cnfg$file$type == 'Termo') {
    QC$qc_stationary_sequence()
  }
  
  #   control: repeated sequences - IMPLEMENT DIFFERENT CRITERIA FOR PCP
  # qc <- qc_repeated_sequence(qc)

  #   control: false zeros
  if (cnfg$file$type == 'Termo') {
    QC$qc_false_zeros()
  }
  
  #   control: out-of-range values (outliers) - TO DO: mover esto a args (argumentos YAML)
  QC$qc_outliers(
    thres = c(cnfg$var$limits$lower[var], cnfg$var$limits$upper[var])
  )

  
  #   control: suspicious values
  # qc <- qc_suspicious(qc)

  #   control: compare with neighbours
  # qc <- qc_neighbours()

  #   flag days to remove
  QC$qc_flag_day()

  #   create second (and definitive) q-controlled data set (`data_qc_2`)
  QC$qc_clean_days()


  
  
  # Inter-variable comparisons ---------------------------------------------

  #   compare variables
  # qc <- qc_tmax_tmin_compare()

  
  
  

  # Export -----------------------------------------------------------------

  if (cnfg$do$verbose) {
    writeLines("   Saving the results")
  }
  
  #   create output dir, if required
  if (!dir.exists(cnfg$dir$output)) {
    dir.create(cnfg$dir$output)
  }

  #   save the `qc` object for further use.
  save(QC,
    file = paste0(cnfg$dir$output, "/", cnfg$var$names[var], ".RData")
  )

  #   export a clean data set for further use.
  if (cnfg$do$export_csv) {
    write.table(
      x = cbind(
        QC$wide$id[[1]],
        QC$wide$data_qc_2[[1]]
      ),
      file = paste0(cnfg$dir$output, "/", cnfg$var$names[var], ".csv.gz") %>%
        gzfile(),
      sep = ",",
      na = "",
      quote = FALSE,
      row.names = FALSE,
      col.names = TRUE
    )
  }

  #   export the curated data to the database
  # TO DO: above.

  



  # Report -----------------------------------------------------------------

  ## Global report ----

  if (cnfg$do$verbose) {
    writeLines("   Producing reports")
  }
  
  # Produce a global report considering all the stations in the data set.
  
  rmarkdown::render(
    input = "./R/qc_report.Rmd",
    output_dir = cnfg$dir$output,
    output_file = paste0(cnfg$var$names[var], ".html"),
    envir = parent.frame()
  )
  
  
  ## Station report ----
  
  # Produce individual reports for each station.

  if (cnfg$do$indiv_reports) {
    
    report_dir <- paste0(arg$dir_output, "/", arg$var_names[var], "_reports")
    if (!dir.exists(report_dir)) {
      dir.create(report_dir)
    }
    
    if (cnfg$do$trial) {
      nst <- 1:20
    } else {
      nst <- 1:nrow(QC$stations)
    }
    for (s in nst) {
      QC$qc_report_station(s, report_dir)
    }
    
  }

}