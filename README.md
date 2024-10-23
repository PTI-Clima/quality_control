# <img src="man/figures/badge.png" alt="image" width="100"/> data_flow

## Description

A series of R scripts used to process climate data from AEMET's "Banco Nacional de Datos del Clima" (BNDC) raw files, to be used within CSIC's PTI+ Clima projects. The process contains the following steps:

1.  **Quality control (qc)**: reads raw data from AEMET's BNDC files and performs a series of checks. Prepares the data for further processing within the LCSC data flow.

2.  **Data pre-processing (pp)**: Taking as input the output of the quality control process, performs various tasks to prepare the data for further processing within the LCSC data flow: crop to the desired analysis period, variable aggregation and transformations, metadata augmentation (manual vs. automatic stations, spatial domain), selection of candidate and auxiliary stations, and computation of distance matrices.

3.  **Gap-filling (gf)**: Taking the output of the pre-processing process as input, fill the gaps (NAs) of the candidate stations, based on the data of other candidate or auxiliary stations.

4.  **Gap-filling validation (gf_validation)**: Generates a cross-validation report of the gap-filling process.

5.  **Homogeneisation (hg)**: Takes as input the output of the gap-filling process, performs the Alexanderon's (1986) SNHT test to detect, and (if requested) correct inhomogeneities in the data series.

The scripts must be run in the mentioned order, as the process is sequential: the output of one step serves as the input to the next step.

Each of the steps consists on a main `.R` file, which sources a series of files: a configuration file `.yml` with particularities of how to process each target variable; a functions `.R` file with the necessary functions to run the process; a markdown `.Rmd` file, which is used to produce a global report for the process.

As a result, the script generates an `.Rdata` file per processed variable. This file contains one single object, containing all the necessary items (station metadata, data matrix, configuration options, etcetera). All scripts generate a general report, and most steps also generate individual reports per series.

The image below summarises the flow of data and code:

<img src="man/figures/diagram-1.png" alt="Flow of data and code" width="1599"/>

This image relates the variables in AEMET's BNDC to the variables used in the PTI's process:

<img src="man/figures/diagram-2.png" alt="image" width="1607"/>

This image summarizes the processes undergone by the PTI variables:

<img src="man/figures/diagram-3.jpg" alt="image" />

## Use instructions

1.  From the terminal, run `Rscript R/hg_main.R <var>`, where \<var\> is a named configuration read from configuration file `config.yml`, indicating the variable to process.

2.  It is also possible to run the `*_main.R` scripts manually. In that case, the commented line `cnfg <- config::get(file = "./R/config.yml", config = '<var>')` needs to be uncommented, replacing `<var>` with the name of the variable in the configuration file.

## Configuration files

There are two configuration files, one for the quality control process and one for the remaining steps. The reason there are two different configuration files is because the AEMET's variables are different from the PTI ones, and the configuration is made per variable. There is a field that maps the AEMET's to the PTI variables. The files are formatted as YAML, and can be edited with a text editor.

**qc_config.yml** – Configuration options used during the quality control step. There are options for each variable set in AEMET's raw data files. Variable sets are contain variables that are expressed in the same units. This is a list of the named configurations in the configuration file (there is one for each variable set):

-   `pluvio`: daily cumulative precipitation (one variable: "P").
-   `canmaxpcp`: maximum n-minutes precipitation intensity (7 variables: "PMAX10", "PMAX20", "PMAX30", "PMAX60", "PMAX2H", "PMAX6H", "PMAX12H").
-   `humedad`: relative humidity, synoptic observations plus two daily maxima (6 variables: "HU00", "HU07", "HU13", "HU18", "HUMAX", "HUMIN").
-   `presion`: air pressure, synoptic observations plus two daily maxima (6 variables: "PRES00", "PRES07", "PRES13", "PRES18", "PRESMAX", "PRESMIN").
-   `viento_1`: wind speed, synoptic observations plus one daily maxima (5 variables: "VEL_00", "VEL_07", "VEL_13", "VEL_18", "R_MAX_VEL").
-   `viento_2`: daily wind travel (two variables: "REC24", "REC77").
-   `viento_3`: wind speed, synoptic observations plus one daily maxima (5 variables: "DIR_00", "DIR_07", "DIR_13", "DIR_18", "R_MAX_DIR").
-   `insolacion`: total daily sun hours (one variable: "TOTSOL").
-   `radiacion`: daily cumulative radiation (three variables: "RDIRDIA", "RDIFDIA", "RGLODIA").

For each named option, the configuration file contains the following items:

```{=html}
  dir:
    input: input directory (where the raw data are stored)
    output: output directory (where to put the resulting files)
  cores: number of cores to use in the process (not currently used)
  do:
    verbose: use verbose output
    trial: use trial mode (only for debugging or demonstration; uses only the first 100 data)
    export: export results to file
    indiv_reports: produce individual reports per series
  file:
    type: file type (each variable is a type)
    source: currenty, only "AEMET"
  var:
    names: name(s) of the variable(s) within the AEMET's raw data files
    longnames: long name(s) of the variable(s)
    units: unit(s) of the variable
    limits:
      lower: reasonable or physical lower limit for the variable(s) data
      upper: reasonable or physical upper limit for the variable(s) data
```
**config.yml** – Configuration options used in all steps other than the quality control. Named configurations (there is one for each variable used in the PTI) are:

-   `tmax`: maximum daily temperature (from AEMET's variable "TMAX").
-   `tmin`: minimum daily temperature (from AEMET's variable "TMIN").
-   `pr`: daily cumulative precipitation (from AEMET's variable "P").
-   `hr`: daily mean relative humidity, average of four synoptic measurements (from AEMET's variables "HU00", "HU07", "HU13", "HU18").
-   `hr2`: daily mean relative humidity, average of daily max and min (from AEMET's variables "HUMAX", "HUMIN").
-   `tdew`: daily mean dewpoint temperature (from AEMET's variables: NOT IMPLEMENTED).
-   `inso`: total daily insolation (from AEMET's variable "TOTSOL").
-   `p`: daily mean surface level atmospheric pressure, average of four synoptic measurements (from AEMET's variables "PRES00", "PRES07", "PRES13", "PRES18").
-   `p2`: daily mean surface level atmospheric pressure, average of daily max and min (from AEMET's variables "PRESMAX", "PRESMIN").
-   `ssrd`: daily total global radiation (from AEMET's variable "RGLODIA").
-   `ws`: daily average wind speed, average of four synoptic measurements (from AEMET's variables "VEL_00", "VEL_07", "VEL_13", "VEL_18").
-   `ws2`: daily average wind speed, from daily wind travel 07-07 h (from AEMET's variable "REC77").
-   `wmax`: highest daily wind gust (from AEMET's variable "R_MAX_VEL").

For each variable, the configuration files contains the following items:

```{=html}
  version: version number (can be used to differenciate between different runs with different options)
  dir:
    qc_dir: quality control data output directory
    pp_dir: pre-process data output directory
    gf_dir: gap-filling data output directory
    hg_dir: homogeneisation data output directory
  cores: number of cores to use
  period_analysis:
    start: starting date of the analysis period
    end: ending date of the analysis period
  do:
    verbose: use verbosity?
    trial: use trial mode?
    global_report: produce global report?
    indiv_reports: produce individual reports?
  var:
    name: variable's name (internal id)
    longname: variable's long name
    AEMET_name: name of the AEMET's variable(s) this variable is based on
    unit: variable's unit
    factor: scaling factor
  cand:
    min_years: minimum number of yeras to consider a series as candidate (this is a general threshold that might get relaxed if there are no series in a given province or island)
  aux:
    distance: type of distance matrix to use when selecting the auxiliary series for a given candidate; it can be either "correlation" or "euclidean"
    overlap_years: miniumum number of overlapping years to consider an auxiliary series; used only if a correlation distance matrix is used.
    max_dist_km: maximum distance allowed for an auxiliary station to be considered
  gf:
    iterations: number of gap-filling iterations to use
  hg:
    correct: should found inhomogeneities be corrected?
```
