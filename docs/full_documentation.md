# Climate Data Quality Control Pipeline

## Purpose

This repository contains an R-based pipeline for automated quality control (QC) of raw climate datasets from the Spanish national climate service (AEMET). It is designed to pre-process climate station data from AEMET’s **Banco Nacional de Datos Climatológicos (BNDC)** by performing a series of QC checks: it flags suspect or erroneous data points and removes them according to predefined rules. The result is a cleaned dataset ready for downstream climate analysis and services. This automation is extremely useful because BNDC data (which includes decades of observations from many stations) can contain anomalies such as sensor errors, out-of-range values, or missing entries; the QC script ensures these issues are handled consistently and efficiently, saving analysts time and improving data reliability.

In practice, this QC step is the **first stage** of the climate data processing workflow within CSIC’s PTI+ Clima projects. After quality control, the data can be confidently used in subsequent processes such as gap-filling (estimating missing values), homogenization of time series (adjusting for changes in station location or instrumentation), calculation of climatological summaries, and spatial gridding of the data for mapping. **Quality control (“Pre-processing”) provides the foundation for all later steps** – by catching and removing bad data early, it ensures that later analyses are not skewed by outliers or errors. For context, the diagram below (in Spanish) illustrates where QC fits in the broader climate data flow, coming before gap-filling, homogenization, climatologies, and gridding steps:

![Quality control](./img/figure1.png)
*Figure: The quality control stage (labeled "Pre-processing") is the first part of the PTI-Clima data workflow, followed by gap-filling (and its validation), homogenization, climatology calculation, and gridding (with its validation). This QC repository deals with the **Pre-processing** step, ensuring data quality before the data proceeds to these subsequent stages.*


## Architecture

**Repository Structure:** The project is organized as an RStudio project (see the `data_flow.Rproj` file) with the main code residing in the **`R/` directory**. Key components of the codebase include:

* **Main script** – `R/main.R` (also referred to as **qc\_main.R** in the documentation) is the entry point of the pipeline. This script orchestrates the entire QC process for a given dataset (climate variable). It loads the configuration and functions, then iterates through the data, applying quality checks and producing outputs.
* **Functions script** – `R/functions.R` (or **qc\_functions.R**) contains the core functions that implement individual quality control checks and data processing steps. All logic for detecting outliers, applying thresholds, handling missing data, etc., is defined here and used by the main script.
* **Configuration file** – `R/qc_config.yml` defines the settings for each type of climate variable dataset to be processed. This YAML file specifies which raw data files to read, mappings of raw variable names to standardized names, acceptable value ranges, and various processing options (detailed below). The main script reads this configuration at runtime.
* **Reporting template** – `R/report.Rmd` (an R Markdown document, referred to as **qc\_report.Rmd**) is used to generate an **HTML summary report** of the quality control results. It compiles overall statistics, plots, and information about the data after QC. The main script will render this report at the end of processing so the user can review what was done.
* *(Optional:* There is a `man/` directory with a `figures/` subfolder containing documentation diagrams used for explanation, but this does not affect the code.)

**Data Input/Output:** The pipeline expects raw data files (e.g., CSV files) from BNDC to be available in a specified **input directory**, and it will produce cleaned data and reports in a specified **output directory**. These paths are configured in the YAML file. The diagram below shows the overall flow of data and code in this QC process:

*Figure: Architecture of the quality control pipeline. The main script (`qc_main.R`) reads input data files from the raw data directory and uses the configuration (`qc_config.yml`) along with helper functions (`qc_functions.R`) and a reporting template (`qc_report.Rmd`). It produces an R data file (`qc<var>.Rdata`) containing the processed data and metadata for the given variable, a summary HTML report (`qc<var>.html`), and individual station reports (`qc/<var>_reports/`) for detailed per-station diagnostics.*

As shown above, when you run the main script for a particular variable (indicated as `<var>` in the diagram and configuration), the following happens:

1. **Configuration Load:** The script reads `qc_config.yml` to get the settings for the target variable `<var>` (for example, "pluvio" for precipitation). The config specifies the input directory (where the raw data files are located) and output directory for results, as well as metadata about the variable (its name, units, valid range, etc.).
2. **Data Ingestion:** All relevant raw data files are loaded. Typically, the BNDC provides data in CSV format (for example, one file per weather station or one file per day – depending on how BNDC data is structured). The script will read these files (e.g., using `read.csv` or similar) from the **input path** defined in the config. It also loads any station metadata if required (for instance, station IDs, names, coordinates, which might be embedded in the data or provided separately).
3. **Quality Control Checks:** Using the functions from `functions.R`, the script performs a series of checks on the data:

   * **Range checks:** Values are validated against physical or reasonable limits defined in the config (each variable entry in `qc_config.yml` has a `limits` section with `lower` and `upper` bounds). Any values outside these bounds are flagged as suspect (e.g., negative precipitation or humidity over 100% would be caught).
   * **Consistency checks:** The code may check for internal consistency in the data. For example, for precipitation intensity variables, it might ensure that shorter-duration rainfall maxima are not lower than longer-duration maxima at the same station/day. Or for temperature, one would ensure daily minimum is not greater than the maximum (if those were included). The exact checks depend on the variable and are encoded in the functions.
   * **Repeat/Outlier checks:** The pipeline likely flags data that appear as outliers compared to neighboring values or that repeat the same value too often (indicative of sensor stuck or data logging issues). For instance, an instrument that reports the same exact value many days in a row might be flagged. Similarly, sudden jumps or spikes could be detected using statistical rules.
   * **Missing data handling:** If data points are missing or marked as invalid, they may be recorded as NA (not available) in the cleaned dataset. The QC process doesn’t fill these gaps (gap-filling is a later step outside this repository’s scope), but it identifies them.
   * Each detected issue does **not immediately remove the data**; rather, all suspect points are collected and typically a decision is made (based on rules in the code) to either mark them or exclude them from final data. According to the repository summary, the script “flags suspect data, and removes it according to a set of rules” – meaning that for each type of flag (out-of-range, outlier, etc.), there is a rule whether to simply note it or actually discard the value. Those rules might be hardcoded or part of the config.
4. **Data Structuring:** After applying QC, the script assembles the results. In memory, it may create two versions of the dataset:

   * A **“long” format** data frame (each row might be an observation with station ID, timestamp, and value), possibly with columns indicating raw value and flags.
   * A **“wide” format** matrix or data frame, where each column is a station’s time series. The diagram hints that the wide format includes columns for raw data, QC stage1 data, and QC stage2 data (post-removal), along with metadata like resolution, and lists of outliers removed. Essentially, the processed data and all relevant metadata are consolidated.
   * Station metadata (station names, coordinates, elevation, etc.) is also included in the output object for reference.
   * All these components are saved into an `.RData` file (`data/qc/<var>.Rdata`), as a single R object (for easy loading later). For example, if `<var>` is "humedad" (humidity), the output might be `qc_humedad.Rdata` containing everything related to the humidity data QC run.
5. **Reporting:** The script renders the `report.Rmd` RMarkdown file to produce an **HTML report** (`data/qc<var>.html`). This report likely includes summary statistics of the QC process (e.g., number of points flagged/removed, plots of each station’s time series with flags, distribution of values before/after QC, etc.). It provides a **global overview** for that variable’s QC. In addition, if configured to do so, the script generates **individual reports per station** (for example, one HTML or PDF file per station in `data/qc/<var>_reports/`). These individual reports let you inspect each station’s data in detail (perhaps with time series plots highlighting outliers for that specific station).
6. **Completion:** Once finished, the cleaned data (in the RData file) and the reports are ready in the output directory. At this point, the data is considered "quality-controlled" and can be passed on to the next stages of the workflow (e.g., gap-filling algorithms, which would read the RData, fill missing values, etc.).

Throughout the process, the **configuration file** controls what happens for each variable. In `qc_config.yml`, each **named section** corresponds to a dataset (or group of related variables) to process. Because AEMET’s raw data uses different codes for various measurements and PTI-Clima has its own standardized variable naming, the config also maps between them. For example, AEMET might record daily total precipitation under the column "P" in its files, but internally we might refer to it as "pr" (for precipitation) in the PTI system. The config ensures the script knows that "P" corresponds to the precipitation variable and what units or limits it has. The image below illustrates the relationship between AEMET’s BNDC variable codes (green boxes on the left of each pair) and the PTI/LCSC standardized variable names (green boxes on the right) for various climate elements:

*Figure: Mapping between raw AEMET variable codes and the internal PTI-Clima variable names. For instance, daily maximum and minimum temperature (`TMAX` and `TMIN` in AEMET data) are mapped to `tmax` and `tmin` respectively in the PTI system; relative humidity observations (`HU00`, `HU07`, etc. in AEMET at different times) map to humidity metrics `hr` or `hr2`; precipitation variables (`P` for daily total, `PMAX10`, `PMAX20`, etc. for rainfall intensity over various durations) map to `pr` and related standardized names; wind speed (`VEL_00`, etc.) maps to `ws`, wind gust (`MAX_VEL`) to `wmax`, and so on. This mapping is defined in the config file so that the QC code knows how to interpret each column of the raw data.)*

**Supported Variables:** According to the configuration, the repository is set up to handle multiple types of climate data. Each type is defined as a section in the YAML config with a unique name. The main ones (and their meanings) are:

* `pluvio` – Daily cumulative precipitation (rainfall), denoted by the raw variable "P" in AEMET data.
* `canmaxpcp` – Maximum precipitation intensity over various durations (e.g., 10 min, 20 min, up to 12 hours). This category includes 7 related variables: "PMAX10", "PMAX20", "PMAX30", "PMAX60", "PMAX2H", "PMAX6H", "PMAX12H". These represent the highest precipitation recorded in those time intervals each day.
* `humedad` – Air relative humidity, taken at synoptic observation times plus daily extremes. This includes 6 variables: "HU00", "HU07", "HU13", "HU18" (humidity at 00, 07, 13, 18 UTC hours) and "HUMAX", "HUMIN" (daily maximum and minimum humidity).
* `presion` – Atmospheric pressure (station pressure), at synoptic times plus daily max/min. Variables: "PRES00", "PRES07", "PRES13", "PRES18" (pressure at 00, 07, 13, 18 UTC) and "PRESMAX", "PRESMIN" (daily highest and lowest pressure).
* `viento_1` – Wind speed at synoptic times plus daily maximum gust speed. Variables: "VEL\_00", "VEL\_07", "VEL\_13", "VEL\_18" (wind speed at 00, 07, 13, 18 UTC in m/s) and "R\_MAX\_VEL" (daily max wind speed, essentially the highest gust).
* `viento_2` – Wind run (distance traveled by wind, sometimes called wind traversal) over 24h. Variables: "REC24" and "REC77" (these might be measurements of wind run in km for standard periods).
* `viento_3` – Wind direction at synoptic times plus daily extreme. Variables: "DIR\_00", "DIR\_07", "DIR\_13", "DIR\_18" (wind direction at those hours, in degrees or compass points) and "R\_MAX\_DIR" (the direction of the maximum wind gust of the day).
* `insolacion` – Total daily sunshine duration. Variable: "TOTSOL" (total hours of sunshine per day).
* `radiacion` – Daily cumulative solar radiation. Variables: "RDIRDIA", "RDIFDIA", "RGLODIA" (components of radiation: direct, diffuse, global daily radiation totals).

Each of these sections in `qc_config.yml` contains fields that guide how the QC is performed for that dataset. Common fields include:

* `dir$input` and `dir$output` – paths for input raw files and output results.
* `cores` – number of CPU cores to use (for parallel processing); *currently not utilized by the script*, so processing is single-threaded.
* `do$verbose` – whether to print detailed logs during processing.
* `do$trial` – if set to true, enables a **trial mode** which processes only a small subset of data (e.g., the first 100 records) for testing or demonstration.
* `do$export` – whether to actually export/save results to files. (If false, the script might run but not save the RData and reports, useful for quick checks.)
* `do$indiv_reports` – whether to generate individual station reports in addition to the main summary.
* `file$type` and `file$source` – metadata about the file type and source. In this project, `file$source` will be "AEMET" for all, and `file$type` might correspond to the variable category or data format.
* `var$names` – the list of variable codes in the raw data that correspond to this category (for example, for `presion`, names would be the six pressure variable codes).
* `var$longnames` – human-readable names for those variables (for use in plots or reports, e.g., “Pressure at 00 UTC”, etc.).
* `var$units` – units of measurement for each variable (e.g., °C, mm, hPa, etc.).
* `var$limits` – the physical or reasonable limits for the variable(s). If a value lies outside these, it will be flagged. For instance, `radiacion` (radiation) might have a lower limit 0 (no negative radiation) and an upper limit based on the maximum possible solar irradiance for a day.

All these settings make the code **configurable without editing R code** – if AEMET changes a variable name or if we want to adjust a threshold, we can update the YAML rather than digging into the functions. New variable categories could also be added to the config (with corresponding code in functions to handle them, if not already general).

In summary, the architecture follows a clear separation: **configuration** (for flexibility and variable-specific details), **functions** (for the logic of QC steps), and a **main driver script** (to tie together config, data, and functions and produce outputs). The outputs (RData and reports) are standardized, facilitating integration into the next steps of the climate service workflow (sometimes referred to as the LCSC data flow in PTI-Clima). All R scripts in this repository produce a general QC report for each run, and most QC stages also produce individual station reports for deeper inspection.

## Installation and Setup

Setting up the project on a local system involves installing R (and required packages) and configuring paths to your data. Follow these steps to get started:

1. **Prerequisites – R Environment:** Ensure that you have a recent version of **R** installed on your computer (R 4.x or later is recommended). Additionally, having **RStudio** can be helpful for development and interacting with the project, though it’s not required for running the scripts. Make sure you also have a toolchain for compiling packages if needed (RTools on Windows, build-essential on Linux, etc.), in case some packages need compilation.
2. **Clone the Repository:** Obtain the code from GitHub. You can clone the repository with git:

   ```bash
   git clone https://github.com/PTI-Clima/quality_control.git
   ```

   Or download it as a ZIP via the GitHub web interface and unzip it. This will give you a directory named `quality_control` containing the R project.
3. **Open the Project:** If using RStudio, double-click the `data_flow.Rproj` file (or open it via *File -> Open Project* in RStudio). This will set your working directory to the project and make it easier to work with. If you’re not using RStudio, set your R session’s working directory to the cloned repository folder (e.g., using `setwd("path/to/quality_control")` in R).
4. **Install Required R Packages:** The QC scripts rely on certain R packages. These likely include packages for data manipulation and plotting (such as **tidyverse** or data.table), YAML parsing (the **yaml** package to read `qc_config.yml`), and report generation (**rmarkdown** and **knitr** for the R Markdown report). There may also be other dependencies (for example, packages for date handling or specific statistical tests). To install all needed packages, you can:

   * Open the `DESCRIPTION` file (if provided) to see the list of packages under *Imports* or *Depends*, and install those via `install.packages()`.
   * Or attempt to run the main script (see next section) and note any “package not found” errors, then install those packages.
   * Alternatively, if you have **devtools** installed, you can run `devtools::install_deps()` in the project directory to automatically install dependencies listed in the project (this works if the repository is structured as an R package with a DESCRIPTION).

   Make sure to also install **rmarkdown** (and ensure you have Pandoc which comes with RStudio) since the report generation requires it.
5. **Configure Data Paths:** Before running the QC, you need to tell the program where your input data resides and where to put outputs. Open the file `R/qc_config.yml` in a text editor or within RStudio. For each variable category (or at least for the ones you plan to process), update the `dir: input:` path to the folder where the raw BNDC data CSV files are located on your system. For example, you might set:

   ```yaml
   dir:
     input: "D:/climate_data/BNDC/raw_files/"
     output: "D:/climate_data/processed_QC/"
   ```

   (Use quotes if your path contains spaces.) Similarly, you can change the `dir: output:` path to where you want the QC results to be stored. You typically only need to set this once if all variables’ data are in the same general location; the config can reuse a global path or you set it per category. **Ensure that the input directory contains the expected files** – for instance, if processing `pluvio`, the input directory should have the precipitation data files from AEMET (with names or format expected by the script). If the data files have unique names or formats, you might need to adjust how the script reads them (this would be in the functions.R code).
6. **Review Configuration Options:** While in `qc_config.yml`, you can also toggle other settings. For a first trial run, you might set `trial: true` under the `do:` section for the variable you’re testing, which will limit the run to the first 100 records. This is useful to ensure everything works without waiting for the entire dataset. You can also enable `verbose: true` to get detailed console output as the script runs, which can help in understanding the progress. Ensure `export: true` so that results are saved, and set `indiv_reports: false` initially if you want to skip generating many individual reports on your first run (individual reports can be numerous if there are many stations, and skipping them speeds up the process for initial tests).
7. **Test the Setup:** You are now ready to run a test. See the next section for how to execute the QC script on an example variable. If the run completes successfully (even just for a small trial), you can then run the full dataset.

By following these setup steps, you prepare your environment so that the QC pipeline can run smoothly. Once the environment is configured and packages are installed, running the QC on any supported variable is straightforward.

## Examples of Use

After installation and setup, using the quality control pipeline involves invoking the main R script with the desired configuration name (variable) as an argument. There are two primary ways to run the QC: via the command line (using `Rscript`) or interactively in R/RStudio.

**1. Command-line usage (batch mode):** This is ideal for automation or when running on a remote server. From a terminal, you can navigate to the repository directory and execute the main script with Rscript. For example, to run the quality control for **daily precipitation** (the `pluvio` configuration), use:

```bash
cd /path/to/quality_control   # change to the repository directory
Rscript R/main.R pluvio
```

In this command, `pluvio` is the argument specifying which dataset to process, corresponding to a section in `qc_config.yml`. The script will load the config for "pluvio", read the raw precipitation data, perform all QC steps, and then output the results to the configured output folder. You should see log messages in the terminal if `verbose` is enabled (or minimal output if not). When finished, check the output directory – it should contain a file like `qc_pluvio.Rdata` and a report `qc_pluvio.html`, assuming those naming conventions. You can open the HTML report in a web browser to review the QC findings for precipitation.

Similarly, you can run other variables. For example, to quality-control humidity data:

```bash
Rscript R/main.R humedad
```

This would process the humidity observations (using the "humedad" config). Or to run wind speed:

```bash
Rscript R/main.R viento_1
```

You simply provide the appropriate config key. **Available options** correspond to the config sections described earlier (pluvio, canmaxpcp, humedad, presion, viento\_1, viento\_2, viento\_3, insolacion, radiacion, etc.).

If you want to automate running **all variables** in sequence (for instance, to refresh the entire dataset nightly), you could write a simple shell script or batch file that calls `Rscript R/main.R` for each config name one after the other. For example, a shell script `run_all_qc.sh` might contain:

```bash
#!/bin/bash
Rscript R/main.R pluvio
Rscript R/main.R canmaxpcp
Rscript R/main.R humedad
# ... and so on for each variable ...
Rscript R/main.R radiacion
```

Then you could run this script to process everything. On a Unix-like system, you may schedule it via **cron** for regular execution. For instance, to run the QC every day at 2:00 AM, you could add a cron entry like:

```
0 2 * * * cd /path/to/quality_control && Rscript R/main.R pluvio
```

(and similarly for other variables or use a script as above). Each run will generate/update the respective outputs. In a production setting, you might stagger the jobs or run in parallel if resources allow (though note the current code does not utilize multiple cores even if `cores` is set, so running multiple instances in parallel is the way to parallelize different variables).

**2. Interactive usage (in R/RStudio):** If you prefer, you can run the QC inside an R session, which is useful for debugging or development. Open `R/main.R` in RStudio. You will see that it likely uses the command-line argument to get the `<var>` name (perhaps via `commandArgs()` in the code). If you want to run it interactively, you can simulate this by manually setting the variable name. For example, you might find a line in `main.R` that parses the argument or a placeholder like `var <- commandArgs(trailingOnly=TRUE)[1]`. In RStudio, you can instead define `var <- "pluvio"` at the top of the script (or in the console) and then source the script. Run the lines of `main.R` or use `source("R/main.R")` after setting up the `var` value. This will execute the same steps as the command-line call. As it runs, you can inspect objects in the environment or modify the code if needed. Make sure to load any needed libraries in the R session (the script might call `library()` for you, but if not, load the packages listed in the script).

Another way is to call the functions directly if the code is structured to allow it (for example, if there is a main function). But in this repository, the simplest is to run the script as-is.

**Verifying the Results:** After running a QC process (whether via command-line or interactive), you should examine the outputs:

* Load the `.RData` file in R to see the data structure. For example:

  ```r
  load("data/qc/qc_pluvio.RData")
  ls()  # see what objects were loaded, perhaps something like 'qc_result'
  # Suppose the main object is qc_result:
  str(qc_result)  # check its structure (metadata, data frames, etc.)
  ```

  You might find elements like `qc_result$stations` (station metadata) or `qc_result$wide` (the wide data matrix), etc., as hinted by the architecture diagram.
* Open the HTML report in a web browser. It should have sections detailing the quality checks. For instance, it may contain a summary table listing how many values were out of range, maybe plots of each station’s time series marking removed points, and textual explanation of what was done. Use this to confirm that the QC behaved as expected. If something looks off (e.g., too many points removed or a station entirely flagged), you might need to adjust thresholds in the config or investigate the raw data for issues.
* If individual station reports were generated (in an output subfolder), open a couple of those to see station-specific details. These often contain plots for that station’s data, highlighting anomalies, and can be useful to diagnose specific station problems (like one station’s sensor was bad during a certain year, etc.).

**Example scenario:** Imagine you want to prepare the dataset for analysis of a drought index. You need daily precipitation and temperature data cleaned. You have raw files from BNDC in `~/data/BNDC/`. You would edit `qc_config.yml` to set input to that path, output to, say, `~/data/BNDC/processed/`. Then run `Rscript R/main.R pluvio` to QC precipitation. Once done, run `Rscript R/main.R temperatura` (assuming a temperature config exists or using appropriate config name if it were included; if not included in this repository, temperature might be handled elsewhere). After these, you’d have cleaned precipitation (and temperature) datasets to feed into a drought index calculation script. This illustrates how a researcher new to climate data can rely on the QC pipeline to handle the gritty details of data cleaning, focusing instead on higher-level analysis.

In summary, to use this QC repository, **choose the variable** you need to process, run the main script with that variable’s config name, and then review the outputs. The process can be repeated for all required variables. The design is modular: one variable at a time, which gives flexibility in scheduling and resource use. If new data comes in (e.g., a new month of observations), you can rerun the script for the affected variables to update the cleaned data files. By following these usage patterns, new team members can confidently process large climate datasets without deep prior knowledge of climatological QC, as the repository encodes expert-derived rules for data quality.

## Lines of Improvement

While the repository provides a working solution for automated climate data QC, there are several areas where it could be enhanced to improve maintainability, performance, and usability. New contributors are encouraged to consider the following potential improvements:

* **Expanded Documentation and Comments:** The current documentation (including this guide) can be supplemented with more in-code comments and function-level help files. For example, each function in `functions.R` could have a comment block explaining its purpose, inputs, and outputs. Additional explanation of the QC logic (why certain thresholds were chosen, references to meteorological standards or WMO guidelines for QC) would help newcomers understand the rationale. Moreover, a tutorial example (perhaps a vignette or an extended README section) walking through a small sample dataset would be very instructive. Enhancing documentation ensures knowledge is shared and reduces the learning curve for new users.
* **Improved Modularity and Packaging:** Currently, the code is organized as scripts, which works, but it could be refactored into a more modular structure. For instance, it could be turned into an **R package** (with a proper `DESCRIPTION`, `NAMESPACE`, and perhaps splitting functions into multiple files by theme). This would allow installation via `devtools::install_github("PTI-Clima/quality_control")` and easier dependency management. Functions could be modularized (e.g., separate files or modules for “range checks”, “temporal checks”, “report generation”). This modular approach makes the code more maintainable and testable. Additionally, a package structure would allow using **roxygen2** to auto-generate documentation for each function and even build a pkgdown site for the project.
* **Testing and Validation:** Introducing a suite of **unit tests** would greatly improve the reliability of the code. One could use the **testthat** framework to write tests for each QC function (e.g., test that a value below the lower limit indeed gets flagged by the corresponding function). Also, **integration tests** using a small synthetic dataset can verify that running the entire pipeline yields expected results (for example, feed in a tiny CSV with known issues and check that the output RData flags exactly those issues). Having tests would guard against future changes breaking functionality and would formalize the expected behavior of the QC algorithms.
* **Error Handling and Edge Cases:** Currently, the code might assume the input data is well-formed and present. It would be beneficial to add more robust error handling. For instance, if an expected input file is missing or empty, the script should gracefully report this and skip or stop with a clear message, rather than throwing an unhandled exception. Edge cases such as stations with all data missing, or all data out of range, should be handled (perhaps the code could still output an RData with that station marked as having no valid data, rather than failing). Another edge case is when new variable codes appear in BNDC data – the config mapping might need updates, but the code could detect unknown columns and warn the user. By anticipating and coding for these scenarios, the pipeline becomes more robust for operational use.
* **Parallel Processing and Performance:** The configuration has a `cores` option which is not yet utilized. For large datasets (e.g., thousands of stations or many years of high-frequency data), the QC could be time-consuming. Implementing parallel processing would speed this up. For example, the code could process each station’s data in parallel, or split by year, using packages like **parallel** or **future.apply**. Care would be needed to avoid race conditions when writing output (maybe each parallel task returns a result and then combined). Additionally, performance could be improved by using efficient data structures (for instance, using **data.table** for big tables or vectorized operations instead of R loops where possible). Profiling the code to find bottlenecks and optimizing those sections would help the pipeline scale to larger data volumes.
* **Configuration Flexibility:** The YAML config works well, but it could be improved further. One idea is to allow **global config** sections to avoid repetition. For example, if the input directory is the same for all variables, having to repeat it for each in YAML is redundant – a global setting or an environment variable could be used. Another improvement is to allow overriding config via command-line arguments. Currently, you pass just the variable name to `main.R`. It could be extended so you could do something like `Rscript R/main.R pluvio config_alt.yml` to use an alternate config file, or `Rscript R/main.R pluvio --no-indiv-reports` to override certain options on the fly. This would make the tool more flexible. Using a command-line parsing library (like **optparse** or **argparse** in R) could facilitate this.
* **Extensibility to New Data or Variables:** As new types of data or new QC rules emerge, the code should be easy to extend. Currently, adding a new variable means editing the YAML and likely adding corresponding handling in the functions. It might be worth designing a more abstract framework where adding a variable doesn’t require much new code, just new config (if the checks are generic). For example, if tomorrow AEMET provides a new variable (say soil temperature), one could simply add a section in YAML with limits and it might go through generic checks (range check would just work). To achieve this, ensure that the functions are written in a general way (e.g., looping through any number of variables defined in config, not hard-coding specific ones). Where special-case handling is needed, clearly isolate it.
* **Integration and Data Flow:** Currently, the QC step is somewhat standalone. A possible improvement is better **integration hooks** with the subsequent steps (gap-filling, etc.). For instance, once QC is done and the RData is produced, perhaps the next script or tool could be automatically triggered or at least the QC output could be packaged in a standardized format (maybe following a specific class or data structure) that the gap-filling code expects. Documenting the output structure (as we started in this guide) is part of this, but also potentially outputting in common formats (CSV, NetCDF, etc., in addition to RData) could be considered to allow non-R workflows to ingest the QCed data. This goes slightly beyond the repository’s scope, but it’s something to keep in mind as an improvement for usability.
* **User Interface and Feedback:** For the benefit of non-developer users, one could create a simple interface – for example, an RShiny app or an interactive RMarkdown – where a user can select a variable and click a button to run the QC, and then see the results inline. This might be useful for demonstrations or for those unfamiliar with running scripts. Even without a full GUI, improving console messages (so that it clearly states “Now processing station X…”, “Flagged Y outliers”, etc.) would make the tool more transparent as it runs.

By addressing these improvement points, the repository can evolve from a working prototype into a more robust, user-friendly, and maintainable system. New team members are encouraged to contribute to these areas: for example, start by writing a few unit tests for key functions, or add comments/documentation to a complex part of the code. Even small improvements can greatly enhance the longevity and utility of the codebase. The goal is to ensure that the automated quality control not only works correctly, but is also easy to understand, modify, and trust for all users involved in PTI-Clima projects.
