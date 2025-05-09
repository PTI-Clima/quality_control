# <img src="man/figures/badge.png" alt="image" width="100"/> quality_control

This repository contains an automated R-based pipeline for performing quality control (QC) on raw climate data from AEMET’s Banco Nacional de Datos Climatológicos (BNDC). It flags and removes suspect data using variable-specific thresholds and consistency checks, producing cleaned datasets and QC reports for downstream processing.

## ✅ Purpose

The QC process ensures data quality before subsequent tasks like gap-filling, homogenization, climatology computation, and gridding. This repository covers the **first step** of the climate data workflow in PTI+ Clima projects.

## 📁 Repository Structure

- `R/main.R` – Main script for running QC.
- `R/functions.R` – QC functions and checks.
- `R/report.Rmd` – R Markdown template for summary reports.
- `R/qc_config.yml` – Configuration for variables, file paths, limits.
- `data/qc/` – Output directory for cleaned data and reports.

## 🚀 Getting Started

### Installation

1. Clone the repo:
   ```bash
   git clone https://github.com/PTI-Clima/quality_control.git
   ```
2. Open the `data_flow.Rproj` in RStudio (or set the working directory manually).
3. Install dependencies:
   ```r
   install.packages(c("yaml", "rmarkdown", "tidyverse"))  # Add others as needed
   ```

### Configuration

Edit `R/qc_config.yml` to specify:
- Input/output paths
- Variable names, units, valid ranges
- Processing options (`trial`, `verbose`, `export`, etc.)

### Usage

To run QC for a variable (e.g., precipitation):

```bash
Rscript R/main.R pluvio
```

This will generate:
- `data/qc/qc_pluvio.Rdata` – Cleaned dataset
- `data/qc/qc_pluvio.html` – Summary QC report
- Optionally: individual station reports

You can schedule these commands using cron jobs or batch scripts.

## 📊 Supported Variables

Includes configurations for:

- Daily precipitation (`pluvio`)
- Humidity (`humedad`)
- Wind speed & direction (`viento_1`, `viento_2`, `viento_3`)
- Solar radiation (`radiacion`)
- Sunshine duration (`insolacion`)
- Atmospheric pressure (`presion`)
- Precipitation intensities (`canmaxpcp`)

## 🔧 Areas for Improvement

- Modularize as an R package
- Add unit tests (e.g., with `testthat`)
- Improve error handling and logging
- Enable real parallel processing
- Allow richer command-line arguments

---

📄 **Full documentation** is available in [`docs/full_documentation.md`](docs/full_documentation.md) for detailed onboarding, architecture diagrams, and guidance.

```

