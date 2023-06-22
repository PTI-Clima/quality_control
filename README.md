# Daily quality control on the raw input data

---

This package is in charge of the first part of the climatic variables data flow. 
The input data comes from two different sources:

- AEMET (Spanish Meteorological Agency)
- SIAR (Spanish Agro-Climatic Information System for Irrigation)

Several quality controls are performed over the different input variables. For
more information you can read the pdf file in this repo.


## Details

---

There is a bunch of R files that contain private functions that execute the
quality control itself.

The user can only use these public functions defined in the file `launch.R`:

- `launch.all.controls(data.source = "AEMET")`: This function launches all 
quality controls for the variables, which are wind speed, relative humidity,
precipitation, sunshine duration, radiation and temperature.  

- `launch.controls(vars, data.source = "AEMET")`: This function applies quality 
controls to the specified variables.  

- `Termo`: This function provides to the user a small sample dataframe of the 
variable temperature. For more information, write on the console: `help(Termo)`.  

## Arguments

---

- `data.source`: It represents the original source of the data. There are two 
options: it can be "AEMET" or "SIAR".

- `vars`: A string or vector of strings containing the name of the climatological 
variable to apply quality controls to. It must be one of the following: "t" 
(temperature), "w" (wind), "hr" (relative humidity), "pr" (precipitation), 
"in" (insolation), "t" (temperature), "ra" (radiation), "p" (pressure).

---
## Installation

Install the latest stable version from GitHub:

```{r}
library(devtools)  
install_github('lcsc/quality_control', auth_token = "XXXXXXXXXXXXXXX")
```
This is a private repository, so a Github authentication token is necessary. 
You can generate it in your profile page.

## Version history
---

### Version 1.0, April 2023. 

- First version on GitHub, based on the work that Miquel Tomás did back in 2016  
- Giving package structure  
- Implementation of test functions  
- Update README with installation instructions

### Version 1.1.0, June 2023. 

1. Organize input and output paths to distinguish between AEMET and SIAR data.
2. Write output files in compressed format (.gz).
3. Fix a problem with some metadata files that were overwriting each other.
4. Fix a bug with radiation quality control process.

### Version 1.1.1, June 2023. 

1. Fix a mistake in the contact email of one contributor.

### Version 1.2.0, June 2023

1. Change an internal library function that reads and projects station coordinates 
to stop using `rgdal` and `rgeos` dependencies.

### Version 1.2.1, June 2023

1. Update tests to work with last releases.

---

## References  

Tomas Burguera, M., Jiménez Castañeda, A., Luna Rico, Y., Morata Gasca, A., Vicente
Serrano, S. M., González Hidalgo, J. C., & Beguería, S. (2016). Control de calidad
de siete variables del banco nacional de datos de AEMET. En: Olcina Cantos, Jorge,
Rico Amorós, Antonio M., Moltó Mantero, Enrique (eds.), Clima, sociedad, riesgos y
ordenación del territorio. Alicante: Instituto Interuniversitario de Geografía,
Universidad de Alicante y Asociación Española de Climatología, pp. 407-415. DOI:
http://dx.doi.org/10.14198/XCongresoAECAlicante2016-38.

---

## Examples  

```{r}
## Not run:

# The function reads the data from a folder named "data_raw" so, if it does not 
# exists, it is necessary to create one and put the data inside 
if (!dir.exists("data_raw")) {
  dir.create("data_raw")
}

# Create the data
INDICATIVO <- rep("B013X", 200)
AÑO <- rep(2023, 200)
MES <- sample(1:12, size = 200, replace = T)
DIA <- sample(1:28, size = 200, replace = T)
HU00 <- sample(5:100, size = 200, replace = T)
HU07 <- sample(5:100, size = 200, replace = T)
HU13 <- sample(5:100, size = 200, replace = T)
HU18 <- sample(5:100, size = 200, replace = T)

# Transformation into a dataframe
df <- data.frame("INDICATIVO" = INDICATIVO,
                 "AÑO" = AÑO,
                 "MES" = MES,
                 "DIA" = DIA,
                 "HU00" = HU00,
                 "HU07" = HU07,
                 "HU13" = HU13,
                 "HU18" = HU18)
                 
# Exportation like a .csv or a .txt in the file /data_raw/ (the first letter of the
# climatological variable must be capitalized)
write.table(df,
            "data_raw/Humedad.csv",
            fileEncoding = "latin1",
            row.names = F,
            quote = F,
            sep = ";")# Now, we can run the function

# Launching the controls
library(qualityControl)
launch.controls(vars = "hr", data.source = "AEMET")

```
