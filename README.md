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

## Arguments

---

- `data.source`: It represents the original source of the data. There are two 
options: it can be "AEMET" or "SIAR".

- `vars`: A string or vector of strings containing the name of the climatological 
variable to apply quality controls to. It must be one of the following: "tmax" 
(maximum temperature), "tmin"(minimum temperature), "w" (wind), "hr" 
(relative humidity), "pr" (precipitation), "in" (insolation), "td" (temperature),
"ra" (radiation), "p" (pressure).

---
## Installation

Install the latest stable version from GitHub:

library(devtools)
install_github('lcsc/quality_control', auth_token = "XXXXXXXXXXXXXXX")
This is a private repository, so a Github authentication token is necessary. 
You can generate it in your profile page.

## Version history
---

### Version 1.0, April 2023. 

- First version on GitHub, based on an old couple of functions from 2016  
- Giving package structure  
- Implementation of test functions  
- Update README with installation instructions  

---

## References  

Olcina Cantos, Jorge; Rico Amorós, Antonio M.; Moltó Mantero, Enrique (eds.). 
Clima, sociedad, riesgos y ordenación del territorio. Alicante: Instituto 
Interuniversitario de Geografía, Universidad de Alicante; [Sevilla]: 
Asociación Española de Climatología, 2016, p. 407-415  

---

## Examples  

```{r}
## Not run:

# Create the data
INDICATIVO <- rep("B013X", 200)
AÑO <- rep(2023, 200)
MES <- sample(1:12, size = 200, replace = T)
DIA <- sample(1:28, size = 200, replace = T)
HU00 <- sample(5:100, size = 200, replace = T)
HU07 <- sample(5:100, size = 200, replace = T)
HU13 <- sample(5:100, size = 200, replace = T)
HU18 <- sample(5:100, size = 200, replace = T)

# We convert it into a dataframe
df <- data.frame("INDICATIVO" = INDICATIVO,
"AÑO" = AÑO,
"MES" = MES,
"DIA" = DIA,
"HU00" = HU00,
"HU07" = HU07,
"HU13" = HU13,
"HU18" = HU18)

# We export it like a .csv or a .txt in the file \data\ (the first letter of the
climatological variable must be capitalized)
write.table(df,
          "~/data/Humedad.csv",
          fileEncoding = "latin1",
          row.names = F,
          quote = F,
          sep = ";")

# Now we can run the function
library(quality_control)
launch.controls(vars = "hr", data.source = "AEMET")

```
