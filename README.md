# gdx2iamc

R package to load GDX file(s) into data.frame compatible with the IAMC format.  

## Installation

```R
library(devtools)
install_github('IAMconsortium/gdx2iamc')
```

it requires the package gdxtools to be installed as follows:

```R
install_github('lolow/gdxtools')
```


## Usage

```R

## REQUIREMENTS

if(!"gdx2iamc" %in% rownames(installed.packages())){
  library(devtools)
  install_github('lolow/gdx2iamc')
}
if(packageVersion("gdxtools")<numeric_version("0.2.0")){
  stop("You need to install a newer version of gdx2iamc (>=0.2.0). Please run remove.packages('gdx2iamc'), restart R and rerun this script.")
}
library(gdx2iamc)
library(gdxtools)
library(data.table)
library(stringr)

## PARAMETERS
#Absolute folder where the gdx files are located
templatedir = "C:\\WITCH\\my_runs"
templatefile = "PROJECT_template.xlsx"
model_name = "WITCH2016"
convert_usd =  1.10774 # Factor to convert USD2005 into USD2010
max_row_xlsx = 15000 # Maximum number of rows in an excel file (approx 3MB)
# NREP is case-sensitive. It should have the same case as in the registration template
nrep.keep = c('USA', 'EUROPE', 'CHINA', 'INDIA', 'World')
# Convert GDX filename in scenario name
scenario_name <- function(gdxfile){
  name = basename(gdxfile)
  name = str_sub(name,str_length("db_")+1,-5)
  return(name)
}

## PROCESS

# Load IIASA xls template
template = iamc.template(file.path(templatedir, templatefile))

# Load variable definition
iamc.vars = load_var(template, convert_usd = usd2005_2010)

# list of gdx
gdxfiles = Sys.glob(file.path(templatedir, 'db*.gdx'))

#Load data from the GDX files and store it in gdx.data
gdx.data = load_gdx(template, gdxfiles, nrep.keep, scen_func=scenario_name)

```

## IAMC submission

You can create an excel spreadsheet for IAMC submission with the function `save_xls`

```R
#Save data into xls files
res = save_xls(template,gdx.data, iamc.vars, model_name, maxrowfile=max_row_xlsx)
```


