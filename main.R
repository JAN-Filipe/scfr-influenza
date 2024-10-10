################################################################################
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(here)

### forders
input_dir  <- paste0(getwd(),"/data")
source_dir <- paste0(getwd(),"/code")
output_dir <- paste0(getwd(),"/output")
fs::dir_create(output_dir)

### time referencing
TODAY <- format(Sys.Date(), "%d-%m-%Y")
TIME  <- format(Sys.time(),'%H.%M.%S_%d-%m-%Y')

### running codes
cat("Gathering data and tables...\n")
source(file = paste0(source_dir,"/Data-analysis_seasonal-sCFR-IFR.r"))

cat("Making figures...\n")
source(file = paste0(source_dir,"/Figures_seasonal-sCFR-IFR.r"))

################################################################################
