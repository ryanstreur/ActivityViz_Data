##################################################################################
# Convert OMX file to CSV file used as input to activity_viz_passive_data.R
# Author: Aditya Gore
##################################################################################

#### Sections
# 1. Load/Install required packages
# 2. Define Constants
# 3. Load required databases
# 4. Create output data
# 4a. Passive Data Scenario
# 5. Write output data


### Load/Install required packages ###############################################
##################################################################################
library(data.table)
library(jsonlite)
library(stringr)
library(sf)
library(geojsonsf)
library(omxr)
library(rmapshaper)
library(tigris)
library(tidyverse)


### Define Constants #############################################################
##################################################################################

# Input files
data_dir   = file.path(getwd(), "data")

omx_file   = file.path(data_dir, "raw_data", "Nashville_Symetric.omx")

# Output files
od_mx_file   = file.path(data_dir, "raw_data", "ODME_i7.csv")

### Load required datasets #######################################################
##################################################################################

# Filter matrices to read
mat_names = list_omx(omx_file)$Matrices$name
keep_names = c(grep("Residents_", mat_names, value = TRUE), grep("Visitors_", mat_names, value=TRUE))
omx_dt = data.table(origin=integer(), destination=integer())
rows = read_lookup(omx_file, "Rows")$Lookup
cols = read_lookup(omx_file, "Cols")$Lookup
for(mat_name in keep_names){
  cat(mat_name, "\n")
  omx_dt = merge(omx_dt,
                 gather_matrix(read_omx(omx_file, mat_name), value_name = mat_name),
                 by=c("origin","destination"),
                 all=TRUE)
}
omx_dt = melt(omx_dt, id.vars=c("origin", "destination"), variable.name = "Type", value.name = "Trips",
              variable.factor=FALSE)
omx_dt[,Type:=gsub("OTHER_","", Type)]
omx_dt[,Type:=gsub("WORK_","", Type)]
omx_dt = omx_dt[,.(Trips=sum(Trips)),.(origin, destination, Type)]
omx_dt[,c("Residency", "Period"):=tstrsplit(Type,"_")]
omx_dt[,Residency:=paste0("Auto_", Residency)]
omx_dt = dcast(omx_dt, origin+destination+Period~Residency, value.var="Trips")
omx_dt[,TYPE:=Period]
omx_dt[Period=="MD", TYPE:="MIDDAY"]
omx_dt[Period=="OP", TYPE:="OFFPEAK"]

fwrite(omx_dt, file = od_mx_file)
