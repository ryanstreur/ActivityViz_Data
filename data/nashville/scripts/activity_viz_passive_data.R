##################################################################################
# Create data for Nashville OD Data Visulization using ActivityViz
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

# od_AM_mx_file   = file.path(data_dir, "raw_data", "ODME_AM_i7.csv")
# od_MD_mx_file   = file.path(data_dir, "raw_data", "ODME_MD_i7.csv")
# od_PM_mx_file   = file.path(data_dir, "raw_data", "ODME_PM_i7.csv")
# od_OP_mx_file   = file.path(data_dir, "raw_data", "ODME_OP_i7.csv")
od_mx_file   = file.path(data_dir, "raw_data", "ODME_i7.csv")



# Geography input files
taz_file       = file.path(data_dir, "raw_shapefile", "TAZ", "TAZ_nashville_split.shp")
ext_zone_file  = file.path(data_dir, "raw_shapefile", "ExtZones", "nashville_nodes_2010_ExtSt.shp")

# Output files
od_output_dir = file.path(getwd(), "OD")

# Passive_Data
taz_shapefile_file                    = file.path(getwd(),
                                                  "taz.json")         # TAZ Shapefile
county_shapefile_file                 = file.path(getwd(),
                                                  "counties.json")    # Counties Shapefile
county_filter_file                    = file.path(od_output_dir,
                                                  "counties.csv")    # Counties Shapefile
daily_dest_file                       = file.path(od_output_dir,
                                                  "daily_dest_trips.csv")  #
daily_overall_file                    = file.path(od_output_dir,
                                                  "daily_overall_trips.csv")  # Daily overall passive data OD charts
daily_overall_time_file               = file.path(od_output_dir,
                                                  "daily_overall_period_trips.csv") # Daily overall passive data OD
                                                                                    # charts by Timezone
daily_am_file                         = file.path(od_output_dir,
                                                  "daily_am_trips.csv")    # Daily am passive data OD charts
daily_md_file                         = file.path(od_output_dir,
                                                  "daily_md_trips.csv")    # Daily md passive data OD charts
daily_pm_file                         = file.path(od_output_dir,
                                                  "daily_pm_trips.csv")    # Daily pm passive data OD charts
daily_op_file                         = file.path(od_output_dir,
                                                  "daily_op_trips.csv")    # Daily op passive data OD charts
daily_tod_file                        = file.path(od_output_dir,
                                                  "trip_tod.csv")    #



### Load required datasets #######################################################
##################################################################################

# od_AM_dt   = fread(od_AM_mx_file)
# od_MD_dt   = fread(od_MD_mx_file)
# od_PM_dt   = fread(od_PM_mx_file)
# od_OP_dt   = fread(od_OP_mx_file)
# 
# 
# od_AM_dt[Auto_Residents < 0, Auto_Residents:=0]
# od_MD_dt[Auto_Residents < 0, Auto_Residents:=0]
# od_PM_dt[Auto_Residents < 0, Auto_Residents:=0]
# od_OP_dt[Auto_Residents < 0, Auto_Residents:=0]
# 
# od_AM_dt[Auto_Visitors < 0, Auto_Visitors:=0]
# od_MD_dt[Auto_Visitors < 0, Auto_Visitors:=0]
# od_PM_dt[Auto_Visitors < 0, Auto_Visitors:=0]
# od_OP_dt[Auto_Visitors < 0, Auto_Visitors:=0]
# 
# od_AM_dt[, TYPE:="AM"]
# od_MD_dt[, TYPE:="MIDDAY"]
# od_PM_dt[, TYPE:="PM"]
# od_OP_dt[, TYPE:="OFFPEAK"]
# 
# trip_dt = rbindlist(list(od_AM_dt, od_MD_dt, od_PM_dt, od_OP_dt),
#                     use.names = TRUE,
#                     fill = TRUE) 
trip_dt = fread(od_mx_file)

taz_sf       = st_read(taz_file)
taz_dt       = data.table(taz_sf)
ext_zones_sf = st_read(ext_zone_file)
ext_zones_dt = data.table(ext_zones_sf)

time_order = c("AM", "MIDDAY", "PM", "OFFPEAK")

### Create output data ###########################################################
##################################################################################

### Simplify shapefile ###########################################################
##################################################################################
# format(object.size(taz_sf), units="Mb")
# taz_gg = ggplot(taz_sf) + geom_sf()
# Attach County Data
state_fips  = "47"   # Tennessee
# county_fips = "037"  # Davidson County
county_fips = NULL
# bg_sf = st_as_sf(block_groups(state_fips, county_fips))
county_sf = st_as_sf(counties(state_fips))
county_sf = st_transform(county_sf, st_crs(taz_sf))
taz_add_sf = st_intersection(taz_sf, county_sf[,c("COUNTYFP", "NAME", "NAMELSAD", "geometry")])
taz_add_sf = taz_add_sf[!is.na(taz_add_sf$ID_NEW_NEW),]
taz_add_sf$IAREA = units::set_units(st_area(taz_add_sf), "mi^2")
taz_add_sf$prop = as.numeric(taz_add_sf$IAREA)/taz_add_sf$AREA
taz_add_sf = taz_add_sf[taz_add_sf$prop > 1e-1,]
taz_sf$NAME = taz_add_sf$NAME[match(taz_sf$ID_NEW_NEW, taz_add_sf$ID_NEW_NEW)]
taz_add_dt = data.table(taz_add_sf)
taz_simplify_sf = st_as_sf(ms_simplify(input = as(taz_add_sf[,c("ID_NEW_NEW",
                                                                "NAME",
                                                                "geometry")], "Spatial"),
                                       keep = 0.04,
                                       weighting = 0.8,
                                       keep_shapes = TRUE))
colnames(taz_simplify_sf) = c("id", "NAME", "geometry")
taz_simplify_sf = taz_simplify_sf[order(taz_simplify_sf$id),]
# county_simplify_sf = taz_add_sf %>% group_by(NAME, NAMELSAD) %>% summarize(AREA=sum(AREA))
# county_simplify_sf = st_as_sf(ms_simplify(input = as(county_simplify_sf[,c("NAME",
#                                                                            "NAMELSAD",
#                                                                            "geometry")], "Spatial"),
#                                        keep = 0.04,
#                                        weighting = 0.8,
#                                        keep_shapes = TRUE))
# format(object.size(taz_simplify_sf), units="Mb")
# taz_simplify_gg = ggplot(taz_simplify_sf) + geom_sf()
# 
# gridExtra::grid.arrange(taz_gg, taz_simplify_gg, nrow = 1)

### Passive Data Scenario ########################################################
##################################################################################

# County Filter File
county_filter_sf = county_sf[county_sf$NAME %in% taz_simplify_sf$NAME,c("NAME", "geometry")]
county_mx = diag(nrow=nrow(county_filter_sf))
colnames(county_mx) = county_filter_sf$NAME
county_filter_dt = data.table(ID=seq_along(county_filter_sf$NAME),
                              COUNTY=county_filter_sf$NAME,
                              data.table(county_mx))

# County File
county_filter_sf = county_filter_sf[order(county_filter_sf$NAME),c("NAME", "geometry")]
ext_zones_sf$NAME = "External"
ext_add_sf = ext_zones_sf[,c("NAME", "geometry")]
order_names = c("External", rev(sort(county_filter_sf$NAME)))
# county_filter_sf = rbind(county_filter_sf, ext_add_sf)
county_filter_sf = county_filter_sf %>% group_by(NAME) %>% summarise() %>% ungroup()
county_filter_sf =county_filter_sf[order(-(match(county_filter_sf$NAME,order_names))),]
county_filter_sf$ID = seq_len(nrow(county_filter_sf))
county_filter_sf = county_filter_sf[,c("ID", "NAME", "geometry")]

# Chord Diagram
trip_dt[taz_add_dt,COUNTY_O:=i.NAME, on=.(origin=ID_NEW_NEW)]
trip_dt[taz_add_dt,COUNTY_D:=i.NAME, on=.(destination=ID_NEW_NEW)]
trip_dt[origin %in% ext_zones_dt$ID_NEW & is.na(COUNTY_O),
        COUNTY_O:="External"]
trip_dt[destination %in% ext_zones_dt$ID_NEW & is.na(COUNTY_D),
        COUNTY_D:="External"]

# Overall

# Daily Destination
daily_dest_dt    = trip_dt[,.(#ALL      =round(sum(Auto_Residents+Auto_Visitors), 2),
                              RESIDENTS=round(sum(Auto_Residents), 2),
                              VISITORS =round(sum(Auto_Visitors), 2)),
                           by = .(ZONE = destination, COUNTY = COUNTY_D)]
# daily_dest_dt[,ZONE:=county_filter_sf$ID[match(COUNTY,county_filter_sf$NAME)]]
setcolorder(daily_dest_dt, c("ZONE"))
daily_dest_dt = melt.data.table(daily_dest_dt,
                                id.vars = c("ZONE", "COUNTY"),
                                variable.name = "PERSON GROUP",
                                variable.factor = FALSE,
                                value.name = "QUANTITY",
                                value.factor = FALSE)
daily_dest_dt = daily_dest_dt[order(ZONE, COUNTY, match(`PERSON GROUP`,c("RESIDENTS", "VISITORS", "ALL")))]


# Daily Total
daily_overall_dt = trip_dt[,.(TOTAL    =round(sum(Auto_Residents+Auto_Visitors), 2),
                              RESIDENTS=round(sum(Auto_Residents), 2),
                              VISITORS =round(sum(Auto_Visitors), 2)),
                           .(FROM = COUNTY_O,
                              TO   = COUNTY_D)]
setkey(daily_overall_dt, FROM, TO)
daily_overall_dt = daily_overall_dt[CJ(FROM, TO, unique = TRUE)]
daily_overall_dt[is.na(TOTAL),     TOTAL:=0]
daily_overall_dt[is.na(RESIDENTS), RESIDENTS:=0]
daily_overall_dt[is.na(VISITORS),  VISITORS:=0]


## TIME Distribution
daily_time_dt = trip_dt[,.(TOTAL    =round(sum(Auto_Residents+Auto_Visitors), 2),
                           RESIDENTS=round(sum(Auto_Residents), 2),
                           VISITORS =round(sum(Auto_Visitors), 2)),
                        .(FROM     = COUNTY_O,
                          TO       = COUNTY_D,
                          TIMEZONE = TYPE)]
time_temp_dt = dcast.data.table(daily_time_dt, FROM+TO~TIMEZONE, value.var = "TOTAL")
time_temp_dt = merge(time_temp_dt,
                     dcast.data.table(daily_time_dt, FROM+TO~TIMEZONE, value.var = "RESIDENTS"),
                     by  = c("FROM", "TO"),
                     all = TRUE,
                     suffixes = c("_TOTAL", ""))
time_temp_dt = merge(time_temp_dt,
                     dcast.data.table(daily_time_dt, FROM+TO~TIMEZONE, value.var = "VISITORS"),
                     by  = c("FROM", "TO"),
                     all = TRUE,
                     suffixes = c("_RESIDENTS", "_VISITORS"))
daily_time_dt = copy(time_temp_dt)
rm(time_temp_dt)

setkey(daily_time_dt, FROM, TO)
daily_time_dt = daily_time_dt[CJ(FROM, TO, unique = TRUE)]
trip_names = setdiff(names(daily_time_dt), c("FROM", "TO"))
daily_time_dt[, c(trip_names):=lapply(.SD, function(x) {x[is.na(x)] = 0; x}),
              .SDcols=c(trip_names)]

# Time Total Resident Visitor
time_trip_dt = trip_dt[,.(TOTAL    =round(sum(Auto_Residents+Auto_Visitors), 2),
                          RESIDENTS=round(sum(Auto_Residents), 2),
                          VISITORS =round(sum(Auto_Visitors), 2)),
                       .(FROM     = COUNTY_O,
                         TO       = COUNTY_D,
                         TIMEZONE = TYPE)]
## AM
am_trips_dt = time_trip_dt[TIMEZONE=="AM"][,TIMEZONE:=NULL][]
setkey(am_trips_dt, FROM, TO)
am_trips_dt = am_trips_dt[CJ(FROM, TO, unique = TRUE)]
am_trips_dt[is.na(TOTAL),     TOTAL:=0]
am_trips_dt[is.na(RESIDENTS), RESIDENTS:=0]
am_trips_dt[is.na(VISITORS),  VISITORS:=0]
## MD
md_trips_dt = time_trip_dt[TIMEZONE=="MIDDAY"][,TIMEZONE:=NULL][]
setkey(md_trips_dt, FROM, TO)
md_trips_dt = md_trips_dt[CJ(FROM, TO, unique = TRUE)]
md_trips_dt[is.na(TOTAL),     TOTAL:=0]
md_trips_dt[is.na(RESIDENTS), RESIDENTS:=0]
md_trips_dt[is.na(VISITORS),  VISITORS:=0]
## PM
pm_trips_dt = time_trip_dt[TIMEZONE=="PM"][,TIMEZONE:=NULL][]
setkey(pm_trips_dt, FROM, TO)
pm_trips_dt = pm_trips_dt[CJ(FROM, TO, unique = TRUE)]
pm_trips_dt[is.na(TOTAL),     TOTAL:=0]
pm_trips_dt[is.na(RESIDENTS), RESIDENTS:=0]
pm_trips_dt[is.na(VISITORS),  VISITORS:=0]
## OP
op_trips_dt = time_trip_dt[TIMEZONE=="OFFPEAK"][,TIMEZONE:=NULL][]
setkey(op_trips_dt, FROM, TO)
op_trips_dt = op_trips_dt[CJ(FROM, TO, unique = TRUE)]
op_trips_dt[is.na(TOTAL),     TOTAL:=0]
op_trips_dt[is.na(RESIDENTS), RESIDENTS:=0]
op_trips_dt[is.na(VISITORS),  VISITORS:=0]

# Time of day vs Resident/Visitor
tod_trips_dt = trip_dt[,.(#ALL      =round(sum(Auto_Residents+Auto_Visitors), 2),
                          RESIDENTS=round(sum(Auto_Residents), 2),
                          VISITORS =round(sum(Auto_Visitors), 2)),
                       .(`TIME OF DAY` = TYPE)]

tod_trips_dt = melt.data.table(tod_trips_dt, id.vars = c("TIME OF DAY"),
                               variable.name = "PERSON GROUP",
                               variable.factor = FALSE,
                               value.name = "TRIPS",
                               value.factor = FALSE)
tod_trips_dt[,CHART:="TRIPS BY TIME OF DAY"]

tod_trips_dt = tod_trips_dt[order(match(`TIME OF DAY`, time_order), `PERSON GROUP`)]

### Write output data ############################################################
##################################################################################

## Passive Data
# Shapefile
st_write(taz_simplify_sf, dsn = taz_shapefile_file, driver = "GeoJSON", delete_dsn = TRUE)
st_write(county_filter_sf, 
         dsn = county_shapefile_file, driver = "GeoJSON", delete_dsn = TRUE)

# Filter File
fwrite(county_filter_dt,   file = county_filter_file)

# Trip OD
fwrite(daily_dest_dt[COUNTY!="External"],      file = daily_dest_file)
fwrite(daily_overall_dt,   file = daily_overall_file)
fwrite(daily_time_dt,      file = daily_overall_time_file)
fwrite(am_trips_dt,        file = daily_am_file)
fwrite(md_trips_dt,        file = daily_md_file)
fwrite(pm_trips_dt,        file = daily_pm_file)
fwrite(op_trips_dt,        file = daily_op_file)
fwrite(tod_trips_dt,       file = daily_tod_file)



