# Code to evaluate the influence of wind patterns on LCM dispersal

# load libraries
devtools::install_github("matthewkling/windscape")
library(windscape)
library(ncdf4)
library(tidyverse)

# Reworked windscape functions, found in script functions.R
# webpage with all data: https://www.ncei.noaa.gov/oa/prod-cfs-reanalysis/index.html#time-series/
# webpage with some of the data: https://www.ncei.noaa.gov/thredds/catalog/model-cfs_reanl_ts/catalog.html

#### Get the wind data ####
# LCM detected starting in August according to David B. data
wind <- cfsr_dl(variable = "wnd10m", years = 1980:2010, months = 8:11,
                xlim = c(-90, -66) + 360, # shift longitudes to be in [0, 360] range for CFSR
                ylim = c(37, 46))

wind.2 <- terra::shift(wind, dx = -360)

writeRaster(wind.2, "wind.nc", overwrite=TRUE, filetype="netCDF")

# convert data into a formal wind field time series object
series <- wind_series(wind.2, order = "uuvv")

#### Convert to wind rose object #####

# In order to estimate wind flows among sites of interest, first convert
# wind field time series into a directed connectivity graph representing
# the average wind flow between each grid cell and each of its eight "queen"
# neighbors.

# The steps in this process include summarizing the time series into a wind_rose
# raster object, optionally modifying the wind rose to incorporate non-wind
# factors influencing dispersal and/or to increase its spatial resolution, and
# then converting this into a wind_graph transition object.

# creating wind_rose object
# trans argument: defines the transformation that turns wind speed into conductance strength.
# default = 1, make conductance proportional to wind speed
# alternatively, conductance can be made proportional to aerodynamic drag (speed^2)
# or to wind force (speed^3), it can account for threshold speeds above which seed
# abscission is likely to occur, or can ignore speed entirely and consider only direction
# If input speeds are in m/s and trans = 1, then the wind rose conductance values are in units of 1/hours

rose = wind_rose(series, trans = 1)
plot(rose)

##### check site distances #####

# check how the wind rose resolution will work for these sites
check_cell_distance(rose, sites)






