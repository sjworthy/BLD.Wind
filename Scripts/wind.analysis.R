# Code to evaluate the influence of wind patterns on LCM dispersal

# load libraries
devtools::install_github("matthewkling/windscape")
library(windscape)
library(ncdf4)
library(tidyverse)

# Reworked windscape functions, found in script functions.R
# webpage with all data: https://www.ncei.noaa.gov/oa/prod-cfs-reanalysis/index.html#time-series/
# webpage with some of the data: https://www.ncei.noaa.gov/thredds/catalog/model-cfs_reanl_ts/catalog.html

# get the wind data
# LCM detected starting in August according to David B. data
wind <- cfsr_dl(variable = "wnd10m", years = 1980:2010, months = 8:11,
                xlim = c(-90, -66) + 360, # shift longitudes to be in [0, 360] range for CFSR
                ylim = c(37, 46))

wind.2 <- terra::shift(wind, dx = -360)

terra::writeRaster(wind, "wind_10m_stack.tif", overwrite = TRUE)

# convert data into a formal wind field time series object

series = wind_series(wind.2, order = "uuvv")

