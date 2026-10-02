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

#### Check site distances ####

# read in site data
locals = read.csv("Formatted.Data/unique.FIA.FAGR.locals.csv", row.names = 1)

# need locals within the bounds of the rose data which differs from the wind data
locals.2 = locals %>% 
  filter(LON >= -89.84413, LON <= -66.09416,
         LAT >=37.15325, LAT <= 45.89519)

# this is too many sites and needs 60.4 Gb to run
# randomly selecting 1 plot from each state, unit, county, combo
set.seed(13)

sampled.locals = locals.2 %>%
  group_by(STATECD, UNITCD, COUNTYCD) %>%
  slice_sample(n = 1) %>%
  ungroup()
# gives 536 sites compared to 87,955

# subset for just the LON/LAT columns as a two column matrix of site coordinates
sites = cbind(x = sampled.locals$LON, y = sampled.locals$LAT)

# check if any sites are not found in rose
cells <- terra::cellFromXY(rose, sites)
table(is.na(cells))

# check how the wind rose resolution will work for these sites
check_cell_distance(rose, sites)

# Total point pairs: 143380
# Point pairs in the same grid cell: 107 (0.0746%)
# Distribution of cell-point distance discrepancies:
  #0--1%: 46605 (32.5%)
  #1--2.5%: 45552 (31.8%)
  #2.5--5%: 29055 (20.3%)
  #5--10%: 14756 (10.3%)
  #10--25%: 5927 (4.13%)
  #25--Inf%: 1485 (1.04%)

# try to see which cells have multiple sites
cells <- terra::cellFromXY(rose, sites)
sort(table(cells))

sampled.locals$cells = cells

# filter out sites in the same cell, choose first locations

sampled.locals.2 = sampled.locals %>%
  group_by(cells) %>% 
  slice(1) %>% 
  ungroup()
  
# 442 sites

# subset for just the LON/LAT columns as a two column matrix of site coordinates
sites.2 = cbind(x = sampled.locals.2$LON, y = sampled.locals.2$LAT)

# check if any sites are not found in rose
cells <- terra::cellFromXY(rose, sites.2)
table(is.na(cells))

# check how the wind rose resolution will work for these sites
check_cell_distance(rose, sites.2)

#Total point pairs: 97461
#Point pairs in the same grid cell: 0 (0%)
#Distribution of cell-point distance discrepancies:
  #0--1%: 32536 (33.4%)
  #1--2.5%: 31424 (32.2%)
  #2.5--5%: 19213 (19.7%)
  #5--10%: 9579 (9.83%)
  #10--25%: 3851 (3.95%)
  #25--Inf%: 858 (0.88%)

# downscale the rose data and try check_cell_distance again
rose.2 = downscale(rose, 5)
check_cell_distance(rose.2, sites.2)

# still have some pairs with high distance error, but 96.7% of site pairs have distance discrepancy of < 2.5%
# choosing this as a compromise resolution so computation is still possible on a local computer
#Total point pairs: 97461
#Point pairs in the same grid cell: 0 (0%)
#Distribution of cell-point distance discrepancies:
  #0--1%: 83081 (85.2%)
  #1--2.5%: 11247 (11.5%)
  #2.5--5%: 2308 (2.37%)
  #5--10%: 659 (0.676%)
  #10--25%: 154 (0.158%)
  #25--Inf%: 12 (0.0123%)

#### Building a Wind Graph ####
# A wind graph is a transitionLayer as defined in the gdistance package.
# Need to choose between downwind and upwind connectivity model. There are
# inversions of the same idea: the outbound wind (downwind) conductance from 
# site A to site B is the same thing as the inbound (upwind) conductance arriving
# to site B and site A. 

downwind = wind_graph(rose.2, direction = "downwind")
#saveRDS(downwind, "downwind.rds")
upwind = wind_graph(rose.2, direction = "upwind")
#saveRDS(upwind, "upwind.rds")

#### Estimating Wind Connectivity ####
# The wind_graph created above is the final wind connectivity model and can be
# used to estimate rates of wind transport among sites. This package supports two
# algorithms: least cost path (LCP) and random walk (RW). Because wind involves
# directional flow, wind connectivity has to be represented by a "directed cyclic"
# graph, which makes it impossible to use circuit theory-based algorithms that are
# commonly used in other landscape connectivity applications

# The LCP algorithm finds the fastest wind travel route across a landscape, 
# based on the local connectivity between each cell and its neighbors. It is
# computationally efficient, and represents the speed at which the first air
# particles would arrive at site after diffusing across the landscape from a 
# given origin location, given certain assumptions.

# The RW method instead runs a stepwise simulation. It is computationally much slower
# but it captures the full distribution of particles diffusing in different
# directions rather than simply the speed of the first, fastest particles to reach a site.

# least_cost_surface() and least_cost_distance() both calculate wind LCP but for
# different data structures. For wind models constructed with trans = 1, they
# produce results in units of travel time between grid cells

# going to use a downwind model that will calculate travel times from the 
# focal site to cells across the landscape. least_cost_surface() calculates the
# least cost distance between a set of user-defined sites (one or more point locations)
# and every grid cell across the region, producing a wall-to-wall raster of wind cost-distance.

# The user defined site is Hemlock Ridge Park, a Lake County Ohio Metropark
# Probable location of where BLD was first found.

BLD.site = matrix(c(-81.17671003653118,41.711114641962325),ncol = 2)
downwind_hrs = least_cost_surface(downwind,BLD.site)

# restructure the data and plot
d = downwind_hrs %>% 
  setNames("downwind") %>% 
  as.data.frame(xy=T) %>% 
  gather(direction, wind_hours, -x, -y)

range(d$wind_hours)
# 0.0000 814.8005

ggplot(d)+
  geom_raster(aes(x,y,fill = wind_hours))+
  geom_contour(aes(x,y,z = wind_hours), bins = 20, color = "white", linewidth = .25)+
  geom_point(data = as.data.frame(BLD.site), aes(V1,V2))+
  coord_fixed(ratio = 1.2)+
  scale_fill_gradientn(colors = c("yellow", "red", "blue", "black"))+
  theme_classic()

# least_cost_distance() calculates wind travel times between every pair of sites
# in a user-specified set of locations. It returns an asymmetric matrix in 
# which element [i,j] represents the cost-distance from the ith to the jth site.
# values represent hours of travel time.

wind_time = least_cost_distance(downwind, sites.2)
range(wind_time, na.rm = TRUE)
# 2.067288 1288.772689

# RW is implemented via the windscape function random_walk(). It involves an
# iterative computation, with "particle mass" diffusing from cells to their
# neighbors at each iteration, in proportion to local directional wind conductance.

# We need to specify the initial conditions (the starting distribution of the
# particle mass), the number of iterations, and the simulation mode. As an example,
# let's start with one unit of particle mass in a single central site and run a 
# random walk for 900 iterations, recording the distribution of particle mass every
# 100 iterations. Default mode = "pulse", which models the fleeting
# diffusion of the particle mass that is initially present. Mode = "ratchet" runs
# a propagating simulation in which local particle mass never declines in any cell
# with every location continuing to transmit mass at the cumulative maximum rate;
# this is more akin to a biological process in which dispersing particles reproduce
# locally after establishment, or in which a continuous stream of particles is 
# released from the original source at every time step. 

# run the RW computation
walk = random_walk(rose.2, init = BLD.site, iter = 600, record = seq(100,600,100))

# plot the results

walk %>%
  as.data.frame(xy = T) %>%
  gather(layer, value, -x, -y) %>%
  mutate(layer = paste0(layer, " (", round(as.integer(str_remove(layer, "iter")) * iter_length(walk)), " hours", ")"),
         layer = factor(layer, levels = unique(layer))) %>%
  ggplot(aes(x, y, fill = value)) +
  geom_raster() +
  annotate(geom = "point", x = BLD.site[1], y = BLD.site[2], color = "red", size = .5) +
  facet_wrap(~layer, nrow = 2) +
  scale_fill_viridis_c(trans = "sqrt") +
  theme_minimal() +
  labs(fill = "mass", x = NULL, y = NULL)

# run the RW computation
walk.ratchet = random_walk(rose.2, init = BLD.site, mode = "ratchet", iter = 600, record = seq(100,600,100))

# plot the results

walk.ratchet %>%
  as.data.frame(xy = T) %>%
  gather(layer, value, -x, -y) %>%
  mutate(layer = paste0(layer, " (", round(as.integer(str_remove(layer, "iter")) * iter_length(walk)), " hours", ")"),
         layer = factor(layer, levels = unique(layer))) %>%
  ggplot(aes(x, y, fill = value)) +
  geom_raster() +
  annotate(geom = "point", x = BLD.site[1], y = BLD.site[2], color = "red", size = .5) +
  facet_wrap(~layer, nrow = 2) +
  scale_fill_viridis_c(trans = "sqrt") +
  theme_minimal() +
  labs(fill = "mass", x = NULL, y = NULL)

#### Focus on least_cost_distance ####

# Need to add the original site as one of the sites for BLD

sampled.locals.3 = sampled.locals.2
sampled.locals.3[443,] = list(39,NA,85,NA,NA,41.711114641962325,-81.17671003653118,NA)
# 443 sites

write.csv(sampled.locals.3, file = "./Formatted.Data/sampled.locals.3.csv")

# manually added infection year column to sampled.locals.3 to make sampled.locals.4

sampled.locals.4 = read.csv("Formatted.Data/sampled.locals.4")

# subset for just the LON/LAT columns as a two column matrix of site coordinates
sites.3 = cbind(x = sampled.locals.3$LON, y = sampled.locals.3$LAT)

# check if any sites are not found in rose
cells <- terra::cellFromXY(rose, sites.3)
table(is.na(cells))

# check how the wind rose resolution will work for these sites
check_cell_distance(rose, sites.3)

#Total point pairs: 97903
#Point pairs in the same grid cell: 0 (0%)
#Distribution of cell-point distance discrepancies:
#0--1%: 32689 (33.4%)
#1--2.5%: 31567 (32.2%)
#2.5--5%: 19308 (19.7%)
#5--10%: 9614 (9.82%)
#10--25%: 3863 (3.95%)
#25--Inf%: 862 (0.88%)

# downscale the rose data and try check_cell_distance again
check_cell_distance(rose.2, sites.3)

# still have some pairs with high distance error, but 96.8% of site pairs have distance discrepancy of < 2.5%
# choosing this as a compromise resolution so computation is still possible on a local computer
#Total point pairs: 97903
#Point pairs in the same grid cell: 0 (0%)
#Distribution of cell-point distance discrepancies:
#0--1%: 83443 (85.2%)
#1--2.5%: 11312 (11.6%)
#2.5--5%: 2320 (2.37%)
#5--10%: 662 (0.676%)
#10--25%: 154 (0.158%)
#25--Inf%: 12 (0.0123%)



# least_cost_distance() calculates wind travel times between every pair of sites
# in a user-specified set of locations. It returns an asymmetric matrix in 
# which element [i,j] represents the cost-distance from the ith to the jth site.
# values represent hours of travel time.

wind_time.2 = least_cost_distance(downwind, sites.3)
range(wind_time.2, na.rm = TRUE)
# 2.067288 1288.772689



# extract out the wind_time from first observation site to all other sites





#### Testing Statistical Relationships ####

# Does wind connectivity predict order of invasion/symptom observation?



# time distance in hours like the wind and how it relates to wind flow, wind speed, wind asymmetry

# steps
# 1. nematodes disperse in fall of year 1
# 2. symptoms observed in spring of year 2

# Does wind connectivity predict order of invasion?
# for each site calculate wind_time from the original outbreak to the site and
# year disease was first observed. 

# If wind contributes to spread, sites with lower wind_time should generally become
# infected earlier than sites with higher wind time. 

# Year ~ Geographic Distance
# Year ~ Wind_Time
# Year ~ Geographic Distance + Wind Time 


# BLD is likely to appear at sites that are most accessible by fall wind dispersal
# from sites that wre already infected that same fall. Comparing whether a site
# was connected by wind before it became infected.

# For every stie detected in year Y:
# Potential source sites are those detected in years < Y
# compute the wind travel time from each of those previously infected sites
# Ask whether the site had strong wind connectivity to the infected network before  it became infected

# Dataframe would be Site, Detection Year, minimum wind_time from infected sites the previous fall
# minimum wind_time, mean wind_time to infected sites,
# number of infected sites reachable within 12 h
# number reachable within 24 h

# This becomes a discrete-time survival model. EAch site contributed one record per year until infection

# would need to control geographic distance since probably correlated with wind_time

# For each site, calculate minimum wind_time from any previously infected site


