#Code Started 4/14/2026 by Rory S.
# Code adapted by Sam W.
#Pulling PLOT and TREE data for 15 FIA States to calculate annual and cumulative mortality rates of beech 

library(tidyverse)
library(ggpubr)
library(sf) #for mapping
library(tigris) #FOR FIPS DATA
options(tigris_use_cache = TRUE)
library(terra) #for raster data
library(patchwork)

#Reading in all FIA data and sub-setting beech#### (note all this data takes ~30 minutes to load)####

#setwd("C:/Users/rschiafo/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Hemlock - Forest Health/Potential Datasets/FIA/Data")
##
###CT, DE, ME, MD, MA, MI, NC, NH, NJ, NY, OH, PA, RI, VA, VT, WV
##
#a_CT_plot<-read.csv("CT_PLOT.csv")
#a_CT_tree<-read.csv("CT_TREE.csv")
#a_CT_tree<-subset(a_CT_tree, SPCD==531)  #American beech= 531
#
#a_DE_plot<-read.csv("DE_PLOT.csv")
#a_DE_tree<-read.csv("DE_TREE.csv")
#a_DE_tree<-subset(a_DE_tree, SPCD==531)  #American beech= 531F
#
#a_MA_plot<-read.csv("MA_PLOT.csv")
#a_MA_tree<-read.csv("MA_TREE.csv")
#a_MA_tree<-subset(a_MA_tree, SPCD==531)  #American beech= 531
#
#a_MD_plot<-read.csv("MD_PLOT.csv")
#a_MD_tree<-read.csv("MD_TREE.csv")
#a_MD_tree<-subset(a_MD_tree, SPCD==531)  #American beech= 531
#
#a_ME_plot<-read.csv("ME_PLOT.csv")
#a_ME_tree<-read.csv("ME_TREE.csv")
#a_ME_tree<-subset(a_ME_tree, SPCD==531)  #American beech= 531
#
#a_MI_plot<-read.csv("MI_PLOT.csv")
#a_MI_tree<-read.csv("MI_TREE.csv")
#a_MI_tree<-subset(a_MI_tree, SPCD==531)  #American beech= 531
#
#a_NC_plot<-read.csv("NC_PLOT.csv")
#a_NC_tree<-read.csv("NC_TREE.csv")
#a_NC_tree<-subset(a_NC_tree, SPCD==531)  #American beech= 531
#
#a_NH_plot<-read.csv("NH_PLOT.csv")
#a_NH_tree<-read.csv("NH_TREE.csv")
#a_NH_tree<-subset(a_NH_tree, SPCD==531)  #American beech= 531
#
#a_NJ_plot<-read.csv("NJ_PLOT.csv")
#a_NJ_tree<-read.csv("NJ_TREE.csv")
#a_NJ_tree<-subset(a_NJ_tree, SPCD==531)  #American beech= 531
#
#a_NY_plot<-read.csv("NY_PLOT.csv")
#a_NY_tree<-read.csv("NY_TREE.csv")
#a_NY_tree<-subset(a_NY_tree, SPCD==531)  #American beech= 531
#
#a_OH_plot<-read.csv("OH_PLOT.csv")
#a_OH_tree<-read.csv("OH_TREE.csv")
#a_OH_tree<-subset(a_OH_tree, SPCD==531)  #American beech= 531
#
#a_PA_plot<-read.csv("PA_PLOT.csv")
#a_PA_tree<-read.csv("PA_TREE.csv")
#a_PA_tree<-subset(a_PA_tree, SPCD==531)  #American beech= 531
#
#a_RI_plot<-read.csv("RI_PLOT.csv")
#a_RI_tree<-read.csv("RI_TREE.csv")
#a_RI_tree<-subset(a_RI_tree, SPCD==531)  #American beech= 531
#
#a_VA_plot<-read.csv("VA_PLOT.csv")
#a_VA_tree<-read.csv("VA_TREE.csv")
#a_VA_tree<-subset(a_VA_tree, SPCD==531)  #American beech= 531
#
#a_VT_plot<-read.csv("VT_PLOT.csv")
#a_VT_tree<-read.csv("VT_TREE.csv")
#a_VT_tree<-subset(a_VT_tree, SPCD==531)  #American beech= 531
#
#a_WV_plot<-read.csv("WV_PLOT.csv")
#a_WV_tree<-read.csv("WV_TREE.csv")
#a_WV_tree<-subset(a_WV_tree, SPCD==531)  #American beech= 531
##
###Break
##
###Merging all TREE dataframes
#all_tree<-rbind(a_CT_tree, a_DE_tree, a_MA_tree, a_MD_tree, a_ME_tree, a_MI_tree, a_NC_tree, a_NH_tree, a_NJ_tree, a_NY_tree, a_OH_tree, a_PA_tree, a_RI_tree, a_VA_tree, a_VT_tree, a_WV_tree)
###Merging all PLOT dataframes
#all_plot<-rbind(a_CT_plot, a_DE_plot, a_MA_plot, a_MD_plot, a_ME_plot, a_MI_plot, a_NC_plot, a_NH_plot, a_NJ_plot, a_NY_plot, a_OH_plot, a_PA_plot, a_RI_plot, a_VA_plot, a_VT_plot, a_WV_plot)
##
##Outputting these combined dataframes that only contain Fagus for easier data uploads
#setwd("C:/Users/rschiafo/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Hemlock - Forest Health/Analysis/FIA Analysis/Input Data")
#write.csv(all_tree, file = "FIA_TREE_TargetState_Fagus_data_full.csv", row.names=FALSE)
#write.csv(all_plot, file = "FIA_PLOT_TargetState_Fagus_data_full.csv", row.names=FALSE)

#Break####

#Start here

# setwd("C:/Users/rschiafo/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Hemlock - Forest Health/Analysis/FIA Analysis/Input Data")

#Reading Back in the FIA Data that I have downloaded for my states of interest, clipped to only Fagus, and combined
#all_tree<-read.csv("FIA_TREE_TargetState_Fagus_data_full.csv")
#all_plot<-read.csv("FIA_PLOT_TargetState_Fagus_data_full.csv")

all_plot = read.csv("Formatted.Data/FIA_PLOT_TargetState_Fagus_data_full.csv")

#Reading in BLD infection year data 
bld_infect_years<-read.csv("Formatted.Data/BLD.counties.csv")

#also reading in the County and State codes from package for merging
#These FIPS are consistent with FIA codes and with fips generated in GIS
counties <- counties(cb = TRUE, year = 2025) #NOTE: using 2025 b/c after 2021 the CT counties changed to a 9-region setup that DOES the data I have for BLD infection
counties_old <- counties(cb = TRUE, year = 2020) #These are the old county names (relevant for CT)
counties_old <- counties_old %>% filter(STATEFP=="09") %>% 
  rename(GEOIDFQ=AFFGEOID)
counties<-rbind(counties, counties_old)

fips_master <- counties[, c("STATEFP", "COUNTYFP", "NAME", "NAMELSAD")]


#########Exploring and Subsetting PLOT data###############
#
##########################################

#how to group data to get unique PLOTS? 
#This is the minimum I need to group_by to get only n=1 for each INVYR indicating that I have unique "plots" (no plot was surveyed more than once in a year- if n>1 then the "plot" is not unique and you are looking at plots that differ in either STATE, UNIT, COUNTY, or DESIGN)
unique<- all_plot %>% 
  group_by(STATECD, UNITCD, COUNTYCD, DESIGNCD, PLOT) %>% 
  count(INVYR)
#When I say "plots" I mean the unique combination of these things!

#Do I need to include DESIGNCD? 
#YES- this is required for usable comparisons across years so is a requirement that plots are grouped this way
#makes it a little weird though b.c inventories could be from the same physical plots but could not line up if different designs
#At this point these are treated as separate "plots" so I can make sure I am keeping only "plots" with at least 2 inventories that can be compared (different DESIGNCD can't be compared even if from same physcial plot)

#Counting unique plots
unique_plots<-all_plot %>% group_by(STATECD, UNITCD, COUNTYCD, DESIGNCD, PLOT) %>% 
  count(name="INVYR_n") 
#185113 unique plots 

# get unique plots based on lat/long
unique_locals = all_plot %>%
  distinct(LAT,LON, .keep_all = TRUE)
# 121006 unique locations

# check and make sure it worked
unique_plots = unique_locals %>% group_by(STATECD, UNITCD, COUNTYCD, DESIGNCD, PLOT) %>% 
  count(name="INVYR_n")
# nope some plots have different lat/lons. Seems to be plots with an older census

# group by plot and select plot with most recent measurement year
unique_locals_2 = unique_locals %>% 
  group_by(STATECD, UNITCD, COUNTYCD, DESIGNCD, PLOT) %>% 
  slice_max(MEASYEAR, n = 1, with_ties = FALSE) %>%
  ungroup()

# check and make sure it worked, it does work
unique_plots<-unique_locals_2 %>% group_by(STATECD, UNITCD, COUNTYCD, DESIGNCD, PLOT) %>% 
  count(name="INVYR_n")


#Break

#How many inventory years does each Unique Plot have?
count_invyr<-all_plot %>% group_by(STATECD, UNITCD, COUNTYCD, DESIGNCD, PLOT) %>% 
  count(name="INVYR_n") 

#REMOVING any plots that have less than one INVYR 
#KEEPING only plots have at least 2 INVYR
subset_invyr<-count_invyr %>% filter(INVYR_n>1)


#checking subset work- Yes- these are the plots that only have one inventory year- 
#two dataframes add up to 185113 plots
subset_invyr_test<-count_invyr %>% filter(INVYR_n==1)
#Good

#There are 185113 Unique Plots with More than 1 INVYR

#Break

#NOW Keeping only the Plots that are in subset_invyr in the all_plot dataframe (where I have the data)
all_plot_subset<-inner_join(all_plot, subset_invyr, by= c("STATECD", "UNITCD", "COUNTYCD", "DESIGNCD", "PLOT"))
#double checking this worked by counting number of unique plots in all_plot_subset 
test_count_all_plot_subset<-all_plot_subset %>% group_by(STATECD, UNITCD, COUNTYCD, DESIGNCD, PLOT) %>% 
count(name="INVYR_n")
#adds up to 67913

#Break
#Break

#Now working with the subset with observations with INVYR_n>1 only

#Seeing how many unique DesignCD there are for each "physical" plot
unique_design <- all_plot_subset %>%
  group_by(STATECD, UNITCD, COUNTYCD, PLOT) %>%
  summarize(DESIGNCD_n = n_distinct(DESIGNCD, na.rm = TRUE)) %>% 
  filter(DESIGNCD_n==1)
#where am I removing from?
test_design_antijoin <- anti_join(subset_invyr, unique_design)
#There are only 78 instances (compared to >56K) where there are more than one Designs within a physical Plot
#IMPORTANT- For simplicity of methods I am going to remove any plots where there are design discrepancies 
#if there is more than one DESIGNCD that shows up across INVYR for that plot, they cannot be compared 



#note these are all from CT- removing

#Break
#Break####

#
#

#NOW there are 67835 unique plots with more than 1 INVYR but only one DESIGNCD

#Removing any physical plot that has more than one DESIGNCD!

#NOW Keeping only the Plots that are in subset_invyr in the all_plot dataframe (where I have the data)
all_plot_subset_subset<-inner_join(all_plot_subset, unique_design, by= c("STATECD", "UNITCD", "COUNTYCD",  "PLOT"))
#double checking this worked by counting number of unique plots in all_plot_subset (Should match up to 117538)
test_count_all_plot_subset_subset<-all_plot_subset_subset %>% group_by(STATECD, UNITCD, COUNTYCD,  PLOT) %>% 
count(name="INVYR_n") #Looks good- comes to 67835 plots


#Break
#Break
#BReak

#Adding A UNIQUE PLOT IDENTIFIER that has all State, Unit, County, Plot information in one code (in that order!)
#all_plots
all_plot_subset_subset<-all_plot_subset_subset %>% 
  unite(col = PLOTID, 
        STATECD, UNITCD, COUNTYCD, PLOT,  
        sep= "_", remove = FALSE)

# doing this for my plots

unique_locals_3 = unique_locals_2 %>% 
  unite(col = PLOTID, 
        STATECD, UNITCD, COUNTYCD, PLOT,  
        sep= "_", remove = FALSE)

#Double checking inner join
test_unique_plots<-all_plot_subset_subset %>% group_by(PLOTID) %>% #(and PLOTID seems to be working b/c we get same of unique plots)
count(name="INVYR_n") 
#67835

#break
#break


#IMPORTANT: SEE notes when making geospatial_1
#changing the lat long from 1985 Connecticut plots that have conflicting lat or long information for a singe plot across years 
#Making the 1985 match the 1998
#These plots are 9_1_15_97 (LON differs), 9_1_5_17 (LAT differs), 9_1_5_77 (LAT differs)

#Fixing Lat and Long to be consistent (match 1985 to 198 records)
plots_fix <- c("9_1_15_97", "9_1_5_17", "9_1_5_177")
coords_1998 <- all_plot_subset_subset %>%
  filter(PLOTID %in% plots_fix, INVYR == 1998) %>%
  select(PLOTID, LAT_1998 = LAT, LON_1998 = LON)

all_plot_subset_subset <- all_plot_subset_subset %>%
  left_join(coords_1998, by = "PLOTID") %>%
  mutate(
    LAT = if_else(PLOTID %in% plots_fix, LAT_1998, LAT),
    LON = if_else(PLOTID %in% plots_fix, LON_1998, LON)
  ) %>%
  select(-LAT_1998, -LON_1998)

#done

#

#all_plot_subset_subset has the PLOT data (with pretty good confidence) of the plots (with appropriate designs) that can be used to calculate mortality rates 

#Sub-setting this to just the list w/out any data 
#(note that I will probably want to come back and grab the Lat Long stuff)

#Keeping Only columns I need and removing duplicates (IMPORANT! comment out select line if I want all columns for outputting data)
all_plot_working <- all_plot_subset_subset %>%
  select(STATECD, UNITCD, COUNTYCD, PLOT, PLOTID, LAT, LON) %>%
  distinct(PLOTID, .keep_all = TRUE)
#Shows 67835 unique plots

# doing this for my plots
unique_locals_4 = unique_locals_3 %>% 
  select(STATECD, UNITCD, COUNTYCD, PLOT, PLOTID, LAT, LON)

write.csv(unique_locals_4, file = "./Formatted.Data/unique.FIA.FAGR.locals.csv")
  

#outputting for Sam (keeping all columns for her!!!)- These are all the PLOTS within Fagus target states
#setwd("C:/Users/rschiafo/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Hemlock - Forest Health/Analysis/FIA Analysis/R Output Data")
#write.csv(all_plot_working, "FIA_PLOT_Fagus_BLD_Range.csv")

#Break
#Break
#Break
#

##############Exploring and Sub-setting TREE Data############ 
#
#
#########################################################


#Giving the all_tree dataframe the same UNIQUE PLOT IDENTIFIER hat has all State, Unit, County, Plot, Design information in one code (in that order!)
all_tree<-all_tree %>% 
  unite(col = PLOTID, 
        STATECD, UNITCD, COUNTYCD, PLOT, 
        sep= "_", remove = FALSE)


#Using the list I generated with PLOT to subset the TREE data so I am only using plots with >1 INVYR and <1 DESIGNCD
all_tree_subset<-inner_join(all_tree, all_plot_working, by= c("STATECD", "UNITCD", "COUNTYCD",  "PLOT", "PLOTID"))


#Counting number of unique plots in the TREE data 
unique_tree_plots<-all_tree_subset %>% 
  group_by(PLOTID) %>% 
  count()
#9985 observations (unique plots)

##WHY ARE THESE NUMBERS SO DIFFERENT between the "plots" I can use and those that are represented in TREES for me to use? 
#Just fewer plots represented in the TREE dataset?
#Likely b/c there are no Beech in the plots I lost....
##Come back to this and make a Beech P/A with all plots in TREE for all states... and do some spot checking where I check plots I lost and see if the do or don't have hemlocks


#Getting to the INDIVIDUAL tree level


#Giving Individual Trees a Unique Identifier
#THis is the PlotID+ Subplot and Tree No.
all_tree_subset<-all_tree_subset %>% 
  unite(col = TREEID, 
        STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE,   
        sep= "_", remove = FALSE)



#Keeping only important columns- see HWA code for details on the columns I selected 
all_tree_subset_subset<-all_tree_subset %>% 
  select(CN, PLT_CN, PREV_TRE_CN, 
         INVYR, PLOTID, TREEID, STATECD, UNITCD, COUNTYCD, PLOT, SUBP, TREE, SPCD, 
         CONDID, PREVCOND, STATUSCD, AGENTCD, MORTYR, MORTCD, PREV_STATUS_CD,
         DIA, DIACHECK, HT, ACTUALHT, CR, CCLCD, CPOSCD, CVIGORCD, CDENCD, CDIEBKCD,
         DAMAGE_AGENT_CD1, DAMAGE_AGENT_CD2, DAMAGE_AGENT_CD3, 
         BHAGE, TOTAGE, 
         CYCLE, SUBCYCLE)

#Break
#Break

#Counting number of unique trees per plot
unique_tree_counts<-all_tree_subset_subset %>% 
  group_by(PLOTID) %>% 
  summarize(unique_trees = n_distinct(TREEID)) %>%
  ungroup()
#The max number of unique trees is 127. There are many plots (2285) with just 1 tree 

#For each unique tree, how many years of data is there? 
unique_tree_count_years<-all_tree_subset_subset %>% 
  group_by(TREEID) %>% 
  summarize(unique_years = n_distinct(INVYR)) %>%
  ungroup()

#miniumum is 1 year and maximum is 6 years


#Picking a tree with 6 years of data and looking at some patterns in the data
#test_tree<-all_tree_subset_subset %>% filter(TREEID=="13_5_123_40_2_4")


#IMPORTANT!!! REMOVING all TREES with STATUS 0 and 3 b/c they are not usable (How many observations does this remove?)
all_tree_subset_subset_notusable<-all_tree_subset_subset %>% filter(STATUSCD==0| STATUSCD==3) #I lose 7389 observations 
all_tree_subset_subset<-all_tree_subset_subset %>% filter(STATUSCD==1| STATUSCD==2)  #Still have 196511 observations! Plenty
#BUT how many plots do I lose?
unique_tree_plots_subset<-all_tree_subset_subset %>% 
  group_by(PLOTID) %>% 
  count()
# I now have 9957 Plots compared to the previous 9985 Plots before removing 0 and 3 status Trees-
#Fine


all_tree_subset_subset
#use this later


#############BLD Infection years###############################################
#
##############################################################################

#Reading in BLD infection year data 
bld_infect_years<-read.csv("Formatted.Data/BLD.counties.csv")


#Data prep to merge BLD infection year data with TREE data

#bld infection year bins from GIS- rename and select columns
bld_infect_years<-bld_infect_years %>%
  rename(STATECD= STATEFP, COUNTYCD= COUNTYFP, COUNTY= NAMELSAD, INFECTION_YEAR= BLD.Year) %>% 
  select(STATECD, COUNTYCD, NAME, COUNTY, INFECTION_YEAR, State, GEOID) #keeping only necessary columns 

#Making a numeric FIPS column (should match GEOID)
bld_infect_years$FIPS <- as.numeric(paste0(
  bld_infect_years$STATECD,
  sprintf("%03d", bld_infect_years$COUNTYCD)
))  



#Turning NAs into "Uninfected" 
bld_infect_years <- bld_infect_years %>%
  mutate(INFECTION_YEAR = as.character(INFECTION_YEAR),
    INFECTION_YEAR = replace_na(INFECTION_YEAR, "Uninfected"))

#FIPs - rename and select columns and make FIPS column with State and countyCD combined (for merging with bld_infect_years)
fips_working<- fips_master %>% 
  rename(STATECD= STATEFP, COUNTYCD= COUNTYFP, COUNTY= NAMELSAD) %>% 
  mutate(FIPS = paste(STATECD, COUNTYCD, sep = "")) %>% 
  relocate(FIPS, .before = STATECD)


fips_working$STATECD<-as.numeric(fips_working$STATECD)
fips_working$COUNTYCD<-as.numeric(fips_working$COUNTYCD)
fips_working$FIPS<-as.numeric(fips_working$FIPS)


#merging bld infection years with FIPS and NAME
bld_infect_years_full <- left_join(fips_working, bld_infect_years, by = c("FIPS", "STATECD", "COUNTYCD", "NAME", "COUNTY"))
#This gives me all the counties in the states- even if there in not data in the bld infection year (uninfected)


#keeping only states I need 
#Maine, Vermont, New Hampshire, Massachusetts, Connecticut, Rhode Island, New York, Pennsylvania, New Jersey, Delaware, Maryland, Virginia, West Virgina, North carolina, Ohio, Michigan
bld_infect_years_full<-bld_infect_years_full %>% filter(
                                                          STATECD==  9 | #Connecticut 
                                                          STATECD==  10 | #Delaware
                                                          STATECD== 23 | #Maine
                                                          STATECD== 24 | #Maryland
                                                          STATECD== 25 | #Mass
                                                          STATECD== 26 | #Michigan
                                                          STATECD== 33 | #New Hampshire
                                                          STATECD==  34 | #New Jersey
                                                          STATECD== 36 | #New York
                                                          STATECD== 37 | #North Carolina 
                                                          STATECD== 39 | #ohio
                                                          STATECD== 42 | #Penn
                                                          STATECD== 44 | #Rhode island
                                                          STATECD== 50 | #Vermont
                                                          STATECD== 51 | #Virginia
                                                          STATECD==  54) #west viginia

#Manually adding in infection years for CT counties
bld_infect_years_full<- bld_infect_years_full %>%   mutate(INFECTION_YEAR = case_when(
  FIPS == "9001" ~ "2019",
  FIPS == "9003" ~ "2021",
  FIPS == "9005" ~ "2020",
  FIPS == "9007" ~ "2020",
  FIPS == "9009" ~ "2020",
  FIPS == "9011" ~ "2020",
  FIPS == "9013" ~ "2021",
  FIPS == "9015" ~ "2020",
  TRUE ~ INFECTION_YEAR
  ))

#Filling in NAs as "uninfected" for the remaining North Carolina counties
bld_infect_years_full <- bld_infect_years_full %>%
  mutate(INFECTION_YEAR = as.character(INFECTION_YEAR),
         INFECTION_YEAR = replace_na(INFECTION_YEAR, "Uninfected"))

# adding in state and GEOID info for North Carolinea
bld_infect_years_full <- bld_infect_years_full %>%
  mutate(State = case_when(
    STATECD == 37 ~ "North Carolina",
    TRUE ~ State # Keeps original value if STATECD is not 37
  ))

#Adding in GEOID info for North Carolina
bld_infect_years_full <- bld_infect_years_full %>%
  mutate(GEOID = if_else(is.na(GEOID), FIPS, GEOID))

bld_infect_years_final<-bld_infect_years_full

#done

#Break
#Break
#Break

#merging infection year data with TREE data
all_tree_working<-merge(all_tree_subset_subset, bld_infect_years_final, by= c("STATECD", "COUNTYCD"), all = FALSE)
all_tree_working <- all_tree_working %>%
  relocate(INFECTION_YEAR, .after = INVYR)
#THIS SHOULD BE MY MAIN DATAFRAME TO CALCULATE MORTALITY RATE STUFF

#OUTPUTTING FOR SAM- The TREE data for all target states/ plots
setwd("C:/Users/rschiafo/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Hemlock - Forest Health/Analysis/FIA Analysis/R Output Data")
#write.csv(all_tree_working, "FIA_TREE_Fagus_BLD_Range.csv")



#How many unique trees in all_tree_working?
all_tree_working %>% 
  ungroup() %>%
  summarize(unique_tree = n_distinct(TREEID))
#76484

#This code tells me whether Infection year is usable (ie has a date and isnt No Data or Uninfected)
#Also gives T/F for whether it has Pre and Post Infection Year Data

#Doing this with PLOTID
#How many unique plots in all_plots_working?
all_tree_working %>% 
  ungroup() %>%
  summarize(unique_plot = n_distinct(PLOTID))
#9957

#Making a dataframe that lists only unique plots that have beech trees- For sam
all_tree_working_unique_plots<- all_tree_working %>% 
  distinct(PLOTID, .keep_all = TRUE) %>% 
  select( "PLOTID", "STATECD", "UNITCD", "COUNTYCD", "PLOT","INFECTION_YEAR", "State",  "FIPS", "NAME", "COUNTY", "GEOID")

#adding in lat/long (not as geometry b/c then you cant export)
all_plots_location<-all_plot_working %>% 
  select("PLOTID", "LAT", "LON")


#left join to keep only plots in Tree data
all_tree_working_unique_plots_final<-left_join(all_tree_working_unique_plots, all_plots_location, by = "PLOTID")


#OUTPUTTING FOR SAM- The Plots that have at least 1 beech tree across target states
setwd("C:/Users/rschiafo/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - Hemlock - Forest Health/Analysis/FIA Analysis/R Output Data")
#write.csv(all_tree_working_unique_plots_final, "FIA_TREE_PLOTS_Fagus_BLD_Range.csv")


#This code tells me whether Infection year is usable (ie has a date and isnt No Data or Uninfected)
#Also gives T/F for whether it has Pre and Post Infection Year Data
plots_classification <- all_tree_working %>%
  mutate(
    valid_infection = !INFECTION_YEAR %in% c("no data", "Uninfected"),
    valid_invyr     = !INVYR %in% c("no data", "Uninfected")
  ) %>%
  filter(valid_invyr) %>%
  mutate(
    pre  = valid_infection & INVYR <= INFECTION_YEAR,
    post = valid_infection & INVYR >  INFECTION_YEAR
  ) %>%
  group_by(PLOTID) %>%
  summarise(
    INFECTION_YEAR = first(INFECTION_YEAR),
    has_pre  = any(pre),
    has_post = any(post),
    .groups = "drop"
  )


#plots with Pre and Post
plots_pre_post <- plots_classification %>%
  filter(has_pre & has_post) %>% 
  select(PLOTID)

#plots with Pre or Post, but not both
plots_one_side_only <- plots_classification %>%
  filter(
    !INFECTION_YEAR %in% c("no data", "Uninfected") &
      xor(has_pre, has_post))%>% 
  select(PLOTID)

#plots with Uninfected 
plots_uninfected <- plots_classification %>%
  filter(INFECTION_YEAR %in% c("Uninfected")) %>% 
  select(PLOTID)

#plots with no infection year data
plots_noinfectiondata <- plots_classification %>%
  filter(INFECTION_YEAR %in% c("no data")) %>% 
  select(PLOTID)




#Checking- I should have 9957 unique plots total
#There are:
#0 plots with no infection data  
#4692 plots in Uninfected counties
#4125 plots that DO NOT have Pre and Post infection data (one sided)
#1140 plots that DO HAVE Pre and Post infection data 
#TOtal= 9264


#plots with Pre and Post OR One-sided but removing uninfected, and no data for infection year 
plots_infectiondata <- plots_classification %>%
  filter(
    !INFECTION_YEAR %in% c("no data", "Uninfected")) %>% 
  select(PLOTID)



#END

#Break
#Break
#Break

#Keeping only PLOTS that have pre and post infection year data
#SUb-setting all_tree_working by plots_pre_post
all_tree_working_pre_post<-inner_join(all_tree_working, plots_pre_post, by=c("PLOTID"))

#An aside to use later....
#also Keeping only PLOTS that do not have pre/post infection year data but ARE INFECTED
#Subs-setting all_tree_working by plots_one_side_only
all_tree_working_one_side_only<-inner_join(all_tree_working, plots_one_side_only, by=c("PLOTID"))
#also Keeping only PLOTS that are UNIFECTED 
#Subs-setting all_tree_working by plots_uninfected
all_tree_working_uninfected<-inner_join(all_tree_working, plots_uninfected, by=c("PLOTID"))
#also Keeping only PLOTS that have no infection data
#Subs-setting all_tree_working by plots_noinfecitondata
all_tree_working_noinfectiondata<-inner_join(all_tree_working, plots_noinfectiondata, by=c("PLOTID"))
#all_tree_working_BLANK- these four dataframes add up to all_tree_working observations (186397)

#These are all the trees with infection year data (pre/post + one sided)
all_tree_working_infectiondata <- inner_join(all_tree_working, plots_infectiondata, by=c("PLOTID")) #adds up to 186397 with no infection and no data

#Break
#Break
#BREAK




