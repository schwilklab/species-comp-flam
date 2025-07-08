# read-data.R
# Dylan Schwilk

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

###############################################################################
# Reading and cleaning the datasets
###############################################################################

###############################################################################
# Constants
###############################################################################

# Xiulin Gao used 0.921 J/g as the Specific heat of the aluminum alloy of which
# our discs are made. From the grass experiments we had 52.91g and 53.21g which
# are the weight of Disc one and Disc two respectively(Gao and Schwilk, 2021).
# The gas flow from the Blue Rhino gas cylinder was 20.35 gram per minute.

SPECIFIC_HEAT_AL <- 0.921 # in J/g
MASS_DISK_1 <- 52.91  # 
MASS_DISK_2 <- 53.21 # g


###############################################################################
## Read all the data files
###############################################################################

samples <-  read_csv("./data/samples.csv") 

water_potentials_fmc <- read_csv("./data/lfmc_wp.csv") 

burn_trials <- read_csv("./data/burn_trials.csv")

drydown <- read_csv("./data/dry_down.csv")

###############################################################################
## Cleaning the data
##############################################################################

wp_fmc <- water_potentials_fmc %>%
  mutate(wp = -1*wp) %>%
  mutate(fmc = ((fresh_mass - dry_mass)/dry_mass)*100) %>%
  mutate(fmc = round(fmc, 2)) %>%
  select(- fresh_mass, - dry_mass)

burn_trials <- burn_trials %>%
  mutate(heat1 = (max_temp - disc1_pre) * MASS_DISK_1 * SPECIFIC_HEAT_AL,
         heat2 = (max_temp - disc2_pre) * MASS_DISK_2 * SPECIFIC_HEAT_AL,
         heat_release_j = (heat1 + heat2)/2, # average heat release of two disks
         pre_burning_temp = (disc1_pre + disc2_pre)/2)

# Correct heat release to set lowest value at 0 (all relative anyway)
burn_trials$heat_release_j <- burn_trials$heat_release_j - min(burn_trials$heat_release_j, na.rm=TRUE)

burn_trials <- burn_trials %>%
  rename(sample1_volume_burn = vol_burned_sample1,
    sample2_volume_burn = vol_burned_sample2) %>%
  pivot_longer(cols = starts_with("sample"),
               names_to = c("sample_number", ".value"),
               names_pattern = "sample(\\d+)_(.*)") %>%
  rename(sample_id = id) %>%
  rename(status = sample_number) %>%
  mutate(status = ifelse(status == 1, "igniter", "ignitee"))

###################################################################################
# Dry down
###################################################################################

time_wp <- drydown %>%
  mutate(date_time = mdy_hms(paste(drydown$date, drydown$time))) %>%
  group_by(sample_id) %>%
  mutate(hours = as.numeric(difftime(date_time, first(date_time), units = "hours")),
         hours = round(hours, 2)) 
  
################################################################################
# Alldata
################################################################################

alldata <- samples %>%
  left_join(wp_fmc) %>%
  left_join(burn_trials) %>%
  filter(! sample_id %in%  c("DCK08", "DCK13", "DCK23",
                        "DCK30", "DCK32", "DCK63",
                        "DCK64", "DCK65", "DCK70")) %>%
  mutate(ignite_others = ifelse(ignite_others == "yes", 1, 0))




