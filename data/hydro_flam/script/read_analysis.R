# Reading and cleaning and analysis

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)


# Xiulin Gao used 0.921 J/g as the Specific heat of the aluminum alloy of which
# our discs are made. From the grass experiments we had 52.91g and 53.21g which
# are the weight of Disc one and Disc two respectively(Gao and Schwilk, 2021).
# The gas flow from the Blue Rhino gas cylinder was 20.35 gram per minute.

SPECIFIC_HEAT_AL <- 0.921 # in J/g
MASS_DISK_1 <- 52.91  # 
MASS_DISK_2 <- 53.21 # g

##################################################################################
# Reading scripts
##################################################################################

samples <- read_csv("./data/hydro_flam/samples.csv")
lfmc_wp <- read_csv("./data/hydro_flam/lfmc_wp.csv") 
burn_trials <-  read_csv("./data/hydro_flam/burn_trials.csv")

##################################################################################
# Cleaning scripts
##################################################################################

burn_trials <- burn_trials %>%
  mutate(heat1 = (max_temp - d1_pre) * MASS_DISK_1 * SPECIFIC_HEAT_AL,
         heat2 = (max_temp - d2_pre) * MASS_DISK_2 * SPECIFIC_HEAT_AL,
         heat_release_j = (heat1 + heat2)/2, # average heat release of two disks
         pre_burning_temp = (d1_pre + d2_pre)/2)

burn_trials$heat_release_j <- burn_trials$heat_release_j - min(burn_trials$heat_release_j, na.rm=TRUE)

###############################################################################
## Combining all
###############################################################################

alldata_hydro_flam <- samples %>%
  left_join(lfmc_wp) %>%
  left_join(burn_trials) %>%
  mutate(trial = ifelse(trial == "nb", "Not Hydraulically Connected", "Hydraulically Connected")) %>%
  select(species, trial, wp, ignition_delay, heat_release_j, status) %>%
  na.omit()
  

###############################################################################
## Plots, exposed first
###############################################################################

ig_exposed <- ggplot(filter(alldata_hydro_flam, status != "non_exposed"), aes(-1*wp, ignition_delay, color = trial)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 2) +
  facet_wrap(~species) +
  labs(x = "Water potential (MPa)",
       y = "Time to ignition (s)",
       color = "") + 
  scale_color_manual(values = c(
      "Hydraulically Connected" = "blue",
      "Not Hydraulically Connected" = "red")) +
  theme_bw() +
  theme(strip.text = element_text(face = "italic", size = 12),
        legend.position = c(0.80, 0.85),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size =12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))

ggsave("./results/hydro_flam/ig_exposed.pdf", plot = ig_exposed, 
       height = 160, width = 180, units = "mm", dpi = 300) 

heat_exposed <- ggplot(filter(alldata_hydro_flam, status != "non_exposed"), aes(-1*wp, heat_release_j, color = trial)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 2) +
  facet_wrap(~species) +
  labs(x = "Water potential (MPa)",
       y = "Heat release (J)",
       color = "") + 
  scale_color_manual(values = c(
    "Hydraulically Connected" = "blue",
    "Not Hydraulically Connected" = "red")) +
  theme_bw() +
  theme(strip.text = element_text(face = "italic", size = 12),
        legend.position = c(0.80, 0.85),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size =12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))

ggsave("./results/hydro_flam/heat_exposed.pdf", plot = heat_exposed, 
       height = 160, width = 180, units = "mm", dpi = 300) 

#############################################################################################
# Non-Exposed
#############################################################################################

ig_non_exposed <- ggplot(filter(alldata_hydro_flam, status == "non_exposed"), aes(trial, ignition_delay, color = trial)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2, alpha = 0.7) +
  facet_wrap(~species) +
  labs(x = "Hydraulic connection status (fully hydrated)",
       y = "Ignition delay time (s)",
       color = "") + 
  scale_color_manual(values = c(
    "Hydraulically Connected" = "blue",
    "Not Hydraulically Connected" = "red")) +
  theme_bw() +
  theme(strip.text = element_text(face = "italic", size = 12),
        legend.position = c(0.80, 0.85),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size =12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))

ggsave("./results/hydro_flam/ig_non_exposed.pdf", plot = ig_non_exposed, 
       height = 160, width = 180, units = "mm", dpi = 300) 

heat_non_exposed <- ggplot(filter(alldata_hydro_flam, status == "non_exposed"), aes(trial, heat_release_j, color = trial)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2, alpha = 0.7) +
  facet_wrap(~species) +
  labs(x = "Hydraulic connection status (fully hydrated)",
       y = "Heat release (J)",
       color = "") + 
  scale_color_manual(values = c(
    "Hydraulically Connected" = "blue",
    "Not Hydraulically Connected" = "red")) +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "italic"),
        legend.position = c(0.80, 0.85),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size =12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))

ggsave("./results/hydro_flam/heat_non_exposed.pdf", plot = heat_non_exposed, 
       height = 160, width = 180, units = "mm", dpi = 300) 

