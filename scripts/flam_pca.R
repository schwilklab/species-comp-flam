#!/usr/bin/Rscript --vanilla
# PCA analysis

###############################################################################
# PCA analysis. Merging the hobo data with burn trails by label data set to do
# the PCA.
###############################################################################

pca_data <-  burn_trials %>%
  filter(self_ignition != 1) %>%
  mutate(vol_burned = (vol_burned_sample1 + vol_burned_sample2)/2) %>%
  left_join(hobos_wider) %>%
  dplyr::select(sample1_id, sample2_id,
         vol_burned, flame_height, heat_release_j, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay)

dim(pca_data)
any(is.na(pca_data)) 

###############################################################################
# PCA by prcomp, correlation matrix since I am using scale is TRUE
###############################################################################

flam_pca <- prcomp(pca_data[,-(1:2)], 
                   scale=TRUE)

summary(flam_pca) 
flam_loadings <- flam_pca$rotation[ ,(1:2)] 
flam_loadings
#biplot(flam_pca)

###############################################################################
# Assigning PC1 to pca_data_2022 and then merging with alldata_2022
# data set for doing rest of the analysis.
###############################################################################

pca_data$PC1 <- flam_pca$x[ ,1]
pca_data$PC2 <- flam_pca$x[ ,2]

pca_data <- pca_data %>%
  pivot_longer(cols = starts_with("sample"),
               names_to = c("sample_number", ".value"),
               names_pattern = "sample(\\d+)_(.*)") %>%
  rename(sample_id = id) %>%
  rename(status = sample_number) %>%
  mutate(status = ifelse(status == 1, "igniter", "ignitee"))

############################################################################
# Cleasning environments
############################################################################
rm(flam_loadings, burn_trials, hobos_wider, pca_data)



