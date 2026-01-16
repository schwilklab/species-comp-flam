#!/usr/bin/Rscript --vanilla

# Analysis

library(emmeans)

###################################################################################
# First probability of ignition
###################################################################################

ig_mod <- glm(ignite_others ~ drought_condition * species_combination, data = filter(alldata, status == "ignitee"),
  family = binomial(link = "logit"))
car::Anova(ig_mod, type = "III", test.statistic = "LR")
emmeans(ig_mod, ~ species_combination | drought_condition, type = "response")

####################################################################################
# Now heat release
####################################################################################

anova_fit_heat_release <- aov(heat_release_j ~ drought_condition * species_combination, 
                              data = filter(alldata, status == "ignitee"))
summary(anova_fit_heat_release)
car::Anova(anova_fit_heat_release, type = "III", test.statistic = "F")
emmeans(anova_fit_heat_release, pairwise ~ drought_condition | species_combination, adjust = "tukey")


############################################################################
# Cleasning environments
############################################################################
rm(anova_fit_heat_release, ig_mod)


