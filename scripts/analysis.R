#!/usr/bin/Rscript --vanilla

# Analysis

library(emmeans)

###################################################################################
# First probability of ignition
###################################################################################

ig_mod <- glm(ignite_others ~ drought_condition * species_combination, data = filter(alldata, status == "ignitee"),
  family = binomial(link = "logit"))
ig_mod_anova <- car::Anova(ig_mod, type = "III", test.statistic = "LR")
ig_mod_emm <- emmeans(ig_mod, ~ species_combination | drought_condition, type = "response")

####################################################################################
# Now heat release
####################################################################################

anova_fit_heat_release <- aov(heat_release_j ~ drought_condition * species_combination, 
                              data = filter(alldata, status == "ignitee"))
summary(anova_fit_heat_release)
heat_release_anova <- car::Anova(anova_fit_heat_release, type = "III", test.statistic = "F")
heat_release_emm <- emmeans(anova_fit_heat_release, pairwise ~ drought_condition | species_combination, adjust = "tukey")

############################################################################
# Cleasning environments
############################################################################
rm(anova_fit_heat_release, ig_mod)


