#!/usr/bin/Rscript --vanilla

source("./scripts/ggplot_theme.R")

#############################################################################
# Figures, first ignition proability
#############################################################################

figure2 <- ggplot(filter(alldata, status == "ignitee"),
       aes(x = drought_condition, y = ignite_others,
           color = species_combination, group = species_combination)) +
  geom_jitter(height = 0.05, width = 0.1, alpha = 0.5) +
  geom_smooth(
    method = "glm",
    method.args = list(family = binomial(link = "logit")),
    se = FALSE) +
  facet_wrap(~ species_combination) +
  labs(y = "Probability of igniting neighboring fuel",
    x = "Drought condition",
    color = "Species combination") +
  scale_color_manual(values = c("#67000D",  "#CB181D", "#FDAE6B","#EF6548")) +
  pubtheme +
  theme(strip.text = element_text(face = "italic", size = 10),
        legend.text = element_text(face = "italic"),
        legend.position = c(0.85, 0.7),
        axis.text.x = element_text(hjust = 0.5, vjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank())

ggsave("./results/figure2.pdf", plot = figure2, 
       height = 160, width = 180, units = "mm", dpi = 300) 

#############################################################################
# Heat Release
##############################################################################
  
figure3 <- ggplot(filter(alldata, status == "ignitee" & self_ignition != 1), 
                       aes(x = drought_condition, y = heat_release_j,
                           fill = species_combination)) +
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.6, outlier.shape = NA) +
  geom_jitter(aes(color = species_combination),
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9),
              size = 1.5, alpha = 0.5) +
  labs(x = "Drought condition", y = "Heat release (J)") +
  scale_fill_manual(values = c("#67000D","#CB181D", "#FDAE6B", "#EF6548")) +
  scale_color_manual(values = c("#67000D","#CB181D","#FDAE6B", "#EF6548")) +
  pubtheme +
  theme(legend.text = element_text(face = "italic"),
        legend.position = c(0.5, 0.9),
        legend.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5, vjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave("./results/figure3.pdf", plot = figure3, 
       height = 160, width = 180, units = "mm", dpi = 300) 

#############################################################################
# Ignition delay
##############################################################################

ign_delay <- ggplot(filter(alldata, status == "ignitee"),
       aes(x = drought_condition, y = ignition_delay,
           color = species_combination, group = species_combination)) +
  geom_jitter(height = 0.05, width = 0.1, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ species_combination) +
  labs(y = "Ignition delay (s)",
    x = "Drought condition",
    color = "Species combination") +
  scale_color_manual(values = c("#67000D",  "#CB181D", "#FDAE6B","#EF6548")) +
  pubtheme +
  theme(strip.text = element_text(face = "italic", size = 10),
        legend.text = element_text(face = "italic"),
        legend.position = c(0.85, 0.9),
        legend.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5, vjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave("./results/ign_delay.pdf", plot = ign_delay, 
       height = 160, width = 180, units = "mm", dpi = 300) 

############################################################################
# Cleasning environments
############################################################################

rm(figure2, figure3, ign_delay)

