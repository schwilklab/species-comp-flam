#!/usr/bin/Rscript --vanilla

source("./scripts/ggplot_theme.R")

library(patchwork)

############################################################################
# Only figure 2 and Figure 3 is for the manuscript, rest of them are
# for supplementary
#############################################################################
# Probability of igniting neighbors
#############################################################################


figure2 <- ggplot(filter(alldata, status == "ignitee"),
                  aes(x = drought_condition, y = ignite_others, group = species_combination)) +
  geom_jitter(height = 0.05, width = 0.1, alpha = 0.7, size = 2) +
  facet_wrap(~ species_combination) +
  labs(y = "Ignition of neighboring fuel",
       x = "Drought treatments",
       color = "Species combination") +
  scale_y_continuous(breaks = c(0,1),
                     labels = c("No","Yes"),
                     limits = c(-0.05,1.05)) +
  pubtheme +
  theme(strip.text = element_text(face = "italic", size = 10),
        legend.text = element_text(face = "italic"),
        legend.position = c(0.85, 0.7),
        axis.text.x = element_text(hjust = 0.5, vjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank())

ggsave("./results/fig2.pdf", plot = figure2, 
       height = 160, width = 180, units = "mm", dpi = 300) 

#############################################################################
# Heat Release
##############################################################################

figure3 <- ggplot(filter(alldata, status == "ignitee" & self_ignition != 1), 
                  aes(x = drought_condition, y = heat_release_j)) +
  geom_boxplot(color = "black",
               fill = "white",
               alpha = 0.6, outlier.shape = NA) +
  geom_jitter(color = "black",
              size = 2, alpha = 0.5) +
  facet_wrap(~ species_combination) +
  labs(x = "Drought treatments", y = "Heat release (J)") +
  pubtheme +
  theme(strip.text = element_text(face = "italic", size = 10),
        axis.text.x = element_text(hjust = 0.5, vjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave("./results/fig3.pdf", plot = figure3, 
       height = 160, width = 180, units = "mm", dpi = 300) 

#############################################################################
# Supplementary Figure 1
##############################################################################

#############################################################################
# Figure 1, the data generating figure 1 is from both 2023 and 2024 
#############################################################################
#######################################################################################
# Now waterpotential vs ignition delay plot
######################################################################################

wp_ig <- ggplot(data_for_fig1, aes(wp, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay time (s)") +
  labs(tag = "(a)") +
  pubtheme +
  theme(legend.text = element_text(face = "italic"),
        legend.position = c(0.25, 0.85),
        legend.title = element_blank(),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.02, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 


cmc_ig <- ggplot(data_for_fig1, aes(cmc, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("LFMC (%)") +
  ylab("") +
  labs(tag = "(b)") +
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.02, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 


combined_cmc_ig <- wp_ig | cmc_ig

#######################################################################################
# Now water status vs heat release plot
######################################################################################

wp_heat_release <- ggplot(data_for_fig1, aes(wp, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("") +
  ylab("Heat release (kJ)") +
  ylim(0, NA) +
  labs(tag = "(c)") +
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.02, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))

cmc_heat_release <- ggplot(data_for_fig1, aes(cmc, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("") +
  ylab("") +
  labs(tag = "(d)") +
  ylim(0, NA) +
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.02, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))

combined_water_status_flam <-   (wp_heat_release | cmc_heat_release) / (combined_cmc_ig)

ggsave("./results/figure1.pdf", plot = combined_water_status_flam,
       height = 7.25, width = 7.25, units = "in", dpi = 300)


supp_figure1 <- ggplot(dry_down, aes(hours, -1*wp, color = species)) +
  geom_point(size=3, alpha = 0.5, shape = 16) +
  geom_smooth(method="lm", se = FALSE, size = 1.5) +
  xlab("Time (hr)") +
  ylab("Water potential (MPa)") +
  pubtheme +
  theme(legend.position = c(0.85, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(face = "italic"),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 

ggsave("./results/supp_figure1.pdf", plot = supp_figure1, 
       height = 160, width = 180, units = "mm", dpi = 300) 

#############################################################################
#  Supplementary Figure 1
##############################################################################

supp_figure2 <- ggplot(filter(alldata, status == "ignitee"),
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

ggsave("./results/supp_figure2.pdf", plot = supp_figure2, 
       height = 160, width = 180, units = "mm", dpi = 300) 

############################################################################
# Tables, first ignition probability
############################################################################

print(xtable::xtable(ig_mod_anova), type = "html", file = "./results/ig_mod_anova.html")

ig_mod_emm_data_frame <- as.data.frame(ig_mod_emm)

class(ig_mod_emm_data_frame) <- "data.frame"

row.names(ig_mod_emm_data_frame) <- NULL

print(xtable::xtable(ig_mod_emm_data_frame, digits = c(0, 0, 0, 2, 2, 0, 2, 2)),
  type = "html", file = "./results/emmeans_ig_mod_table.html", include.rownames = FALSE)

############################################################################
# Now heat release
############################################################################

print(xtable::xtable(sum_data), type = "html",
      file = "./results/summarised_wp.html")

print(xtable::xtable(heat_release_anova), type = "html", 
      file = "./results/heat_release_mod_anova.html")

heat_release_mod_emm_data_frame <- as.data.frame(heat_release_emm)

class(heat_release_mod_emm_data_frame) <- "data.frame"

row.names(heat_release_mod_emm_data_frame) <- NULL

print(xtable::xtable(heat_release_mod_emm_data_frame, digits = c(0, 0, 0, 2, 2, 0, 2, 2, 2)),
      type = "html", file = "./results/emmeans_heat_release_mod_table.html", 
      include.rownames = FALSE)

############################################################################
# The next part if for the figures for presentations
############################################################################

#######################################################################################
# Now waterpotential vs ignition delay plot
######################################################################################

wp_ig1 <- ggplot(data_for_fig1, aes(wp, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay time (s)") +
  prestheme +
  theme(legend.text = element_text(face = "italic"),
        legend.position = c(0.35, 0.85),
        legend.title = element_blank(),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.02, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 


cmc_ig1 <- ggplot(data_for_fig1, aes(cmc, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("LFMC (%)") +
  ylab("") +
  prestheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.02, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 


combined_cmc_ig1 <- wp_ig1 | cmc_ig1

#######################################################################################
# Now water status vs heat release plot
######################################################################################

wp_heat_release1 <- ggplot(data_for_fig1, aes(wp, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("") +
  ylab("Heat release (kJ)") +
  ylim(0, NA) +
  prestheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.02, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))

cmc_heat_release1 <- ggplot(data_for_fig1, aes(cmc, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("") +
  ylab("") +
  ylim(0, NA) +
  prestheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.02, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))

combined_water_status_flam1 <-   (wp_heat_release1 | cmc_heat_release1) / (combined_cmc_ig1)

ggsave("./results/figure1_pres.pdf", plot = combined_water_status_flam1,
       height = 7.25, width = 7.25, units = "in", dpi = 300)

figure31 <- ggplot(filter(alldata, status == "ignitee"),
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
  prestheme +
  theme(strip.text = element_text(face = "italic", size = 10),
        legend.text = element_text(face = "italic"),
        legend.position = c(0.75, 0.7),
        axis.text.x = element_text(hjust = 1, vjust = 1, angle = 45, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank())

ggsave("./results/figure31.pdf", plot = figure31, 
       height = 160, width = 180, units = "mm", dpi = 300) 

#############################################################################
# Heat Release
##############################################################################

figure41 <- ggplot(filter(alldata, status == "ignitee" & self_ignition != 1), 
                   aes(x = drought_condition, y = heat_release_j,
                       fill = species_combination)) +
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.6, outlier.shape = NA) +
  geom_jitter(aes(color = species_combination),
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9),
              size = 1.5, alpha = 0.5) +
  labs(x = "Drought condition", y = "Heat release (J)") +
  scale_fill_manual(values = c("#67000D","#CB181D", "#FDAE6B", "#EF6548")) +
  scale_color_manual(values = c("#67000D","#CB181D","#FDAE6B", "#EF6548")) +
  prestheme +
  theme(legend.text = element_text(face = "italic", size = 12),
        legend.position = c(0.5, 0.9),
        legend.title = element_blank(),
        axis.text.x = element_text(hjust = 0.6, vjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave("./results/figure41.pdf", plot = figure41, 
       height = 160, width = 180, units = "mm", dpi = 300) 


############################################################################
# Cleasning environments
############################################################################

rm(data_for_fig1, cmc_ig, cmc_heat_release, wp_ig, wp_heat_release,
   combined_cmc_ig, combined_water_status_flam, figure3, dry_down, 
   supp_figure1, supp_figure2, sum_data, flam_pca, heat_release_anova,
   heat_release_mod_emm_data_frame, heat_release_emm, ig_mod_anova, 
   ig_mod_emm_data_frame, ig_mod_emm)

