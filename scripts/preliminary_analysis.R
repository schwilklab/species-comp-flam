
source("./scripts/ggplot_theme.R")

library(ggplot2)

desired_combinations <- c("10-10","14-14", "48-48","72-72", "2-2", "6-6", "8-8", "30-30", "10-2", "14-6", "48-8", "72-30",
                          "2-10", "6-14", "8-48","30-72")
drought_labels <- c(
  "2-2"   = "Low",
  "10-10" = "Low",
  "2-10"  = "Low",
  "10-2"  = "Low",
  
  "6-6"   = "Moderate",
  "14-14" = "Moderate",
  "6-14"  = "Moderate",
  "14-6"  = "Moderate",
  
  "8-8"   = "High",
  "48-48" = "High",
  "8-48"  = "High",
  "48-8"  = "High",
  
  "30-30" = "Extreme",
  "72-72" = "Extreme",
  "30-72" = "Extreme",
  "72-30" = "Extreme"
)

alldata$drought_condition <- drought_labels[as.character(alldata$hours_combination)]

alldata$drought_condition <- factor(
  alldata$drought_condition,
  levels = c("Low", "Moderate", "High", "Extreme"))

sum_alldata <- alldata %>%
  filter(!is.na(ignite_others)) %>% 
  filter(status == "ignitee") %>%
  group_by(species_combination, drought_condition) %>%
  summarise(
    ignited = sum(ignite_others == 1),
    not_ignited = sum(ignite_others == 0),
    .groups = "drop"
  ) %>%
  mutate(total = ignited + not_ignited) %>%
  pivot_longer(
    cols = c(ignited, not_ignited),
    names_to = "status",
    values_to = "count") %>%
  mutate(percentage = 100 * count / total)   # convert to percent


fig1 <- ggplot(sum_alldata, aes(x = drought_condition,
                     y = percentage,
                     fill = status)) +
  geom_col(position = "stack") +
  facet_wrap(~ species_combination, scales = "free_x") +
  labs(x = "Drought condition",
       y = "Percentage of samples",
       fill = "Ignition status") +
  prestheme +
  theme(strip.text = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave("./results/sp_dr_ig.png", plot = fig1, 
       height = 160, width = 180, units = "mm", dpi = 300) 

fig2 <- ggplot(filter(alldata, status == "ignitee" & self_ignition != 1), aes(x = drought_condition, y = heat_release_j, fill = species_combination)) +
  geom_violin(position = position_dodge(width = 0.9), alpha = 0.6) +
  geom_jitter(aes(color = species_combination),
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9),
              size = 1.5, alpha = 0.7) +
  labs(x = "Drought condition", y = "Heat release (J)") +
  prestheme +
  theme(strip.text = element_text(face = "italic"),
        axis.text.x = element_text(hjust = 1, vjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave("./results/sp_dr_heat.png", plot = fig2, 
       height = 160, width = 180, units = "mm", dpi = 300) 

fig3 <- ggplot(time_wp, aes(hours, -1*(wp), color = species)) +
  geom_point(size=3, alpha = 0.5, shape = 16) +
  geom_smooth(method="lm", se = FALSE, size = 1.5) +
  xlab("Time (hr)") +
  ylab("Water potential (MPa)") +
  labs(color = "") +
  pubtheme +
  theme(legend.position = c(0.75, 0.75),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"),
        legend.text = element_text(face = "italic"))


ggsave("./results/ig_prob.pdf", plot = fig2,
       height = 7.5, width = 7.5, units = "in", dpi = 600)

fig3 <- ggplot(filter(alldata, status == "ignitee"), aes(x = as.factor(drought_condition), y = ignition_delay,
                                                         color = as.factor(drought_condition))) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.7, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  facet_wrap(~ species_combination, scales = "free_x") +
  labs(x = "Combination",
       y = "Ignition delay time (s)") +
  prestheme +
  theme(strip.text = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

fig4 <- ggplot(filter(alldata, status == "ignitee" & self_ignition != 1), aes(x = as.factor(drought_condition), y = heat_release_j,
                                                         color = as.factor(drought_condition))) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.7, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  facet_wrap(~ species_combination, scales = "free_x") +
  labs(x = "Combination of hours",
       y = "Heat release (J)",
       color = "") +
  prestheme +
  theme(strip.text = element_text(face = "italic"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave("./results/heat_release.pdf", plot = fig4,
       height = 7.5, width = 7.5, units = "in", dpi = 600)


anova_fit_heat_release <- aov(heat_release_j ~ drought_condition * species_combination, data = alldata)
summary(anova_fit)
anova(anova_fit)
library(emmeans)
emmeans(anova_fit, pairwise ~ drought_condition | species_combination, adjust = "tukey")


