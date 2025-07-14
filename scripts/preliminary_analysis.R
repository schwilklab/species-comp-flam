
source("./scripts/ggplot_theme.R")

library(ggplot2)

fig1 <- ggplot(time_wp, aes(hours, -1*(wp), color = species)) +
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

fig2 <- ggplot(filter(alldata, status == "ignitee"), aes(x = as.factor(hours_combination), y = ignite_others, color = as.factor(hours_combination))) +
  geom_jitter(width = 0.20, height = 0.05, alpha = 0.8, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  facet_wrap(~ species_combination, scales = "free_x") +
  labs(x = "Combination of hours",
       y = "Probability of ignition",
       color = "") +
  prestheme +
  theme(strip.text = element_text(face = "italic"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave("./results/ig_prob.pdf", plot = fig2,
       height = 7.5, width = 7.5, units = "in", dpi = 600)

fig3 <- ggplot(filter(alldata, status == "ignitee"), aes(x = as.factor(combination), y = ignition_delay, color = as.factor(combination))) +
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

fig4 <- ggplot(filter(alldata, status == "ignitee"), aes(x = as.factor(hours_combination), y = heat_release_j,
                                                         color = as.factor(hours_combination))) +
  geom_jitter(width = 0.20, height = 0.05, alpha = 0.7, size = 2) +
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

fig5 <- ggplot(filter(alldata, status == "ignitee"), aes(x = wp, y = heat_release_j, color = as.factor(combination))) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.7, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  facet_wrap(~ species_combination, scales = "free_x") +
  labs(x = "Water potential (MPa)",
       y = "Heat release (J)") +
  prestheme +
  theme(strip.text = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))
