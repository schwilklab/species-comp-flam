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

fig2 <- ggplot(filter(alldata, status == "ignitee"), aes(x = as.factor(hours), y = ignite_others, color = as.factor(hours))) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.7, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  facet_wrap(~ species, scales = "free_x") +
  prestheme +
  theme(strip.text = element_text(face = "italic"))

fig3 <- ggplot(filter(alldata, status == "ignitee"), aes(x = as.factor(hours), y = ignition_delay, color = as.factor(hours))) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.7, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  facet_wrap(~ species, scales = "free_x") +
  prestheme +
  theme(strip.text = element_text(face = "italic"))

fig4 <- ggplot(filter(alldata, status == "ignitee"), aes(x = as.factor(hours), y = heat_release_j, color = as.factor(hours))) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.7, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  facet_wrap(~ species, scales = "free_x") +
  prestheme +
  theme(strip.text = element_text(face = "italic"))

fig5 <- ggplot(filter(alldata, status == "ignitee"), aes(x = wp, y = heat_release_j)) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.7, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  facet_wrap(~ species, scales = "free_x") +
  prestheme +
  theme(strip.text = element_text(face = "italic"))
