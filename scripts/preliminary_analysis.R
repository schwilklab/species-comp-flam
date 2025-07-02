library(ggplot2)

ggplot(filter(alldata, status == "ignitee"), aes(x = as.factor(hours), y = ignite_others, color = as.factor(hours))) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.7, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  facet_wrap(~ species, scales = "free_x") +
  prestheme +
  theme(strip.text = element_text(face = "italic"))

ggplot(filter(alldata, status == "ignitee"), aes(x = as.factor(hours), y = ignition_delay, color = as.factor(hours))) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.7, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  facet_wrap(~ species, scales = "free_x") +
  prestheme +
  theme(strip.text = element_text(face = "italic"))

ggplot(filter(alldata, status == "ignitee"), aes(x = as.factor(hours), y = heat_release_j, color = as.factor(hours))) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.7, size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  facet_wrap(~ species, scales = "free_x") +
  prestheme +
  theme(strip.text = element_text(face = "italic"))