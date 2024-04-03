# run_ANOVA

data <- data_long

# change type
data$condition <- as.factor(data$condition)

# Aggregate
average_velocity <- data %>%
  group_by(participant, trial, condition) %>%
  summarise(average_velocity = mean(linear_velocity, na.rm = TRUE))

# ANOVA
# anova_results <- aov(average_velocity ~ condition + Error(participant), data = average_velocity)
anova_results <- aov(average_velocity ~ condition, data = average_velocity)
summary(anova_results)

# Post-hoc
posthoc_results <- TukeyHSD(anova_results)
posthoc_results

#################
### visaulize ###
#################
# posthoc result
plot(posthoc_results)

# anova result
plot(anova_results)

# plot the data
ggplot(average_velocity, aes(x = condition, y = average_velocity)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  theme_minimal() +
  labs(title = "Average Velocity by Condition", x = "Condition", y = "Average Velocity") +
  ylim(c(0, 1.5))
