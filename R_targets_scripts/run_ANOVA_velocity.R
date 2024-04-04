# run_ANOVA_velocity

data <- data_long

# change type
data$condition <- as.factor(data$condition)
data$participant <- as.factor(data$participant)

# isolate for within maze
data <- data %>%
  filter(within_maze == 1)
# check that the filter worked
unique(data$within_maze)

# Aggregate
average_velocity <- data %>%
  group_by(participant, trial, condition) %>%
  summarise(average_velocity = mean(linear_velocity, na.rm = TRUE))

average_velocity_by_participant <- average_velocity %>%
  group_by(participant, condition) %>%
  summarise(average_velocity = mean(average_velocity, na.rm = TRUE))

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
ggplot(average_velocity, aes(x = condition, y = average_velocity, color=condition)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  theme_minimal() +
  labs(title = "Average Velocity by Condition", x = "Condition", y = "Average Velocity") +
  ylim(c(0, 1.5))


ggplot(average_velocity, aes(x=participant, y=average_velocity, group=participant, colour=participant)) +
  geom_boxplot() +
  geom_jitter(width=0.2, height=0) +
  theme_minimal() +
  labs(title = "Average Velocity by Participant", x = "Participant", y = "Average Velocity") +
  ylim(c(0, 1.5))

hist(average_velocity$average_velocity, breaks = 20, col = "skyblue", border = "black", xlab = "Average Velocity", main = "Histogram of Average Velocity")

ggplot(average_velocity_by_participant, aes(x = condition, y = average_velocity, color=condition)) +
  geom_violin() +
  geom_boxplot(width=0.2) +
  geom_jitter(width = 0.2, height=0) +
  theme_minimal() +
  labs(title = "Average Participant Velocity by Condition", x = "Condition", y = "Average Velocity") +
  ylim(c(0, 1.5))

# bar chart
ggplot(average_velocity_by_participant, aes(x = condition, y = average_velocity, fill=condition)) +
  stat_summary(fun = "mean", geom = "col") +
  geom_jitter(width = 0.2, height=0) +
  theme_minimal() +
  labs(title = "Average Participant Velocity by Condition", x = "Condition", y = "Average Velocity") +
  ylim(c(0, 1.5))

