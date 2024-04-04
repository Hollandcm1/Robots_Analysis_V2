# run_ANOVA_force

data <- data_long

# change type
data$condition <- as.factor(data$condition)
data$participant <- as.factor(data$participant)
data$haptic <- as.factor(data$haptic)
data$visual <- as.factor(data$visual)

# isolate for within maze
data <- data %>%
  filter(within_maze == 1)
# check that the filter worked
unique(data$within_maze)

# Aggregate
max_force_by_trial <- data %>%
  group_by(participant, trial, condition) %>%
  summarise(max_force = max(force_magnitude, na.rm = TRUE))

max_force_by_participant <- max_force_by_trial %>%
  group_by(participant, condition) %>%
  summarise(max_force = max(max_force, na.rm = TRUE))

max_force_by_trial_hv <- data %>%
  group_by(participant, trial, haptic, visual) %>%
  summarise(max_force = max(force_magnitude, na.rm = TRUE))

max_force_by_participant_hv <- max_force_by_trial_hv %>%
  group_by(participant, haptic, visual) %>%
  summarise(max_force = max(max_force, na.rm = TRUE))

# ANOVA 1
anova_results <- aov(max_force ~ condition, data = max_force_by_participant)
summary(anova_results)

anova_results <- aov(max_force ~ condition, data = max_force_by_trial)
summary(anova_results)


# ANOVA 2
anova_results_hv <- aov(max_force ~ haptic * visual, data = max_force_by_participant_hv)
summary(anova_results_hv)

anova_results_hv <- aov(max_force ~ haptic * visual, data = max_force_by_trial_hv)
summary(anova_results_hv)



#################
### Visualize ###
#################
ggplot(max_force_by_participant, aes(x = condition, y = max_force, color=condition)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  theme_minimal() +
  labs(title = "max Force by Condition", x = "Condition", y = "max Force") 
#ylim(c(0, 1.5))

ggplot(max_force_by_participant_hv, aes(x = haptic, y = max_force, color=visual, group=haptic)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  theme_classic() +
  labs(title = "max Force by Haptic and Visual", x = "Haptic", y = "max Force") +
  facet_wrap(~visual)
#ylim(c(0, 1.5))
