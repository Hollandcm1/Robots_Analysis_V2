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
average_force_by_trial <- data %>%
  group_by(participant, trial, condition) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

average_force_by_participant <- average_force_by_trial %>%
  group_by(participant, condition) %>%
  summarise(average_force = mean(average_force, na.rm = TRUE))

average_force_by_trial_hv <- data %>%
  group_by(participant, trial, haptic, visual) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

average_force_by_participant_hv <- average_force_by_trial_hv %>%
  group_by(participant, haptic, visual) %>%
  summarise(average_force = mean(average_force, na.rm = TRUE))

# ANOVA 1
anova_results <- aov(average_force ~ condition, data = average_force_by_participant)
summary(anova_results)

anova_results <- aov(average_force ~ condition, data = average_force_by_trial)
summary(anova_results)


# ANOVA 2
anova_results_hv <- aov(average_force ~ haptic * visual, data = average_force_by_participant_hv)
summary(anova_results_hv)

anova_results_hv <- aov(average_force ~ haptic * visual + Error(participant/trial), data = average_force_by_trial_hv)
summary(anova_results_hv)

# check assumptions 
# check for normality
shapiro.test(average_force_by_trial_hv$average_force)
hist(average_force_by_trial_hv$average_force, breaks = 20, col = "skyblue", border = "black", xlab = "Average Force", main = "Histogram of Average Force")
library(stats)
mauchly_result <- mauchly(anova_results_hv)
print(mauchly_result)

# GGe correction anova 
library(ez)
library(afex)
library(sjstats)
results <- aov_ez(id = "participant", 
                  dv = "average_force", 
                  data = average_force_by_trial_hv,
                  within = c("haptic", "visual"),
                  #between = "Condition",
                  detailed = TRUE)

print(results)
# summary(results)

effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)


results <- ezANOVA(wid = "participant", 
                  dv = "average_force", 
                  data = average_force_by_trial_hv,
                  within = c("trial", "haptic", "visual"),
                  #between = "Condition",
                  detailed = TRUE)

print(results)


#################
### Visualize ###
#################
ggplot(average_force_by_participant, aes(x = condition, y = average_force, color=condition)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  theme_minimal() +
  labs(title = "Average Force by Condition", x = "Condition", y = "Average Force") 
  #ylim(c(0, 1.5))

ggplot(average_force_by_participant_hv, aes(x = haptic, y = average_force, color=visual, group=haptic)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  theme_classic() +
  labs(title = "Average Force by Haptic and Visual", x = "Haptic", y = "Average Force") +
  facet_wrap(~visual)
  #ylim(c(0, 1.5))

ggplot(average_force_by_trial_hv, aes(x = haptic, y = average_force, color=visual, group=haptic)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  theme_classic() +
  labs(title = "Average Force by Haptic and Visual", x = "Haptic", y = "Average Force") +
  facet_wrap(~visual)
  #ylim(c(0, 1.5))

ggplot(average_force_by_trial_hv, aes(x = visual, y = average_force, colour=visual)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  theme_minimal() +
  labs(title = "Average Force", x = "Vision", y = "Average Force") 
  #ylim(c(0, 1.5)



# Anova analysis for predicting time within maze by average force

# aggregate
average_force_by_participant <- data %>%
  group_by(participant, condition, trial, time_through_maze) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

average_force_by_participant$force_group <- cut(average_force_by_participant$average_force, breaks = 2)
summary(average_force_by_participant$force_group)

# change type
average_force_by_participant$force_group <- as.factor(average_force_by_participant$force_group)

# average_force_by_participant <- average_force_by_participant %>%
#   mutate(force_group_2 = ntile(average_force, breaks = 2))


# ANOVA 1
anova_results <- aov(time_through_maze ~ force_group, data = average_force_by_participant)
summary(anova_results)

# visualize
ggplot(data = average_force_by_participant, aes(x = force_group, y = time_through_maze)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Time Through Maze by Average Force", x = "Average Force", y = "Time Through Maze")

ggplot(data = average_force_by_participant, aes(x = force_group, y = time_through_maze)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Time Through Maze by Average Force", x = "Average Force", y = "Time Through Maze") 

ggplot(data = average_force_by_participant, aes(x = force_group, y = time_through_maze)) +
  geom_point() +  # Plot individual data points
  stat_summary(fun = mean, geom = "point", size = 3, color = "red") +  # Plot means as points
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +  # Connect means with line
  theme_minimal() +
  labs(title = "Time Through Maze by Average Force", x = "Average Force", y = "Time Through Maze")

ggplot(data = average_force_by_participant, aes(x = force_group, y = time_through_maze, color = condition, group = condition)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Time Through Maze by Average Force", x = "Average Force", y = "Time Through Maze") +
  facet_wrap(~condition)
