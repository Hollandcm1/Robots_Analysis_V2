# chi_square analysis 

library(tidyverse)

data <- tar_read(strategic_data_appended)

#################
### Aggregate ###
#################
average_by_trial <- data %>%
  group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, strategic_both, strategic_either, collaborative) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

average_by_trial$condition_nums_num <- as.numeric(average_by_trial$condition_nums)

chi_sq_result <- chisq.test(average_by_trial$condition_nums, average_by_trial$collaborative)

print(chi_sq_result)

#visualize
ggplot(average_by_trial, aes(x = collaborative, fill = condition_nums, group = condition_nums)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Chi-Square Analysis of Collaborative and Condition",
       x = "Condition",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(average_by_trial, aes(x = condition_nums, fill = collaborative, group = collaborative)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Chi-Square Analysis of Collaborative and Condition",
       x = "Condition",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
