# Model

# library(lme4)
# library(sjPlot)
# library(flexplot)
# library(here)
# library(dplyr)

# data <- strategic_data_appended

strategic_LME_analysis <- function(data) {

  #################
  ### Aggregate ###
  #################
  average_by_trial <- data %>%
    group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map, strategic_both, strategic_either, collaborative) %>%
    summarise(average_force = mean(force_magnitude, na.rm = TRUE))
  
  ##############
  ### Models ###
  ##############
  
  # Model 1
  model1 <- lmer(time_through_maze ~ haptic * visual * path_length * map * collaborative + (1|participant), data = average_by_trial)
  summary(model1)
  tab_model(model1)
  capture.output(model1, file = here('output', 'Strategic_Models', 'model1.txt'))
  
  #convert haptic and visual to numeric
  average_by_trial$haptic_num <- as.numeric(average_by_trial$haptic)
  average_by_trial$visual_num <- as.numeric(average_by_trial$visual)
  average_by_trial$collaborative <- as.numeric(average_by_trial$collaborative)
  cor(average_by_trial[, c("haptic_num", "visual_num", "path_length", "map", "collaborative")])
  
  
  # Model Buidling
  model_0 <- lmer(time_through_maze ~ 1 + (1|participant), data = average_by_trial)
  model_1 <- lmer(time_through_maze ~ haptic + (1|participant), data = average_by_trial)
  model_2 <- lmer(time_through_maze ~ haptic + visual + (1|participant), data = average_by_trial)
  model_3 <- lmer(time_through_maze ~ haptic + visual + path_length + (1|participant), data = average_by_trial)
  model_4 <- lmer(time_through_maze ~ haptic + visual + path_length + map + (1|participant), data = average_by_trial)
  model_4_1 <- lmer(time_through_maze ~ map + (1|participant), data = average_by_trial)
  model_5 <- lmer(time_through_maze ~ haptic + visual + path_length + map + collaborative + (1|participant), data = average_by_trial)
  model_6 <- lmer(time_through_maze ~ haptic + visual + path_length + map + collaborative + haptic:visual + (1|participant), data = average_by_trial)
  model_7 <- lmer(time_through_maze ~ haptic + visual + path_length + map + collaborative + haptic:visual + haptic:map + (1|participant), data = average_by_trial)
  model_8 <- lmer(time_through_maze ~ collaborative + (1|participant), data = average_by_trial)
  model_9 <- lmer(time_through_maze ~ haptic * visual * collaborative + (1|participant), data = average_by_trial)
  
  # Visual
  flexplot(time_through_maze ~ collaborative, data = average_by_trial)
  flexplot(time_through_maze ~ visual + collaborative, data = average_by_trial)
  flexplot(time_through_maze ~ haptic + visual | path_length, data = average_by_trial)
  flexplot(time_through_maze ~ visual + collaborative | path_length, data = average_by_trial)
  flexplot(time_through_maze ~ visual + path_length | collaborative + haptic, data = average_by_trial)

}