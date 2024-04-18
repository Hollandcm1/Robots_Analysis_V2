# complex_model_1

# add maps as a factor
data$map <- as.factor(data$map)

#################
### Aggregate ###
#################
average_by_trial <- data %>%
  group_by(participant, trial, condition_nums, time_through_maze, max_force, path_length, haptic, visual, map) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

##############
### Models ###
##############

# Model 1
model1 <- lmer(time_through_maze ~ haptic * visual * average_force * path_length * map * max_force + (1|participant) + (1|trial), data = average_by_trial)
summary(model1)
tab_model(model1)
capture.output(model1, file = here('output', 'Model_time_through_maze', 'model1.txt'))





#####################
### Visualisation ###
#####################

average_by_trial$map <- as.factor(average_by_trial$map)
flexplot(data = average_by_trial, time_through_maze ~ average_force + path_length | haptic + visual + map, method = 'lm')


flexplot(data = average_by_trial, time_through_maze ~ max_force + path_length | haptic + map, method = 'lm')

# interesting! 
flexplot(data = average_by_trial, time_through_maze ~ max_force + path_length | visual, method = 'lm')


