# data_long_calculations 

# time to get through maze
# average velocity
# path length
# near object (eventually)

data <- data_long

########################
### Average Velocity ###
########################

# calculate average velocity by participant and trial
average_velocity_by_trial <- data %>%
  group_by(participant, trial) %>%
  filter(within_maze == 1) %>%
  summarise(average_velocity = mean(linear_velocity, na.rm = TRUE))

# merge back to original data
data <- merge(data, average_velocity_by_trial, by = c("participant", "trial"))


#########################
### Time Through Maze ###
#########################

# calculate time through maze by participant and trial
time_through_maze_by_trial <- data %>%
  group_by(participant, trial) %>%
  filter(within_maze == 1) %>%
  summarise(time_through_maze = max(time) - min(time))

# merge back to original data
data <- merge(data, time_through_maze_by_trial, by = c("participant", "trial"))


###################
### Path Length ###
###################

# calculate path length by participant and trial
data <- data %>%
  group_by(participant, trial) %>%
  arrange(participant, trial, frame) %>%
  mutate(prev_x = lag(position_x_robot, order_by = frame),
         prev_y = lag(position_y_robot, order_by = frame),
         distance_since_prev_frame = sqrt((position_x_robot - prev_x)^2 + (position_y_robot - prev_y)^2)
         ) %>%
  ungroup()

# fix NA values
data$distance_since_prev_frame[is.na(data$distance_since_prev_frame)] <- 0

# calculate path length by participant and trial
path_length_by_trial <- data %>%
  group_by(participant, trial) %>%
  filter(within_maze == 1) %>%
  summarise(path_length = sum(distance_since_prev_frame))

# merge back to original data
data <- merge(data, path_length_by_trial, by = c("participant", "trial"))


#####################
### Average Force ###
#####################

# calculate average force by participant and trial
average_force_by_trial <- data %>%
  group_by(participant, trial) %>%
  filter(within_maze == 1) %>%
  summarise(average_force = mean(force_magnitude, na.rm = TRUE))

# merge back to original data
data <- merge(data, average_force_by_trial, by = c("participant", "trial"))


#################
### Max Force ###
#################

# calculate max force by participant and trial
max_force_by_trial <- data %>%
  group_by(participant, trial) %>%
  filter(within_maze == 1) %>%
  summarise(max_force = max(force_magnitude, na.rm = TRUE))

# merge back to original data
data <- merge(data, max_force_by_trial, by = c("participant", "trial"))




# 
# participants <- unique(data$participant)
# for (p_num in participants) {
#   
#   print(paste("Participant", p_num))
#   
#   trials <- unique(data$trial)
#   for (trial in trials) {
#     
#     # get the data for the participant
#     trial_data <- data %>%
#       filter(participant == p_num & trial == trial)
#     
#     
#   }
# }
# 
# 
# average_force_by_trial <- data %>%
#   group_by(participant, trial, condition) %>%