# build_data_long_force

# peak force?
# time by force matrix?

build_data_long <- function(data) {
  
  print("Building data_long_force")
  data_long <- data.frame()
  
  # loop through all participants
  participants <- names(data)
  for (p_num in 1:length(participants)) {
    
    print(paste("Participant", participants[p_num]))
    
    # loop through all trials
    trials <- names(data[[participants[p_num]]])
    for (trial_num in 1:length(trials)) {
      
      start_time <- Sys.time()
      
      print(paste("---Trial", trials[trial_num]))
      
      trial_data <- data[[participants[p_num]]][[trials[trial_num]]]
      
      force <- as.data.frame(trial_data['force_input'])
      
      if (nrow(force) == 0) {
        warning(paste("Participant", participants[p_num], "Trial", trials[trial_num], "has no force data"))
        next
      }
      
      # get magnitude of x and y force
      force$force_mag <- sqrt(force[,1]^2 + force[,2]^2)
      
      # plot force magnitude by time
      # ggplot(force, aes(x = 1:nrow(force), y = force_mag)) +
      #   geom_line() +
      #   labs(title = paste("Participant", participants[p_num], "Trial", trials[trial_num])) +
      #   theme_classic()
      
      # add needed row information for long dataframe
      dat <- force %>% 
        mutate(participant = p_num, trial = trial_num, frame = 1:nrow(force))
      
      # within maze data
      within_maze <- as.data.frame(trial_data['within_maze'])
      dat$within_maze <- within_maze$within_maze
      
      # time data
      time <- as.data.frame(trial_data['time'])
      dat$time <- time$time
      
      # linear velocity
      linear_velocity <- as.data.frame(trial_data['linear_and_angular_velocity_robot'])
      dat$linear_velocity <- linear_velocity$linear_and_angular_velocity_robot.1
      
      # position robot
      position <- as.data.frame(trial_data['position_robot'])
      dat$position_x_robot <- position$position_robot.1
      dat$position_y_robot <- position$position_robot.2
      
      # position leader
      position <- as.data.frame(trial_data['position_leader'])
      dat$position_x_leader <- position$position_leader.1
      dat$position_y_leader <- position$position_leader.2
      
      # save dat to data_long
      data_long <- rbind(data_long, dat)
      
      # time reports for code optimization
      current_time <- Sys.time() - start_time
      print(current_time)
      
    }
    
  }
  
  return(data_long)
  
}