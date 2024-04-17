# flag_possible_fighting

flag_possible_fighting <- function(data) {
   
  length_of_fight <- 1000 # in frames
  # force_criteria <- 0.9593
  force_criteria <- summary(data$force_magnitude)[[5]][1] # 3rd quartile
  
  # loop through every participant
  for (p_num in unique(data$participant)) {
    
    # get the data for this participant
    participant_data <- data[data$participant == p_num,]
    
    # loop through every trial
    for (t_num in unique(participant_data$trial)) {
      
      # get the data for this trial
      trial_data <- participant_data[participant_data$trial == t_num,]
      
      # mark all high force
      trial_data$high_force <- trial_data$force_magnitude > force_criteria
      
      results <- rollapply(trial_data$high_force, width = length_of_fight, FUN = function(x) sum(x) == length_of_fight, align = "left", fill = NA)
      indices <- which(results == TRUE)
      
      if (length(indices) > 0) {
        print(paste("Participant", p_num, "Trial", t_num, "Possible Fighting Detected"))
      }
      
    }
  }
  
}
