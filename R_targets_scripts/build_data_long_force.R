# build_data_long_force

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
      
      # get magnitude of x and y force
      force$force_mag <- sqrt(force[,1]^2 + force[,2]^2)
      
      # plot force magnitude by time
      # ggplot(force, aes(x = 1:nrow(force), y = force_mag)) +
      #   geom_line() +
      #   labs(title = paste("Participant", participants[p_num], "Trial", trials[trial_num])) +
      #   theme_classic()
      
      # save force_mag to data_long
      data_list <- list()
      for (i in 1:nrow(force)) {
        row <- tibble(participant = p_num, 
                      trial = trial_num, 
                      frame = i, 
                      force_mag = force$force_mag[i])
        #data_list[[i]] <- row
        data_long <- rbind(data_long, row)
      }
      
      # peak force?
      # time by force matrix?
      # i still need to clean to be within the maze
      
      current_time <- Sys.time() - start_time
      print(current_time)
      
    }
    
  }
  
  return(data_long)
  
}