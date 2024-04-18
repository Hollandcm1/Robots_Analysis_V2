# plot_all_trial_force

#data <- data

for (p_num in unique(data$participant)) {
  
  print(paste("Plotting participant", p_num))
  participant_data <- data[data$participant == p_num,]
  
  for (t_num in unique(participant_data$trial)) {
    
    print(paste("Plotting trial", t_num))
    trial_data <- participant_data[participant_data$trial == t_num,]
    
    g1 <- ggplot(trial_data, aes(x = position_x_robot, y = position_y_robot, colour = force_magnitude)) +
      geom_point() +
      scale_colour_gradient(low = "blue", high = "red") +
      ggtitle(paste("Participant", p_num, "Trial", t_num)) +
      theme_classic() +
      xlim(-10, 10) +
      ylim(-10, 10)
    ggsave(here('output', 'figures_force', paste("participant", p_num), paste("_trial", t_num, ".png")), plot = g1, device = "png")
  }
  
}

