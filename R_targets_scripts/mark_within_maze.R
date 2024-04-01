# mark_within_maze

data <- corrected_data


p_num <- 1
df_participant <- data[[paste(p_num)]]
df_trial <- df_participant[[1]]

position_data <- df_trial[['position_robot']]
rotation <- df_trial[['rotation']]

# This will determine which column to use for how to cut the data. When traveling
# vertically we want to cut the data based on a y criteria, when traveling horizontally
# we want to cut the data based on an x criteria.
if (rotation == 0 | rotation == 180) {
  column_decider <- 2
} else if (rotation == 90 | rotation == 270) {
  column_decider <- 1
}


# The logic of this next bit kinda sucks to follow, but the idea is that it will
# iterate through every frame of a trial, first looking for when the robot
# has moved into the maze. One it has, it will jump forward 500 frames to prevent
# an accidental exit (i.e. they immediately leave). Then it will continue to mark
# frames as being inside the maze until the robot leaves the maze. Once the robot
# leaves the maze, it will stop marking frames. within = 1, outside = 0.
jumps_left <- 0
start_found <- 0
end_found <- 0
inside_maze <- rep(0, nrow(position_data))
for (counter_frame in 1:nrow(position_data)) {
  
  # jump 500 when start is found - this is to prevent issues with people entering
  # and accidentally leaving breifly. The data for those 500 frames is still recorded
  if (jumps_left > 0) {
    jumps_left <- jumps_left - 1
    # mark inside maze
    inside_maze[counter_frame] <- 1
    next
  }
  
  # do nothing if outside the maze / (find when they leave the maze?)
  if (position_data[counter_frame, column_decider] < -5 | 
      position_data[counter_frame, column_decider] > 5 | 
      end_found == 1) {
    
    # mark that the end has been found, forcing the rest of of the trials to be skipped
    if (start_found == 1) {
      end_found <- 1
    }
    
    next
    
  } else { # mark frames that they are inside the maze
    
    inside_maze[counter_frame] <- 1
    
    # mark that the start has been found and provide jumps 
    if (sum(inside_maze == 0)) {
      start_found <- 1
      jumps_left <- 500
    }
    
  }
}
