

# libraries
library(ggplot2)
library(here)

# load the data
data <- read.csv(here('output', 'Sierra_Verrifications', 'Robots_data_long.csv'))
codes <- tar_read('codes_conditions')

# transpose the codes
codes <- t(codes)

# use 'Factor' row as column labels
colnames(codes) <- codes["Factor",]

# remove the 'Factor' row
codes <- codes[-1,]

# convert to dataframe
codes <- as.data.frame(codes)

average_by_trial <- data

average_by_conditions <- average_by_trial %>%
  group_by(participant_number, haptic, vision, condition) %>%
  summarise(average_velocity = mean(average_velocity, na.rm = TRUE),
            average_time_through_maze = mean(time_through_maze, na.rm = TRUE),
            average_path_length = mean(path_length, na.rm = TRUE))

data <- average_by_conditions

# relabel visual, where 1 = Full Vision and 0 = Flickering Vision in data
data$visual <- factor(data$vision, levels = c(0, 1), labels = c("Flickering Vision", "Full Vision"))

# relabel haptic, where 1 = Haptic and 0 = No Haptic in data
data$haptic <- factor(data$haptic, levels = c(0, 1), labels = c("No Haptic Feedback", "Haptic Feedback"))

data$condition_nums <- data$condition


##################################
### Functions to Simplify Code ###
##################################

fix_means_df <- function(means, codes) {
  
  # rename the columns
  colnames(means) <- c("condition", "mean", "sd", "sample_size", "se")
  
  # add new colums called 'visual' and 'haptic' based on codes dataframe
  means$visual <- codes$Visual
  means$haptic <- codes$Haptic
  
  # relabel visual, where 1 = Full Vision and 0 = Flickering Vision
  means$visual <- factor(means$visual, levels = c(0, 1), labels = c("Flickering Vision", "Full Vision"))
  
  # relabel haptic, where 1 = Haptic and 0 = No Haptic
  means$haptic <- factor(means$haptic, levels = c(0, 1), labels = c("No Haptic Feedback", "Haptic Feedback"))
  
  return(means)
}


###############################
### Velocity (Bar + Points) ###
###############################


# calculate the mean and standard deviation for each condition
means <- aggregate(average_velocity ~ condition_nums, data=data, mean)
sds <- aggregate(average_velocity ~ condition_nums, data=data, sd)
sample_size <- aggregate(average_velocity ~ condition_nums, data=data, length)
se <- aggregate(average_velocity ~ condition_nums, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="condition_nums")
means <- merge(means, sample_size, by="condition_nums")
means <- merge(means, se, by="condition_nums")

# fix means df
means <- fix_means_df(means, codes)

# create the scatter plot jitter
data$visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

g1 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="Average Velocity by Condition (Experiment 1)", x="Condition", y="Average Velocity (units/second)") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=visual_jitter, y=average_velocity), 
                   alpha=0.7, cex=1.8, width = 0.15)

print(g1)



###########################
### Time (Bar + Points) ###
###########################

# calculate the mean and standard deviation for each condition
means <- aggregate(average_time_through_maze ~ condition_nums, data=data, mean)
sds <- aggregate(average_time_through_maze ~ condition_nums, data=data, sd)
sample_size <- aggregate(average_time_through_maze ~ condition_nums, data=data, length)
se <- aggregate(average_time_through_maze ~ condition_nums, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="condition_nums")
means <- merge(means, sample_size, by="condition_nums")
means <- merge(means, se, by="condition_nums")

# fix means df
means <- fix_means_df(means, codes)

# create the scatter plot jitter
data$visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

g2 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="Average Time Through Maze by Condition (Experiment 1)", x="Condition", y="Average Time Through Maze (seconds)") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=visual_jitter, y=average_time_through_maze), 
                   alpha=0.7, cex=1.8, width = 0.15)

print(g2)


##################################
### Path Length (Bar + Points) ###
##################################

# calculate the mean and standard deviation for each condition
means <- aggregate(average_path_length ~ condition_nums, data=data, mean)
sds <- aggregate(average_path_length ~ condition_nums, data=data, sd)
sample_size <- aggregate(average_path_length ~ condition_nums, data=data, length)
se <- aggregate(average_path_length ~ condition_nums, data=data, function(x) sd(x)/sqrt(length(x)))

# merge the means and sds
means <- merge(means, sds, by="condition_nums")
means <- merge(means, sample_size, by="condition_nums")
means <- merge(means, se, by="condition_nums")

# fix means df
means <- fix_means_df(means, codes)

# create the scatter plot jitter
data$visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

g3 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = position_dodge2(padding = 0.7), linewidth=1) +
  theme_classic() +
  labs(title="Average Path Length by Condition (Experiment 1)", x="Condition", y="Average Path Length (units)") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = 'top', # adjust these values as needed
        legend.justification = c(0.5, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=visual_jitter, y=average_path_length), 
                   alpha=0.7, cex=1.8, width = 0.15)

print(g3)




#################################
### Verification From Scratch ###
#################################

data <- read.csv(here('output', 'Sierra_Verrifications', 'Robots_data_long.csv'))

average_by_trial <- data

average_by_conditions <- average_by_trial %>%
  group_by(participant_number, haptic, vision, condition) %>%
  summarise(average_velocity = mean(average_velocity, na.rm = TRUE),
            average_time_through_maze = mean(time_through_maze, na.rm = TRUE),
            average_path_length = mean(path_length, na.rm = TRUE))

# convert haptic and vision to factors
average_by_conditions$haptic <- factor(average_by_conditions$haptic, levels = c(0, 1), labels = c("No Haptic Feedback", "Haptic Feedback"))
average_by_conditions$vision <- factor(average_by_conditions$vision, levels = c(0, 1), labels = c("Flickering Vision", "Full Vision"))

# plot scatter plot of average velocity by condition
g1 <- ggplot(average_by_conditions, aes(x=haptic, y=average_velocity)) +
  geom_point() +
  labs(title="Average Velocity by Condition (Experiment 1)", x="Condition", y="Average Velocity (units/second)") +
  theme_classic()
print(g1)

g2 <- ggplot(average_by_conditions, aes(x=vision, y=average_velocity)) +
  geom_point() +
  labs(title="Average Velocity by Condition (Experiment 1)", x="Condition", y="Average Velocity (units/second)") +
  theme_classic()
print(g2)




