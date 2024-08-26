# standard figures

# libraries
library(ggplot2)
library(here)

# load the data
data <- read.csv(here('output', 'average_by_conditions.csv'))
codes <- tar_read('codes_conditions')

# transpose the codes
codes <- t(codes)

# use 'Factor' row as column labels
colnames(codes) <- codes["Factor",]

# remove the 'Factor' row
codes <- codes[-1,]

# convert to dataframe
codes <- as.data.frame(codes)

# relabel visual, where 1 = Full Vision and 0 = Flickering Vision in data
data$visual <- factor(data$visual, levels = c(0, 1), labels = c("Flickering Vision", "Full Vision"))

# relabel haptic, where 1 = Haptic and 0 = No Haptic in data
data$haptic <- factor(data$haptic, levels = c(0, 1), labels = c("No Haptic", "Haptic"))

# # relabel condition_nums where 1 = VH 2 = H, 3 = V, 4 = nothing
# data$condition_nums <- factor(data$condition_nums, levels = c(1, 2, 3, 4), labels = c("VH", "H", "V", "nothing"))
# 
# # reorder condition_nums as nothing, H, V, VH
# data$condition_nums <- factor(data$condition_nums, levels = c("nothing", "H", "V", "VH"))
# 
# # plot average_time_through_maze by condition
# ggplot(data, aes(x=condition_nums, y=average_time_through_maze, fill=condition_nums)) +
#   geom_boxplot() +
#   geom_jitter(width = 0.3, height = 0, alpha = 0.5) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title="Average Time Through Maze by Condition", x="Condition", y="Average Time Through Maze (s)")


########################################
### Time Through Maze (Bar + Points) ###
########################################

# calculate the mean and standard deviation for each condition
means <- aggregate(average_time_through_maze ~ condition_nums, data=data, mean)
sds <- aggregate(average_time_through_maze ~ condition_nums, data=data, sd)

# merge the means and sds
means <- merge(means, sds, by="condition_nums")

# rename the columns
colnames(means) <- c("condition", "mean", "sd")

# add new colums called 'visual' and 'haptic' based on codes dataframe
means$visual <- codes$Visual
means$haptic <- codes$Haptic

# # convert to integers
# means$visual <- as.integer(means$visual)
# means$haptic <- as.integer(means$haptic)

# relabel visual, where 1 = Full Vision and 0 = Flickering Vision
means$visual <- factor(means$visual, levels = c(0, 1), labels = c("Flickering Vision", "Full Vision"))

# relabel haptic, where 1 = Haptic and 0 = No Haptic
means$haptic <- factor(means$haptic, levels = c(0, 1), labels = c("No Haptic", "Haptic"))

data$condition <- data$condition_nums
library(ggbeeswarm)

data$visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic", 0.2, 
         ifelse(data$haptic == "No Haptic", -0.2, -0.2))

ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge2(padding = 0.7)) +
  theme_classic() +
  labs(title="Average Time Through Maze by Condition", x="Condition", y="Average Time Through Maze") +
  guides(fill=guide_legend(title=NULL)) + 
  # add scatter plot data on top of this
  # Use geom_quasirandom to jitter the points
  geom_quasirandom(data=data, aes(x=visual_jitter, y=average_time_through_maze), 
                   alpha=0.9, cex=1.8, width = 0.15)
  # geom_beeswarm(data=data, aes(x=visual, y=average_time_through_maze, group=haptic), 
  #               position=position_dodge2(width = 0.75, padding = 0.05), 
  #               alpha=0.9, cex=1.8) 


  

# ggplot() +
#   geom_beeswarm(data=data, aes(x=visual, y=average_time_through_maze, group=haptic), alpha=0.9, cex=1.8) +
#   theme(legend.position = "none")

# save the plot
ggsave(here('output', 'standard_figures', 'average_time_through_maze.png'), width=8, height=6)





##################################
### Path Length (Bar + Points) ###
##################################

# calculate the mean and standard deviation for each condition
means <- aggregate(average_path_length ~ condition_nums, data=data, mean)
sds <- aggregate(average_path_length ~ condition_nums, data=data, sd)

# merge the means and sds
means <- merge(means, sds, by="condition_nums")

# rename the columns
colnames(means) <- c("condition", "mean", "sd")

data$condition <- data$condition_nums

ggplot(means, aes(x=condition, y=mean, fill=condition)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Average Path Length by Condition", x="Condition", y="Average Path Length") +
  geom_beeswarm(data=data, aes(x=condition_nums, y=average_path_length), alpha=0.9, cex=1.8) +
  theme(legend.position = "none")

# save the plot
ggsave(here('output', 'standard_figures', 'average_path_length.png'), width=8, height=6)




