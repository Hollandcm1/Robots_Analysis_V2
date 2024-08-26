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

data$condition <- data$condition_nums
library(ggbeeswarm)

ggplot(means, aes(x=condition, y=mean, fill=condition)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Average Time Through Maze by Condition", x="Condition", y="Average Time Through Maze (s)") +
  geom_beeswarm(data=data, aes(x=condition_nums, y=average_time_through_maze), alpha=0.9, cex=1.8) +
  # geom_point(data=data, aes(x=condition_nums, y=average_time_through_maze, group = condition), 
  #            position=position_jitter(width=0.2), alpha=1) +
  theme(legend.position = "none")

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




