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
data$haptic <- factor(data$haptic, levels = c(0, 1), labels = c("No Haptic Feedback", "Haptic Feedback"))

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

# relabel visual, where 1 = Full Vision and 0 = Flickering Vision
means$visual <- factor(means$visual, levels = c(0, 1), labels = c("Flickering Vision", "Full Vision"))

# relabel haptic, where 1 = Haptic and 0 = No Haptic
means$haptic <- factor(means$haptic, levels = c(0, 1), labels = c("No Haptic Feedback", "Haptic Feedback"))

# create the scatter plot jitter
data$visual_jitter <- as.numeric(data$visual) + 
  ifelse(data$haptic == "Haptic Feedback", 0.2, 
         ifelse(data$haptic == "No Haptic Feedback", -0.2, -0.2))

g1 <- ggplot(means, aes(x=visual, y=mean, fill=haptic)) +
  geom_bar(stat="identity", position = position_dodge2(padding = 0.05)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge2(padding = 0.7)) +
  theme_classic() +
  labs(title="Average Time Through Maze by Condition", x="Condition", y="Average Time Through Maze (seconds)") +
  guides(fill=guide_legend(title=NULL)) + 
  # move legend
  theme(legend.position = c(0.6, 0.8), # adjust these values as needed
        legend.justification = c(0, 1)) +
  # add scatter plot data on top of this
  geom_quasirandom(data=data, aes(x=visual_jitter, y=average_time_through_maze), 
                   alpha=0.9, cex=1.8, width = 0.15)

print(g1)

save(g1, file = here('output', 'standard_figures', 'average_time_through_maze.RData'), width=6, height=8)



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




