# Load libraries
library("ggplot2")

# Read CSV file
cap_data <- read.csv("C:/Users/bismi/OneDrive/Desktop/Chili Analysis/Capsicum Genus Research.csv")

####################################################### Preprocessing ###########################################################

# Cleaning table
head(cap_data)
colnames(cap_data)[1] <- 'ID' 
colnames(cap_data)[3] <- 'Gender' 
colnames(cap_data)[4] <- 'Birthday' 
colnames(cap_data)[5] <- 'Location'
new_data <- cap_data[-c(1), ]

# Data cleansing
new_data$X1.c...Basic.Demographics....State <- paste(new_data$X1.c...Basic.Demographics....State, new_data$X2.What.is.your.race.,new_data$X, new_data$X.1, new_data$X.2, new_data$X.3)
new_data$X2.What.is.your.race. <- paste(new_data$X.4, new_data$X3.What.is.your.ethnicity. ,new_data$X.5)
new_data <- new_data[,-c(8:14)]

# Cleaning new table
colnames(new_data)[6] <- 'Race'
colnames(new_data)[7] <- 'Ethnicity'
colnames(new_data)[8] <- 'Satisfaction_growing_plants'
colnames(new_data)[9] <- 'Satisfaction_growing_veggies'
colnames(new_data)[10] <- 'Familiar_with_capsicum'
colnames(new_data)[11] <- 'Grow_capsicum'

# Data cleansing pt 2
new_data$X7.I.regularly.grow.Capsicum.Species.in.my.garden. <- paste(new_data$X7.I.regularly.grow.Capsicum.Species.in.my.garden. , new_data$X8.I.am.familiar.with.a.wholesale.greenhouse.growing.company..e.g..Bonnie.Plants)
new_data <- new_data[,-c(13)]

# Cleaning new table again
colnames(new_data)[12] <- 'Familiar_with_wholesale'
colnames(new_data)[13] <- 'Purchase_from_wholesale'
colnames(new_data)[14] <- 'Satisfies_with_wholesale'
colnames(new_data)[15] <- 'Yield_from_wholesale'
colnames(new_data)[16] <- 'Save_seeds'
colnames(new_data)[17] <- 'Share_seeds'

# Final data cleaning
new_data$X13.I.often.share.seeds..from.vegetables.that.I.grew..with.friends.and.family.in.hopes.they.will.grow.them. <- paste(new_data$X13.I.often.share.seeds..from.vegetables.that.I.grew..with.friends.and.family.in.hopes.they.will.grow.them. , new_data$X14.I.would.be.willing.to.grow.a.wild.Capsicum.species.to.try.to.help.protect.the.species.)
new_data <- new_data[,-c(19,21)]

# Final cleaning of table
colnames(new_data)[18] <- 'Willing_to_grow_endangered'
colnames(new_data)[19] <- 'Comments'

# Replacing cap_data with the new_data table
cap_data <- new_data

# Removing new_data to reduce space in the global environment
rm(new_data)

# Writing new CSV of the cap_data
write.csv(cap_data, "cap_data.csv", row.names=FALSE)

# Removing comments as some people put identifiable information in there
cap_data <- cap_data[,-c(19)]

# Export CSV for further processing
write.csv(cap_data, "cap_data_github.csv", row.names = FALSE)

######################################################## Analysis #################################################################

# Loading in required data
cap_data <- read.csv("~\\cap_data.csv")

# Basic intro data exploration
table(cap_data$Location)

xaxis <- c("Completely Dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Completely Satisfied")

ggplot(cap_data , aes(x = Satisfaction_growing_plants)) + 
  geom_histogram(binwidth = 1, fill = 'lightgrey', col = 'black') +
  theme_minimal() +
  ggtitle('Satisfaction of Growers') +
  theme(plot.title = element_text(hjust = 0.4)) +
  ylab('# of Responses') +
  xlab('Level of Satisfaction') +
  scale_x_continuous(breaks = c(2, 3, 4, 5),
                     labels = c("Dissatisfied", "Neutral", "Satisfied", "Completely Satisfied"))

ggplot(cap_data , aes(x = Familiar_with_capsicum)) +
  geom_histogram(binwidth = 1, fill='lightgrey', col = 'black') +
  theme_minimal() +
  ggtitle('Capsicum Knowledge of Growers') +
  theme(plot.title = element_text(hjust = 0.4)) +
  ylab('# of Responses') +
  xlab('Level of Familiarity') +
  scale_x_continuous(breaks = c(3, 4, 5),
                   labels = c("Heard of", "Familiar", "Knowledgeable"))

p_save <- ggplot(cap_data , aes(x = Save_seeds)) 

p_save + geom_histogram(binwidth = 1, fill='lightgrey', col = 'black') +
  theme_minimal() +
  ggtitle('Do Growers Save Seeds?') +
  theme(plot.title = element_text(hjust = 0.4)) +
  ylab('# of Responses') +
  xlab('Are Seeds Saved?') +
  scale_x_continuous(breaks = c(1,2,3, 4, 5),
                     labels = c("Completely Disagree", "Disagree", "Neutral", "Agree", "Completely Agree"))

p_save + geom_histogram(aes(fill = Gender), binwidth = 1, fill='lightgrey', col = 'black') +
  stat_bin(binwidth=1, geom='text', color='black', size=3,
           aes(label=..count.., group=Gender), position=position_stack(vjust=0.4)) +
  theme_minimal() +
  ggtitle('Do Growers Save Seeds?') +
  theme(plot.title = element_text(hjust = 0.4)) +
  ylab('# of Responses') +
  xlab('Are Seeds Saved?') +
  scale_x_continuous(breaks = c(1,2,3, 4, 5),
                     labels = c("Completely Disagree", "Disagree", "Neutral", "Agree", "Completely Agree"))

ggplot(cap_data , aes(x = Save_seeds, fill = Gender)) +
  geom_histogram(aes(fill = Gender), binwidth = 1, fill='lightgrey', col = 'black') +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
