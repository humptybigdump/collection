# Media Management Tutorial Example Exercises

# 1. Data preparation-----------------------------------------------------------

# clear environment 
rm(list = ls())

# load packages
library(corrplot)
library(dplyr)
library(ggplot2)

# load in data (you might have to adjust your path here)
data <- read.csv("datasets/games_may2024_full.csv",
                 na.strings = c(NA, NaN, " ", ""))



# check variables
colnames(data)


# check for missing values
# function to count missing values in a column
check_na <- function(column) sum(is.na(column))

# check_na-function is applied to every column of the dataset, the counts are divided by the total number of observations (dim(data)[1]) to get fractions
sapply(data, check_na) / dim(data)[1]

# it is not very readable, we can round the results on 4 digits
round(sapply(data, check_na) / dim(data)[1], 4)

# we can save the result as dataframe
missing_percentages <- as.data.frame(round(sapply(data, check_na) / dim(data)[1], 4))

# you can give the one column a nicer name
names(missing_percentages) <- c("missing_percent")

# add the indices as columns
missing_percentages$variable <- row.names(missing_percentages)

# now you can change the indices to numbers
rownames(missing_percentages) <- 1:nrow(missing_percentages)

# now we can sort the column with the missing values so we get a better idea about the missings
missing_percentages <- missing_percentages[order(missing_percentages$missing_percent, decreasing = TRUE), ]

# now we have a nice overview over how many missings each variable has


# if we want to deal with the missing values, there are multiple ways
# for simplicity I would only expect you to delete rows with missings
# (another way would e.g. be imputation)

# first we remove the variables with too many missings

# we could for example say that variables with more than 10% missings are ok to remove, if we do not need these variables
var_to_remove <- missing_percentages$variable[missing_percentages$missing_percent > 0.1]
# now we have a list of variables with a high number of missings, we can check this list
print(var_to_remove)
# none of these variables are relevant for our later tasks, so we can remove them
# in general it makes sense to remove all variables that we do not need, but for now it is fine to not go through every single variable
# let us remove the variables
data <- data[, !names(data) %in% var_to_remove]


# subsequently we remove all rows with missings in this case (in real world applications other procedures may make more sense)
# let us also check how many observations we still have
print(nrow(data))
data <- data[complete.cases(data), ]
print(nrow(data))
# we lost about 4000 rows, given the size of the dataset it is ok for now
# in practice you would discuss more about which variables to keep in general and how to deal with missing values

# this is a relatively difficult case of missings, your datasets will be more tame in this regard, but you will be able to get information about how to deal with a lot of things from this notebook



# 2. Data exploration-----------------------------------------------------------

# let us get some descriptives
dim(data)

summary(data)

# correlation plot
data_cor <- cor(select_if(data, is.numeric))
corrplot(data_cor)



# Task 1------------------------------------------------------------------------
# Show recent developments in gaming by filtering the data for recent games and 
# plotting the metacritic scores on a timeline. Can you make out a trend? 
# Discuss!

# filter dataset for recent games, let's say from 2020 onwards
summary(data$release_date)

# transform date-variable to date-datatype
data$release_date <- as.Date(data$release_date)

# filter for video games that released from 2020 onwards
data_task_1 <- data[data$release_date >= "2020-01-01", ]


# plot the metacritic scores over time
data_task_1 %>% ggplot(aes(x = release_date, y = metacritic_score)) +
  geom_point()

# filter for video games that have a score above 0 (as 0 was given as 
# an NA in this case)
data_task_1[data_task_1$metacritic_score != 0, ] %>% ggplot(aes(x = release_date, y = metacritic_score)) +
  geom_point()

# make the graph nicer
data_task_1[data_task_1$metacritic_score != 0, ] %>% ggplot(aes(x = release_date, y = metacritic_score)) +
  geom_point(col = "cornflowerblue") +
  theme_bw() +
  ylim(c(0, 100)) +
  xlab("Release date") + ylab("Metacritic Score") +
  ggtitle("Metacritic scores over time")


# you can export your graphs
png("graphic_1.png")
data_task_1[data_task_1$metacritic_score != 0, ] %>% ggplot(aes(x = release_date, y = metacritic_score)) +
  geom_point(col = "cornflowerblue") +
  theme_bw() +
  ylim(c(0, 100)) +
  xlab("Release date") + ylab("Metacritic Score") +
  ggtitle("Metacritic scores over time")
dev.off()



# Task 2------------------------------------------------------------------------
# Conduct a regression analysis with metacritic_score as the independet variable 
# and recommendations as the dependent variable. Interpret the results. 
# Do consumers have "good taste"? Discuss!

# univariate regression of metacritic_score on recommendations
reg_1 <- lm(recommendations ~ metacritic_score, data = data_task_1[data_task_1$recommendations != 0, ])
summary(reg_1)

# add more variables to try to explain the number of recommendations a game receives
reg_2 <- lm(recommendations ~ metacritic_score + num_reviews_total, 
            data = data_task_1[data_task_1$recommendations != 0, ])
summary(reg_2)


reg_3 <- lm(recommendations ~ price, 
            data = data_task_1[data_task_1$recommendations != 0, ])
summary(reg_3)


reg_4 <- lm(recommendations ~ price + num_reviews_total, 
            data = data_task_1[data_task_1$recommendations != 0, ])
summary(reg_4)

# we can see that the effect of price gets smaller when we include num_revies_total as proxy for number of purchases, since number of purchases is not in the data... the effect of price might even disappear entirely if we had the number of purchases as a variable

reg_5 <- lm(recommendations ~ price + num_reviews_total + median_playtime_forever, 
            data = data_task_1[data_task_1$recommendations != 0, ])
summary(reg_5)

# some games might be short but very recommendable


# You will have to link concepts from the lecture to the data and explain whether 
# you think you can see these concepts in the data. Focus on what you have learned 
# in the lecture and try to tell a story. This will probably become clearer once 
# you get your exercises, but you can also feel free to ask what it means once you 
# have read them. The important thing is to elaborate using the concepts from the 
# lecture, the data analysis part is (unfortunately) only a minor aspect of the 
# tasks you are given. 








