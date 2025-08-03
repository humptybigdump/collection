# Use data set 'badhealth' from R package 'COUNT'
# Relevant outcome variable is 'numvisit'.
library(COUNT)
data("badhealth")
?badhealth
head(badhealth)
mean(badhealth$numvisit)

# Load necessary packages
library(ggplot2)
library(dplyr)

# Define grid for horizontal axis
y <- 0:15
# Get Poisson and empirical probabilities
poisson_prob <- dpois(y, lambda = mean(badhealth$numvisit))
emp_prob <- table(badhealth$numvisit)[1:16]/
  nrow(badhealth)

# Make data frame that contains Poisson probabilities for 
# two different choices of lambda
df <- data.frame(y = y, p = poisson_prob, 
                 name = "Poisson") %>%
  rbind(data.frame(y = y, 
                   p = as.numeric(emp_prob), 
                   name = "Empirical"))

# Make plot
ggplot(df, aes(x = y, y = p, fill = name)) + 
  # bar plot
  geom_bar(stat = "identity", position = "dodge") + 
  # edit plot layout
  theme_minimal() + 
  theme(legend.position = "top") + 
  scale_fill_discrete(name = "Model") + 
  xlab("y") + ylab("Probability") + 
  scale_x_continuous(breaks = y)

# Additionally fit a geometric distribution
geom_prob <- dgeom(y, prob = 1/(1+mean(badhealth$numvisit)))
df_geom <- df %>% rbind(data.frame(y = y,
                                   p = geom_prob,
                                   name = "Geometric"))
# Produce the same plot with a third set of bars for the
# geometric distribution
ggplot(df_geom, aes(x = y, y = p, fill = name)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() + 
  theme(legend.position = "top") + 
  scale_fill_discrete(name = "Model") + 
  xlab("y") + ylab("Probability") + 
  scale_x_continuous(breaks = y)
