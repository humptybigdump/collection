rm(list = ls())

library(ggplot2)
library(dplyr)

# Define grid for horizontal axis
y <- 0:8
# Make data frame that contains Poisson probabilities for 
# two different choices of lambda
df <- data.frame(y = y, p = dpois(y, lambda = exp(0.5)), 
                 name = "exp(0.5)") %>%
  rbind(data.frame(y = y, 
                   p = dpois(y, lambda = exp(1.3)), 
                   name = "exp(1.3)"))

# Make plot
ggplot(df, aes(x = y, y = p, fill = name)) + 
  # bar plot
  geom_bar(stat = "identity", position = "dodge") + 
  # edit plot layout
  theme_minimal() + 
  theme(legend.position = "right") + 
  scale_fill_discrete(name = expression(lambda)) + 
  xlab("y") + ylab("Probability") + 
  scale_x_continuous(breaks = y)

# Modification with more accurate legend
ggplot(df, aes(x = y, y = p, fill = name)) + 
  geom_col(position = "dodge") +
  theme_minimal() + 
  theme(legend.position = c(0.8, 0.63), legend.title = element_blank(),
        legend.background = element_rect(fill="white", size=0.2,
                                         linetype = "blank")) + 
  scale_fill_discrete(labels = c(bquote(lambda == .(unique(df$name[1]))),
                                 bquote(lambda == .(unique(df$name[2]))))) + 
  xlab("y") + ylab("Probability") + 
  scale_x_continuous(breaks = y)
