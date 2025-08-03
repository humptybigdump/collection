df <- data.frame()

for (n in 2:99) {
  consistent <- round(1:n/n, 2)
  for (i in 1:99/100) {
    color <- ifelse(is.element(i, consistent), "white", "black")
    df <- rbind(df, c(n,i,color))
  }
}

colnames(df) <- c("sample_size", "decimal_place", "color")

df$sample_size <- as.numeric(df$sample_size)
df$decimal_place <- as.numeric(df$decimal_place)

library(ggplot2)

ggplot(data = df, aes(x = sample_size, y = decimal_place, fill = color)) +
  geom_tile() +
  scale_fill_manual(values = c('white' = "#FFFFFF", 'black' = "#000000")) +
  #scale_x_discrete(labels = letter_labs, name = '') +
  #scale_y_discrete(labels = letter_labs, name = '') +
  theme(legend.position = 'none')
