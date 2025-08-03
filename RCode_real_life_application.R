library(pROC)
library(randomForest)
library(dplyr)
library(ggplot2)

total = 30000
successes = 15000

sample_data <-
  data.frame(
    target = as.factor(c(rep(1,successes), rep(0, total - successes))),
    is_male = as.factor(c(sample(c(0,1), successes, replace = TRUE, prob=c(0.9,0.1)), sample(c(0,1), total- successes, replace = TRUE, prob=c(0.5,0.5)))),
    shoe_size = round(rnorm(total, mean = 40, sd = 5),0),
    is_urban = c(sample(c("yes","no"), successes, replace = TRUE, prob=c(0.7,0.3)), sample(c("yes","no"), total - successes, replace = TRUE, prob=c(0.5,0.5))),
    age = c(round(runif(successes, min = 17, max = 45), 0), round(runif(total - successes, min = 15, max = 89), 0)),
    bundesland = c(rep("NRW", successes / 4), 
                   sample(c("NRW", "BW", "BY", "HS", "SL", "SH", "RP", "NS", "HB", "HH", "BE", "BR", "MV", "SA", "SC", "TH"),
                          successes / 4 * 3  + total - successes,
                          replace = TRUE,
                          prob = c(5, 11, 13, 6, 1, 3, 4, 8, 0.7, 2, 4, 2.5, 1.6, 2.2, 4, 2)))
  )



sampler <- sample(c("train", "test", "validation"), replace = TRUE, total)
model <- randomForest(target ~ ., data = sample_data[sampler == "train",], importance=TRUE)

pred <- predict(model, newdata = sample_data[sampler == "validation", ], type = "prob")


plot(roc(as.numeric(sample_data[sampler == "validation", "target"]) - 1, pred[,2], direction="<"),
     col="darkblue", lwd=2,
     type="l", main="ROC - Validation", print.auc = TRUE, grid = TRUE)


varImpPlot(model, type = 1, main = "Most Important Features")


sample_data %>%
  group_by(bundesland) %>%
  summarize(total = n(), orders = sum(target == 1)) %>%
  mutate(cr = orders/total)


g <- ggplot(sample_data, aes(bundesland))
g + geom_bar(aes(fill = target)) + 
  theme_bw() + 
  ylab("Number of Golden Flyers sent")

 
sample_data <- sample_data[sample_data$bundesland != "NRW", ]
sampler <- sample(c("train", "test", "validation"), replace = TRUE, nrow(sample_data))
model1 <- randomForest(target ~ ., data = na.omit(sample_data[sampler == "train",]), importance=TRUE, ntree = 500, mtry = 3)
model2 <- randomForest(target ~ ., data = na.omit(sample_data[sampler == "train",]), importance=TRUE, ntree = 500, mtry = 5)
model3 <- randomForest(target ~ ., data = na.omit(sample_data[sampler == "train",]), importance=TRUE, ntree = 500, mtry = 2)


plot(roc(as.numeric(sample_data[sampler == "validation", "target"]) - 1,
         predict(model1, newdata = sample_data[sampler == "validation", ], type = "prob")[,2], 
         direction="<"),
     col="darkblue", lwd=2,
     type="l", main="ROC für Random Forest", print.auc = TRUE, grid = TRUE)
plot(roc(as.numeric(sample_data[sampler == "validation", "target"]) - 1,
         predict(model2, newdata = sample_data[sampler == "validation", ], type = "prob")[,2], 
         direction="<"),
     col="firebrick", lwd=2,
     type="l", print.auc = TRUE, add = TRUE, print.auc.y = 0.45)
plot(roc(as.numeric(sample_data[sampler == "validation", "target"]) - 1,
         predict(model3, newdata = sample_data[sampler == "validation", ], type = "prob")[,2], 
         direction="<"),
     col="darkgreen", lwd=2,
     type="l", print.auc = TRUE, add = TRUE, print.auc.y = 0.40)  
  



true_positive_rate <- function(y, model){
prediction_df <-
  data.frame( prediction = predict(model, newdata = sample_data[sampler == "validation", ], type = "prob")[,2] >= y,
              true = sample_data[sampler == "validation", "target"])

tp = sum((prediction_df$true == 1)*(prediction_df$prediction == TRUE))
fp = sum((prediction_df$true == 0)*(prediction_df$prediction == TRUE))
return(c(precision = tp/(tp + fp), size = sum(prediction_df == TRUE)))
}
true_positive_rate(0, model1)

idx = seq(from = 0, to = 0.95, by = 0.01)
tpr_res <- 
rbind(as.data.frame(t(sapply(idx, true_positive_rate, model = model1))),
      as.data.frame(t(sapply(idx, true_positive_rate, model = model2))),
      as.data.frame(t(sapply(idx, true_positive_rate, model = model3))))

tpr_res$name <- c(rep("model1", nrow(tpr_res)/3),
                  rep("model2", nrow(tpr_res)/3),
                  rep("model3", nrow(tpr_res)/3))


h <- ggplot(tpr_res, aes(x=size / 9195, y=precision, group = name, color = name))
h + geom_line()+
  scale_color_manual(values = c("darkblue", "firebrick", "darkgreen")) +
  theme_bw() + 
  ylab("Precision") + 
  xlab("Proportion of Customers Contacted")


### Sources
# https://blog.revolutionanalytics.com/2016/08/roc-curves-in-two-lines-of-code.html