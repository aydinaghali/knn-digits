setwd("~/Documents/DSMA_assignments/seminar1/applications1")

library(readr)
library(tidyverse)
library(class)
library(caret)
library(kknn)
library(magick)
library(ggplot2)

data <- read_csv("train.csv")
pred_set <- read_csv("test.csv")

data <- data %>% apply(2, as.numeric) %>% data.frame()
data[,1] <- factor(data[,1])
pred_set <- pred_set %>% apply(2, as.numeric) %>% data.frame()

# function to draw a digit
display_digit <- function(digit){
  d <- matrix(digit, nrow=28) %>% apply(2, as.numeric) %>% { .[,ncol(.):1] }
  image(1:28, 1:28, d)
}

# test the function
display_digit(pred_set[1,])

# split data
set.seed(123)
split_idx <- sample(1:nrow(data), size = 0.8*nrow(data))
train_set <- data[split_idx,]
test_set <- data[-split_idx,]

# look at average digits
for(i in 0:9){
  train_set[train_set$label==i,][2:785] %>% apply(2, FUN=mean) %>% display_digit()
}
 
### KNN with class

# run with different K values and see which one is best
accuracies <- data.frame()
for(i in 1:30){
  print(i)
  pred <- knn(
    train = train_set[,2:ncol(train_set)][1:5000,],
    test = test_set[,2:ncol(test_set)][1:1000,],
    cl = train_set[,1][1:5000],
    k = i
  )
  
  acc <- confusionMatrix(data=pred, reference = test_set[,1][1:1000])$overall[1] %>% `names<-`(c())
  accuracies <- rbind(accuracies, data.frame("k"=i, "accuracy"=acc))
}

plot(accuracies$k, accuracies$acc)

# run model with K=3
pred <- knn(
  train = train_set[,2:ncol(train_set)][1:33600,],
  test = test_set[,2:ncol(test_set)][1:1000,],
  cl = train_set[,1][1:33600],
  k = 3
)
# check accuracy
confusionMatrix(data=pred, reference = test_set[,1][1:1000])

### KNN with caret (97% accuracy, preferred method)

# set up the cross validation (30-fold)
ctrl.knn = trainControl(method = "repeatedcv", number = 10, repeats = 3, verbose = TRUE)

# run cv, find best K
set.seed(123)
tune.knn <- train(
  label ~ ., 
  data = train_set[sample(1:nrow(train_set), size = 2000),],
  method = "knn", 
  trControl = ctrl.knn, 
  tuneGrid = data.frame(k = 1:30)
)


# look at some plots
plot(tune.knn$results$k, tune.knn$results$Accuracy, xlab = "K", ylab="Accuracy")
ggplot(tune.knn$results, aes(x = k, group = k) ) +
  geom_errorbar(aes(ymax = Accuracy+AccuracySD*1.96, ymin = Accuracy-AccuracySD*1.96)) +
  geom_point(aes(x = k, y = Accuracy)) +
  xlab("K") + ylab("Accuracy 95% confidence interval")

# train model with best K
set.seed(123)
model.knn <- knn3(
  label ~ .,
  data = train_set,
  #k = tune.knn$bestTune$k
  k = 5
)

# predict and check accuracy
pred <- predict(model.knn, newdata = test_set[1:1000,], type="class")
confusionMatrix(data=pred, reference = test_set[,1][1:1000])

# save model to be used in the web app
saveRDS(model.knn, file="knn_model.rda")

###############

#testvec <- readRDS("web/test") %>% as.vector() * 255
#test <- data.frame(testvec) %>% t() %>% data.frame()
#colnames(test) <- paste0("pixel", 0:(28*28-1))
#display_digit(test[1,])
#predict(model.knn, newdata = test, type="class")
#predict(model.knn, newdata = test, type="prob")
#
#mat <- matrix(testvec, nrow=28, ncol=28)
#
#trim_sides <- function(mat){
#  mat <- mat[, apply(mat, 2, FUN=function(x){sum(x)>0})]
#  mat <- mat[apply(mat, 1, FUN=function(x){sum(x)>0}), ]
#  return(mat)
#}

