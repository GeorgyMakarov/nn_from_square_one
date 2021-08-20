# Implement binary classification neural network with a single hidden layer.
# Evaluate classification performance using planar data and compare it to
# logistic regression performance.
library(dplyr)


# Prepare initial data ----------------------------------------------------


# We use a subset of `iris` dataset for training and testing. Versicolor and
# virginica intersect non-linearly by sepal length and sepal width.
data("iris")
plot(x     = iris$Sepal.Length[iris$Species == "virginica"],
     y     = iris$Sepal.Width[iris$Species == "virginica"],
     col   = "dodgerblue1",
     pch   = 19,
     frame = F,
     xlab  = "x1",
     ylab  = "x2")
points(x   = iris$Sepal.Length[iris$Species == "versicolor"],
       y   = iris$Sepal.Width[iris$Species == "versicolor"],
       col = "green",
       pch = 19)

mydata <- 
  iris %>% 
  filter(Species %in% c("virginica", "versicolor")) %>% 
  select(c("Sepal.Length", "Sepal.Width", "Species"))

rm(iris)


# Setup baseline accuracy from random guessing. Lets assume we want to predict
# versicolor species -- 1, virginica then is 0. Since there are 50 versicolor
# items out of 100 observations the baseline accuracy is 50 / 100 = 0.5. Hence
# out algorithm must perform at least better than 50%.
guess_acc <- 
  mydata %>% 
  group_by(Species) %>% 
  summarize(count = n()) %>% 
  mutate(weight = count / sum(count)) %>% 
  filter(Species == "versicolor") %>% 
  select(weight) %>% 
  as.numeric()


# Shuffle the dataset, so that different species are mixed to avoid bias from
# ordered initial data. Randomly split the dataset into training and testing.
set.seed(123)
shuffle  <- sample(x = 1:nrow(mydata), size = nrow(mydata), replace = F)
mydata <- mydata[shuffle, ]
rm(shuffle)

mydata$y <- 0
mydata$y[mydata$Species == "versicolor"] <- 1
mydata           <- mydata %>% select(-Species)
colnames(mydata) <- c("x1", "x2", "y")

set.seed(123)
in_train <- sample(x = 1:nrow(mydata), size = 0.8 * nrow(mydata), replace = F)
train_set <- mydata[in_train, ]
test_set  <- mydata[-in_train, ]
rm(in_train)


# Prior to building NN consider logistic regression performance on the data. 
# We use the earlier prepared model from `logistic_regression_nn.R`. We have to
# separate the variables from the response for our model. The response must be
# a numeric vector of [0; 1].
source("logistic_regression_nn.R")
logistic_out <- log_nn(x_train = train_set[, 1:2],
                       y_train = train_set$y,
                       x_test  = test_set[, 1:2],
                       y_test  = test_set$y,
                       n_iter  = 2000,
                       lr      = 0.005)


# Logistic regression performs not really well on non-linear data. The accuracy
# on training is 48.75% -- worse than random guessing, but the accuracy on test
# is slightly better than random guessing.
logistic_out$accuracy$train_acc
logistic_out$accuracy$test_acc


