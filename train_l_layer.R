rm(list = ls())


# Local dependencies -----------------------------------------------------------
pa <- "./activation_helpers"
ph <- "./nn_helpers"

acts  <- list.files(path = pa)
hlps <- list.files(path = ph)

invisible(lapply(X = acts, FUN = function(x){source(paste0(pa, '/', x))}))
invisible(lapply(X = hlps, FUN = function(x){source(paste0(ph, '/', x))}))

rm(pa, ph, acts, hlps)


# Prepare raw data for classification ------------------------------------------
df <- iris[iris$Species %in% c("virginica", "versicolor"), ]
df <- df[, c(1, 2, 5)]

set.seed(123)
idx <- sample(x = 1:nrow(df), size = nrow(df), replace = F) ## shuffle data
df  <- df[idx, ]
rm(idx)

df$y[df$Species == "virginica"]  <- 0
df$y[df$Species == "versicolor"] <- 1
df <- df[, c(1, 2, 4)]
colnames(df) <- c('x1', 'x2', 'y')


# Split data into training and testing set -------------------------------------
set.seed(123)
idx    <- sample(x = 1:nrow(df), size = 0.8 * nrow(df), replace = F)
tr_set <- df[idx, ]
te_set <- df[-idx, ]
rm(idx)


# Preprocess data --------------------------------------------------------------
x_train <- scale(tr_set[, c(1:2)])
y_train <- tr_set$y
x_test  <- scale(te_set[, c(1:2)])
y_test  <- te_set$y

# Add another dimension to output variable to convert it to matrix
dim(y_train) <- c(length(y_train), 1)
dim(y_test)  <- c(length(y_test), 1)

# Transpose matrices for better multiplication
x_train <- t(x_train)
x_test  <- t(x_test)
y_train <- t(y_train)
y_test  <- t(y_test)

rm(df, te_set, tr_set)


# Train neural network ---------------------------------------------------------
act <- c("tanh", "tanh", "sigmoid")
fit <- train_nn(x = x_train, 
                y = y_train, 
                epochs = 6000, 
                lr     = 0.9, 
                hn     = c(10, 5), 
                f      = act, 
                im     = "stand",
                lambda = 0,
                kp     = 1,
                track  = T,
                optim  = "adam")


# Predict on test set ----------------------------------------------------------
pred_fit <- predict_nn(x_test, y_test, fit, act, T, 0.5)


# Compute model accuracy --------------------------------------------------
compute_metrics(y_test, pred_fit, md_name = "neural network", show = T)

