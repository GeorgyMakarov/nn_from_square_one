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

# Split data into training and testing set --------------------------------
set.seed(123)
idx    <- sample(x = 1:nrow(df), size = 0.8 * nrow(df), replace = F)
tr_set <- df[idx, ]
te_set <- df[-idx, ]
rm(idx)


# Preprocess data ---------------------------------------------------------


