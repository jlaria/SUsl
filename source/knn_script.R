load("source/tumor.RData")

library(class)

# We separate the response from the data.
cl = df$recovery
df$recovery = NULL

cl_test = df_test$recovery
df_test$recovery = NULL

# Scale the training data
X = scale(df)

# Scale the test data
X_test = scale(df_test, 
               center = attr(X, "scaled:center"), 
               scale = attr(X, "scaled:scale")) 

# Predict the test data using the training
# kNN with k=5 neighbors
pred = knn(X, X_test, cl, k = 5)

library(ggplot2)
source("https://raw.githubusercontent.com/jlaria/dsclass/master/R/knn_viz.R")

# The training data
knn_viz(X, cl, k=5, prob_size = F)

# The test data
knn_viz(X, cl, k = 5, validate = X_test, cl_validate = cl_test, prob_size = F)

t = table(pred = pred, real = cl_test); t


train.idx = sample(nrow(df), 20)
X_train = X[train.idx, ]
X_validate = X[-train.idx, ]

cl_train = cl[train.idx]
cl_validate = cl[-train.idx]

pred_validate = knn(X_train, X_validate, cl_train, k = 5) 
t = table(pred = pred_validate, real = cl_validate); t
sum(diag(t))/sum(t)

source("https://raw.githubusercontent.com/jlaria/dsclass/master/R/knn_viz.R")
# Multiple cross-validation

nruns = 30
ccr = rep(0, nruns)
for (run in 1:nruns) {
  
  # Compute train/validate indices
  idx = prop.split(nrow(X), c(0.5, 0.5))
  
  # Split the data
  X_train = X[ idx[[1]] , ]
  cl_train = cl[ idx[[1]] ]
  
  X_validate = X[ idx[[2]], ]
  cl_validate = cl[ idx[[2]] ]
  
  # Validate the model
  pred_validate = knn(X_train, X_validate, cl_train, k = 5)
  t = table(pred_validate, cl_validate)
  
  # Save the CCR
  ccr[run] = sum(diag(t))/sum(t)
}


# 10-fold cross validation
idx = prop.split(40, rep(0.1, 10))
ccr = rep(0, 10)

for (fold in 1:10) {
  # split the data, leaving one of the folds to validate
  X_validate = X[ idx[[fold]], ]
  cl_validate = cl[ idx[[fold]] ]
  
  X_train = X[ -idx[[fold]], ]
  cl_train = cl[ -idx[[fold]] ]
  
  # Validate the model
  pred_validate = knn(X_train, X_validate, cl_train, k = 5)
  t = table(pred_validate, cl_validate)
  
  # Save the CCR
  ccr[fold] = sum(diag(t))/sum(t)
}

# 10-fold CV ccr
mean(ccr)