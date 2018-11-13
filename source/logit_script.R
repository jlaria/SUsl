data = read.csv("source/Social_Network_Ads.csv")
rownames(data) = data$User.ID
data$User.ID = NULL

data$Purchased = factor(ifelse(data$Purchased==1, "yes", "no"))
save(data, file = "source/Ads.RData")

X = model.matrix(Purchased~., data)
head(X)

fit.logit = glm(Purchased~., data = data, family = "binomial")
summary(fit.logit)

source("https://raw.githubusercontent.com/jlaria/dsclass/master/R/prop.split.R")

# Split the data
idx = prop.split(400, c(0.6, 0.4))
data.train = data[idx[[1]],]
data.validate = data[idx[[2]],]

# Train the model
fit.logit = glm(Purchased~., data.train, family = "binomial")
summary(fit.logit)
# Select variables
fit.logit = glm(Purchased~Age+EstimatedSalary, data.train, family = "binomial")
summary(fit.logit)

# Validate it
pred = predict(fit.logit, newdata = data.validate, type = "response")
y_pred = ifelse(pred>0.7, "yes", "no")
t = table(y_pred, data.validate$Purchased); t
ccr = sum(diag(t))/sum(t); ccr

load("source/data_suic.RData")

fit.logit = glm(INT_ACTUAL~., data, family = "binomial")
summary(fit.logit)
pred = predict(fit.logit, newdata = data, type="response")
y_pred = ifelse(pred>0.5, 1, 0)
t = table(y_pred, data$INT_ACTUAL); t
ccr = sum(diag(t))/sum(t); ccr

# Filter significant variables
fit.logit = glm(INT_ACTUAL~., data, family = "binomial")
s = summary(fit.logit)
predictors = (s$coefficients[,4] < 0.05)[-1]

data = data[, c(predictors,T)]

# Split the data set
idx = prop.split(828, p=c(0.6,0.4))
train = data[idx[[1]], ]
validate = data[idx[[1]],]

# Fit the model
fit.logit = glm(INT_ACTUAL~., train, family = "binomial")
summary(fit.logit)

# Predict
pred = predict(fit.logit, newdata = validate, type = "response")
y_pred = ifelse(pred>0.5, 1, 0)
t = table(y_pred, validate$INT_ACTUAL); t
ccr = sum(diag(t))/sum(t); ccr
tpr = t[2,2]/sum(t[,2]); tpr



y_pred = ifelse(pred>0.35, 1, 0)
t = table(y_pred, validate$INT_ACTUAL); t
ccr = sum(diag(t))/sum(t); ccr
tpr = t[2,2]/sum(t[,2]); tpr

y_pred = ifelse(pred>0.15, 1, 0)
t = table(y_pred, validate$INT_ACTUAL); t
ccr = sum(diag(t))/sum(t); ccr
tpr = t[2,2]/sum(t[,2]); tpr

y_pred = ifelse(pred>0.05, 1, 0)
t = table(y_pred, validate$INT_ACTUAL); t
ccr = sum(diag(t))/sum(t); ccr
tpr = t[2,2]/sum(t[,2]); tpr

library(ROCR)
pred = prediction(pred, validate$INT_ACTUAL)
perf = performance(pred, "tpr", "fpr")

perf_df = data.frame(
  x = perf@x.values,
  y = perf@y.values,
  cutoff = perf@alpha.values
)

colnames(perf_df) = c("fpr", "tpr", "cutoff")
ggplot(perf_df)+
  aes(x = fpr, y = tpr, color = cutoff)+
  geom_line()+
  geom_abline(intercept = 0, slope = 1, lty = "dotted")

perf = performance(pred, "acc")
max(perf@y.values[[1]])
optimal.cutoff = perf@x.values[[1]][which.max(perf@y.values[[1]])]
optimal.cutoff
