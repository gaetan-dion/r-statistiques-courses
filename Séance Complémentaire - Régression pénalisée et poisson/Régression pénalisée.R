library(tidyverse)
library(ROCR)
install.packages("glmnet")
library(glmnet)

# read dataset
df = read_delim('/cloud/project/dataset/bank-additional-full.csv', delim=";", col_names = TRUE, quote = "\"")

df.format = df %>% mutate(target = case_when(y == "yes" ~ 1, y == "no" ~ 0))
df.format$y <- NULL


# split train / test
set.seed(123)
index = sample(nrow(df.format), nrow(df.format)*.7)

train <- df.format[index, ]
test <- df.format[-index, ]


# train classic logistic regression
fit = glm(target ~ ., data=train, family=binomial)
summary(fit)
# test model
test$pred = predict(fit, test, type="response")

# AUC
pr <- prediction(test$pred, test$target)
performance(pr, measure = "auc")@y.values[[1]]
        

                                  
# train penalized regression

# split train / test
set.seed(123)
index = sample(nrow(df.format), nrow(df.format)*.7)

train <- df.format[index, ]
test <- df.format[-index, ]

# create matrix of predictors and transform categorical variables to dummy variables
x <- model.matrix(target~., train)[,-1]
y <- train$target

# train models with kfold / try different lambda values
elasticn <- cv.glmnet(x, y, alpha=0.5, family=binomial, nfolds=3)
# lambda value which give the most accurate model, and minimize error
plot(elasticn)

elasticn$lambda.min
# lambda value which minimize error + find the simplest model
elasticn$lambda.1se

# display coefficients
cbind(coef(elasticn, elasticn$lambda.min), coef(elasticn, elasticn$lambda.1se))

# train final model with optimzed lambda
finalmodel <- glmnet(x, y, alpha = 0.5, family = "binomial", lambda = elasticn$lambda.min)

# predict model
x.test <- model.matrix(target ~., test)[,-1]
test$pred <- predict(finalmodel, x.test, type="response")

# AUC
pr <- prediction(test$pred, test$target)
performance(pr, measure = "auc")@y.values[[1]]
