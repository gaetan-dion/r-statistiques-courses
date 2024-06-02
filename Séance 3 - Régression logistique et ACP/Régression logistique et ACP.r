#install.packages("factoextra" ,repos='http://cran.r-project.org')
library(tidyverse)
library(ROCR)

df = read_delim('/cloud/project/dataset/bank-additional-full.csv', delim=";", col_names = TRUE, quote = "\"")

df.format = df %>% mutate(target = case_when(y == "yes" ~ 1, y == "no" ~ 0))
df.format$y <- NULL

#######################
# Régression logistique
#######################

# split train / test
set.seed(123)
index = sample(nrow(df.format), nrow(df.format)*.7)

train <- df.format[index, ]
test <- df.format[-index, ]

# plot logistic regression with one variable : duration with target
fitx = glm(target ~ duration, data=train, family=binomial(logit))

# build dataframe with one variable to plot regression
xdim <- data.frame(duration=seq(min(test$duration), max(test$duration), len=12357))
xdim$pred = predict(fitx, xdim, type="response")

# graphique par rapport aux prédictions 0/1
plot(target ~ duration, data=test, col="blue2")
lines(pred ~ duration, xdim, col="red4", lwd=2)

# graphique par rapport à la prédiction de 0 à 1
# 435 / 41118 > 1250 de duration, soit 1% qui ont plus de 75% de chance de souscrire avec un entretien supérieur à 1250 sec
ggplot(xdim, aes(x=duration, y=pred)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

# | VN | FP |
# -----------
# | FN | VP |
table(test$target, xdim$pred>0.9995)
# colonnes => prédictions
# lignes => valeurs réelles


# train model with all variables
fit = glm(target ~ ., data=train, family=binomial)
# test model
test$pred = predict(fit, test, type="response")
# confusion matrix
table(test$target, test$pred>0.3)

test %>% select(target, pred) %>% arrange(desc(pred))


# rocr package
pr <- prediction(test$pred, test$target)
# roc curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)


# precision / recall
prf <- performance(pr, measure = "prec", x.measure = "rec")
plot(prf)


# auc
auc <- performance(pr, measure = "auc")

# @ pour sélectionner un élément d'une liste
# $ pour les éléments d'un dataframe
auc <- auc@y.values[[1]]
auc


########################
# Analyses Factorielles
########################

library("FactoMineR")
library("factoextra")

df = df.format %>% select_if(is_numeric)

# pca centré réduit
pca = prcomp(df, center = TRUE, scale = TRUE)

# variance cumulée
eig.val <- get_eigenvalue(pca)
plot(eig.val$cumulative.variance.percent, col="blue", lwd=2, type = "l")

# variance expliquée par chaque axe
fviz_screeplot(pca, ncp=8, addlabels = TRUE)

# contribution de chaque variable pour l'axe 1
fviz_contrib(pca, choice = "var", axes = c(1))

# cercle des corrélations
fviz_pca_var(pca, col.var="contrib", axes = c(3,4)) + 
  scale_color_gradient2(low="white", mid="blue", high="red", midpoint=6) + theme_minimal()

# création d'un nouveau dataframe avec les axes principaux
pca_df <- data.frame(cbind(pca$x[,1:8], target=df$target))

