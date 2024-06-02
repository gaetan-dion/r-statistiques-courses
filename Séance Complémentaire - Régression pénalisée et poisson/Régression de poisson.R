library(tidyverse)

# Exemple avec le risque de crédit

df_bank = read_csv('/cloud/project/dataset/loan_risk.csv')

ggplot(df_bank, aes(x=log(df_bank$creddebt), y=df_bank$income))+
  geom_point() 

# Réciprocité ln / exp
logv = log(df_bank$income[1])
logv

exp(logv)



# Régression de poisson

df_math = read_csv("dataset/competition_awards_data.csv")

ggplot(df_math, aes(x=df_math$mathScore, y=df_math$awards))+
  geom_point() 

set.seed(256)
index = sample(nrow(df_math), nrow(df_math)*.7)

train <- df_math[index, ]
test <- df_math[-index, ]

pois <- glm(awards~mathScore, family="poisson", data=train) 
summary(pois)

# Pseudo R2
# Amélioration de l'ajustement (déviance) du modèle par "pire" modèle (juste la constante)
1 - pois$deviance / pois$null.deviance

# Dispersion (ratio < 1)
pois$deviance / 198

test$pred = predict.glm(pois, test, type="response")

ggplot(test, aes(x=mathScore)) +
  geom_point(aes(y=awards), color="blue") +
  geom_point(aes(y=pred), color="red")

# RMSE
sqrt(mean((test$pred - test$awards)^2))


# Test pour vérifier si notre variable suit une distribution de poisson 
mean(df_math$awards)

ggplot(df_math) +
  geom_histogram(aes(awards), fill="blue") +
  geom_histogram(aes(rpois(200, 0.63), fill="red"))


