
df_bank = read_csv('/cloud/project/dataset/loan_risk.csv')

# linear regression employ
ggplot(df_bank) +
  aes(x = employ, y = income) +
  geom_smooth(method = "lm") +
  geom_point() +
  xlab("Types d'emplois") +
  ylab("Revenus")

# r2
cor(df_bank$employ, df_bank$income) ^ 2

# linear reg model
lin_reg <- lm(df_bank$income ~ df_bank$employ)
summary(lin_reg)

# r2 with credit debts and others debts
cor(df_bank$othdebt, df_bank$income) ^ 2
cor(df_bank$creddebt, df_bank$income) ^ 2

# add credit and other debt
df_bank <- df_bank %>% mutate(alldebt = creddebt + othdebt)

# r2 all debts
cor(df_bank$alldebt, df_bank$income) ^ 2

# plot linear regression with all debts
ggplot(df_bank) +
  aes(x = alldebt, y = income) +
  geom_smooth(method = "lm") +
  geom_point() +
  xlab("Dettes") +
  ylab("Revenus")


## create model train / test
## multiple linear regression

set.seed(256)
index = sample(nrow(df_bank), nrow(df_bank)*.7)

train <- df_bank[index, ]
test <- df_bank[-index, ]

lin_reg <- lm(train$income ~ ., train)
summary(lin_reg)

test$pred <- predict(lin_reg, test)

# RMSE
summary(test$income)

# remove null value
test_nona = drop_na(test, pred)
# calculate error
error <- test_nona$income - test_nona$pred

# mae <= rmse
# rmse = mae si les erreurs ont toutes la même magnitude
# si rmse est plus grand alors il y a une variance importante dans les erreurs
# rmse donne plus d'importance aux grosses erreurs
rmse(error)
mae(error)


# Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
