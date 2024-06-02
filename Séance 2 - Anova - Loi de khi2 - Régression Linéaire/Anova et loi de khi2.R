#install.packages("ROCR" ,repos='http://cran.r-project.org')
library(tidyverse)
library(corrplot)
library(ROCR)

df = read_delim('/cloud/project/dataset/bank-additional-full.csv', delim=";", col_names = TRUE, quote = "\"")

head(df, 4)

df.format = df %>% mutate(target = case_when(y == "yes" ~ 1, y == "no" ~ 0))
df.format$y <- NULL

df.format %>% group_by(target) %>% count()
ratio = df %>% group_by(y) %>% count()
ratio
ratio$n[2] / df %>% count()

df.format = df.format %>% mutate(jobs = as.numeric(factor(job)))
df.format$job = NULL

summary(df2.format)


#########################
# Matrice de corrélation
#########################
matrix_corr <- df.format %>% select_if(is.numeric) %>% cor(method = "pearson")

col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))
corrplot(matrix_corr, method="color", col=col(200), type="upper", tl.cex = 0.8,
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         diag=FALSE # Cacher les coefficients de corrélation sur la diagonale
)

##########
# Anova
##########
fit <- aov(age ~ marital + loan, df.format)
summary(fit)

df.format %>% select(marital) %>% distinct

#####################
# Khi2 / Cramer's V
#####################

# création du tableau de contingence
tcontingency = table(df$housing, df$loan)
tcontingency

#Test du chi square
chi2 = chisq.test(tcontingency)
chi2

# récupération du minimum entre le nombre de colonnes ou lignes, moins 1
min_rowCol = min(nrow(tcontingency), ncol(tcontingency)) - 1

#V de cramer 
#=> racine carrée du khi2 divisé par l'effectif total * le minimum de colonnes ou lignes
sqrt(chi2$statistic / (sum(tcontingency) * min_rowCol))


#cramerv : education + job => 0.3598
#cramerv : housing + loan => 0.7078
