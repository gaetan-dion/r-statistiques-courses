library(tidyverse)

#install.packages("tidyverse", repos='http://cran.r-project.org') 
#install.packages("corrplot" ,repos='http://cran.r-project.org')

df = read_csv('/cloud/project/dataset/listings_summary_dec18.csv')

median(df$price)
mn <- mean(df$price)
mn
var(df$price)
std <- sd(df$price)
std

a = c(NA,2,3)
sum(is.na(a))

# 142% largement > à 50% => forte dispersion
(std / mn)*100

###################################
# exemple d'affichage d'un box plot
###################################

ggplot(df) +
  aes(x = neighbourhood, y = price) +
  geom_boxplot(na.rm = TRUE, fill="lightblue", color="darkblue", alpha=0.6) +
  coord_flip(ylim=c(0, 2000)) +
  scale_y_continuous(breaks = seq(0, 2000, 200)) +
  ggtitle("Prix des airbnb") +
  xlab("Quartiers") +
  ylab("Prix")

########################
# matrice de correlation
########################

library(corrplot)
mat <- df %>% select_if(is.numeric) %>% cor()

head(round(mat, 2))
