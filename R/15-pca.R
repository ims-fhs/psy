imsbasics::clc()
# install.packages("logisticPCA")
library(logisticPCA)

source("r/00-psy-import.r")


library(dplyr)
df <- df[!is.na(df$impact_on_work), ]
iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
table(df$impact_on_work)
# V1: Impact auf Work "spürbar"
impact_on_work <- !as.character(df$impact_on_work) %in% c("Normal", "Satisfactory")
iss <- cbind(iss, impact_on_work)

pca <- logisticPCA(iss)

plot(pca)
print(pca)
