imsbasics::clc()
library(tree)

source("r/00-psy-import.r")

library(dplyr)
df <- df[!is.na(df$impact_on_work), ]
iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
table(df$impact_on_work)
# V1: Impact auf Work "spürbar"
impact_on_work <- !as.character(df$impact_on_work) %in% c("Normal", "Satisfactory")
iss <- cbind(iss, impact_on_work)
male <- as.numeric(df$gender) - 1
iss <- cbind(iss, male)

iss <- as.data.frame(sapply(iss, as.logical))

library(xgboost)
xgboost(as.matrix(iss))
?xgboost
