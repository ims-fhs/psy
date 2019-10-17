imsbasics::clc()
library(arulesViz)

source("r/00-psy-import.r")

arules::apriori(df)

library(dplyr)
df <- df[!is.na(df$impact_on_work), ]
iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
table(df$impact_on_work)
# V1: Impact auf Work "spürbar"
impact_on_work <- !as.character(df$impact_on_work) %in% c("Normal", "Satisfactory")
iss <- cbind(iss, impact_on_work)

# iss <- iss[1:1000, ]

iss <- as.data.frame(sapply(iss, as.logical))

iss.apriori <- arules::apriori(iss, parameter = list(support = 0.0001))
arules::inspect(iss.apriori)

# Häufige Mengen innerhalb "sick"
iss <- iss[iss$impact_on_work, -ncol(iss)]
iss.eclat <- arules::eclat(iss, parameter = list(support = 0.05))
arules::inspect(iss.eclat)

# V2: Impact auf Work "stark spürbar"
iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
sick <- !as.character(df$impact_on_work) %in% c("Normal", "Satisfactory", "Impaired")
iss <- cbind(iss, sick)
# iss <- iss[1:1000, ]

iss <- as.data.frame(sapply(iss, as.logical))

iss.apriori <- arules::apriori(iss, parameter = list(support = 0.00006))
arules::inspect(iss.apriori)

# Häufige Mengen innerhalb "sick"
iss <- iss[iss$sick, -ncol(iss)]
iss.eclat <- arules::eclat(iss, parameter = list(support = 0.05))
arules::inspect(iss.eclat)

# V3: Impact auf Work "On Sick Leave/Absent"
iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
sick <- as.character(df$impact_on_work) == "On Sick Leave/Absent"
iss <- cbind(iss, sick)
# iss <- iss[51:1000, ]

iss <- as.data.frame(sapply(iss, as.logical))

iss.apriori <- arules::apriori(iss, parameter = list(support = 0.00005))
arules::inspect(iss.apriori)

# Häufige Mengen innerhalb "sick"
iss <- iss[iss$sick, -ncol(iss)]
iss.eclat <- arules::eclat(iss, parameter = list(support = 0.05))
arules::inspect(iss.eclat)


