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

# iss.tree <- tree(impact_on_work~., data = iss)
iss.tree <- tree(impact_on_work~.-iss_burnout, data = iss)
summary(iss.tree)
plot(iss.tree)
text(iss.tree, pretty = 0)

# V2: On Sick Leave
iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
sick <- as.character(df$impact_on_work) == "On Sick Leave/Absent"
iss <- cbind(iss, sick)
# iss <- iss[51:1000, ]

iss <- as.data.frame(sapply(iss, as.logical))

iss.tree <- tree(as.factor(sick)~., data = iss)
# iss.tree <- tree(sick~.-iss_burnout, data = iss)
summary(iss.tree)
plot(iss.tree)
text(iss.tree, pretty = 0)


set.seed(101)
train <- sample(1:nrow(iss), nrow(iss)/3)

tree.iss = tree(as.factor(sick)~., data = iss, subset=train)
plot(tree.iss)
text(tree.iss, pretty=0)

tree.pred = predict(tree.iss, iss[-train,], type="class")
