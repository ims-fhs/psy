
# https://www.statmethods.net/advstats/cart.html
imsbasics::clc()
library(tidyverse)
library(ggplot2)

df_raw <- readxl::read_xlsx(path = "data/All Cases 2018.xlsx", sheet = "ICAS210219")
df <- df_raw
df$impact_on_work <- as.factor(df$impact_on_work)
df$impact_on_work <- factor(df$impact_on_work, levels(df$impact_on_work)[c(2,4,1,5,3)])

issues <- sort(c("alcohol", "divorce", "work", "conflict", "violence", "trauma", "tax", "stress", "relationship", "redundancy", "mobbing"), decreasing = FALSE)
sapply(issues, function(x) df <<- df %>% mutate(!!x := grepl(x, df$issues_services, ignore.case = TRUE)))

df <- df[df$impact_on_work != "Normal", ]

library(rpart)

df_train <- df[sample(1:nrow(df),200), ]
fit <- rpart(impact_on_work ~ gender + alcohol + conflict + divorce + mobbing + redundancy + relationship + stress + tax + trauma + violence + work, method = "class", data = df_train)
print(fit)
plot(fit, uniform=TRUE, 
     main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

fit <- glm(impact_on_work~ gender + alcohol + conflict,data=df,family=binomial())
summary(fit)
plot(fit)


