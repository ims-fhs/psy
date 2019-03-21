imsbasics::clc()
library(tidyverse)
library(ggplot2)

df_raw <- readxl::read_xlsx(path = "data/All Cases 2018.xlsx", sheet = "ICAS210219")
df <- df_raw
df$impact_on_work <- as.factor(df$impact_on_work)
df$impact_on_work <- factor(df$impact_on_work, levels(df$impact_on_work)[c(2,4,1,5,3)])

ggplot(df %>% group_by(gender, impact_on_work) %>%
         summarise (n = n()) %>%
         mutate(percentage = 100 * n / sum(n))
       ,aes(x=impact_on_work, y = percentage, fill = gender))+geom_bar(stat = "identity", position = "dodge")


issues <- sort(c("alcohol", "divorce", "work", "conflict", "violence", "trauma", "tax", "stress", "relationship", "redundancy", "mobbing"), decreasing = FALSE)
sapply(issues, function(x) df <<- df %>% mutate(!!x := grepl(x, df$issues_services, ignore.case = TRUE)))

for(i in seq_along(issues)) {
  df1 <- df %>%
           group_by_(issues[i], "impact_on_work") %>%
           summarise (n = n()) %>%
           mutate(percentage = 100 * n / sum(n))
  print(ggplot(df1,aes_string(x="impact_on_work", y = "percentage", fill = issues[i]))+geom_bar(stat = "identity", position = "dodge"))
}

