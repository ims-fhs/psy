imsbasics::clc()

library(tidyverse)
library(ggplot2)

source("r/00-psy-import.r")
df$impact_on_work <- !as.character(df$impact_on_work) %in% c("Normal", "Satisfactory")


ggplot(df, aes(lubridate::year(df$created_at))) + geom_bar() + xlab('Year')
ggplot(df %>% mutate(month = lubridate::month(df$created_at, label = TRUE)) %>% group_by(month, impact_on_work) %>%
         summarise(n = n()) %>%
         mutate(percentage = 100 * n / sum(n))
       ,aes(x=impact_on_work, y = percentage, fill = month))+geom_bar(stat = "identity", position = "dodge")
ggplot(df %>% mutate(month = lubridate::month(df$created_at, label = TRUE)) %>% group_by(month, iss_depression) %>%
         summarise(n = n()) %>%
         mutate(percentage = 100 * n / sum(n)) %>%
         filter(iss_depression == TRUE)
       ,aes(x=iss_depression, y = percentage, fill = month))+geom_bar(stat = "identity", position = "dodge")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
{
iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
iss$gender <- df$gender
iss$impact_on_work <- df$impact_on_work
iss %<>% filter(impact_on_work == TRUE) %>% select(-gender) %>% select(-impact_on_work)
iss <- colSums(iss)
iss <- as.data.frame(sort(iss, decreasing = TRUE))
iss$issue <- row.names(iss)
iss$percentage <- iss$`sort(iss, decreasing = TRUE)`
iss$percentage <- iss$percentage/sum(iss$percentage)*100
iss <- iss[1:20, -1]

ggplot(iss, aes(x=reorder(issue, -percentage), y = percentage)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
iss$gender <- df$gender
iss$impact_on_work <- df$impact_on_work
iss %<>% filter(impact_on_work == TRUE, gender == "M") %>% select(-gender) %>% select(-impact_on_work)
iss <- colSums(iss)
iss <- as.data.frame(sort(iss, decreasing = TRUE))
iss$issue <- row.names(iss)
iss$percentage <- iss$`sort(iss, decreasing = TRUE)`
iss$percentage <- iss$percentage/sum(iss$percentage)*100
iss <- iss[1:20, -1]

ggplot(iss, aes(x=reorder(issue, -percentage), y = percentage)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

iss <- df %>% select(starts_with("iss_")) %>% select(-iss_na) %>% select(-iss_)
iss$gender <- df$gender
iss$impact_on_work <- df$impact_on_work
iss %<>% filter(impact_on_work == TRUE, gender == "F") %>% select(-gender) %>% select(-impact_on_work)
iss <- colSums(iss)
iss <- as.data.frame(sort(iss, decreasing = TRUE))
iss$issue <- row.names(iss)
iss$percentage <- iss$`sort(iss, decreasing = TRUE)`
iss$percentage <- iss$percentage/sum(iss$percentage)*100
iss <- iss[1:20, -1]

ggplot(iss, aes(x=reorder(issue, -percentage), y = percentage)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
