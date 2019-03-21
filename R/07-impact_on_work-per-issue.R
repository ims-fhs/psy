imsbasics::clc()
library(tidyverse)
library(ggplot2)

df_raw <- readxl::read_xlsx(path = "data/All Cases 2018.xlsx", sheet = "ICAS210219")
df <- df_raw

df <- df %>% 
  mutate(issues_services = map(issues_services, ~strsplit(strsplit(., ',')[[1]], ':')[[1]][1] %>% sort),
         I = row_number()) %>% 
  unnest(issues_services) %>%
  spread(issues_services, issues_services) %>% 
  mutate_at(-(1:17), ~!is.na(.)) %>% 
  select(-I)

cols <- colnames(df)[17:90]


df %>%
  group_by(`Alcohol Use/Misuse/Abuse     Alkoholmissbrauch/Alkoholsucht`, impact_on_work) %>%
  summarise (n = n()) %>%
  mutate(`Anteil [%]` = 100 * n / sum(n))

group_impact_issue <- function(data, issue_name) {
  browser()
  data <- data %>% 
    group_by_(issue_name, "impact_on_work") %>%
    summarise (n = n()) %>%
    mutate(`Anteil [%]` = 100 * n / sum(n))
  return(data)
}
group_impact_issue(df, cols[1])

plot_impact_on_work_vs_issue <- function(data, issue_name) {
  browser()
  ggplot(df %>%
           group_by(eval(issue_name), "impact_on_work") %>%
           summarise (n = n()) %>%
           mutate(`Anteil [%]` = 100 * n / sum(n))
         ,aes(x=impact_on_work, y = `Anteil [%]`, fill = eval(issue_name)))+geom_bar(stat = "identity", position = "dodge")
  
}
plot_impact_on_work_vs_issue(df, cols[1])

ggplot(df %>%
         group_by(`Alcohol Use/Misuse/Abuse     Alkoholmissbrauch/Alkoholsucht`, impact_on_work) %>%
         summarise (n = n()) %>%
         mutate(`Anteil [%]` = 100 * n / sum(n))
       ,aes(x=impact_on_work, y = `Anteil [%]`, fill = `Alcohol Use/Misuse/Abuse     Alkoholmissbrauch/Alkoholsucht`))+geom_bar(stat = "identity", position = "dodge")
