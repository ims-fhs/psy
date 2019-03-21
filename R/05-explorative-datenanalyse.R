imsbasics::clc()
df <- readxl::read_xlsx(path = "data/All Cases 2018.xlsx", sheet = "ICAS210219")
colnames(df)
df <- df %>% mutate_if(is.character, as.factor)

plot(df$gender)
plot(df$status_of_caller)
plot(df$language_code)
plot(df$impact_on_work)

plot(df$impact_on_work, df$gender)
plot(df$impact_on_work, df$language_code)

df %>%
  group_by(impact_on_work, gender) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

View(df %>%
  group_by(impact_on_work, language_code, gender) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)))

df <- df %>% mutate(alcohol = as.factor(grepl("Alcohol", df$issues_services)))
plot(df$impact_on_work, df$alcohol)

df %<>%
  group_by(alcohol, impact_on_work) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

library(ggplot2)

ggplot(df,aes(x=impact_on_work, y = freq))+geom_point()+facet_grid(~alcohol)
ggplot(df,aes(x=impact_on_work, y = freq, fill = gender))+geom_bar(stat = "identity")+facet_grid(~alcohol)
ggplot(df,aes(x=impact_on_work))+geom_histogram(stat = "percentage")+facet_grid(~alcohol)


ggplot(df,aes(x=impact_on_work, y = freq))+geom_bar(stat = "identity")+facet_grid(~alcohol)
ggplot(df,aes(x=impact_on_work, y = freq, fill = gender))+geom_bar(stat = "identity")
ggplot(df,aes(x=impact_on_work, y = freq, fill = alcohol))+geom_bar(stat = "identity", position = "dodge")
