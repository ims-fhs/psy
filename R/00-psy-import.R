df <- xlsx::read.xlsx(file = "data/All Cases 2018.xlsx", sheetName = "ICAS210219", stringsAsFactors = FALSE)
df_issues_services <- df$issues_services
df_issues_services <- read.table(text = df_issues_services, sep = ",", as.is = TRUE)
stringr::word(df$issues_services, sep = ",")
