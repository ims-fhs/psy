# How do services occur? one service per row? many services per row? is there a mapping between issues and services?

imsbasics::clc()
source("R/00-psy-import.R")
df$issues_services
iss_list <- strsplit(df$issues_services, ",")

# find non-complete elements
ids_to_remove <- sapply(iss_list, function(i) length(i) == 1)
# remove found elements
iss_list <- iss_list[!ids_to_remove]

# There is a mapping issues <> services. We ignore it for the moment and for the sake of a less sparse data.frame
iss_list <- iss_list[order(sapply(iss_list,length),decreasing=T)]
iss_list[1:10]
