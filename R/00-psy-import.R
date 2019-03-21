imsbasics::clc()
library(tidyverse)

#' read the given xls and generate new features based on the field 'issues_services' 
#'
#' @param file_path filename and path
#' @param sheet_name sheet name to be processed
#'
#' @return
#' @export
#'
#' @examples
import_xls <- function(file_path) {
  # make sure, only one spreadsheet provided
  num_sheets <- length(readxl::excel_sheets(file_path))
  assertthat::assert_that(num_sheets == 1,msg = "Only one Spreadsheet expeced!")
  df <- readxl::read_xlsx(path = file_path, na = "NA")
return(df)
}

#' reads all available xlsx files from the path provided and merges the content
#' into one single data.frame. existing columns are reused, new ones extend the
#' current dataframe.
#'
#' @param data_path the path where the xlss-files are to be taken from 
#'
#' @return a data.frame containing all the data
#' @export
append_all_xls <- function(data_path) {
  primary_key_id_counter <- 1
  ret_val <- NULL
  files <- list.files(path = data_path, pattern = ".*?\\.xlsx")
  for (file in files) {
    df <- import_xls(file_path = paste0(data_path, "/", file))
    df[,"ims_internal_id"] <- primary_key_id_counter:(primary_key_id_counter + nrow(df) - 1)
    primary_key_id_counter <- primary_key_id_counter + nrow(df)
    if (is.null(ret_val)) {
      ret_val <- df
    } else {
      ret_val <- merge(ret_val, df, all = TRUE)
    }
  }
  return(ret_val)
}

#' Import all available xlsx and cleanup/merge/drop ambiguous columns
#'
#' @return one dataframe containing all data
cleanup_icas_import <- function() {
  df1 <- append_all_xls("data/rawdata")
  # Merge columns 'Case No.', 'Case no.' into 'id'
  df1 <- df1 %>% mutate(`id` = coalesce(`id`, `Case no.`, `Case No.`))
  df1$`Case no.` <- NULL
  df1$`Case No.` <- NULL
  # Merge column 'Issues / Services' into 'issues_services'
  df1 <- df1 %>% mutate(`issues_services` = coalesce(`issues_services`, `Issues / Services`))
  df1$`Issues / Services` <- NULL
  return (df1)
}

generate_issue_features <- function(df) {
  ret_val <- df
  # determine issue categories: split by comma
  categories <- unique(unlist(strsplit(df$issues_services,split = ",")))
  # remove services (everything after ':')
  category_names <- gsub("([^:]+):.*", "\\1", categories)
  # remove german description
  category_names <- gsub("(.*?) {3,5}.+", "\\1", category_names)
  # remove not allowed special chars
  category_names <- gsub("[-\\/\\& \\(\\)]", "_", category_names)
  # replace multiple sequences of underscores by one single underscore
  category_names <- gsub("_+", "_", category_names)
  # set overall prefix for easier identification of issues
  category_names <- paste0("iss_", category_names)
  
  # convert to lower case
  category_names <- tolower(category_names)
  # repair inconsistent category-names
  bad_name_ids <- which(category_names == "routing_to_familienservice_weiterleitung_an_familienservice") 
  category_names[bad_name_ids] <- "routing_to_familienservice"
  bad_name_ids <- which(category_names == "bem_betriebliches_eingliederungs_management_occupational_reintegration") 
  category_names[bad_name_ids] <- "occupational_reintegration"
  bad_name_ids <- which(category_names == "concern_about_employee_sorgen_bezüglich_mitarbeiter") 
  category_names[bad_name_ids] <- "concern_about_employee"
  bad_name_ids <- which(category_names == ":livechat") 
  category_names[bad_name_ids] <- "livechat"
  
  for (i in 1:length(categories)) {
    feature_name <- category_names[i]
    if (!is.na(feature_name)) {
      related_categories <- categories[which(category_names == feature_name)]
      if (feature_name %in% colnames(df)) {
        ret_val[[feature_name]] <- ret_val[[c(feature_name)]] || ret_val$issues_services %in% related_categories
      } else {
        ret_val[[feature_name]] <- ret_val$issues_services %in% related_categories
      }
      cat("Feature nr. ", i, " with name '", feature_name, "' successfully generated.\n")
    }
  }
  return(ret_val)    
}

df <- cleanup_icas_import()
df <- generate_issue_features(df)


# df1$impact_on_work <- as.factor(df1$impact_on_work)
# df1 <- df1[,!sapply(df1,function(x) any(is.na(x)))]
# df1 <- df1 %>% mutate_if(is.character, as.factor)
# 
# library(randomForest)
# 
# set.seed(131)
# psy.rf <- randomForest(impact_on_work ~ ., data = df1, importance=TRUE, proximity=TRUE, na.action=na.omit)
# 
# print(psy.rf)
# View(round(importance(psy.rf), 2))
