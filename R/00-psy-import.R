imsbasics::clc()
library(tidyverse)
library(lubridate)
library(haven)

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
  assertthat::assert_that(num_sheets == 1, msg = "Only one Spreadsheet expected!")
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
  df1$impact_on_work <- factor(df1$impact_on_work, 
                               levels = c("Normal", "Satisfactory", 
                                          "Impaired", "Severely Impaired", 
                                          "On Sick Leave/Absent Satisfactory"))
  return(df1)
}

convert_datatypes <- function(df) {
  ret_val <- df
  ret_val$status_of_caller <- as.factor(ret_val$status_of_caller)
  ret_val$case_status <- as.factor(ret_val$case_status)
  ret_val$language_code <- as.factor(ret_val$language_code)
  ret_val$gender <- as.factor(ret_val$gender)
  ret_val$referral_type <- as.factor(ret_val$referral_type)
  ret_val$impact_on_work <- as.factor(ret_val$impact_on_work)
  return(ret_val)
}

remove_special_char <- function(v) {
  gsub("[-\\/\\& \\(\\)]", "_", v)
}


#' generate features
#'
#' @param df the data.frame
#'
#' @return the modified / updated data.frame
generate_issue_features <- function(df) {
  ret_val <- df
  # determine issue categories: split by comma
  categories <- unique(unlist(strsplit(df$issues_services,split = ",")))
  # remove services (everything after ':')
  category_names <- unique(gsub("([^:]+):.*", "\\1", categories))
  # remove german description
  category_names <- gsub("(.*?) {3,5}.+", "\\1", category_names)
  # remove not allowed special chars
  category_names <- gsub("[-\\/\\& \\(\\)]", "_", category_names)
  # replace multiple sequences of underscores by one single underscore
  category_names <- gsub("_+", "_", category_names)
  
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
  
  
  
  # set overall prefix for easier identification of issues
  category_names <- paste0("iss_", category_names)
  
  
  
  for (i in 1:length(categories)) {
    feature_name <- category_names[i]
    if (!is.na(feature_name)) {
      related_categories <- categories[which(category_names == feature_name)]
      if (feature_name %in% colnames(ret_val)) {
        assertthat::assert_that(sum(ret_val[[feature_name]]) == sum(ret_val[[feature_name]] | ret_val$issues_services %in% related_categories))
        ret_val[[feature_name]] <- ret_val[[feature_name]] | ret_val$issues_services %in% related_categories
      } else {
        ret_val[[feature_name]] <- ret_val$issues_services %in% related_categories
      }
      cat("Feature nr. ", i, " with name '", feature_name, "' successfully generated.\n")
    }
  }
  return(ret_val)    
}

#' convert logical to booleans
#'
#' @param df 
#'
#' @return
convert_boolean_to_factors <- function(df) {
  idx <- grep("iss_", colnames(df))
  for (i in idx) {
    df[,i] <- factor(df[,i], levels = c("TRUE", "FALSE"))
  }
  return (df)
}


#' removes corrupt data (i.e. records having no gender)
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
strip_corrupt_data <- function(df) {
  ret_val <- df
  ret_val <- ret_val %>% filter(!is.na(gender))
  return(ret_val)
}

add_boolean_sum <- function(key) {
  function(df) {
    df[, paste0("sum_", key)] <- rowSums(df[, grepl(key, colnames(df))], na.rm = TRUE) - 1
    return(df)
  }
}
add_sum_issues <- add_boolean_sum("iss_")
add_sum_services <- add_boolean_sum("ser_")

generate_issues_services <- function(df) {
  foo <- strsplit(df$issues_services, ",")
  # Get all possible features
  feature <- unique(unlist(lapply(foo, function(x) sub(":.*", ":", x))))
  # Get all possible services
  service <- unique(unlist(lapply(foo, function(x) sub(".*:", ":", x))))
  
  # Generate occurrence table
  result <- sapply(c(feature, service), grepl, df$issues_services)
  # Name final result
  colnames(result) <- c(paste0("iss_", sub(":", "", feature)),
                        paste0("ser_", sub(":", "", service)))
  result <- as.data.frame(result)
  
  df <- cbind(df, result)
}

#' add date fields to the dataframe
#'
#' @param df the dataframe
#'
#' @return the enriched dataframe
enrich_additional_features <- function(df) {
  df <- df %>% mutate(created_month = lubridate::month(df$created_at))
  df <- df %>% mutate(created_quarter = lubridate::quarter(df$created_at))
  df <- df %>% mutate(created_year = lubridate::year(df$created_at))
  df <- df %>% mutate(created_hour = lubridate::hour(df$created_at))
  df <- df %>% mutate(created_weekday = weekdays(df$created_at))
  df <- df %>% mutate(case_closed = ifelse(!is.na(df$case_closed_at), 1, 0))
  df <- df %>% mutate(case_timespan = ifelse(!is.na(df$case_closed_at), 
                                             difftime(time1 = date(df$case_closed_at), 
                                                      time2 = df$created_at,
                                                      units = c("days")), NA))

  # set duration to 1 for cases, which have been closed on same day
  df$case_timespan[df$case_timespan == 0] <- 1
  return(df)
}

file_path <- "data/RData/"
file_name <- "psy_data"
file_name_rds <- paste0(file_name, ".rds")

# render file, if not yet generated
if (!file.exists(paste0(file_path, file_name_rds))) {
  df <- cleanup_icas_import()
  df <- convert_datatypes(df)
  
  df$issues_services <- remove_special_char(df$issues_services)
  df <- generate_issues_services(df)
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- gsub("(.*?)_{3,5}.+", "\\1", colnames(df))
  
  df <- strip_corrupt_data(df)
  df <- add_sum_issues(df)
  df <- add_sum_services(df)
  df <- enrich_additional_features(df)
  
  issues <- sort(c("alcohol", "divorce", "work", "conflict", "violence", "trauma", "tax", "stress", "relationship", "redundancy", "mobbing"), decreasing = FALSE)
  sapply(issues, function(x) df <<- df %>% mutate(!!x := grepl(x, df$issues_services, ignore.case = TRUE)))

  # save data to local rds file
  saveRDS(df, paste0(file_path, file_name_rds))

  # export 4 SPSS
  haven::write_sav(df, paste0(file_path, file_name, "_spss.sav"))

  # export 4 CSV
  write.csv2(df, paste0(file_path, file_name, ".csv"), fileEncoding = "UTF-8")
} else {
  # load data from rds
  df <- readRDS(paste0(file_path, file_name_rds))
}

