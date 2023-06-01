# Loading the required libraries
library(dplyr)
library(gtsummary)
library(flextable)   # to save gtsummary object into word docs
library(writexl)
library(plotrix) #has a std.error function
library(kableExtra)
library(readxl) # read the excel file
library(huxtable) # to convert gtsummary table into huxtable and then extract into excel file.

# set working directory
# setwd("~/anlitiks/stats-report-automation/descriptive_shiny_app")

# setting theme
theme_gtsummary_printer(
  print_engine = c("flextable"),
  set_theme = TRUE
)

# theme function
source("custemised_theme.R")

# usrds_data = read.csv(file = "./data/nearestNeighborMatching9-24-21.csv")
# usrds_data = data.frame(usrds_data, stringsAsFactors = FALSE)
# usrds_data = usrds_data[,-c(1:2)]
# 
# continuous_vars1 <- "AGE"
# Factor_vars1 <- c("race","gender")
# strata_vars1 <- c("mefyear")
# 
# cont_test <- "kruskal.test"  # t.test (if we have only two groups for comparision and add_difference() is applied), kruskal.test, wilcox.test
# cat_test <- "chisq.test"   # Ex: fisher.test, chisq.test
# ci_val <- TRUE
# font_name <- 'Arial'
# font_size <- 10
# export_var1 <- c("Word", "Excel","PDF")
# tab_title <- NULL
# 
# data = usrds_data %>% select(continuous_vars1,Factor_vars1,strata_vars1)

# create a function body to calculate the descriptive stats
DescStatFun1 <- function(continuous_vars1, strata_vars1, Factor_vars1, data,cont_test,cat_test,ci_val, font_name, font_size,tab_title, export_var1){
  
  # loading the input values
  mydata <- as.data.frame(data);       # convert to R data frame
  continuous_var <- continuous_vars1;  # continuous variable
  Factor_var <- Factor_vars1;          # categorical variable
  strata_var <- strata_vars1;  # stratified the continuous variables and factor variables
  ci_val <- ci_val
  tab_title <- ifelse(!is.null(tab_title), as.character(tab_title), "Descriptive Statistics")
  export_var1 <- c(export_var1)
  
  # removing NA's from the data set
  mydata <- mydata[complete.cases(mydata),]
  
  # Make categorical variables as factors
  varsToFactor <- unique(c(Factor_var,strata_var))
  mydata[varsToFactor] <- lapply(mydata[varsToFactor], factor)
  
  if(ci_val == TRUE & is.null(cont_test) == FALSE & is.null(cont_test) == FALSE){ 
    summary_table = data %>% 
      tbl_summary(by = all_of(strata_var), statistic = list(all_continuous() ~ "{mean} ({std.error})",
                                                            all_categorical() ~ "{n} ({p}%)"),
                  missing = "no",
                  digits = list(all_continuous() ~ c(2))) %>%
      add_n() %>%
      add_ci(statistic = list(all_categorical() ~ "({conf.low}%, {conf.high}%)", all_continuous() ~ "({conf.low}, {conf.high})")) %>%
      add_p(test = list(all_continuous() ~ paste0(cont_test, collapse = NULL), all_categorical() ~ paste0(cat_test, collapse = NULL)), pvalue_fun = purrr::partial(style_pvalue, digits = 2)) %>%
      modify_header(label = "**Variable**") %>% # update the column header
      modify_header(all_stat_cols() ~ "**{(level)}**, N =  {n} ({style_percent(p)}%)") %>%
      bold_labels() %>%
      modify_caption(caption = tab_title)
    
  } else if(ci_val == FALSE & is.null(cont_test) != TRUE & is.null(cont_test) != TRUE){
    summary_table = data %>% 
      tbl_summary(by = all_of(strata_var), statistic = list(all_continuous() ~ "{mean} ({std.error})",
                                                            all_categorical() ~ "{n} ({p}%)"),
                  missing = "no",
                  digits = list(all_continuous() ~ c(2))) %>%
      add_n() %>%
      add_p(test = list(all_continuous() ~ paste0(cont_test, collapse = NULL), all_categorical() ~ paste0(cat_test, collapse = NULL)), pvalue_fun = purrr::partial(style_pvalue, digits = 2)) %>%
      modify_header(label = "**Variable**") %>% # update the column header
      modify_header(all_stat_cols() ~ "**{(level)}**, N =  {n} ({style_percent(p)}%)") %>%
      bold_labels() %>%
      modify_caption(caption = tab_title)
    
  } else if (ci_val == TRUE & is.null(cont_test) == TRUE | is.null(cont_test) == TRUE){
    summary_table = data %>% 
      tbl_summary(by = all_of(strata_var), statistic = list(all_continuous() ~ "{mean} ({std.error})",
                                                            all_categorical() ~ "{n} ({p}%)"),
                  missing = "no",
                  digits = list(all_continuous() ~ c(2))) %>%
      add_n() %>%
      add_ci(statistic = list(all_categorical() ~ "({conf.low}%, {conf.high}%)", all_continuous() ~ "({conf.low}, {conf.high})")) %>%
      modify_header(label = "**Variable**") %>% # update the column header
      modify_header(all_stat_cols() ~ "**{(level)}**, N =  {n} ({style_percent(p)}%)") %>%
      bold_labels() %>%
      modify_caption(caption = tab_title)
    
  } else {
    summary_table = data %>% 
      tbl_summary(by = all_of(strata_var), statistic = list(all_continuous() ~ "{mean} ({std.error})",
                                                            all_categorical() ~ "{n} ({p}%)"),
                  missing = "no",
                  digits = list(all_continuous() ~ c(2))) %>%
      add_n() %>%
      modify_header(label = "**Variable**") %>% # update the column header
      modify_header(all_stat_cols() ~ "**{(level)}**, N =  {n} ({style_percent(p)}%)") %>%
      bold_labels() %>%
      modify_caption(caption = tab_title)
  }
  
  # save these results
  
  for( x in export_var1){
    switch(x,
           
           "Word" = {
             summary_table %>%  
               as_flex_table() %>%
               flextable::save_as_docx(
                 path = "./output/result_word.docx")
           },
           "PDF" = {
             summary_table %>%
               as_gt() %>%
               gtsave(filename = "results.pdf",path = "./output/")
           },
           "Excel" = {
             gt_table = summary_table %>% as_hux_table() %>% as_tibble()
             col_nm = as.character(c(gt_table[1,]))
             names(gt_table) = col_nm
             gt_table = gt_table[-1,]
             writexl::write_xlsx(x = gt_table,
                                 path = "./output/results_excel.xlsx",
                                 col_names = TRUE,
                                 format_headers = TRUE,
                                 use_zip64 = FALSE)
           }
    )
  }
  
  
  return(summary_table %>% as_gt() %>% my_theme(font_name = font_name, font_size = font_size)
         )
}