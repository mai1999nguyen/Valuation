# Control
control = list(
  descriptions = "Vietnam exchange analysis",
  date = "2020/08/06",
  unit = "billion",
  currency = "dong",
  threshold_extreme = 2
)
# Add library 
library(xml2)
library(httr)
library(rvest)
library(jsonlite)
library(purrr)
library(tibble)
library(dplyr)
library(magrittr)
library(tidyselect)
library(lubridate)
library(readxl)
library(rslang)
library("writexl")


#RAW
raw <- read_xls(path = "~/R/Connector/KQKD_2020.6T_20200807.xls",
                sheet = NULL,
                range = NULL,
                col_names = TRUE,
                col_types = NULL,
                na = "",
                trim_ws = TRUE,
                skip = 0,
                n_max = Inf, guess_max = 1000,
                progress = readxl_progress(),
                .name_repair = "unique") %>% 
  dplyr::mutate(earning_classification_code = dplyr::case_when(earning_value > 0 & earning_compare_same_period >0 ~ "Excelent",
                                                               earning_value > 0 & earning_compare_same_period <0 ~ "Moderation",
                                                               TRUE ~ "Bad"))
#tinh earning tren san
earning_exchange <- raw %>% 
  dplyr::group_by(primary_exchange_code) %>% 
  dplyr::arrange(dplyr::desc(earning_value)) %>% 
  dplyr::summarise(number_consituent = n(),
                   sum_earning_value = sum(earning_value, na.rm = TRUE),
                   top_10_company = paste(purrr::map(1:10, ~ dplyr::nth(ticker,n = .)),
                                          collapse = ",")) %>% 
  dplyr::ungroup()

#earning
earning_classification <- raw %>% 
  dplyr::group_by(primary_exchange_code,earning_classification_code) %>% 
  dplyr::arrange(dplyr::desc(earning_value)) %>% 
  dplyr::summarise(number_consituent = n(),
                   sum_earning_value = sum(earning_value, na.rm = TRUE),
                   top_10_company = paste(purrr::map(1:10, ~dplyr::nth(ticker, n=.)),collapse = ",")) %>% 
  dplyr::ungroup()

#GICS
vietnam_gics_classification <- read_xlsx(path = "~/R/Connector/vietnam_bloomberg_gics.xlsx",
                                         sheet = NULL,
                                         range = NULL,
                                         col_names = TRUE,
                                         col_types = NULL, 
                                         na = "",
                                         trim_ws = TRUE,
                                         skip = 0,
                                         n_max = Inf, guess_max = 1000,
                                         progress = readxl_progress(),
                                         .name_repair = "unique")
#add RAW
raw <- raw %>% 
  dplyr::left_join(dplyr::select(vietnam_gics_classification,ticker,dplyr::ends_with("_name")), by = "ticker")

#detect extreme
detect_extreme_raw <- raw %>% 
  dplyr::group_by(gics_sector_name) %>% 
  dplyr::summarise(pe_industry_average = mean(current_pe_ratio, na.rm = TRUE),
                   pe_industry_median = median(current_pe_ratio, na.rm = TRUE),
                   pe_industry_cap = median(current_pe_ratio, na.rm = TRUE) + control$threshold_extreme * sd(current_pe_ratio, na.rm = TRUE),
                   pe_industry_floor = median(current_pe_ratio, na.rm = TRUE) - control$threshold_extreme * sd(current_pe_ratio, na.rm = TRUE),
                   pe_industry_range = paste(min(current_pe_ratio, na.rm = TRUE),max(current_pe_ratio, na.rm = TRUE), sep = "-")) %>% 
  dplyr::ungroup()
#NORMAL value
normal_raw <- raw %>% 
  dplyr::left_join(detect_extreme_raw, by = "gics_sector_name") %>% 
  dplyr::mutate(is_extreme = dplyr::case_when(current_pe_ratio > pe_industry_cap |
                                                current_pe_ratio <  pe_industry_floor ~ "Extreme",
                                              TRUE ~ "Normal")) %>% 
  dplyr::filter(is_extreme == "Normal") %>% 
  dplyr::select(!(pe_industry_average:pe_industry_range))

#valuation
gics_valuation <- normal_raw %>% 
  dplyr::group_by(gics_sector_name) %>%
  dplyr::summarise(pe_industry_average = mean(current_pe_ratio, na.rm = TRUE),
                   pe_industry_median = median(current_pe_ratio, na.rm = TRUE),
                   pe_industry_range = paste(min(current_pe_ratio, na.rm = TRUE),
                                             max(current_pe_ratio, na.rm = TRUE), sep = "-")) %>% 
  dplyr::ungroup() 

#PE classification
pe_gics_classification <- normal_raw %>%
  dplyr::left_join (gics_valuation, by = "gics_sector_name") %>%
  dplyr::mutate(pe_to_sector_percentage = (current_pe_ratio/pe_industry_median)-1) %>%
  dplyr::mutate(pe_gics_classification = dplyr::case_when(pe_to_sector_percentage > 0 ~ "Premium",
                                                          TRUE ~ "Discount")) %>% 
  dplyr::select(gics_sector_name,pe_industry_average, pe_industry_median, pe_industry_range,pe_to_sector_percentage,pe_gics_classification) %>% 
  write_xlsx(pe_gics_classification,path = "~/R/Connector/vietnam_20206Q_analyst.xls")
