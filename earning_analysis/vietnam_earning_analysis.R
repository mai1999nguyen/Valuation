#' [CONTROL]
# Control ====
control <- list(
  description = "Analysis Earning of Vietnam Market",
  updated_date = as.Date("2020/08/06"),
  unit = "billion",
  currency = "VND",
  raw_file_path = here::here("manager", "instruction", "earning_analysis", "earning_raw_20206M.xlsx"),
  threshold_extreme = 2
)

#' [PACKAGES]
# Packages ====
library(xml2)
library(here)
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

#' Raw
raw <- readxl::read_xlsx(path = control$raw_file_path,
                         sheet = 1, range = NULL, col_names = TRUE,
                         col_types = NULL, na = "", trim_ws = TRUE, skip = 0,
                         n_max = Inf, guess_max = 1000,
                         progress = readxl_progress(), .name_repair = "unique") %>%
  dplyr::mutate(earning_classification_code = dplyr::case_when(earning_value > 0 & earning_compare_same_period > 0 ~ "Excellence",
                                                          earning_value > 0 & earning_compare_same_period < 0 ~ "Moderate",
                                                          TRUE ~ "Bad"))

#' Earning by Primary Exchange
earning_exchange <- raw %>%
  dplyr::group_by(primary_exchange_code) %>%
  dplyr::arrange(dplyr::desc(earning_value)) %>%
  dplyr::summarise(number_consituent = n(),
                   earning_value = sum(earning_value, na.rm = TRUE),
                   top_5_companies_by_earning = paste(purrr::map(1:5, ~ dplyr::nth(ticker, n = .)),
                                                      collapse = ", ")) %>%
  dplyr::ungroup()

#' Earning 
earning_classification <- raw %>%
  dplyr::group_by(primary_exchange_code, earning_classification_code) %>%
  dplyr::arrange(dplyr::desc(earning_value)) %>%
  dplyr::summarise(number_consituent = n(),
                   earning_value = sum(earning_value, na.rm = TRUE),
                   top_10_company = paste(purrr::map(1:10, ~ dplyr::nth(x = ticker, n = .)), collapse = ", ")) %>%
  dplyr::ungroup()

#' Earning 
earning_classification_validate <- raw %>%
  dplyr::group_by(primary_exchange_code, earning_classification_code, fundamental_status) %>%
  dplyr::arrange(dplyr::desc(earning_value)) %>%
  dplyr::summarise(number_consituent = n(),
                   earning_value = sum(earning_value, na.rm = TRUE),
                   top_10_company = paste(purrr::keep(purrr::map(1:10, ~ dplyr::nth(x = ticker, n = .)),
                                                      ~ !is.na(.)), collapse = ", ")) %>%
  dplyr::ungroup()

#' GICS 
vietnam_gics_classification <- readxl::read_xlsx(path = here::here("manager", "instruction", "earning_analysis", "vietnam_bloomberg_gics.xlsx"), 
                                                 sheet = "DATA", range = NULL, col_names = TRUE,
                                                 col_types = NULL, na = c("", "NA"), trim_ws = TRUE, skip = 0,
                                                 n_max = Inf, guess_max = 1000,
                                                 progress = readxl_progress(), .name_repair = "unique")

#' Raw
raw <- raw %>%
  dplyr::left_join(dplyr::select(vietnam_gics_classification, ticker, 
                                 dplyr::ends_with("_name")), by = "ticker")  

#' Classification
earning_classification_by_sector <- raw %>%
  dplyr::group_by(primary_exchange_code, gics_sector_name, fundamental_status, earning_classification_code) %>%
  dplyr::arrange(dplyr::desc(earning_value)) %>%
  dplyr::summarise(number_consituent = n(),
                   earning_value = sum(earning_value, na.rm = TRUE),
                   current_pe_ratio = median(current_pe_ratio, na.rm = TRUE),
                   top_10_company = paste(purrr::keep(purrr::map(1:10, ~ dplyr::nth(x = ticker, n = .)),
                                                      ~ !is.na(.)), collapse = ", ")) %>%
  dplyr::ungroup()


detect_extreme_raw <- raw %>% 
  dplyr::group_by(gics_sector_name) %>%
  dplyr::summarise(pe_industry_average = mean(current_pe_ratio, na.rm = TRUE),
                   pe_industry_median = median(current_pe_ratio, na.rm = TRUE),
                   pe_industry_cap = median(current_pe_ratio, na.rm = TRUE) + control$threshold_extreme * sd(current_pe_ratio, na.rm = TRUE),
                   pe_industry_floor = max(0, median(current_pe_ratio, na.rm = TRUE) + control$threshold_extreme - sd(current_pe_ratio, na.rm = TRUE)),
                   pe_industry_range = paste(min(current_pe_ratio, na.rm = TRUE),
                                             max(current_pe_ratio, na.rm = TRUE), sep = " - ")) %>%
  dplyr::ungroup()


#' Extreme Value
normal_raw <- raw %>%
  dplyr::left_join(detect_extreme_raw, by = "gics_sector_name") %>%
  dplyr::mutate(is_extreme = dplyr::case_when(current_pe_ratio > pe_industry_cap | current_pe_ratio < pe_industry_floor ~ "Extreme",
                                              TRUE ~ "Normal")) %>%
  dplyr::filter(is_extreme == "Normal")

gics_valuation <- normal_raw %>%
  dplyr::group_by(gics_sector_name) %>%
  dplyr::summarise(pe_industry_average = mean(current_pe_ratio, na.rm = TRUE),
                   pe_industry_median = median(current_pe_ratio, na.rm = TRUE),
                   pe_industry_range = paste(min(current_pe_ratio, na.rm = TRUE),
                                             max(current_pe_ratio, na.rm = TRUE), sep = " - ")) %>%
  dplyr::ungroup()

ticker_valuation <- raw %>%
  dplyr::left_join(gics_valuation, by = "gics_sector_name") %>% 
  dplyr::mutate(valuation_percentage = round(current_pe_ratio/pe_industry_median - 1, 3)) %>% 
  dplyr::mutate(valuation_status = dplyr::case_when(valuation_percentage > 0 ~ "Premium",
                                                    TRUE ~ "Discount"))


extract_component <- ticker_valuation %>%
  dplyr::group_by(primary_exchange_code, earning_classification_code, valuation_status) %>% 
  dplyr::arrange(dplyr::desc(earning_value)) %>%
  dplyr::summarise(number_consituent = n(),
                   earning_value = sum(earning_value, na.rm = TRUE),
                   current_pe_ratio = median(current_pe_ratio, na.rm = TRUE),
                   top_10_company = paste(purrr::keep(purrr::map(1:10, ~ dplyr::nth(x = ticker, n = .)),
                                                      ~ !is.na(.)), collapse = ", ")) %>%
  dplyr::ungroup()




foreach::foreach() %do% {
  
}

ticker_return <- xml2::read_html(x = "https://s.cafef.vn/thong-ke/timeline-3-thang/san-all.chn") %>%
  rvest::html_nodes(xpath = '//*[@id="bigtable"]') %>%
  rvest::html_table(trim = TRUE) %>%
  purrr::pluck(1) %>%
  tibble::as_tibble() %>%
  purrr::set_names(nm = c("ticker", "market_cap_bln_vnd", "close", "volume", "return_3_month", "empty_1", "volume_compare_3m", "empty_2")) %>%
  dplyr::select(-tidyselect::starts_with("empty_")) %>% 
  dplyr::mutate(dplyr::across(c("market_cap_bln_vnd", "close", "volume", "volume_compare_3m"), ~ as.numeric(.))) %>%
  dplyr::mutate(return_3_month = as.numeric(stringr::str_extract(return_3_month, "\\d+\\.\\d+")))


ggplot2::ggplot(data = earning_classification_by_sector, mapping = aes(x = earning_classification_code)) %+%
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) %+%
  facet_wrap(~ gics_sector_name)

