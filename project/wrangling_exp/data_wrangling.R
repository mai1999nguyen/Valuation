
raw <- read_xlsx(path = here::here("R","Connector","BCTC Q2.xlsx"),
                 range = NULL,
                 col_names = TRUE,
                 col_types = NULL,
                 na = "0",
                 trim_ws = TRUE,
                 skip = 0,
                 n_max = Inf, guess_max = 1000,
                 progress = readxl_progress(),
                 .name_repair = "unique")


new_raw <- raw %>% 
  tidyr::pivot_longer(cols = KQHDKD_HN:TM_CTM,
                      names_to = "report_type",
                      values_to = "date") %>% 
  dplyr::mutate(date = dplyr::case_when(!is.na(lubridate::dmy(date))
                                        ~lubridate::dmy(date),
                                        TRUE ~ as.Date(as.numeric(date), origin ="1899/12/30")))
new_raw <- separate(new_raw,report_type, into= c("report","type")) 
new_raw <- new_raw %>% dplyr::mutate(report= dplyr::case_when(report == "KQHDKD" ~ "business_performance",
                                                              report == "BCDKT" ~ "accounting_balance",
                                                              report == "BCLCTT" ~ "cash_flow",
                                                              TRUE ~ "cash")) %>% 
  dplyr::mutate(type = dplyr::case_when(type == "HN"~ "consolidated",
                                        TRUE ~"parent_company"))

