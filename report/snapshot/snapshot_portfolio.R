#' [LOAD]
# LOAD ====
source("R/internal.R")
c(
  c("SSI_api", "SSI_function") %>% purrr::map_chr(~ file.path("API", "SSI", glue::glue("{.}.R")))
) %>%
  purrr::walk(~ source(.))

#' [CONFIG]
# CONFIG ====
config <- list(
  folder = file.path("basement", "snapshot"),
  parameters = list(
    portfolio = c("VIC", "KSB", "HPG", "VCB", "DXG")
  ),
  frame = list(
    from = lubridate::today() - 20,
    to = lubridate::today()
  )
)

#' [COMPONENT]
# COMPONENT ====

#' Frame
data <- config$parameters$portfolio %>%
  purrr::map_dfr(~ SSI_price(ticker = ., from = config$frame$from, to = config$frame$to))

#' Real Time
real_time_price <- c("HOSE", "HANO", "UPCO") %>%
  purrr::map_dfr(~ SSI_price_real_time(.)) %>%
  tidytable::filter.(ticker %in% config$parameters$portfolio) %>%
  tidytable::select.(ticker, price_reference, matched_price, price_change_percent)

#' Get Link
corporate <- readxl::read_excel(file.path("API", "VS", "data", "corporate.xlsx")) %>%
  tidytable::select.(ticker, ticker_uri)

#' Calculate
snapshot_portfolio <- config$parameters$portfolio %>%
  purrr::map_dfr(~ SSI_api_company_profile(.)) %>%
  tidytable::select.(ticker = symbol, industryname, exchange, market_cap = marketcap, foundingdate, companyname) %>%
  tidytable::mutate.(
    .by = ticker,
    company_classification = tidytable::case.(
      market_cap > 10^5, "Large Cap",
      all(market_cap > 10^4, market_cap <= 10^5), "Mid Cap",
      all(market_cap > 10^2, market_cap <= 10^4), "Small Cap",
      market_cap < 10^2, "Micro Cap",
      default = "Non Classification"
    )
  ) %>%
  dplyr::left_join(data, by = "ticker") %>%
  dplyr::left_join(real_time_price, by = "ticker") %>%
  dplyr::left_join(corporate, by = "ticker") %>%
  tidytable::mutate.(
    .by = ticker,
    golden_point = "+A",
    golden_momentum = "Higher"
  )

#' [EXPORT]
# EXPORT ====
#' Snapshot Portfolio
vroom::vroom_write(snapshot_portfolio, path = file.path(config$folder, "ticker_porfolio.tsv"))
