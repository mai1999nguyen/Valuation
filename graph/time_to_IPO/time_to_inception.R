#' Time to IPO
#'
#' @description get histogram of time (count by years) since company has founded to IPO
#' @source
#' Ticker Information scrape from SSI

#' Load
source(here::here("R", "internal.R"))

#' Config
config <- list(
  description = "Counter since Founded-IPO",
  folder = here::here("instruction", "time_to_IPO"),
  lastest_run = as.Date("2020-10-27")
)

#' Read
ticker_profile <- vroom::vroom(file = file.path(config$folder, "data", "ticker_profile.tsv"))

#' Clean
#'
#' @note
#' Error Capture:
#' 1. Founding Date, Listing Date is in Character > Date
#' 2. Using timezone is in Asia/Ho Chi Minh
#' 3. Founding date in 1752 is false, relace to NA
#'
#' Enhancement:
#' 1. Valid information random case from [0 - 2] years
#' 2. Valid missing case of founding date above
#' @export
ticker_IPO <- ticker_profile %>%
  tidytable::select.(
    ticker = symbol,
    industry_name = industryname,
    market_cap = marketcap,
    founding_date = foundingdate,
    listing_date = listingdate
  ) %>%
  tidytable::mutate.(
    founding_date = as.Date(lubridate::dmy_hms(founding_date, tz = "Asia/Ho_Chi_Minh")),
    listing_date = as.Date(lubridate::dmy_hms(listing_date, tz = "Asia/Ho_Chi_Minh")),
    market_cap = market_cap / 10^9
  ) %>%
  tidytable::mutate_across.(
    .cols = c(founding_date, listing_date),
    .fns = ~ tidytable::case.(
      lubridate::year(.) == 1752, NA,
      default = .
    )
  ) %>%
  tidytable::mutate.(
    .by = ticker,
    time_IPO = tidytable::case.(
      listing_date < founding_date, NA,
      default = round(as.numeric(difftime(listing_date, founding_date, unit = "days")) / 365, 0)
    ),
    company_classification = tidytable::case.(
      market_cap > 10^5, "Large Cap",
      all(market_cap > 10^4, market_cap <= 10^5), "Mid Cap",
      all(market_cap > 10^2, market_cap <= 10^4), "Small Cap",
      market_cap < 10^2, "Micro Cap",
      default = "Non Classification"
    )
  ) %>%
  tidytable::select.(ticker, market_cap, company_classification, founding_date, listing_date, time_IPO) %>%
  tidytable::filter.(time_IPO > 0, company_classification != "Non Classification")

#' Ticker IPO by Company Classification
ticker_IPO_by_cc <- ticker_IPO %>%
  tidytable::summarise.(
    .by = company_classification,
    number = n.(),
    min = min(time_IPO, na.rm = TRUE),
    mean = mean(time_IPO, na.rm = TRUE),
    max = max(time_IPO, na.rm = TRUE)
  )

#' Chart
#' Overall Market
ggplot2::ggplot(data = ticker_IPO, mapping = aes(x = time_IPO)) +
  ggplot2::geom_histogram(bins = 20)

#' Classification by Company Classification
ggplot2::ggplot(ticker_IPO, mapping = ggplot2::aes(x = time_IPO, group = company_classification)) +
  ggplot2::geom_histogram(bins = 20) +
  ggplot2::facet_wrap(~company_classification)

#' Turn Off
dev.off()
