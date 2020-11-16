#' Market Sentimen by Industry
#'
#' @description Calculate ratio of increase stock in a industry daily and monthly
#' @details stocks in 2 HSX and HNX

#' [Config]
# Config ====
.__config_MARKET_SENTIMENT__ <- list(
  description = "Market Sentiment by Industry",
  folder = file.path("basement", "snapshot"),
  exchange_code = c("HOSE"), #' Can expand to HNX and UPCOM
  unit = "billion (except return)",
  source = "SSI",
  frame = list(
    from = as.Date("2020-01-01"),
    to = lubridate::today()
  )
)

#' [Load]
# Load ====
source("R/internal.R")

#' [Component]
# Component ====

#' Ticker Profile
ticker_profile <- vroom::vroom(file = file.path(.__config_MARKET_SENTIMENT__$folder, "data", "ticker_profile.tsv"))

#' GICS Sector
ticker_sector <- pool_query(
  avocado,
  modify_sql(
    avocado,
    "SELECT 
    DISTINCT cs.ticker,
    cs.primary_exchange_code,
    gs.gics_sector_name
    FROM common_stock AS cs
    LEFT JOIN gics_sector AS gs ON cs.gics_sector_code = gs.gics_sector_code
    WHERE 
    cs.primary_exchange_code IN ({exch*})",
    exch = .__config_MARKET_SENTIMENT__$exchange_code
  )
)

#' [Worker]
# Worker ====

#' Information about Selected Ticker
ticker_info <- ticker_profile %>%
  tidytable::filter.(exchange %in% "HOSE") %>%
  tidytable::select.(ticker = symbol, market_cap = marketcap) %>%
  tidytable::mutate.(market_cap = market_cap / 10^9) %>%
  tidytable::left_join.(ticker_sector, by = "ticker") %>%
  tidytable::mutate.(
    gics_sector_name = tidytable::case.(
      is.na(gics_sector_name), "Not Classified",
      default = gics_sector_name
    )
  )

#' Price Data by FRAME
ticker_price <- ticker_info$ticker %>%
  tidytable::map_dfr.(~ SSI_price(.,
    from = .__config_MARKET_SENTIMENT__$frame$from,
    to = .__config_MARKET_SENTIMENT__$frame$to
  ))

#' Return and Status
ticker_return <- ticker_price %>%
  tidytable::arrange.(ticker, -date) %>%
  tidytable::mutate.(
    daily_return = round((close / tidytable::leads.(close, n = 1) - 1) * 100, digits = 5)
  )

#' Industry Sentiment
industry_sentiment <- ticker_return %>%
  tidytable::mutate.(
    return_status = tidytable::case.(
      daily_return > 0, "positive",
      daily_return < 0, "negative",
      default = "non_change"
    )
  ) %>%
  tidytable::left_join.(ticker_info, by = "ticker") %>%
  tidytable::summarise.(
    .by = c(date, gics_sector_name, return_status),
    number = n.()
  ) %>%
  tidytable::pivot_wider.(., names_from = return_status, values_from = number) %>%
  tidytable::replace_na.(list(positive = 0, negative = 0, non_change = 0)) %>%
  tidytable::mutate.(
    number_companies = ifelse(is.na(positive), 0, positive) +
      ifelse(is.na(negative), 0, negative) +
      ifelse(is.na(non_change), 0, non_change)
  ) %>%
  tidytable::mutate.(
    positive_ratio = ifelse(is.na(positive), 0, positive) / number_companies,
    negative_ratio = ifelse(is.na(negative), 0, negative) / number_companies,
    non_change_ratio = ifelse(is.na(non_change), 0, non_change) / number_companies,
    pos_to_neg_ratio = tidytable::case.(
      negative != 0, round(positive / negative, 5),
      default = na_dbl
    )
  )

#' Industry Chart
ggplot2::ggplot(industry_sentiment, mapping = aes(x = date, y = pos_to_neg_ratio, group = gics_sector_name)) +
  ggplot2::geom_path(na.rm = TRUE) +
  ggplot2::facet_wrap(~gics_sector_name)

#' Breakdown Change
#' @note
#' Not have any mean with this information because it's can't answer 2 questions:
#' 1. Is there any counterpart with any companies in that companies?
#' TODO: Transfer English>> Liệu rằng có sự tác động của ngành tới các công ty trong ngành hay không
#' 2. Có tương tác giữa các ngành không?
#'
#' Enhancement
#' Change Classification Type
industry_sentiment %>%
  tidytable::select.(
    date, gics_sector_name, negative, non_change, positive
  ) %>%
  tidytable::pivot_longer.(
    cols = c(negative, non_change, positive),
    names_to = "status",
    values_to = "counter",
    values_drop_na = FALSE
  ) %>%
  tidytable::mutate.(
    month = lubridate::month(date)
  ) %>%
  tidytable::summarise.(
    .by = c(month, gics_sector_name, status),
    counter = mean(counter, na.rm = TRUE)
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(mapping = ggplot2::aes(x = month, y = counter, fill = status), position = "fill") +
  ggplot2::facet_wrap(~gics_sector_name)

#' Enhancement
#' Industry Sentiment Enhance
#' By Month [BEST CASE]
ticker_return %>%
  tidytable::mutate.(
    return_status = tidytable::case.(
      daily_return > 2, "positive",
      daily_return < -2, "negative",
      default = "non_effect"
    )
  ) %>%
  tidytable::left_join.(ticker_info, by = "ticker") %>%
  tidytable::summarise.(
    .by = c(date, gics_sector_name, return_status),
    counter = n.()
  ) %>%
  tidytable::mutate.(
    month = lubridate::month(date)
  ) %>%
  tidytable::summarise.(
    .by = c(month, gics_sector_name, return_status),
    counter = mean(counter, na.rm = TRUE)
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(mapping = ggplot2::aes(x = month, y = counter, fill = return_status), position = "fill") +
  ggplot2::facet_wrap(~gics_sector_name)

#' By Date
#' Can't see the resolution 
ticker_return %>%
  tidytable::mutate.(
    return_status = tidytable::case.(
      daily_return > 2, "positive",
      daily_return < -2, "negative",
      default = "non_effect"
    )
  ) %>%
  tidytable::left_join.(ticker_info, by = "ticker") %>%
  tidytable::summarise.(
    .by = c(date, gics_sector_name, return_status),
    counter = n.()
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(mapping = ggplot2::aes(x = date, y = counter, fill = return_status), position = "fill") +
  ggplot2::facet_wrap(~gics_sector_name)
