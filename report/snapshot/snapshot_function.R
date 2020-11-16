#' Snapshot Ticker
#'
#' @description get Return 1 month of ticker
#'
#' @param ticker [character(n)] multiple ticker
#' @param year [numeric] year of updated year
#'
#' @export
snapshot_monthly <- function(ticker, year) {

  #' Stop
  base::stopifnot(
    !rlang::is_missing(ticker), !rlang::is_missing(year)
  )

  #' Price
  .ticker_price <- ticker %>%
    purrr::map_dfr(
      ~ SSI_price(ticker = ., from = date_create(year, 01, 01), to = date_create(year, 12, 31))
    ) %>%
    tidytable::mutate.(
      date = as.Date(date),
      month = lubridate::month(date)
    )

  #' Valid
  if (nrow(.ticker_price) > 0) {

    #' Return
    .ticker_return <- .ticker_price %>%
      tidytable::filter.(
        .by = c(ticker, month),
        date %in% c(max(date), min(date))
      ) %>%
      tidytable::arrange.(ticker, -date) %>%
      tidytable::mutate.(
        .by = c(ticker, month),
        return_1M = round((close / tidytable::leads.(close, n = 1) - 1) * 100, 2)
      ) %>%
      tidytable::filter.(!is.na(return_1M)) %>%
      tidytable::select.(-c(open, high, low, close, volume, date))
  } else {

    #' Return
    .ticker_return <- tidytable::tidytable(
      ticker = ticker,
      month = NA_integer_,
      return_1M = NA_integer_
    )[0, ]
  }

  #' Missing
  .missing <- purrr::map2_dfr(
    .x = ticker[1],
    .y = c(1:12)[c(1:12) %notin% .ticker_return$month],
    ~ tidytable::tidytable(
      ticker = .x,
      month = .y
    )
  )

  #' Complete missing month
  complete_return <- .ticker_return %>%
    tidytable::bind_rows.(.missing) %>%
    tidytable::filter.(!is.na(date)) %>%
    tidytable::mutate.(
      return_1M = return_1M,
      month = format(date, "T%m")
    ) %>%
    tidytable::complete.(ticker, month) %>%
    tidytable::arrange.(ticker, month) %>%
    tidytable::select.(-date) %>%
    tidytable::pivot_wider.(
      names_from = month,
      values_from = return_1M
    )

  #' Return
  return(complete_return)
}
