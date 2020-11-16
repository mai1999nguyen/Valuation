#' Snapshot Ticker Return by Monthly
#'
#' @description get data from SSI and calculate the return monthly
#' @noRd
NULL

#' [Load]
# Load ====

#' Internal
source("R/internal.R")

#' Snapshot
c(
  purrr::map_chr(c("snapshot_function"), ~ file.path("basement", "snapshot", glue::glue("{.}.R"))),
  purrr::map_chr(c("SSI_api", "SSI_function"), ~ file.path("API", "SSI", glue::glue("{.}.R")))
) %>%
  purrr::walk(~ source(.))

#' [Config]
# Config ====
.__config_snapshot_return__ <- list(
  folder = file.path("basement", "snapshot"),
  ticker = list(
    Energy = c("PXS", "PJT", "BSR", "GSP", "PVT", "NBC", "PGS", "PVB", "ASP", "APP"),
    Materials = c("NSH", "HLY", "QBS", "PDB", "HAI", "STP", "DNP", "NFC", "TBX", "DAC"),
    Industrials = c("VMS", "SSG", "SDT", "ARM", "PVO", "CLM", "SKG", "CTT", "VNF", "PC1"),
    Consumer_Discretionary = c("HAT", "DSN", "TMT", "OCH", "STT", "TET", "BDB", "IBC", "COM", "SGH"),
    Consumer_Staples = c("BLF", "AAM", "HNG", "THB", "SLS", "MLS", "NGC", "HGA", "HLG", "SSC"),
    Health_Care = c("DCL", "MKV", "DHT", "SPM", "PME", "DBD", "DMC", "DNM", "IMP", "OPC"),
    Financials = c("TPB", "FIT", "APS", "MBB", "HCM", "VIG", "OGC", "BID", "TCB", "CTS"),
    Information_Technology = c("POT", "ELC", "VIE", "TST", "CMG", "KST", "FPT", "UNI", "DGW", "SMT"),
    Communication_Services = c("ECI", "DAD", "LBE", "EBS", "DAE", "BST", "ADC", "BED", "SED", ""),
    Utilities = c("GEG", "LDW", "PGD", "SEB", "NTH", "SHP", "SJD", "BWE", "VSH", "GDW"),
    Bank = c("ACB", "MBB", "VCB", "BID", "CTG", "VIB", "LPB", "TPB", "HDB", "STB", "TCB", "VPB"),
    Real_Estate = c("NVT", "KAC", "HDC", "KBC", "NRC", "VPH", "TIX", "PDR", "VCR", "HLD")
  ),
  year = 2020
)

#' [Component]
# Component ====

#' [TICKER PROFILE]
#' Ticker Profile
ticker_profile <- .__config_snapshot_return__$ticker %>%
  unlist() %>%
  unname() %>%
  purrr::map_dfr(~ SSI_api_company_profile(ticker = .)) %>%
  tidytable::select.(ticker = symbol, marketcap) %>%
  tidytable::mutate.(market_cap = round(as.numeric(marketcap) / 10^9, 0))

#' Ticker Component
ticker_component <- SSI_stock_list()

#' TODO: Reduce Company with MC > 1.000 VND bils.

#' Ticker Snapshot
ticker_return_snapshot <- names(gics_ticker) %>%
  purrr::map_dfr(
    ~ mutate.(snapshot_monthly(ticker = gics_ticker[[.]], year = .__config_snapshot_return__$year), gics_sector = .)
  ) %>%
  tidytable::left_join.(tidytable::select.(ticker_component, ticker, ticker_name_english), by = "ticker") %>%
  tidytable::mutate.(
    ticker_name_english = stringr::str_replace(
      ticker_name_english,
      pattern = "Joint Stock Company",
      replacement = "JSC"
    )
  ) %>%
  tidytable::left_join.(ticker_profile, by = c("ticker" = "symbol")) %>%
  tidytable::mutate.(
    .by = ticker,
    year_accumulated_return = (prod(T01 / 100 + 1,
      T02 / 100 + 1,
      T03 / 100 + 1,
      T04 / 100 + 1,
      T05 / 100 + 1,
      T06 / 100 + 1,
      T07 / 100 + 1,
      T08 / 100 + 1,
      T09 / 100 + 1,
      T10 / 100 + 1,
      T11 / 100 + 1,
      T12 / 100 + 1,
      na.rm = TRUE
    ) - 1) * 100
  ) %>%
  tidytable::relocate.(ticker_name_english, marketcap, .after = ticker)

#' [Write]
# Write ====
#' Snapshot
vroom::vroom_write(
  x = ticker_return_snapshot,
  path = file.path(.__config_snapshot_return__$folder, "data", glue::glue("ticker_return_snapshot.tsv")),
  delum = "\t"
)

#' Capture Configuration
base::write(
  x = jsonlite::toJSON(.__config_snapshot_return__),
  file = file.path(.__config_snapshot_return__$folder, "data", "snapshot_return_report.json")
)
