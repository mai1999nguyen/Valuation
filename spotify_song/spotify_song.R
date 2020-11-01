#'
library(dplyr)
library(magrittr)
library(tibble)
library(ggplot2)
library(foreach)

utils::download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv", 
                     destfile = here::here("data","spotify_song.csv"), 
                     method = "libcurl",
                     quiet = FALSE, mode = "w",
                     cacheOK = TRUE)
raw <- utils::read.csv(file = here::here("data","spotify_song.csv"), 
                header = TRUE) %>% tibble::as_tibble()

dplyr::glimpse(raw)

#' Features
features <- names(raw)[12:23]

#' A
raw %>%
  dplyr::select(features) %>% 
  tidyr::pivot_longer(cols = tidyselect::everything(), names_to =  "feature", values_to = "value") %>%
  ggplot2::ggplot(mapping = aes(x = value, group = feature)) %+%
  geom_density(aes(color = feature), alpha = 0.5) +
  facet_wrap(~feature, ncol = 3, scales = 'free')

 convert_milisecond <- function(duration, by) {
   stopifnot(duration>0)
   
   ms <- switch(stringr::str_sub(by, 1L, 1L),
                "s" = 1000,
                "m" = 1000*60,
                "h" = 1000*60^2,
                "d" = 1000*60^2*24,
   )
   ms <- duration / ms
   return(ms)
 }
 
raw %>% dplyr::filter(energy > 0.75, convert_milisecond(duration_ms, "m") >5 )


get_feature <- function(.data, number) {
  .range <- .data %>% 
    dplyr::filter(dplyr::row_number() == !!number) %>% 
    dplyr::select(12:21) %>% 
    as.list()
  return(.range)
}


eculean <- function(...) {
  .d <- rlang::dots_list(...,
                         .named = FALSE,
                         .ignore_empty = "none",
                         .preserve_empty = FALSE,
                         .homonyms ="error",
                         .check_assign = FALSE)
  .eculean <- 0
  
  for (i in 1:length(.d[[1]])) {
    
    .data <- (.d[[1]][[i]] - .d[[2]][[i]])^2
    .eculean <- sum(.eculean,.data)
  }
  
  eculean <- sqrt(.eculean)
  return(eculean)
  
}

ecul <- foreach::foreach(i = 1:1000, .combine = dplyr::bind_rows, .errorhandling = "remove") %do% {
  
  hihi <- get_feature(raw, 1)
  .sub <- get_feature(raw, i)
  .ecu <- eculean(hihi, .sub) %>% 
    tibble::enframe(name = NULL, value = "eculean") %>% 
    dplyr::mutate(id = i)
  cli::cli_alert_success("done {i}")
  return(.ecu)
}
