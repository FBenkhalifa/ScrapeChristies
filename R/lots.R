library(RCurl)
library(XML)
library(stringr)
library(rvest)
library(magick)
library(curl)
library(httr)
library(tidyverse)
# Filter level ------------------------------------------------------------
SLEEP = 0.5

# if (!file.exists("./jpgs/")) dir.create("./jpgs/")
# if (!file.exists("./meta_data/")) dir.create("./meta_data//")

# Get lots names on lots page
auction <- read_html(URL_FILTERED)
auction_names <- auction %>% html_nodes(xpath = "//h6/a[@target ='_self']") %>% html_attr("href")
auction_URLs <- auction %>%
  html_nodes(xpath = "//h6/a[@target ='_self']") %>%
  html_attr("href") %>%
  paste0(URL, .)

for (i in seq_along(auction_URLs)){

  auction_name <- auction_URLs[i] %>%
    basename %>%
    parse_character %>%
    gsub("\\..*", "", .)

      # Lot level ---------------------------------------------------------------
  lots <- read_html(auction_URLs[i])

  Sys.sleep(SLEEP)

   # Get summary on lot page
  lots_summary <- lots %>% html_nodes("strong , em") %>% html_text

  URL_print <- lots %>% html_nodes(xpath = "//a[@target = '_blank' and @class ='print--page']") %>% html_attr("href")
  URL_results <- lots %>% html_nodes("#LotListings") %>% html_nodes(xpath = "//a") %>% html_attr("href") %>% .[1]



  # Lots level --------------------------------------------------------------

  lots_print <- read_html(URL_print)

  description <- lots_print %>% html_nodes(".lot-info .lot-description") %>% html_text

  period <- lots_print %>% html_nodes(".lot-maker") %>% html_text

  estimate <- lots_print %>% html_nodes(xpath = "//td[@class ='estimate']//span[@class ='lot-description'][1]") %>% html_text

  est_range <- estimate %>%
    str_split(., " - ") %>%
    do.call(rbind, .) %>%
    apply(., 2, parse_number) %>%
    as_tibble %>%
    set_names(c("estimate_min", "estimate_max"))

  dom_price <- lots_print %>% html_nodes(xpath = "//td[@class ='estimate']//span[@class ='lot-description'][2]") %>% html_text %>%
    parse_number

    dimensions <- lots_print %>% html_nodes(".medium-dimensions") %>% html_text

    auction <- lots_print %>% html_nodes(".browse-sale-title") %>% html_text(trim = TRUE)

    loc_time <- lots_print %>%
      html_nodes(".sale-number-location1") %>%
      html_text %>%
      gsub("[\t\r\n]", "", .) %>%
      str_split(., ",") %>% unlist %>%
      map_chr(trimws) %>%
      set_names(c("time", "loc"))


  # Results -----------------------------------------------------------------

  lots_results <- URL_results %>% read_html
  US_prices <- lots_results %>%
    html_nodes(xpath = "//span[contains(@id, 'dlResults_lblPrice_')]") %>%
    html_text() %>%
    parse_number


  # Table -------------------------------------------------------------------

  lot_table <- tibble(
    description = description,
    period = period,
    dimensions = dimensions,
    estimate_min = est_range$estimate_min,
    estimate_max = est_range$estimate_max,
    dom_price = dom_price,
    US_prices = US_prices,
    time = loc_time["time"],
    loc = loc_time["loc"],
    auction = auction_name
  ) %>% rowid_to_column(var = "lot")

  # Images ------------------------------------------------------------------
  URL_image <-  lots_print %>% html_nodes("#lot-list img") %>% html_attr("src")
  URL_jpg <-  URL_image %>% map_chr(~gsub("\\?.*", "", .))

  path_jpg <-   paste(paste0("jpgs/",
                             auction_URLs[i] %>%
                               basename %>%
                               parse_character %>%
                               gsub("\\..*", "", .)),
                      seq_along(URL_image), sep = "/Lot")
  walk2(.x = URL_jpg,
        .y = path_jpg,
        .f = download.file)

  table_path <- paste0("./meta_data/", auction_name, ".rdata")
  if (!file.exists(table_path)) save(lot_table, file = table_path)
}




getwd()



