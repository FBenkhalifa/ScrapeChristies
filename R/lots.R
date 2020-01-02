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
GetLotsURL <- function(.url_filtered, .sleep = 0.8){
auction <- read_html(URL_FILTERED)
auction_names <- auction %>% html_nodes(xpath = "//h6/a[@target ='_self']") %>% html_attr("href")
auction_URLs <- auction %>%
  html_nodes(xpath = "//h6/a[@target ='_self']") %>%
  html_attr("href") %>%
  paste0(URL, .)
Sys.sleep(.sleep)
return(auction_URLs)
}

filter <- expand.grid("Jewellery, Watches & Handbags", items$Month, items$Year[2:5])
URLBuilder
GetLotsURL(URL_FILTERED)
log_info <- list()

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

  if(URL_print %>% is_empty){

    warning <- paste0("Auction #", i, ",", auction_name, " has no infos print document")
    log_info[length(log_info)+1] <- c(auction_name, warning)
    print(warning)

    next

  }

  URL_results <- lots %>% html_nodes("#LotListings") %>% html_nodes(xpath = "//a") %>% html_attr("href") %>% .[1]



  # Lots level --------------------------------------------------------------

  lots_print <- read_html(URL_print)

######

  lots_print_box <- lots_print %>%  html_nodes(xpath = "//table[@id ='lot-list']//td[@class = 'lot-info']")

  lot_number <- lots_print %>% html_nodes(".lot-number") %>% html_text %>% parse_number()

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
######


  # Results -----------------------------------------------------------------

  lots_results <- URL_results %>% read_html
  US_prices <- lots_results %>%
    html_nodes(xpath = "//span[contains(@id, 'dlResults_lblPrice_')]") %>%
    html_text() %>%
    parse_number

  US_prices_lots <- lots_results %>%
    html_nodes(xpath = "//span[contains(@id, 'dlResults_lblLotNumber_')]") %>%
    html_text() %>%
    parse_number

  US_table <- tibble(lot = US_prices_lots, US_prices = US_prices)

  # Table -------------------------------------------------------------------

  args <- list(
    lot =lot_number,
    description = description,
    period = period,
    dimensions = dimensions,
    estimate_min = est_range$estimate_min,
    estimate_max = est_range$estimate_max,
    dom_price = dom_price
  )

lot_table <- MetaTable(.args = args, .US_table = US_table)

if(lot_table %>% is_empty){

  log_info[i] <- paste0("No matches for metadata possible in ", auction_name)
  next

}
  # Images ------------------------------------------------------------------
  URL_image <-  lots_print %>% html_nodes("#lot-list img") %>% html_attr("src")
  URL_jpg <-  URL_image %>% map_chr(~gsub("\\?.*", "", .))

  directory <- paste0("./jpgs/", auction_name)

  if(!dir.exists(paths = directory)) dir.create(path = directory)

  path_jpg <-   paste0(paste(paste0("./jpgs/",
                             auction_URLs[i] %>%
                               basename %>%
                               parse_character %>%
                               gsub("\\..*", "", .)),
                      seq_along(URL_image), sep = "/Lot"),
                      ".jpg")

  walk2(.x = URL_jpg[1:2],
        .y = path_jpg[1:2],
        ~download.file(.x, .y, mode = "wb"))

  table_path <- paste0("./meta_data/", auction_name, ".rdata")
  if (!file.exists(table_path)) save(lot_table, file = table_path)

  }


