library(RCurl)
library(XML)
library(stringr)
library(rvest)
library(magick)
library(curl)
library(httr)
library(tidyverse)
# Filter level ------------------------------------------------------------

# Get lots names on lots page
auction <- read_html(URL_FILTERED)
auction_names <- auction %>% html_nodes(xpath = "//h6/a[@target ='_self']") %>% html_attr("href")
auction_URLs <- auction %>%
  html_nodes(xpath = "//h6/a[@target ='_self']") %>%
  html_attr("href") %>%
  paste0(URL, .)

# Lot level ---------------------------------------------------------------
lots <- read_html(auction_URLs[1])
# Get summary on lot page
lots_summary <- lots %>% html_nodes("strong , em") %>% html_text

URL_print <- lots %>% html_nodes(xpath = "//a[@target = '_blank' and @class ='print--page']") %>% html_attr("href")
URL_results <- lots %>% html_nodes("#LotListings") %>% html_nodes(xpath = "//a") %>% html_attr("href") %>% .[1]



# Lots level --------------------------------------------------------------

lots_print <- read_html(URL_print)

description <- lots_print %>% html_nodes(".lot-info .lot-description") %>% html_text
period <- lots_print %>% html_nodes(".lot-maker") %>% html_text
estimate <- lots_print %>% html_nodes(xpath = "//td[@class ='estimate']//span[@class ='lot-description'][1]") %>% html_text
price <- lots_print %>% html_nodes(xpath = "//td[@class ='estimate']//span[@class ='lot-description'][2]") %>% html_text
dimensions <- lots_print %>% html_nodes(".medium-dimensions") %>% html_text

# Results -----------------------------------------------------------------

lots_results <- URL_results %>% read_html
US_prices <- lots_results %>%
  html_nodes(xpath = "//span[contains(@id, 'dlResults_lblPrice_')]") %>%
  html_text() %>%
  sub(",", ".", ., fixed = TRUE) %>%
  as.numeric()

lot_table <- tibble(
  description = description,
  period = period,
  estimate = estimate,
  price = price,
  dimensions = dimensions,
  US_prices = US_prices
) %>% rowid_to_column(var = "id")

# Images ------------------------------------------------------------------
URL_image <-  lots_print %>% html_nodes("#lot-list img") %>% html_attr("src")
URL_image <-  URL_image %>% map_chr(~gsub("\\?.*", "", .))

magick::image_read(URL_image[2])
magick::image_read(URL_image[1])

image_name <- URL_image
paste0("jpgs/", basename(URL_image))
if (!file.exists("jpgs/")) dir.create("jpgs/")

jpg_path <- paste0("jpgs/",basename(jpg_url))
purrr::walk2(.x = jpg_url,
             .y = jpg_path,
             .f = download.file)











