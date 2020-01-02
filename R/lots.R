library(RCurl)
library(XML)
library(stringr)
library(rvest)
library(magick)
library(curl)
library(httr)
library(tidyverse)
library(zip)
# Filter level ------------------------------------------------------------
SLEEP = 0.5
# if (!file.exists("./jpgs/")) dir.create("./jpgs/")
# if (!file.exists("./meta_data/")) dir.create("./meta_data//")

# Get lots names on lots page
GetLotsURL <- function(.url_filtered, .url = URL, .sleep = 0.8){

  auction <- read_html(.url_filtered)
  auction_names <- auction %>% html_nodes(xpath = "//h6/a[@target ='_self']") %>% html_attr("href")
  if(auction_names %>% is_empty){

    auction_URLs <- NA

  } else{

  auction_URLs <- auction %>%
    html_nodes(xpath = "//h6/a[@target ='_self']") %>%
    html_attr("href") %>%
    paste0(.url, .)
  Sys.sleep(.sleep)

  }

  return(auction_URLs)

}

MetaTable <- function(.args = list(
  lot =lot_number,
  description = description,
  period = period,
  dimensions = dimensions[1:4],
  estimate_min = est_range$estimate_min,
  estimate_max = est_range$estimate_max,
  dom_price = dom_price), .US_table = US_table){
  out <- tryCatch(
    {
      # .args2 <- list(a = c(1,2), b = c(1,2))

      do.call(tibble, .args)

      # The return value of `readLines()` is the actual value
      # that will be returned in case there is no condition
      # (e.g. warning or error).
      # You don't need to state the return value via `return()` as code
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      message("Not the same number of elements in tbl")
      message("Try another combination")

      var_length <- map(.args, length) %>% unique %>% length

      if(var_length == 2 & .args$lot %>% length > .args$dom_price %>% length){

        lot_table <- left_join(.US_table, do.call(tibble, .args[names(.args) != "dom_price"]), by = "lot") %>% add_column(dom_price = NA)

      }else{

        lot_table <- character(0)

      }
      # Choose a return value in case of error
      return(lot_table)

    },
    warning=function(cond) {
      message("Data is matched based on US_price")
      return(NULL)
    },
    finally={

      message("Data is matched based on US_price")
    }
  )
  return(out)
}



filter <- expand.grid("Jewellery, Watches & Handbags", items$Month, items$Year[2:5]) %>% as.matrix
URL_FILTERED <- apply(filter, 1, URLBuilder)
auction_URLs <- map(URL_FILTERED, GetLotsURL) %>% unlist %>% na.omit

log_info <- list()
progress_bar <- txtProgressBar(min = 0, max = length(auction_URLs), style = 3)
 # 12:length(auction_URLs)
for (i in seq_along(auction_URLs)){
  progress_bar <- txtProgressBar(min = 0, max = length(auction_URLs), style = 3)
  auction_name <- auction_URLs[i] %>%
    basename %>%
    parse_character %>%
    gsub("\\..*", "", .)

      # Lot level ---------------------------------------------------------------
  lots <- read_html(auction_URLs[i])

  Sys.sleep(runif(1, 0.2, 1.2))

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

lot_table <- MetaTable(.args = args, .US_table = US_table) %>% add_column(time = loc_time["time"],
                                                                          loc = loc_time["loc"],
                                                                          auction = auction_name)

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

  # walk2(.x = URL_jpg[1:2],
  #       .y = path_jpg[1:2],
  #       ~download.file(.x, .y, mode = "wb"))
  for(i in seq_along(URL_jpg)){

    download.file(url = URL_jpg[i], path_jpg[i],  mode = "wb")
    Sys.sleep(runif(1, 0.3, 2))

  }

  # paths <- path_jpg %>% map(~gsub("^./", "", .)) %>% map_chr(~gsub("^jpgs", "", .))
  # files_zip <- dir(directory)
  # zip::zip(zipfile = directory, files = files_zip)
  # setwd("../")
  # zip::zipr(zipfile = auction_name, files = paths)
  table_path <- paste0("./meta_data/", auction_name, ".rdata")
  if (!file.exists(table_path)) save(lot_table, file = table_path)

  setTxtProgressBar(progress_bar, i)

  }

## Some files to zip up
dir.create(tmp <- tempfile())
cat("first file", file = file.path(tmp, "file1"))
cat("second file", file = file.path(tmp, "file2"))

zipfile <- tempfile(fileext = ".zip")
zipr(zipfile, tmp)

## List contents
zip_list(zipfile)

## Add another file
cat("third file", file = file.path(tmp, "file3"))
zipr_append(zipfile, file.path(tmp, "file3"))
zip_list(zipfile)
