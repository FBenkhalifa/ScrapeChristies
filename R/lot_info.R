GetLotInfo <- function(.lots_print){

  # 1 Get the number of the lot
  lot_number <- lots_print %>% html_nodes(".lot-number") %>% html_text %>% parse_number()

  # 2 Get the text description of the lot
  description <- lots_print %>% html_nodes(".lot-info .lot-description") %>% html_text

  # 4 Get the estimated range of the lot
  estimate <- lots_print %>% html_nodes(xpath = "//td[@class ='estimate']//span[@class ='lot-description'][1]") %>% html_text
  est_range <- estimate %>%
    str_split(., " - ") %>%
    do.call(rbind, .) %>%
    apply(., 2, parse_number) %>%
    as_tibble %>%
    set_names(c("estimate_min", "estimate_max"))

  # 5 Get the price of the lot
  price <- lots_print %>% html_nodes(xpath = "//td[@class ='estimate']//span[@class ='lot-description'][2]") %>% html_text %>%
    parse_number

  # 6 Get the currency of the lot
  currency <- lots_print %>% html_nodes(xpath = "//td[@class ='estimate']//span[@class ='lot-description'][2]") %>% html_text %>% gsub('[,0-9]+', '', .)

  # 7 Get the auction name
  auction <- lots_print %>% html_nodes(".browse-sale-title") %>% html_text(trim = TRUE)

  # 8 Get date and time
  loc_time <- lots_print %>%
    html_nodes(".sale-number-location1") %>%
    html_text %>%
    gsub("[\t\r\n]", "", .) %>%
    str_split(., ",") %>% unlist %>%
    map_chr(trimws) %>%
    set_names(c("time", "loc"))

  return(
    list(
      lot =lot_number,
      description = description,
      estimate_min = est_range$estimate_min,
      estimate_max = est_range$estimate_max,
      price = price,
      currency = currency,
      auction = auction,
      location = loc_time["loc"],
      time = loc_time["time"]
    )
  )
}

lots_print <- read_html("https://www.christies.com/lotfinder/print_sale.aspx?saleid=28198&lid=1")
currency <- lots_print %>% html_nodes(xpath = "//td[@class ='estimate']//span[@class ='lot-description'][2]") %>% html_text %>% gsub('[,0-9]+', '', .)

GetLotInfo(lots_print)
