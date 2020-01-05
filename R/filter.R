library(httr)
library(xml2)
library(rvest)
library(RSelenium)
library(tidyverse)
library(robotstxt)

# I Check disallowed files -------------------------------------------------------

robotstxt("https://www.christies.com/") %>%
.$permissions %>%
  as_tibble %>%
  filter(field == "Disallow" & useragent == "*") %>%
  select(value)

# Define static Variables
URL <- "https://www.christies.com"
URL_FILTER <- "https://www.christies.com/Results" # Link to the filter page
FILTER_BUTTON <- '//*[@id="refine-results"]/cc-filters/h4[2]' # Xpath to the lot print button

# II Get filter options -----------------------------------------------------------

# 1 Start connection
rD <- rsDriver(verbose = FALSE, chromever = "79.0.3945.36" ) # For me only chrome version 79.0.3945.36 worked, I had to download it
myclient <- rD$client

# 2 Navigate to Christies page
myclient$navigate(URL_FILTER)


# A Extract filter in RSelenium Session ----------------------------------------------------------

# 1 Start from the auction side and click on filter
cursor <- myclient$findElement(using = "xpath", value = FILTER_BUTTON)
cursor$clickElement()

# 2 get names of first level filter
l1 <- read_html(myclient$getPageSource()[[1]]) %>%
  html_nodes(".item-container--label") %>%
  html_text(trim = TRUE) %>%
  gsub("\n.*$", "", .)

# 3 Get main element
text_box <- read_html(myclient$getPageSource()[[1]]) %>%
  html_nodes(".item-container--dropdown-items") %>%
  set_names(l1)

# 4 Close RSelenium session
rD$server$stop()
rm(rD)
gc()


# B Structure the information to create filter dictionary ----------------

# 1 Extract available options
items <- text_box %>%
  html_text(trim = TRUE) %>%
  gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", ., perl = TRUE) %>%
  strsplit(., "\n")

items

# 2 Extract the nodes where the IDs can be found
id_nodes <- text_box %>%
  html_nodes(xpath = '//li[@class="item-container--dropdown-items--item ng-scope"]//*[@id]')

# 3 Extract ID values
id_values <- id_nodes %>%
  html_attr("id") %>%
  as.numeric %>%
  enframe(name = NULL)

# 4 Get corresponding names of id nodes
id_names <- id_nodes %>%
  html_text %>%
  enframe(name = NULL)

# 5 Bind both together to get a tbl
id_tbl <- bind_cols(id_names, id_values) %>% set_names(c("l2", "id"))

# 6 Convert list to long format
items_tbl <- items %>% enframe(name = "l1", value = "l2") %>% unnest(cols = "l2")

# 7 Create a tbl with the item and the id value
full_tbl <- left_join(items_tbl, id_tbl, key = "l2")

# 8 Check if there are any items which appear on the screen but have no ID assigned
full_tbl %>% filter(id %>% is.na)


# C Construct URLs resulting from the filter --------------------------------

# 1 Hardcode the URLs which result from specifying level 1 in the filter
URL_l1 <- c(Location = "locations",
            Category = "scids",
            Month = "month",
            Year = "year") %>%
  enframe(name = "l1", value = "id_l1")

# 2 Create a dictionary to look up the information to build the URLs
id_dict <- left_join(full_tbl, URL_l1, by = "l1")

# 3 Create a function which gets as input filter keywords and returns corresponding URLs

# Arguments:
# .items = A string vector with the filter requests
# .id_dict = A tbl whcih maps a filter option to the corresponding ID value
# .url = A string with the URL to the Chrisites website

URLBuilder <- function(.items, .id_dict = id_dict, .url = URL){

  # 1 Match selected id with id dictionary
  id_sel <- .id_dict %>% filter(l2 %in% .items) %>% drop_na

  # 2 Split into level 1 groups
  groups <- id_sel %>% select(id_l1, id) %>% group_by(id_l1)

  # 3 Construct the URL parameters for each group
  URL_params <- groups %>%
    group_split() %>%
    set_names(group_keys(groups) %>% pull) %>%
    map_chr(~paste(.$id, collapse = "%7C")) %>%
    enframe()

  # 4 Concatenate first time
  search_queries <- URL_params %>% unite(col = "query", sep = "=")

  # 5 Build final URL
  new_URL <- paste0(paste0(URL, "/Results?"), paste(search_queries$query, collapse = "&"))

  return(new_URL)
}

# 4 Check quicly if function works
filter_vec <- c("Jewellery, Watches & Handbags", "2019", "December")
URLBuilder(.items = filter_vec) # Copy to browser to check of it works








# II Scrape the data ------------------------------------------------------------

# A Set up the directory
if (!dir.exists("./jpgs/")) dir.create("./jpgs/")
if (!dir.exists("./meta_data/")) dir.create("./meta_data//")

#---- B Get information from Level 1 as described in the paper ----

# 1 Restrict search to Jewellry watches & Handbags from the last 4 years in New York and LA
filter_opt <- expand.grid("Jewellery, Watches & Handbags", items$Month, items$Year[2:5]) %>% as_tibble
locations <- c("Los Angeles", "New York")
for (i in locations) filter_opt <- filter_opt %>% add_column(!!i := rep(i, nrow(filter)))

# 2 Get URls for level 1
URL_filter_opt <- apply(filter, 1, URLBuilder) # These pages display the auctions with the corresponding filtered auctins
                                               # Note that the page architecture demands for every month in every year a unique URL


#---- C Get information from Level 2 as described in the paper ----
# Here two functions are defined which will be used in the loop which finally scrapes the data

# 1 Get the URLs to level 2

# This function takes an URL fromt the filtered results and creates URLs level 2 for all of these pages
# Params:
# .url_filtered: A string with the URL of the filtered results
# .sleep: A double specifying how long to wait after each call of the function

GetLotsURL <- function(.url_filtered, .sleep = 0.8){

  # 1 Read the html of the filtered results
  auction <- read_html(.url_filtered)

  # 2 Extract the auctions names which needs to be used to construct the URLs to level 2
  auction_names <- auction %>% html_nodes(xpath = "//h6/a[@target ='_self']") %>% html_attr("href")

  # 3 Catch filters which have no results
  if(auction_names %>% is_empty){

    auction_URLs <- NA

  } else{

    # 4 Get the final URLs to the auctions
    auction_URLs <- auction %>%
      html_nodes(xpath = "//h6/a[@target ='_self']") %>%
      html_attr("href") %>%
      paste0(.url, .)

    # 5 Sleep to
    Sys.sleep(.sleep)

  }

  return(auction_URLs)

}

# 3 Get URls for level 2
auction_URLs <- map(URL_FILTERED, GetLotsURL) %>% unlist %>% na.omit


# 2 Create function which is able to scrape the relevant information on level 4

# This is a function which scrapes the data from a print lot list. In some cases
# the list is not complete and an error occurs when the corresponding elements in
# the HTML have different length. If this is the case, it is mostly  due to the
# achieved price not beeing assigned to every lot. If this happens we go to
# the lot results page which only stores all prices available plus corresponding
# lot number. The function then scrapes these prices and matches the information
# from the lot print page with the price list by the lot number.

# params:
# .args: A named list with the informations read from the html on the print lot list
# .res_table A tbl with the lot results information
MetaTable <- function(.args = list(
  lot =lot_number,
  description = description,
  estimate_min = est_range$estimate_min,
  estimate_max = est_range$estimate_max,
  price = price), .res_table = res_table){

  out <- tryCatch(

    {
      # 1 Check if tibble can be constructed
      do.call(tibble, .args)

    },
    error=function(cond) {
      message("Probably not the same number of elements in tbl")
      message("Try another combination")

      # 2 Check if the cause of the error is the price having differing length from the rest
      var_length <- map(.args, length) %>% unique %>% length
      if(var_length == 2 & .args$lot %>% length > .args$price %>% length){

        # 3 If true join with lot results list by lot number
        lot_table <- inner_join(.res_table, do.call(tibble, .args[names(.args) != "price"]), by = "lot") #%>% add_column(dom_price = NA)

      }else{

        # 4 If false sth else occured and an empty string is returned
        lot_table <- character(0)

      }
      # Choose a return value in case of error
      return(lot_table)

    },
    warning=function(cond) {
      message("Data is matched based on result table")
      return(NULL)
    },
    finally={

      message("Data is matched based on result table")
    }
  )
  return(out)
}


log_info <- list()



# D Run the loop ----------------------------------------------------------



for (i in seq_along(auction_URLs)){

  # 1 Set progress bar
  progress_bar <- txtProgressBar(min = 0, max = length(auction_URLs), style = 3)

  # 2 Get auction name to identify the loop later
  auction_name <- auction_URLs[i] %>%
    basename %>%
    parse_character %>%
    gsub("\\..*", "", .)

  # 3 Read the landing page of the lots
  lots <- read_html(auction_URLs[i])

  # 4 Take a short break
  Sys.sleep(runif(1, 0.2, 1.2))

  # 5 Get print URL
  URL_print <- lots %>% html_nodes(xpath = "//a[@target = '_blank' and @class ='print--page']") %>% html_attr("href")

  # 6 Skip the lot in case there is no URL print on the page as e.g. here: https://www.christies.com/Christies-Jewels-Online-28172.aspx?lid=1&dt=050120201047
  if(URL_print %>% is_empty){

    warning <- paste0("Auction # ", i, ",", auction_name, " has no infos for print document")
    log_info[[auction_name]][["Print Document"]] <- "No infos for print document"
    print(warning)

    next

  }

  # Get the URl for the result list where we get the
  URL_results <- lots %>% html_nodes("#LotListings") %>% html_nodes(xpath = "//a") %>% html_attr("href") %>% .[1]



  #---- b. Lots level/Print lot list -----

  lots_print <- read_html(URL_print)

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

  # 6 Get the auction name
  auction <- lots_print %>% html_nodes(".browse-sale-title") %>% html_text(trim = TRUE)

  # 7 Get date and time
  loc_time <- lots_print %>%
    html_nodes(".sale-number-location1") %>%
    html_text %>%
    gsub("[\t\r\n]", "", .) %>%
    str_split(., ",") %>% unlist %>%
    map_chr(trimws) %>%
    set_names(c("time", "loc"))

  #---- c. Lots level/Result list -----

  lots_results <- URL_results %>% read_html

  # 1 Get the prices from result list
  res_prices <- lots_results %>%
    html_nodes(xpath = "//span[contains(@id, 'dlResults_lblPrice_')]") %>%
    html_text() %>%
    parse_number

  # 2 Get the lot number
  res_prices_lots <- lots_results %>%
    html_nodes(xpath = "//span[contains(@id, 'dlResults_lblLotNumber_')]") %>%
    html_text() %>%
    parse_number

  # 3 Merge lot number with corresponding price
  res_table <- tibble(lot = res_prices_lots, price = res_prices)

  #---- d. Construct the final lot table -----
  lot_table <- MetaTable(.args = list(
    lot =lot_number,
    description = description,
    estimate_min = est_range$estimate_min,
    estimate_max = est_range$estimate_max,
    price = price),
    .res_table = res_table) %>%
    drop_na

  # 1 Skip the loop and write to if no lot table could be constructed
  if(lot_table %>% is_empty){

    log_info[[auction_name]][["Lot table"]] <- paste0("No matches for metadata possible in ", auction_name)
    next

  }

  # 2 Save the rdata file to the corresponding directory
  table_path <- paste0("./meta_data/", auction_name, ".rdata")
  if (!file.exists(table_path)) save(lot_table, file = table_path)
  if(FALSE){
    #------ d. Download images ------

    # 1 Get the URL to the jpg
    URL_image <-  lots_print %>% html_nodes("#lot-list img") %>% html_attr("src")

    # 2 Clean the URL
    URL_jpg <-  URL_image %>% map_chr(~gsub("\\?.*", "", .))

    # 3 Create directory path to save the files in
    directory <- paste0("./jpgs/", auction_name)

    # 4 Check if directory exists and if not, create one
    if(!dir.exists(paths = directory)) dir.create(path = directory)

    # 5 Construct the jpg paths
    path_jpg <-   paste0(paste(paste0("./jpgs/",
                                      auction_URLs[i] %>%
                                        basename %>%
                                        parse_character %>%
                                        gsub("\\..*", "", .)),
                               seq_along(URL_image), sep = "/Lot"),
                         ".jpg")

    # 6 Loop over the paths and download jpgs
    for(j in seq_along(URL_jpg)){

      # 1 Check if file exists and skip download if TRUE
      if(!file.exists(path_jpg[j])){

        # 2 Download
        download.file(url = URL_jpg[j], path_jpg[j],  mode = "wb")

        # § Give server time to chill
        Sys.sleep(runif(1, 0.3, 2))
      }

    }

  }

}





