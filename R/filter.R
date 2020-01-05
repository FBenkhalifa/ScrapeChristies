library(httr)
library(xml2)
library(rvest)
library(RSelenium)
library(tidyverse)


# Check robots text -------------------------------------------------------


library(robotstxt)
paths_allowed("http://google.com/")
rtxt <- robotstxt(domain="wikipedia.org")
class(rtxt)
rtxt$check(paths = c("/","api/"), bot = "*")
rtxt$check(paths = c("/","api/"), bot = "Orthogaffe")
r_text        <- get_robotstxt("nytimes.com")


paths_allowed("https://www.christies.com/")
rtxt <- robotstxt("https://www.christies.com/")
r_text        <- get_robotstxt("https://www.christies.com/")


RobotsCheck <- function(URL, useragent, field){


  rtxt$permissions %>% as_tibble %>% filter(field == "Disallow" & useragent == "*") %>% select(value)



}

paths_allowed(paths = "https://www.christies.com/", domain = "auto", bot = "*",
              user_agent = utils::sessionInfo()$R.version$version.string,
              warn = TRUE, force = FALSE,
              ssl_verifypeer = c(1, 0), use_futures = TRUE, robotstxt_list = NULL)


# Define static Variables
URL_FILTER <- "https://www.christies.com/Results"
FILTER_BUTTON <- '//*[@id="refine-results"]/cc-filters/h4[2]'
BACK <- '//*[@id="refine-results"]/cc-filters/div[1]/ul/li[1]/cc-multi-select-box/div/label'

# Set up client -----------------------------------------------------------
cursor$highlightElement()
cursor$clickElement()
myclient$quit()
myclient$close()
rD$server$stop()
rm(rD)
gc()
# 1 Start connection
rD <- rsDriver(verbose = FALSE, chromever = "79.0.3945.36" )
myclient <- rD$client

# 2 Navigate to Christies page
myclient$navigate(URL_FILTER)


# Extract filter ----------------------------------------------------------

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

# 2 Extract available options
items <- text_box %>%
  html_text(trim = TRUE) %>%
  gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", ., perl = TRUE) %>%
  strsplit(., "\n")

# 3 Convert list to long format
items_tbl <- items %>% enframe(name = "l1", value = "l2") %>% unnest(cols = "l2")

# 4 Extract the nodes where the IDs can be found
id_nodes <- text_box %>%
  html_nodes(xpath = '//li[@class="item-container--dropdown-items--item ng-scope"]//*[@id]')

# 5 Extract ID values
id_values <- id_nodes %>%
  html_attr("id") %>%
  as.numeric %>%
  enframe(name = NULL)

# 6 Get corresponding names of id nodes
id_names <- id_nodes %>%
  html_text %>%
  enframe(name = NULL)

# 7 Bind both together to get a tbl
id_tbl <- bind_cols(id_names, id_values) %>% set_names(c("l2", "id"))

# 8 Create a tbl with the item and the id value
full_tbl <- left_join(items_tbl, id_tbl, key = "l2")

# 9 Check if there are any items which appear on the screen but have no ID assigned
full_tbl %>% filter(id %>% is.na)


# Construct URLs resulting from the filter --------------------------------
# 1 Hardcode the URLs which result from specifying level 1 in the filter
URL_l1 <- c(Location = "locations",
            Category = "scids",
            Month = "month",
            Year = "year") %>% enframe(name = "l1", value = "id_l1")

# 2 Create a dictionary to look up the information to build the URLs
id_dict <- left_join(full_tbl, URL_l1, by = "l1")
# Comment: It is not optimal that the URLs are hardcoded but it will save the
# purpose for now

URL <- "https://www.christies.com"

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


vec1 <- c(c("African, Oceanic & Pre-Columbian Art", "Antiquities", "Asian Art",
            "Books & Manuscripts", "Fine Art", "Furniture & Decorative Arts"),
          c("Los Angeles", "Milan", "Mumbai", "New York", "Online"),
          c("June"),
          c( "2015"))

vec2 <- c("Fine Art")

filter <- c("Jewellery, Watches & Handbags", "2019", "December")

URL_FILTERED <- URLBuilder(.items = filter)

rD$server$stop()
rm(rD)
gc()
### Here, I construct the URLs. It contains a bit of hardcoding which is not
### optimal but serves the purpose

