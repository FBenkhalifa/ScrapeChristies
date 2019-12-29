library(httr)
library(xml2)
library(rvest)
library(RSelenium)
library(tidyverse)
library(stringr)

# Define static Variables
URL_FILTER <- "https://www.christies.com/Results"
FILTER_BUTTON <- '//*[@id="refine-results"]/cc-filters/h4[2]'
BACK <- '//*[@id="refine-results"]/cc-filters/div[1]/ul/li[1]/cc-multi-select-box/div/label'

# Set up client -----------------------------------------------------------
cursor$highlightElement()
cursor$clickElement()

# 1 Start connection
rD <- rsDriver(verbose = FALSE, chromever = "79.0.3945.36" )
myclient <- rD$client

# 2 Navigate to Christies page
myclient$navigate(URL_FILTER)


# Extract filter ----------------------------------------------------------


# 1 Start from the auction side and click on filter
cursor <- myclient$findElement(using = "xpath", value = FILTER_BUTTON)
cursor$clickElement()

# 2 Extract available options
items <- text_box %>%
  html_text(trim = TRUE) %>%
  gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", ., perl = TRUE) %>%
  strsplit(., "\n")

# 3 Convert to vector to coerce them to a tbl
items_list <- items %>% unlist(use.names = FALSE)

# 4 Extract the nodes where the IDs can be found
id_nodes <- text_box %>%
  html_nodes(xpath = '//li[@class="item-container--dropdown-items--item ng-scope"]//*[@id]')

# 5 Extract ID values
id_values <- id_nodes %>% html_attr("id") %>% as.numeric %>% unname

# 6 Check if there are any items which appear on the screen but have no ID assigned
id_missing <- !((items_list) %in% (id_nodes %>% html_text))
items_list[id_missing]# The most recent auctions cannot be found

# 7Create a tbl with the item and the id value
tibble(item = items_list[!id_missing], id = id_values)

