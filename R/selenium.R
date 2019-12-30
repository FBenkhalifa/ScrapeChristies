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
myclient$close()
rD$server$stop()

# 1 Start connection
rD <- rsDriver(verbose = FALSE, chromever = "79.0.3945.36" )
myclient <- rD$client

# 2 Navigate to Christies page
myclient$navigate(URL_FILTER)


# Extract filter ----------------------------------------------------------


# 1 Start from the auction side and click on filter
cursor <- myclient$findElement(using = "xpath", value = FILTER_BUTTON)
cursor$clickElement()

# 2 get names of first level
l1 <- read_html(myclient$getPageSource()[[1]]) %>%
  html_nodes(".item-container--label") %>%
  html_text(trim = TRUE) %>%
  gsub("\n.*$", "", .)

# 3 Store the names of the level 2 filter criteria
l2 <- read_html(myclient$getPageSource()[[1]]) %>%
  html_nodes(".checkbox--label") %>%
  html_text(trim = TRUE)

text_box <- read_html(myclient$getPageSource()[[1]]) %>%
  html_nodes(".item-container--dropdown-items") %>%
  set_names(l1)

text_box %>%
  map(~html_nodes(., ".checkbox--label") %>% html_text(trim = TRUE))

text_box %>%
  map(~html_nodes(., ".checkbox--label") %>% html_attr(trim = TRUE))

text_box[4] %>% html_nodes(., ".checkbox--label")  %>% html_attr("id")
# 3 Get xpaths for the filters on level 1
xpath_l1 <- paste0('//*[@id="refine-results"]/cc-filters/div[1]/ul/li[',
                          seq_along(l1),
                          ']/cc-multi-select-box/div/label') %>%
  set_names(l1)

text_box %>% html_text(trim = TRUE) %>% gsub("\n", "", .) %>% str_squish
text_box %>%
  html_text(trim = TRUE) %>%
  gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", ., perl = TRUE) %>%
  strsplit(., "\n")
## Write a function to store the xpaths for the filters

xpath_list <- list()
for (i in xpath_l1[1:3]) {
   i = xpath_l1[1]
   i = xpath_l1[2]
   i = xpath_l1[3]
  # 1 Navigate client to level 1 filter element and click
  cursor <- myclient$findElement(using = "xpath", value = i)
  cursor$highlightElement()
  cursor$clickElement()
  Sys.sleep(1)

  # 3 Store the names of the level 2 filter criteria
  l2 <- read_html(myclient$getPageSource()[[1]]) %>%
    html_nodes(".checkbox--label") %>%
    html_text(trim = TRUE)

  # 4 Construct xpaths able to click the respective button
  xpath_str <- paste0('div/div[2]/ul/li[', c(1:length(l2)), ']/label') %>% set_names(l2)
  xpath_l2 <- xpath_str %>% map_chr(~gsub("label$", ., i))

  # 5 Navigate client to level 1 filter element and click
  # cursor <- myclient$findElement(using = "xpath", value = xpath_l2[1])
  xpath_list[i] <- list(xpath_l2)

  # 2 Bring cursor back
  cursor$findElement(using = "xpath",
                     value = BACK)
  cursor$highlightElement()
  cursor$clickElement()

}

xpath_list %>% set_names(xpath_l1)

# Location
.item-container--dropdown-items


# Extract available options
items <- text_box %>%
  html_text(trim = TRUE) %>%
  gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", ., perl = TRUE) %>%
  strsplit(., "\n")

# Convert to vector to coerce them to a tbl
items_list <- items %>% unlist(use.names = FALSE)

# Extract the nodes where the IDs can be found
id_nodes <- text_box %>%
  html_nodes(xpath = '//li[@class="item-container--dropdown-items--item ng-scope"]//*[@id]')

# Extract ID values
id_values <- id_nodes %>% html_attr("id") %>% as.numeric %>% unname

# Check if there are any items which appear on the screen but have no ID assigned
id_missing <- !((items_list) %in% (id_nodes %>% html_text))

items_list[id_missing]# The most recent auctions cannot be found

# Create a tbl with the item and the id value
tibble(item = items_list[!id_missing], id = id_values)

# Get lots information ----------------------------------------------------

# 3 Extract the lots for category one
html_filter <- read_html(myclient$getPageSource()[[1]])
html_filter %>%
  html_nodes(".image-description--title .ng-binding") %>%
  html_text(trim = TRUE)

html_filter %>%
  html_nodes(".image-description--title .ng-binding") %>%
  html_attr("href")

lot_1 <- read_html(myclient$getPageSource()[[1]]) %>% html_nodes("#ResultContainer .font_medium") %>%
  html_text
html_lot <-



lot <- "https://www.christies.com/property-from-descendants-of-28361.aspx?lid=1&dt=281220190340&saletitle="
read_html(lot) %>% html_nodes("#rmjs-1")

# Filter level ------------------------------------------------------------

# Get lots names on lots page
read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".image-description--title") %>% html_text(trim = TRUE)


# Lot level ---------------------------------------------------------------

# Get summary on lot page
read_html(myclient$getPageSource()[[1]]) %>% html_nodes("#rmjs-1") %>% html_text


# Lots level --------------------------------------------------------------

# Extract lot id to construct lot elements
read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".filter--lots-wrapper--items") %>% html_attr("id")

# Extract lot name to construct lot elements
read_html(myclient$getPageSource()[[1]]) %>% html_nodes("#ResultContainer .font_medium") %>% html_text(trim = TRUE)

# Get link to the lot page
xpath <- "//div[@class ='image-preview-container']/a[@onclick]/@href"
read_html(myclient$getPageSource()[[1]]) %>%
  html_nodes(xpath = xpath) %>%
  html_text(trim = TRUE)


# Elements level ----------------------------------------------------------

read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".lotDetails") %>% html_text(trim = TRUE)


# Overview level ----------------------------------------------------------

read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".print--page") %>%
  html_attr("href")
read_html("https://www.christies.com/lotfinder/print_sale.aspx?saleid=28361&lid=1") %>%
  html_text



# Get header of filter list -----------------------------------------------

read_html(myclient$getPageSource()[[1]]) %>% html_node(".checkbox") %>% html_text
read_html(myclient$getPageSource()[[1]]) %>% html_node("#refine-results .ng-scope") %>% html_text
myclient$getTitle()


myclient$navigate("http://www.google.com/ncr")
webElem <- myclient$findElements(using = "css selector", ".gsfi")
myclient$click()




read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".filter--lots-wrapper--items") %>%
  html_nodes(xpath = "/div/a[@href]")

read_html("https://www.christies.com/lotfinder/print_sale.aspx?saleid=28361&lid=1") %>%
  html_nodes(".lot-description")

