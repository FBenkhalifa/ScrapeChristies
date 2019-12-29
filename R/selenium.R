library(httr)
library(xml2)
library(rvest)
library(RSelenium)
library(tidyverse)
library(seleniumPipes)


# Set up client -----------------------------------------------------------

# 1 Start connection
rD <- rsDriver(verbose = FALSE, chromever = "79.0.3945.36" )
myclient <- rD$client

# 2 Navigate to Christies page
myclient$navigate(url_filter)
myclient$getTitle()

# Extract filter ----------------------------------------------------------

l <- myclient$click("css selector", "#refine-results .ng-binding")
h <- myclient$findElement(using = "css selector", value = ".follow-text")
h <- myclient$findElement(using = "xpath", value ='//*[@id="refine-results"]/cc-filters/h4[2]')
h <- myclient$findElement(using = "css selector", value ='.filter--container--header')

# Write function which loops through location category...
filter_n <- read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".item-container--label") %>% length

# Get xpaths for the filters on level 1
l1 <- read_html(myclient$getPageSource()[[1]]) %>%
  html_nodes(".item-container--label") %>%
  html_text(trim = TRUE) %>%
  gsub("\n.*$", "", .)

xpath_filter_l1 <- paste0('//*[@id="refine-results"]/cc-filters/div[1]/ul/li[', seq_along(l1), ']/cc-multi-select-box/div/label')

# Check manually if we can get by on level 2
# 1 Navigate client to level 1 filter element and click
h <- myclient$findElement(using = "xpath", value = i)
h$highlightElement()
h$clickElement()

# 2 Store the names of the level 2 filter criteria
l2 <- read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".checkbox--label") %>% html_text(trim = TRUE)

# 3 Quick check of the correct is chosen for dubay
dubai <- '//*[@id="refine-results"]/cc-filters/div[1]/ul/li[1]/cc-multi-select-box/div/div/div[2]/ul/li[3]/label'
read_html(myclient$getPageSource()[[1]]) %>% html_nodes(xpath = dubai) %>% html_text
h <- myclient$findElement(using = "xpath", value = dubai)
h$highlightElement()
h$clickElement()

## Write a function to store the xpaths for the filters
for (i in xpath_filter_l1) {
  # 1 Navigate client to level 1 filter element and click
  h <- myclient$findElement(using = "xpath", value = i)
  h$highlightElement()
  h$clickElement()

  # 2 Store the names of the level 2 filter criteria
  l2 <- read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".checkbox--label") %>% html_text(trim = TRUE)

  # 3 Construct xpaths able to click the respective button
  xpath_str <- paste0('div/div[2]/ul/li[', c(1:length(l2)), ']/label') %>% set_names(l2)
  xpath_filter_l2 <- xpath_str %>% map_chr(~gsub("label$", ., i))

  # 1 Navigate client to level 1 filter element and click
  h <- myclient$findElement(using = "xpath", value = xpath_filter_l2[1])
  h$highlightElement()
  h$clickElement()

}




xpath_str <- paste0('div/div[2]/ul/li[', c(1:length(l2)), ']/label')
xpath_filter_l2 <- gsub("label$", xpath_str, i)

myclient$navigate("http://www.google.com/ncr")
webElem <- myclient$findElement(using = "name", value = "q")
webElem$highlightElement()

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


myclient$close()
rD$server$stop()

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

