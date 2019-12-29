library(httr)
library(xml2)
library(rvest)
library(RSelenium)
library(tidyverse)

rD <- rsDriver(verbose = FALSE, chromever = "79.0.3945.36" )
myclient <- rD$client
myclient$navigate(url_filter)
myclient$getTitle()

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


read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".filter--lots-wrapper--items") %>%
  html_nodes(xpath = "/div/a[@href]")

read_html("https://www.christies.com/lotfinder/print_sale.aspx?saleid=28361&lid=1") %>%
  html_nodes(".lot-description")
