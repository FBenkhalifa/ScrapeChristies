library(httr)
library(xml2)
library(rvest)
library(RSelenium)
library(tidyverse)
library(seleniumPipes)

# 1 Start connection
rD <- rsDriver(verbose = FALSE, chromever = "79.0.3945.36" )
myclient <- rD$client

# 2 Navigate to Christies page
myclient$navigate(URL_FILTERED)

# 1 Start from the auction side and click on filter
cursor <- myclient$findElement(using = "xpath", value = FILTER_BUTTON)
cursor$clickElement()

# 2 Get on category
cursor <- myclient$findElement(using = "xpath", value = xpath_l1["Category"])
cursor$highlightElement()
cursor$clickElement()


cursor <- myclient$findElement(using = "xpath", value = xpath_l2["Fine Art"])
cursor$highlightElement()
cursor$clickElement()

cursor <- myclient$findElement(using = "xpath", value = "//*[@id=\"refine-results\"]/cc-filters/div[1]/ul/li[2]/cc-multi-select-box/div/div/div[2]/ul/li[5]/label")
cursor$clickElement(using = "xpath", value = '"//*[@id=\"refine-results\"]/cc-filters/div[1]/ul/li[2]/cc-multi-select-box/div/div/div[2]/ul/li[5]/label')

//*[@id="refine-results"]/cc-filters/div[1]/ul/li[2]/cc-multi-select-box/div/div/div[2]/ul/li[5]


# Filter level ------------------------------------------------------------

# Get lots names on lots page
read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".image-description--title") %>% html_text(trim = TRUE)
read_html(URL_FILTERED) %>% html_nodes(".image-description--title") %>% html_text(trim = TRUE)


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

