library(httr)
library(xml2)
library(rvest)
library(RSelenium)
library(tidyverse)
library(seleniumPipes)

cursor$highlightElement()
cursor$clickElement()
myclient$close()
rD$server$stop()

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


# Observe the XHR request and see that this site is loaded
id = 28316
URL_summary <- read_html("https://www.christies.com/lotfinder/print_sale.aspx?saleid=28361&lid=1")
URL_xhr <- paste0("https://www.christies.com/AjaxPages/SaleLanding/DisplayLotList.aspx?intsaleid=", id)
read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".filter--lots-wrapper--items") %>%
  html_nodes(xpath = "/div/a[@href]")

table <- URL_summary %>%
  html_nodes(xpath = "//table[@id ='lot-list']") %>%
  html_table

description <- URL_summary %>% html_nodes(".lot-info .lot-description") %>% html_text
period <- URL_summary %>% html_nodes(".lot-maker") %>% html_text
estimate <- URL_summary %>% html_nodes(xpath = "//td[@class ='estimate']//span[@class ='lot-description'][1]") %>% html_text
price <- URL_summary %>% html_nodes(xpath = "//td[@class ='estimate']//span[@class ='lot-description'][2]") %>% html_text
dimensions <- URL_summary %>% html_nodes(".medium-dimensions") %>% html_text

lot_table <- tibble(
  description = description,
  period = period,
  estimate = estimate,
  price = price,
  dimensions = dimensions

)

URL_image <-  URL_summary %>% html_nodes("img") %>% html_attr("src")




URL_l2 <- read_html(URL_xhr) %>%
  html_nodes(xpath = "//div[@class='image-preview-container']/a[@href]") %>% html_attr("href")


