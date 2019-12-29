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
myclient$navigate(URL_FILTER)

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
