library(httr)
library(xml2)
library(rvest)
library(RSelenium)

rD <- rsDriver(verbose = FALSE, port = 4334L, chromever = "79.0.3945.36" )
myclient <- rD$client
myclient$navigate(url_filter)
myclient$getTitle()

html_filter <- read_html(myclient$getPageSource()[[1]])
html_filter %>%
  html_nodes(".image-description--title .ng-binding") %>%
  html_text(trim = TRUE)

html_filter %>%
  html_nodes(".image-description--title") %>%
  html_attr("href")

html_filter %>%
  html_nodes(".image-description--title .ng-binding") %>%
  html_attr("href")

myclient$close()
rD$server$stop()

lot <- "https://www.christies.com/property-from-descendants-of-28361.aspx?lid=1&dt=281220190340&saletitle="
read_html("https://www.christies.com/property-from-descendants-of-28361.aspx?lid=1&dt=281220190340&saletitle=")
