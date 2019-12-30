
# Filter level ------------------------------------------------------------

# Get lots names on lots page
lots <- read_html(URL_FILTERED)
lots_names <- lots %>% html_nodes(xpath = "//h6/a[@target ='_self']") %>% html_attr("href")
lots_URLs <- lots %>%
  html_nodes(xpath = "//h6/a[@target ='_self']") %>%
  html_attr("href") %>%
  paste0(URL, .)

# Lot level ---------------------------------------------------------------
lots_d <- read_html(lots_URLs[1])
# Get summary on lot page
lots_summary <- lots_d %>% html_nodes("strong , em") %>% html_text
lot_print <- lots_d %>% html_nodes(xpath = "//a[@target = '_blank' and @class ='print--page']") %>% html_attr("href")

URL_results <- lots_d %>% html_nodes("#LotListings") %>% html_nodes(xpath = "//a") %>% html_attr("href") %>% .[1]

lots_results <- URL_results %>% read_html
US_prices <- lots_results %>%
  html_nodes(xpath = "//span[contains(@id, 'dlResults_lblPrice_')]") %>%
  html_text() %>%
  sub(",", ".", ., fixed = TRUE) %>%
  as.numeric()

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

read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".lotDetails")  %>% html_text(trim = TRUE)


# Overview level ----------------------------------------------------------

read_html(myclient$getPageSource()[[1]]) %>% html_nodes(".print--page") %>%
  html_attr("href")
read_html("https://www.christies.com/lotfinder/print_sale.aspx?saleid=28361&lid=1") %>%
  html_text

https://www.christies.com/lotfinder/lot/a-pair-of-chinese-red-overlay-yellow-glass-5913075-details.aspx?from=salesummery&intobjectid=5913075&sid=32881f09-dada-4e0f-aad8-e03bc5c6a780

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

