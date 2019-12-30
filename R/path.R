

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


