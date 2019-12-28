library(tidyverse)
library(httr)
library(rvest)
library(RCurl)
library(curl)
# Build filter function ---------------------------------------------------
url_filter <- "https://www.christies.com/Results?scids=7"
results <- read_html(url_filter)
h <- results %>% html_text() %>% trimws()
results %>% html_nodes(".image-description--title .ng-binding")
results %>% html_nodes("html")

sth <- "https://www.amazon.com/-/de/dp/B07211W6X2/ref=sr_1_1?__mk_de_DE=%C3%85M%C3%85%C5%BD%C3%95%C3%91&keywords=apple&qid=1577538183&smid=ATVPDKIKX0DER&sr=8-1"
sth <- read_html("https://www.amazon.com/-/de/dp/B07211W6X2/ref=sr_1_1?__mk_de_DE=%C3%85M%C3%85%C5%BD%C3%95%C3%91&keywords=apple&qid=1577538183&smid=ATVPDKIKX0DER&sr=8-1")
sth %>% html_nodes("#bylineInfo_feature_div .a-spacing-none")
html

session <- html_session(url_filter)
cookies(session)
jump_to()
session %>% read_html() %>% html_nodes("#Calendar-Results-Sales .hidden-print") %>% html_text

library(splashr)
sp <- start_splash()
pg <- render_html(url = url_filter)
install_splash()
