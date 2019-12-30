library(robotstxt)

paths_allowed("http://google.com/")
rtxt <- robotstxt(domain="wikipedia.org")
class(rtxt)
rtxt$check(paths = c("/","api/"), bot = "*")
rtxt$check(paths = c("/","api/"), bot = "Orthogaffe")
r_text        <- get_robotstxt("nytimes.com")


paths_allowed("https://www.christies.com/")
rtxt <- robotstxt("https://www.christies.com/")
r_text        <- get_robotstxt("https://www.christies.com/")


RobotsCheck <- function(URL, useragent, field){


  rtxt$permissions %>% as_tibble %>% filter(field == "Disallow" & useragent == "*") %>% select(value)



}

paths_allowed(paths = "https://www.christies.com/", domain = "auto", bot = "*",
              user_agent = utils::sessionInfo()$R.version$version.string,
              warn = TRUE, force = FALSE,
              ssl_verifypeer = c(1, 0), use_futures = TRUE, robotstxt_list = NULL)

library(httr)
html_session("you-url", use_proxy("proxy-ip", port))
