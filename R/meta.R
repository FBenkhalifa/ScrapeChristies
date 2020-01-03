library(lubridate)


# Data Preprocessing ------------------------------------------------------

train_x <- binded %>% select(-description, -auction, -lot, -dom_price) %>%
  mutate(time = time %>% gsub("[-].*", "", .) %>% dmy(., locale = "US"))

train_y <- binded$dom_price


# II Build Keras Model ----------------------------------------------------


