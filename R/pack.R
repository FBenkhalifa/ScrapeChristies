library(tidyverse)
library(magrittr)
library(janitor)


# file loading ------------------------------------------------------------


file_names <- paste0("./meta_data/", dir("./meta_data/"))

out <-  lapply(file_names,function(x){
  env = new.env()
  nm = load(x, envir = env)[1]
  objname = gsub(pattern = './meta_data/', replacement = '', x = x, fixed = T)
  # print(str(env[[nm]]))
  assign(objname, env[[nm]], envir = .GlobalEnv)
  0 # succeeded
} )
obj <- file_names %>% map_chr(~(gsub(pattern = './meta_data/', '', .)))

obj_list <- map(obj, get) %>% map(~remove_empty(., which = "cols"))
obj_list_re <- map(obj_list, ~plyr::rename(.,
                                           replace = c(US_prices= "dom_price"),
                                           warn_missing = FALSE))
obj_list_normal <- obj_list_re %>%
  map_lgl(., ~(ncol(.) > 7))

binded <- obj_list_re[obj_list_normal] %>%
  do.call(bind_rows, .) %>% select(-period, -dimensions) %>% mutate(dom_price = case_when(
    dom_price > estimate_max ~ 1,
    dom_price > estimate_min & dom_price < estimate_max ~ 0,
    dom_price < estimate_min ~ -1
  )) %>%
  mutate_at("dom_price", as.factor)



# Pic loading -------------------------------------------------------------

pic_path <- list.files("./jpgs/", recursive = T) %>% enframe(name = NULL, value = "file") %>%
  separate(.,
           col = file,
           into = c("auction", "lot"),
           sep = "/Lot",
           remove = FALSE) %>%
  mutate_at("lot", parse_number)


binded_full <- inner_join(pic_path, binded)






