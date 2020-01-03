
file_names <- as.list(dir(pattern="stock_*"))
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
out <- obj %>% map(~get(.)) %>% do.call(rbind, .)
