https://www.christies.com/lotfinder/print_sale.aspx?saleid=28183&lid=1

urls <- c(
  "http://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html",
  "http://en.wikipedia.org/wiki/Xz",
  "xxxxx"
)
MetaTable <- function(.args = list(
  lot =lot_number,
  description = description,
  period = period,
  dimensions = dimensions[1:4],
  estimate_min = est_range$estimate_min,
  estimate_max = est_range$estimate_max,
  dom_price = dom_price), .US_table = US_table){
  out <- tryCatch(
    {
      # .args2 <- list(a = c(1,2), b = c(1,2))

      do.call(tibble, .args)

      # The return value of `readLines()` is the actual value
      # that will be returned in case there is no condition
      # (e.g. warning or error).
      # You don't need to state the return value via `return()` as code
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      message("Not the same number of elements in tbl")
      message("Try another combination")

      var_length <- map(.args, length) %>% unique %>% length

        if(var_length == 2 & .args$lot %>% length > .args$dom_price %>% length){

          lot_table <- left_join(.US_table, do.call(tibble, .args[names(.args) != "dom_price"]), by = "lot") %>% add_column(dom_price = NA)

        }else{

          lot_table <- character(0)

        }
      # Choose a return value in case of error
      return(lot_table)

    },
    warning=function(cond) {
      message("Data is matched based on US_price")
      return(NULL)
    },
    finally={

      message("Data is matched based on US_price")
    }
  )
  return(out)
}
MetaTable(.args, .US_table)


time = loc_time["time"],
loc = loc_time["loc"],
auction = auction_name

y <- lapply(urls, readUrl)
