library(httr)
library(xml2)
library(rvest)
library(RSelenium)
library(tidyverse)
library(robotstxt)
library(lubridate)
library(plotly)
library(recipes)
library(rsample)
library(keras)
library(reticulate)
library(tensorflow)
library(plotly)

load("./data.rdata")
### Caution!!! For keras to work, anaconda for python must be installed on the computer
install_keras(method = "conda") # If this does not work, anaconda for python must be installed first. In the error in the console there is a link to python
conda_create("ScrapeChristies")
use_condaenv(condaenv = "ScrapeChristies", required = TRUE)
reticulate::conda_install("pandas", envname = "ScrapeChristies")
reticulate::conda_install("pillow", envname = "ScrapeChristies", pip = TRUE)
tensorflow::install_tensorflow(envname = "ScrapeChristies")
conda_list()
py_config()
import("pillow")
py_module_available("pandas")
py_module_available("tensorflow")
py_module_available("pillow")

# I Check disallowed files -------------------------------------------------------

robotstxt("https://www.christies.com/") %>%
  .$permissions %>%
  as_tibble %>%
  filter(field == "Disallow" & useragent == "*") %>%
  select(value)

# Define static Variables
URL <- "https://www.christies.com"
URL_FILTER <- "https://www.christies.com/Results" # Link to the filter page
FILTER_BUTTON <- '//*[@id="refine-results"]/cc-filters/h4[2]' # Xpath to the lot print button

# II Get filter options -----------------------------------------------------------

# 1 Start connection
rD <- rsDriver(verbose = FALSE, chromever = "79.0.3945.36" ) # For me only chrome version 79.0.3945.36 worked, I had to download it
myclient <- rD$client

# 2 Navigate to Christies page
myclient$navigate(URL_FILTER)


#---- A Extract filter in RSelenium Session ----

# 1 Start from the auction side and click on filter
cursor <- myclient$findElement(using = "xpath", value = FILTER_BUTTON)
cursor$clickElement()

# 2 get names of first level filter
l1 <- read_html(myclient$getPageSource()[[1]]) %>%
  html_nodes(".item-container--label") %>%
  html_text(trim = TRUE) %>%
  gsub("\n.*$", "", .)

# 3 Get main element
text_box <- read_html(myclient$getPageSource()[[1]]) %>%
  html_nodes(".item-container--dropdown-items") %>%
  set_names(l1)

# 4 Close RSelenium session
rD$server$stop()
rm(rD)
gc()


#---- B Structure the information to create filter dictionary ----

# 1 Extract available options
items <- text_box %>%
  html_text(trim = TRUE) %>%
  gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", ., perl = TRUE) %>%
  strsplit(., "\n")

items

# 2 Extract the nodes where the IDs can be found
id_nodes <- text_box %>%
  html_nodes(xpath = '//li[@class="item-container--dropdown-items--item ng-scope"]//*[@id]')

# 3 Extract ID values
id_values <- id_nodes %>%
  html_attr("id") %>%
  as.numeric %>%
  enframe(name = NULL)

# 4 Get corresponding names of id nodes
id_names <- id_nodes %>%
  html_text %>%
  enframe(name = NULL)

# 5 Bind both together to get a tbl
id_tbl <- bind_cols(id_names, id_values) %>% set_names(c("l2", "id"))

# 6 Convert list to long format
items_tbl <- items %>% enframe(name = "l1", value = "l2") %>% unnest(cols = "l2")

# 7 Create a tbl with the item and the id value
full_tbl <- left_join(items_tbl, id_tbl, key = "l2")

# 8 Check if there are any items which appear on the screen but have no ID assigned
full_tbl %>% filter(id %>% is.na)


#---- C Construct URLs resulting from the filter ----

# 1 Hardcode the URLs which result from specifying level 1 in the filter
URL_l1 <- c(Location = "locations",
            Category = "scids",
            Month = "month",
            Year = "year") %>%
  enframe(name = "l1", value = "id_l1")

# 2 Create a dictionary to look up the information to build the URLs
id_dict <- left_join(full_tbl, URL_l1, by = "l1")

# 3 Create a function which gets as input filter keywords and returns corresponding URLs

# Arguments:
# .items = A string vector with the filter requests
# .id_dict = A tbl whcih maps a filter option to the corresponding ID value
# .url = A string with the URL to the Chrisites website

URLBuilder <- function(.items, .id_dict = id_dict, .url = URL){

  # 1 Match selected id with id dictionary
  id_sel <- .id_dict %>% filter(l2 %in% .items) %>% drop_na

  # 2 Split into level 1 groups
  groups <- id_sel %>% select(id_l1, id) %>% group_by(id_l1)

  # 3 Construct the URL parameters for each group
  URL_params <- groups %>%
    group_split() %>%
    set_names(group_keys(groups) %>% pull) %>%
    map_chr(~paste(.$id, collapse = "%7C")) %>%
    enframe()

  # 4 Concatenate first time
  search_queries <- URL_params %>% unite(col = "query", sep = "=")

  # 5 Build final URL
  new_URL <- paste0(paste0(URL, "/Results?"), paste(search_queries$query, collapse = "&"))

  return(new_URL)
}

# 4 Check quicly if function works
filter_vec <- c("Jewellery, Watches & Handbags", "2019", "December")
URLBuilder(.items = filter_vec) # Copy to browser to check of it works








# III Scrape the data ------------------------------------------------------------
# A Set up the directory
if (!dir.exists("./jpgs/")) dir.create("./jpgs/")
if (!dir.exists("./meta_data/")) dir.create("./meta_data//")

#---- B Get information from Level 1 as described in the paper ----
# 1 Use this for a smaller dataset although the following specification were used
filter_opt <- expand.grid("Jewellery, Watches & Handbags", items$Month, items$Year[2:5])

# 1 Restrict search to Jewellry watches & Handbags from the last 4 years in New York and LA
# filter_opt <- expand.grid("Jewellery, Watches & Handbags", items$Month, items$Year[2:11]) %>% as_tibble
# locations <- c("Los Angeles", "New York")
# for (i in locations) filter_opt <- filter_opt %>% add_column(!!i := rep(i, nrow(filter_opt)))


# 2 Get URls for level 1
URL_filter_opt <- apply(filter_opt, 1, URLBuilder) # These pages display the auctions with the corresponding filtered auctins
# Note that the page architecture demands for every month in every year a unique URL


#---- C Get information from Level 2 as described in the paper ----
# Here two functions are defined which will be used in the loop which finally scrapes the data

# 1 Get the URLs to level 2

# This function takes an URL fromt the filtered results and creates URLs level 2 for all of these pages
# Params:
# .url_filtered: A string with the URL of the filtered results
# .sleep: A double specifying how long to wait after each call of the function

GetLotsURL <- function(.url_filtered, .sleep = 0.8, .url = URL){

  # 1 Read the html of the filtered results
  auction <- read_html(.url_filtered)

  # 2 Extract the auctions names which needs to be used to construct the URLs to level 2
  auction_names <- auction %>% html_nodes(xpath = "//h6/a[@target ='_self']") %>% html_attr("href")

  # 3 Catch filters which have no results
  if(auction_names %>% is_empty){

    auction_URLs <- NA

  } else{

    # 4 Get the final URLs to the auctions
    auction_URLs <- auction %>%
      html_nodes(xpath = "//h6/a[@target ='_self']") %>%
      html_attr("href") %>%
      paste0(.url, .)

    # 5 Sleep to
    Sys.sleep(.sleep)

  }

  return(auction_URLs)

}

# 3 Get URls for level 2
auction_URLs <- map(URL_filter_opt, GetLotsURL) %>% unlist %>% na.omit


# 2 Create function which is able to scrape the relevant information on level 4

# This is a function which scrapes the data from a print lot list. In some cases
# the list is not complete and an error occurs when the corresponding elements in
# the HTML have different length. If this is the case, it is mostly  due to the
# achieved price not beeing assigned to every lot. If this happens we go to
# the lot results page which only stores all prices available plus corresponding
# lot number. The function then scrapes these prices and matches the information
# from the lot print page with the price list by the lot number.

# params:
# .args: A named list with the informations read from the html on the print lot list
# .res_table A tbl with the lot results information
MetaTable <- function(.args = list(
  lot =lot_number,
  description = description,
  estimate_min = est_range$estimate_min,
  estimate_max = est_range$estimate_max,
  price = price), .res_table = res_table){

  out <- tryCatch(

    {
      # 1 Check if tibble can be constructed
      do.call(tibble, .args)

    },
    error=function(cond) {
      message("Probably not the same number of elements in tbl")
      message("Try another combination")

      # 2 Check if the cause of the error is the price having differing length from the rest
      var_length <- map(.args, length) %>% unique %>% length
      if(var_length == 2 & .args$lot %>% length > .args$price %>% length){

        # 3 If true join with lot results list by lot number
        lot_table <- inner_join(.res_table, do.call(tibble, .args[names(.args) != "price"]), by = "lot") #%>% add_column(dom_price = NA)

      }else{

        # 4 If false sth else occured and an empty string is returned
        lot_table <- character(0)

      }
      # Choose a return value in case of error
      return(lot_table)

    },
    warning=function(cond) {
      message("Data is matched based on result table")
      return(NULL)
    },
    finally={

      message("Data is matched based on result table")
    }
  )
  return(out)
}





#---- D Run the loop ----

log_info <- list()

for (i in seq_along(auction_URLs[1:45])){


  # 2 Get auction name to identify the loop later
  auction_name <- auction_URLs[i] %>%
    basename %>%
    parse_character %>%
    gsub("\\..*", "", .)

  # 3 Read the landing page of the lots
  lots <- read_html(auction_URLs[i])

  # 4 Take a short break
  Sys.sleep(runif(1, 0.5, 2))

  # 5 Get print URL
  URL_print <- lots %>% html_nodes(xpath = "//a[@target = '_blank' and @class ='print--page']") %>% html_attr("href")

  # 6 Skip the lot in case there is no URL print on the page as e.g. here: https://www.christies.com/Christies-Jewels-Online-28172.aspx?lid=1&dt=050120201047
  if(URL_print %>% is_empty){

    warning <- paste0("Auction # ", i, ",", auction_name, " has no infos for print document")
    log_info[[auction_name]][["Print Document"]] <- "No print document"
    print(warning)

    next

  }

  # Get the URl for the result list where we get the
  URL_results <- lots %>% html_nodes("#LotListings") %>% html_nodes(xpath = "//a") %>% html_attr("href") %>% .[1]


  #---- b. Lots level/Print lot list -----

  lots_print <- read_html(URL_print)
  Sys.sleep(runif(1, 0.5, 2))
  # 1 Get the number of the lot
  lot_number <- lots_print %>% html_nodes(".lot-number") %>% html_text %>% parse_number()

  # 2 Get the text description of the lot
  description <- lots_print %>% html_nodes(".lot-info .lot-description") %>% html_text

  # 4 Get the estimated range of the lot
  estimate <- lots_print %>% html_nodes(xpath = "//td[@class ='estimate']//span[@class ='lot-description'][1]") %>% html_text
  est_range <- estimate %>%
    str_split(., " - ") %>%
    do.call(rbind, .) %>%
    apply(., 2, parse_number) %>%
    as_tibble %>%
    set_names(c("estimate_min", "estimate_max"))

  # 5 Get the price of the lot
  price <- lots_print %>% html_nodes(xpath = "//td[@class ='estimate']//span[@class ='lot-description'][2]") %>% html_text %>%
    parse_number

  # 6 Get date and time
  loc_time <- lots_print %>%
    html_nodes(".sale-number-location1") %>%
    html_text %>%
    gsub("[\t\r\n]", "", .) %>%
    str_split(., ",") %>% unlist %>%
    map_chr(trimws) %>%
    set_names(c("time", "loc"))

  #---- c. Lots level/Result list -----

  lots_results <- URL_results %>% read_html

  # 1 Get the prices from result list
  res_prices <- lots_results %>%
    html_nodes(xpath = "//span[contains(@id, 'dlResults_lblPrice_')]") %>%
    html_text() %>%
    parse_number

  # 2 Get the lot number
  res_prices_lots <- lots_results %>%
    html_nodes(xpath = "//span[contains(@id, 'dlResults_lblLotNumber_')]") %>%
    html_text() %>%
    parse_number

  # 3 Merge lot number with corresponding price
  res_table <- tibble(lot = res_prices_lots, price = res_prices)

  #---- d. Construct the final lot table -----
  lot_table <- MetaTable(.args = list(
    lot =lot_number,
    description = description,
    estimate_min = est_range$estimate_min,
    estimate_max = est_range$estimate_max,
    price = price),
    .res_table = res_table) %>%
    drop_na

  # 1 Skip the loop and write to if no lot table could be constructed
  if(lot_table %>% is_empty){

    log_info[[auction_name]][["Lot table"]] <- paste0("No metadata could be constructed")
    next

  }
  lot_table <- lot_table %>% add_column(auction = auction_name, loc = loc_time["loc"], time =  loc_time["time"]) %>%
    drop_na
  # 2 Save the rdata file to the corresponding directory
  table_path <- paste0("./meta_data/", auction_name, ".rdata")
  if (!file.exists(table_path)) save(lot_table, file = table_path)
  if(FALSE){
    #------ d. Download images ------

    # 1 Get the URL to the jpg
    URL_image <-  lots_print %>% html_nodes("#lot-list img") %>% html_attr("src")

    # 2 Clean the URL
    URL_jpg <-  URL_image %>% map_chr(~gsub("\\?.*", "", .))

    # 3 Create directory path to save the files in
    directory <- paste0("./jpgs/", auction_name)

    # 4 Check if directory exists and if not, create one
    if(!dir.exists(paths = directory)) dir.create(path = directory)

    # 5 Construct the jpg paths but only for those lots which are existent in the lot_table
    path_jpg <-   paste0(paste(paste0("./jpgs/",
                                      auction_URLs[i] %>%
                                        basename %>%
                                        parse_character %>%
                                        gsub("\\..*", "", .)),
                               lot_table$lot, sep = "/Lot"),
                         ".jpg")

    # 6 Loop over the paths and download jpgs
    for(j in lot_table$lot){

      # 1 Check if file exists and skip download if TRUE
      if(!file.exists(path_jpg[j])){

        # 2 Download
        download.file(url = URL_jpg[j], path_jpg[j],  mode = "wb")

        # § Give server time to chill
        Sys.sleep(runif(1, 1, 4))
      }

    }

  }

}
log_info


# IV Load files  ------------------------------------------------------------

meta_paths <- paste0("./meta_data/", dir("./meta_data/"))

out <-  map(meta_paths,function(x){

  # 1 Load environment for the files
  env = new.env()

  # 2 Load object in environment
  nm = load(x, envir = env)[1]

  # 3 Create object name which is simply the auction name
  objname = gsub(pattern = './meta_data/', replacement = '', x = x, fixed = T)

  # 4 Assign the auction name to the rdata
  assign(objname, env[[nm]], envir = .GlobalEnv)

} )
obj_names <- meta_paths %>%
  map_chr(~(gsub(pattern = './meta_data/', '', .)))
obj <- obj_names %>%
  map(., get)

rm(list = obj_names)

obj_normal <- obj %>% map_lgl(., ~(ncol(.) == 8))
data <- obj[obj_normal]%>%
  do.call(bind_rows, .) %>%
  mutate(rating = case_when(
    price > estimate_max ~ 2,
    price > estimate_min & price < estimate_max ~ 1,
    price < estimate_min ~ 0
  )) %>%
  mutate(time = time %>%
           gsub("[-].*|^[0-9][0-9]|^[0-9]", "", .)%>%
           trimws(.)) %>%
  mutate_at(vars( rating, loc, time), as.factor) %>%
  drop_na

# V Preprocessing --------------------------------------------------------------

#---- A Preprocess the data ----

# 1 Split into training and validation set
sample_split <- initial_split(data, prop = 0.75)

# 8 Retrieve testings
data_train <- training(sample_split)
data_test  <- testing(sample_split)

# B Preprocess for metat
# 9 Prepare preprocessing
rec_obj <- recipes::recipe(rating ~. , data = data_train) %>%
  step_normalize(estimate_min, estimate_max, price) %>%
  step_dummy(time, loc, all_outcomes(), one_hot = TRUE) %>% # If an error occurs make sure that loc has more than one factor
  prep(data = data_train, strings_as_factors = FALSE)

# 10 Bake with the recipe
data_train <- bake(rec_obj, new_data = data_train)
data_test <- bake(rec_obj, new_data = data_test)

#---- B Preprocess the data for meta analysis----

# 1 Select predictors of interest for meta analysis
x_meta_train <- data_train %>% select( -description, -auction, -lot, -price,
                                       -starts_with("rating"))
x_meta_test <- data_test %>% select( -description, -auction, -lot, -price,
                                     -starts_with("rating"))

# 2 Store the true values
y_meta_train <- data_train %>% select(starts_with("rating"))
y_meta_test  <- data_test %>% select(starts_with("rating"))

#---- C Preprocess text by tokenization -----

MAX_WORDS <- 10000

# 1 Load the lot descriptions
train_text_raw <- data_train$description

# 2 Convert texts into token
tokenizer <- text_tokenizer(num_words = MAX_WORDS) %>%
  fit_text_tokenizer(train_text_raw)
train_text_sequence <- texts_to_sequences(tokenizer, train_text_raw)

# 3 Get maximal length of text
text_max <- train_text_sequence %>% map_dbl(length) %>% max

# 4 Check what the indices are
index <- tokenizer$word_index

# 5 Build tensors for processing
x_text_train <- pad_sequences(sequences = train_text_sequence, maxlen = text_max)

y_text_train <- y_meta_train

# 6 Reüeat the same for the test set
test_text_raw <- data_test$description

test_text_sequence <- texts_to_sequences(tokenizer, test_text_raw)

x_text_test <- pad_sequences(sequences = test_text_sequence, maxlen = text_max) %>%
  as_tibble

y_text_test <- y_meta_test


#---- D Preprocess images by constructing generator functions -----

# 1 Get all img which could be downloaded
img_path <- paste0("./jpgs/", list.files("./jpgs/", recursive = T)) %>% enframe(name = NULL, value = "file") %>%
  separate(.,
           col = file,
           into = c("auction", "lot"),
           sep = "/Lot",
           remove = FALSE) %>%
  mutate_at("lot", parse_number) %>%
  mutate_at("auction", ~gsub("./jpgs/", "", .)) %>%
  arrange(auction, lot)

# 2 Join together with the metadata
sample_train <- runif(600, 1, nrow(x_img_train))
sample_test <- runif(120, 1, nrow(x_img_train))
x_img_train <- inner_join(img_path, data_train) %>% select(file, starts_with("rating"))
x_img_train <- x_img_train[runif(600, 1, nrow(x_img_train)), ] # Shorten the list because otherwise it takes hours to fit
x_img_test <- inner_join(img_path, data_test) %>% select(file, starts_with("rating"))
x_img_test <- x_img_train[runif(120, 1, nrow(x_img_train)), ]

# 3 Define data generator functions for train and vlai
train_gen <- image_data_generator(rescale = 1/255, validation_split = 0.2)
y_cols <- x_img_train %>% select(starts_with("rating")) %>% names()

# 4 Generate train batches which will be loaded into the ram to decrease the RAM usage
train_generator <- flow_images_from_dataframe(
  # directory = "C:/Users/user/Documents/GitHub/ScrapeChristies",
  dataframe = x_img_train,
  x_col = "file",
  y_col = y_cols,
  generator = train_gen,
  batch_size = 32,
  class_mode = "other",
  subset = "training",
  target_size = c(150, 150),
)

# 5 Generate validation batches
validation_generator <- flow_images_from_dataframe(
  # directory = "/Users/flo_b/OneDrive/Desktop/ScrapeChristies/",
  dataframe = x_img_train,
  x_col = "file",
  y_col = y_cols,
  generator = train_gen,
  batch_size = 32,
  class_mode = "other",
  subset = "validation",
  target_size = c(150, 150),
)

# 6 Generate test batches
test_datagen <- image_data_generator(rescale = 1/255)
test_generator <- flow_images_from_dataframe(
  # directory = "/Users/flo_b/OneDrive/Desktop/ScrapeChristies/",
  dataframe = x_img_test,
  x_col = "file",
  y_col = y_cols,
  generator = test_datagen,
  batch_size = 32,
  class_mode = "other",
  target_size = c(150, 150),
)
# 6 Check if function works
batch <- generator_next(train_generator)
plot(as.raster(batch[[1]][1,,,]))




# VI Modelling
#---- A Model multi-input ----
# II Build Keras Model ----------------------------------------------------

model_build <- function(){
  # 1 Define the input layer for text recognition
  text_input <- layer_input(
    shape = ncol(x_text_train),
    dtype = 'int32',
    name  = 'text_input')


  # TEXT OUT ---------------------------------------------------------------------


  lstm_out <- text_input %>%

    layer_embedding(
      input_dim    = 10000,
      output_dim   = 18,
      input_length = ncol(x_text_train)
    ) %>%

    layer_lstm(
      units             = 18,
      dropout           = 0.5,
      recurrent_dropout = 0.5
    ) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 16, activation = "relu")



  # META OUT ----------------------------------------------------------------


  meta_input <- layer_input(shape = ncol(x_meta_train), name = 'meta_input', dtype = "float32")

  meta_out <- meta_input %>%

    # First hidden layer
    layer_dense(
      units              = 16,
      kernel_initializer = "uniform",
      activation         = "relu"
    ) %>%

  # Dropout to prevent overfitting
  layer_dropout(rate = 0.4) %>%

  # Second hidden layer
  layer_dense(
    units              = 10,
    kernel_initializer = "uniform",
    activation         = "relu")



  # META OUTPUT -------------------------------------------------------------


  main_output <- layer_concatenate(c(lstm_out, meta_out)) %>%
    layer_dense(units = 10, activation = 'relu') %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 5, activation = 'relu') %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 3, activation = "softmax", name = "main_output")


  multi_model <- keras_model(
    inputs = c(text_input, meta_input),
    outputs = c(main_output)
  )


  # Compile ANN
  multi_model %>% compile(
    optimizer = 'adam',
    loss      = 'categorical_crossentropy',
    metrics = c("accuracy")
  )

  # And trained it via:
  multi_history <- multi_model %>% fit(
    x = list(text_input = as.matrix(x_text_train), meta_input = as.matrix(x_meta_train)),
    y = list(main_output = as.matrix(y_text_train)), #text_output = to_categorical(y_text_train, num_classes = 3)),
    epochs = 6,
    batch_size = 100,
    validation_split = 0.25
  )

}

model_build()

multi_model %>% save_model_hdf5("./model/multi_model.h5")
#---- B model img ----


img_model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(150, 150, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 3, activation = "softmax")

img_model %>% compile(
  optimizer =  "adam",
  loss      = 'categorical_crossentropy',
  metrics = c("accuracy")
)

img_history <- img_model %>% fit_generator(
  train_generator,
  steps_per_epoch = 10,
  epochs = 10,
  validation_data = validation_generator,
  validation_steps = nrow(x_img_train)*0.2/32
)

img_model %>% save_model_hdf5("./model/img_epochs.h5")

# III Prediction -------
multi_model <- load_model_hdf5("./model/multi_epochs_8.h5")

yhat_multi_model <- multi_model %>% evaluate(., x = list(text_input = as.matrix(x_text_test[sample_test, ]), meta_input = as.matrix(x_meta_test[sample_test, ])),
                                             y = list(main_output = as.matrix(y_text_test[sample_test, ])))

yhat_img_model <- img_model %>% evaluate_generator(test_generator, steps = 10,
                                     workers = 8)
# IV Plots ------
# ---- B Plot some

# 1 Restrict data to plot only on NY since there are many observation and they are all in USD
plot_data <- data %>% filter(loc == "New York")

# 2 Prepare for plotting
melt_data <- plot_data %>% select(price, estimate_min, estimate_max) %>% gather

# 3 Plot densplot
dens_plot <- ggplot(melt_data, aes(x=value, fill=key)) + geom_density(alpha=0.25)+
  xlim(c(0, 400000))+
  theme_bw()

# 4 Render as JS for interactivity
dens_plot %>% ggplotly

# 5 Plot Boxplot
box_plot <- ggplot(melt_data, aes(x = key, y = value, fill = key)) + geom_boxplot() +
  ylim(c(0, 200000)) +
  theme_bw()
box_plot %>% ggplotly

hist_plot <- ggplot(melt_data, aes(x = value, fill = key)) + geom_histogram(alpha =  0.8)+
  xlim(c(0, 400000))+
  theme_bw()
hist_plot %>% ggplotly

# Check how Christies over and underrates
hist_plot <- ggplot(data, aes(x = rating)) + geom_histogram(alpha =  0.8, stat = "count")+
  theme_bw()
hist_plot %>% ggplotly


