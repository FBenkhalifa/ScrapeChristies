library(lubridate)
library(plotly)
library(recipes)
# dmy(., locale = "US") %>%
#   map_df(~floor_date(., 'month'))) %>%

# Data Preprocessing ------------------------------------------------------

x_data <- binded %>% select(-description, -auction, -lot) %>%
  mutate(time = time %>%
           gsub("[-].*|^[0-9]", "", .)%>%
           trimws(.))  %>%
  mutate_if(is.character, as.factor)



# 7 Split into training and validation set
train_test_split <- initial_split(x_data, prop = 0.75)

# 8 Retrieve testings
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)

p <- ggplot(x_data, aes(dom_price)) + geom_histogram(bins = 100)
p %>% ggplotly()

# 9 Prepare preprocessing
rec_obj <- recipe(dom_price ~., data = train_tbl) %>%
  step_log(all_numeric(), all_outcomes()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal()) %>%
  prep(data = train_tbl)

# 10 Bake with the recipe
x_train_tbl <- bake(rec_obj, new_data = train_tbl) %>% select(-dom_price)
x_test_tbl <- bake(rec_obj, new_data = test_tbl) %>% select(-dom_price)

# Store the truth values
y_train_vec <- train_tbl$dom_price
y_test_vec <- test_tbl$dom_price

# II Build Keras Model ----------------------------------------------------

# Building our Artificial Neural Network
model_meta <- keras_model_sequential()

model_meta %>%

  # First hidden layer
  layer_dense(
    units              = 16,
    kernel_initializer = "uniform",
    activation         = "relu",
    input_shape        = ncol(x_train_tbl)) %>%

  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%

  # Second hidden layer
  layer_dense(
    units              = 16,
    kernel_initializer = "uniform",
    activation         = "relu") %>%

  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%

  # Output layer
  layer_dense(
    units              = 1) %>%

  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'mse',
    metrics   = c('mse')
  )

# Fit the keras model to the training data
history <- fit(
  object           = model_meta,
  x                = as.matrix(x_train_tbl),
  y                = y_train_vec,
  batch_size       = 50,
  epochs           = 35,
  validation_split = 0.25
)
