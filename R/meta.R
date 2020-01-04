library(lubridate)
library(plotly)
library(recipes)
library(tfdatasets)
# dmy(., locale = "US") %>%
#   map_df(~floor_date(., 'month'))) %>%

# Data Preprocessing ------------------------------------------------------

meta_data <- binded %>% select(-description, -auction, -lot) %>%
  mutate(time = time %>%
           gsub("[-].*|^[0-9]", "", .)%>%
           trimws(.))  %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()



# 7 Split into training and validation set
meta_sample_split <- initial_split(meta_data, prop = 0.75)

# 8 Retrieve testings
meta_train <- training(meta_sample_split)
meta_test  <- testing(meta_sample_split)

# p <- ggplot(x_data, aes(dom_price)) + geom_histogram(bins = 100)
# p %>% ggplotly()

# 9 Prepare preprocessing
rec_obj <- recipes::recipe(dom_price ~. , data = meta_train) %>%
  recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
  recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
  recipes::step_dummy(recipes::all_nominal(), -recipes::all_outcomes()) %>%
  recipes::prep(data = meta_train)

# 10 Bake with the recipe
x_meta_train <- bake(rec_obj, new_data = meta_train) %>% select(-dom_price)
x_meta_test <- bake(rec_obj, new_data = meta_test) %>% select(-dom_price)

# Store the truth values
y_meta_train <- bake(rec_obj, new_data = meta_train) %>% select(dom_price)
y_meta_test  <- bake(rec_obj, new_data = meta_test) %>% select(dom_price)

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


meta_input_al <- layer_input(shape = c(28), name = 'meta_input_al')

meta_out_al <- meta_input_al %>%

  # First hidden layer
  layer_dense(
    units              = 8,
    kernel_initializer = "uniform",
    activation         = "relu"
  ) %>%

  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%

  # Second hidden layer
  layer_dense(
    units              = 16,
    kernel_initializer = "uniform",
    activation         = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 3, activation = "softmax")

model_al <- keras_model(
  inputs = meta_input_al,
  outputs = meta_out_al
)

model_al %>% compile(
  optimizer =  "rmsprop",
  loss      = 'categorical_crossentropy',
  metrics = c("accuracy")
)



# Fit the keras model to the training data
history <- fit(
  object           = model_al,
  x                = as.matrix(x_meta_train),
  y                = pull(y_meta_train),
  batch_size       = 50,
  epochs           = 35,
  validation_split = 0.25
)

print(history)

c_meta_train <- bake(rec_obj, new_data = meta_train)
spec3 <- feature_spec(c_meta_train, x = c("estimate_min", "estimate_max")) %>%
  step_numeric_column(estimate_min, estimate_max, normalizer_fn = scaler_standard()) %>%
  fit()

layer <- layer_dense_features(
  feature_columns = dense_features(spec3),
  dtype = tf$float32
)
# layer(meta_train)
input <- layer_input_from_dataset(c_meta_train %>% select(estimate_min, estimate_max, -dom_price))
output <- input %>%
  layer_dense_features(dense_features(spec3)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

model <- keras_model(input, output)

summary(model)
model %>%
  compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )

# Fit the keras model to the training data
history <- fit(
  object           = model,
  x                = c_meta_train %>% select(estimate_min, estimate_max, -dom_price),
  y                = c_meta_train$dom_price,
  batch_size       = 50,
  epochs           = 35,
  validation_split = 0.25
)


build_model <- function() {
  input <- layer_input_from_dataset(c_meta_train %>% select(-dom_price))

  output <- input %>%
    layer_dense_features(dense_features(spec3)) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 3, activation = "softmax")

  model <- keras_model(input, output)

  model %>%
    compile(
      optimizer =  "rmsprop",
      loss      = 'categorical_crossentropy',
      metrics = c("accuracy")
    )

  model
}

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

model <- build_model()

history <- model %>% fit(
  x = train_df %>% select(-label),
  y = train_df$label,
  epochs = 500,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)
plot(history)
meta_input_al <- layer_input_from_dataset(shape = c(28), name = 'meta_input_al')
spec

meta_out_al <- meta_input_al %>%

  # First hidden layer
  layer_dense(
    units              = 8,
    kernel_initializer = "uniform",
    activation         = "relu"
  ) %>%

  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%

  # Second hidden layer
  layer_dense(
    units              = 16,
    kernel_initializer = "uniform",
    activation         = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

model_al <- keras_model(
  inputs = meta_input_al,
  outputs = meta_out_al
)

model_al %>% compile(
  optimizer = 'sgd',
  loss      = 'mean_absolute_error',
  metrics = 'mae'
)





