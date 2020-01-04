library(keras)
library(lime)
library(rsample)
library(yardstick)


# I Prepare the texts -----------------------------------------------------
MAX_WORDS <- 10000
# 1 Load the lot descriptions
text_raw <- binded$description

# 2 Convert texts into token
tokenizer <- text_tokenizer(num_words = MAX_WORDS) %>%
  fit_text_tokenizer(text_raw)
text_sequence <- texts_to_sequences(tokenizer, text_raw)

# 3 Get maximal length of text
text_max <- text_sequence %>% map_dbl(length) %>% max

# 4 Check what the indices are
index <- tokenizer$word_index

# 5 Build tensors for processing
text_data <- pad_sequences(sequences = text_sequence, maxlen = text_max) %>% as_tibble %>% mutate(dom_price = binded$dom_price)


# II Sample ------------------------------------------------------------------


# 7 Split into training and validation set
text_sample_split <- initial_split(text_data, prop = 0.75)

# 8 Retrieve testings
text_train <- training(text_sample_split)
text_test  <- testing(text_sample_split)

x_text_train <- text_train %>% select(-dom_price)
x_text_test <- text_test %>% select(-dom_price)

# Store the truth values
y_text_train <- text_train %>% select(dom_price)
y_text_test  <- text_test %>% select(dom_price)

# II Build Keras Model ----------------------------------------------------

# 1 Define the input layer for text recognition
text_input <- layer_input(
  shape = c(18),
  dtype = 'int32',
  name  = 'text_input')

# 2 Define the embedding layer
gru_out <- text_input %>%

  layer_embedding(
    input_dim    = 10000,
    output_dim   = 16,
    input_length = 18
  ) %>%
  # layer_global_average_pooling_1d() %>%

  layer_lstm(
    units             = 16,
    dropout           = 0.5,
    recurrent_dropout = 0.5
  ) %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 3, activation = "softmax")


model_text <- keras_model(text_input, gru_out)

model_text %>% compile(
  optimizer =  "adam",
  loss      = 'categorical_crossentropy',
  metrics = c("accuracy")
)

history_text <- model_text %>% fit(
  x                = as.matrix(x_text_train),
  y                = to_categorical(y_text_train, num_classes = 3),
  epochs = 40,
  batch_size = 100,
  validation_split = 0.2,
  verbose=2
)




# Fit the keras model to the training data
history <- keras::fit(
  object           = model_al,
  x                = as.matrix(x_text_train),
  y                = to_categorical(y_text_train, num_classes = 3),
  batch_size       = 100,
  epochs           = 100,
  validation_split = 0.25
)




##












# 1 Define the input layer for text recognition
text_input <- layer_input(
  shape = c(18),
  dtype = 'int32',
  name  = 'text_input')

# 2 Define the embedding layer
gru_out <- text_input %>%

  layer_embedding(
    input_dim    = 10000,
    output_dim   = 16,
    input_length = 18
    ) %>%

  layer_gru(
    units             = 16,
    dropout           = 0.2,
    recurrent_dropout = 0.2
    )

text_output <- gru_out %>%

  layer_dense(
    units = 1,
    name  = 'text_output'
    )

meta_input <- layer_input(shape = c(4), name = 'meta_input')

meta_out <- meta_input %>%

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
    activation         = "relu")

meta_output <- meta_out %>% layer_dense(units = 1)


main_output <- layer_concatenate(c(gru_out, meta_out)) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 1)

model <- keras_model(
  inputs = c(text_input, meta_input),
  outputs = c(main_output, text_output)
)


# Compile ANN
model %>% compile(
  optimizer = 'adam',
  loss      = 'mae',
  loss_weights = c(1.0, 0.2)
)

# And trained it via:
model %>% fit(
  x = list(text_input = as.matrix(x_text_train), meta_input = as.matrix(x_meta_train)),
  y = list(main_output = pull(y_text_train), aux_output = pull(y_meta_train)),
  epochs = 20,
  batch_size = 32,
  validation_split = 0.3
)




