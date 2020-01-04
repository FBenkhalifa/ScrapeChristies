

# II Build Keras Model ----------------------------------------------------

model_build <- function(){
# 1 Define the input layer for text recognition
text_input <- layer_input(
  shape = c(18),
  dtype = 'int32',
  name  = 'text_input')


# TEXT OUT ---------------------------------------------------------------------


gru_out <- text_input %>%

  layer_embedding(
    input_dim    = 10000,
    output_dim   = 16,
    input_length = 18
  ) %>%

  layer_lstm(
    units             = 12,
    dropout           = 0.5,
    recurrent_dropout = 0.5
  ) %>%
  layer_dense(units = 8, activation = "relu")


# TEXT OUTPUT ------------------------------------------------------------------



# text_output <- gru_out %>%
#   layer_dropout(0.5) %>%
#   layer_dense(
#     units = 3,
#     activation = "softmax",
#     name  = 'text_output'
#   )



# META OUT ----------------------------------------------------------------


meta_input <- layer_input(shape = c(28), name = 'meta_input', dtype = "float32")

meta_out <- meta_input %>%

  # First hidden layer
  layer_dense(
    units              = 20,
    kernel_initializer = "uniform",
    activation         = "relu"
  ) #%>%

  # # Dropout to prevent overfitting
  # layer_dropout(rate = 0.4) %>%
  #
  # # Second hidden layer
  # layer_dense(
  #   units              = 20,
  #   kernel_initializer = "uniform",
  #   activation         = "relu")



# META OUTPUT -------------------------------------------------------------


# meta_output <- meta_out %>%
#   layer_dropout(rate = 0.1) %>%
#   layer_dense(units = 3, activation = "softmax")


main_output <- layer_concatenate(c(gru_out, meta_out)) %>%
  # layer_dense(units = 20, activation = 'relu') %>%
  # layer_dropout(rate = 0.5) %>%
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 3, activation = "softmax", name = "main_output")


model <- keras_model(
  inputs = c(text_input, meta_input),
  outputs = c(main_output)
)


# Compile ANN
model %>% compile(
  optimizer = 'adam',
  loss      = 'categorical_crossentropy',
  metrics = c("accuracy")
)

# And trained it via:
history <- model %>% fit(
  x = list(text_input = as.matrix(x_text_train), meta_input = as.matrix(x_meta_train)),
  y = list(main_output = to_categorical(y_text_train, num_classes = 3)), #text_output = to_categorical(y_text_train, num_classes = 3)),
  epochs = 40,
  batch_size = 100,
  validation_split = 0.3
)

}

model_build()

model %>% save_model_hdf5("cats_and_dogs_small_1.h5")

