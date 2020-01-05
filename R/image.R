# Pic loading -------------------------------------------------------------

pic_path <- paste0("./jpgs/", list.files("./jpgs/", recursive = T)) %>% enframe(name = NULL, value = "file") %>%
  separate(.,
           col = file,
           into = c("auction", "lot"),
           sep = "/Lot",
           remove = FALSE) %>%
  mutate_at("lot", parse_number) %>%
  mutate_at("auction", ~gsub("./jpgs/", "", .))


binded_full <- inner_join(pic_path, binded)
one_hot <- to_categorical(binded_full$dom_price, num_classes = 3) %>% as_tibble %>% set_names(c("correct", "over", "under"))
binded_full2 <- binded_full %>% select(file) %>% bind_cols(one_hot) %>% mutate_all(as.character)
# Image pre-processing --------------------------------------------------------
"./jpgs/An-Evening-of-Exceptional-28210/Lot1.jpg"
y_cols <- c("correct", "over", "under")
# 1 Define data generator functions for train and vlai
to_categorical(binded_full2$dom_price, num_classes = 3) %>% as.list
train_gen <- image_data_generator(rescale = 1/255,
                                      validation_split = 0.2)
train_generator <- flow_images_from_dataframe(
  directory = "/Users/flo_b/OneDrive/Desktop/ScrapeChristies/",
  dataframe = binded_full2,
  x_col = "file",
  y_col = y_cols,
  generator = train_gen,
  batch_size = 32,
  class_mode = "other",
  subset = "training",
  target_size = c(150, 150),
)

# 2 Check if function works
batch <- generator_next(train_generator)
batch %>% str
plot(as.raster(batch[[1]][1,,,]))

# II Build keras model ----------------------------------------------------

img_model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(150, 150, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 3, activation = "softmax")

model %>% compile(
  optimizer =  "adam",
  loss      = 'categorical_crossentropy',
  metrics = c("accuracy")
)

history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 30,
  epochs = 30
)
