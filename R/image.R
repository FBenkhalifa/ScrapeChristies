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

# Image pre-processing --------------------------------------------------------
"./jpgs/An-Evening-of-Exceptional-28210/Lot1.jpg"
# 1 Define data generator functions for train and vlai
train_gen <- image_data_generator(rescale = 1/255,
                                      validation_split = 0)
train_generator <- flow_images_from_dataframe(
  dataframe = binded_full[1:4, ],
  x_col = "file",
  y_col = "dom_price",
  generator = train_gen,
  batch_size = 32,
  shuffle = FALSE,
  class_mode = "other",
  subset = "training"
)

# 2 Check if function works
batch <- generator_next(train_generator)
batch %>% class
plot(as.raster(batch[[1]][1,,,]))

# II Build keras model ----------------------------------------------------

h <- map(pic_path$file, ~(image_load(path = ., target_size= c(100, 100))%>% image_to_array))
# %>% image_to_array
h %>% map(image_to_array)
h[[1]]
rm(list = obj)
