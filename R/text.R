library(keras)
library(lime)
library(rsample)



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
input <- pad_sequences(sequences = text_sequence, maxlen = text_max) %>% as_tibble

# 6 Get target values
target <- binded$dom_price

# 7 Split into training and validation set
train_test_split <- initial_split(input, prop = 0.75)

# 8 Retrieve testings
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)

plot(input$)
