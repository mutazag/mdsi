## practicing tidy text 

setwd("c:/mdsi/text/tidy")


text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text
str(text)


library(dplyr)


# the below is a dplyr data_frame (not data.frame), creates tibble: good for
# printing, strings are not converted to factors and does not show row names
text_df <- dplyr::data_frame(line=1:4, text = text)
text_df


# A token is a meaningful unit of text, most often a word, that we are
# interested in using for further analysis, and tokenization is the process of
# splitting text into tokens.

# tokenisation: convert text to token
install.packages("tidytext")
library(tidytext)

text_df %>% unnest_tokens(word, text, drop=F)  
text_token <- text_df %>% unnest_tokens(token, text, token="ngrams", drop=F)  
tail(text_token)



# Jane Austen text 

library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>% 
  mutate(linenumber = row_number(), 
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",ignore_case = T)))) %>%
  ungroup()


original_books
levels(original_books$book)
tail(original_books)

tidy_books <- original_books %>% unnest_tokens(word, text)
tidy_books

data("stop_words")
stop_words

tidy_books <- tidy_books %>% anti_join(stop_words)

# count words
tidy_books %>% count(word, sort = TRUE)

# plot

library(ggplot2)
tidy_books %>% count(word, sort=T) %>%
  filter(n>600) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n)) + geom_col() + xlab(NULL) + coord_flip()
