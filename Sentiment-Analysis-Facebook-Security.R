library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(widyr)
library(ggplot2)
library(knitr)
library(kableExtra)

file<-file.choose()
df<-read.csv(file)
glimpse(df[2,])
df<-df1
#data cleaning of status column
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# fix (expand) contractions
df$status_message<- sapply(df$status_message, fix.contractions)
# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
df$status_message <- sapply(df$status_message, removeSpecialChars)
# convert everything to lower case
df$status_message <- sapply(df$status_message, tolower)
#display column status message now
str(df[15, ]$status_message, nchar.max = 300)
#filtering words for stop words and words less than 1-3 char, creating new dataframe 
status_words_filtered <- df %>%
  unnest_tokens(word, status_message) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(nchar(word) > 3)
#displaying status that match word
#general analysis
status_words_filtered %>% 
  filter(word == "phishing") %>%
  select(word, status_type, num_likes,num_angrys, status_published,num_shares) %>%
  arrange() %>%

  kable("html", escape = FALSE, align = "c", caption = "Tokenized Format Example") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"),               full_width = FALSE)
#frequently used words in status
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")
status_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Status count") +
  ggtitle("Most Frequently Used Words in Statuses") +
  coord_flip()
  