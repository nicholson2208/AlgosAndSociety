# library(RJSONIO)
library(jsonlite)
library(tidyverse)
library(tidytext)

# read in json from CaesaParser.py, and flatten
dat <- jsonlite::fromJSON('data.json', flatten = TRUE)[[1]]
dat.df <- dat %>% 
  bind_rows() %>% 
  mutate(department = rep(names(dat), map_dbl(dat, nrow)))

# filter only CTECs with a gender and select a few columns
gender.comments.dept.df <- dat.df %>%
  filter(instructor_gender == "M" | instructor_gender == "F") %>%
  select(department, instructor_gender, comments)

# load sentiment dictionary
nrc_lex <- get_sentiments("nrc") # many sentiments
all_stop_words <- stop_words %>% select(-lexicon) # long list of stop words

# one word per row
comment.words <- gender.comments.dept.df %>%
  unnest %>%
  unnest_tokens(word, comments)

# get rid of stop words
comment.words.interesting <- comment.words %>% anti_join(all_stop_words)

# get sentiment for each words, possibly multiple sentiments for
# each word, which creates multiple rows
comment.words.sentiment <- comment.words.interesting %>% left_join(nrc_lex)

# add a column of raw counts (count.sentiment)
# add a column that divides the raw counts by the totals for that month (freq.sentiment)
sentiment.count <- comment.words.sentiment %>% 
  filter(!is.na(sentiment)) %>% 
  group_by(department, instructor_gender, sentiment) %>% 
  summarize(count.sentiment=n()) %>%
  mutate(freq.sentiment = count.sentiment/sum(count.sentiment))

# sanity check to remove NAs
sentiment.count <- na.omit(sentiment.count)

# plot sentiment proportions by gender only
ggplot(sentiment.count, aes(x=sentiment, y=freq.sentiment)) +
  geom_boxplot(aes(color=instructor_gender)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot sentiment proportions by gender and department
ggplot(sentiment.count, aes(x=sentiment, y=freq.sentiment)) +
  geom_boxplot(aes(color=instructor_gender)) +
  facet_wrap(~department) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
