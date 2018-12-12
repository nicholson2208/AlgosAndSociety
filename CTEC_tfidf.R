library(tidyverse)
library(tidytext)

# load sentiment dictionary
nrc_lex <- get_sentiments("nrc") # many sentiments
all_stop_words <- stop_words %>% select(-lexicon) # long list of stop words

# read in json from CaesaParser.py, and flatten
dat <- jsonlite::fromJSON('data.json', flatten = TRUE)[[1]]
dat.df <- dat %>% 
  bind_rows() %>% 
  mutate(department = rep(names(dat), map_dbl(dat, nrow)))

# filter only CTECs with a gender and select a few columns
gender.comments.dept.df <- dat.df %>%
  filter(instructor_gender == "M" | instructor_gender == "F") %>%
  select(department, instructor_gender, comments) %>%
  mutate(dept_gender = paste(department, instructor_gender, sep="-"))

# one word per row
comment.words <- gender.comments.dept.df %>%
  unnest %>%
  unnest_tokens(ngram, comments, token="ngrams", n=2)

# bigrams
comment.bigrams <- gender.comments.dept.df %>%
  unnest %>%
  unnest_tokens(ngram, comments, token="ngrams", n=5)

# filter only words that have a sentiment score
# skip when n > 1
comment.words.interesting <- semi_join(comment.words, nrc_lex)

# create total word counts
gender.word.count <- comment.words.interesting %>% 
  count(instructor_gender, word, sort=TRUE) %>%
  # count(dept_gender, department, instructor_gender, word, sort=TRUE) %>%
  ungroup()

# bigrams
gender.bigram.count <- comment.bigrams %>%
  count(instructor_gender, ngram, sort=TRUE) %>%
  ungroup()

gender.total.bigrams <- gender.bigram.count %>%
  group_by(instructor_gender) %>%
  summarize(total = sum(n))

gender.bigrams <- left_join(gender.bigram.count, gender.total.bigrams)

gender.bigrams <- gender.bigrams %>%
  bind_tf_idf(ngram, instructor_gender, n)

gender.bigrams %>%
  # filter(department == 'BME' | department == 'EECS') %>%
  arrange(desc(tf_idf)) %>%
  mutate(ngram = factor(ngram, levels=rev(unique(ngram)))) %>%
  group_by(instructor_gender) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(ngram, tf_idf, fill=instructor_gender)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~instructor_gender, ncol=2, scales = "free") +
  coord_flip()

# group word counts
gender.total.words <- gender.word.count %>%
  group_by(instructor_gender) %>%
  summarize(total = sum(n))

gender.words <- left_join(gender.word.count, gender.total.words)

# perform tf-idf
gender.words <- gender.words %>%
  bind_tf_idf(word, instructor_gender, n)

########
# visualize tf-idf

gender.words %>%
  # filter(department == 'BME' | department == 'EECS') %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels=rev(unique(word)))) %>%
  group_by(instructor_gender) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=instructor_gender)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~instructor_gender, ncol=2, scales = "free") +
  coord_flip()
  