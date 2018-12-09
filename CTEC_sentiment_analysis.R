library(jsonlite)
library(tidyverse)
library(tidytext)
library(ggbeeswarm)

# read in json from CaesaParser.py, and flatten
dat <- jsonlite::fromJSON('data.json', flatten = TRUE)[[1]]
dat.df <- dat %>% 
  bind_rows() %>% 
  mutate(department = rep(names(dat), map_dbl(dat, nrow)))

# filter only CTECs with a gender and select a few columns
gender.comments.dept.df <- dat.df %>%
  filter(instructor_gender == "M" | instructor_gender == "F") %>%
  select(department, instructor_gender, instructor, comments)

# add STEM/Humanities
gender.comments.dept.df <- gender.comments.dept.df %>%
  mutate(category = with(., case_when(
    (department == 'BME') ~ 'STEM',
    (department == 'EECS') ~ 'STEM',
    (department == 'English') ~ 'Humanities',
    (department == 'Gender Studies') ~ 'Humanities',
    (department == 'Linguistics') ~ 'Humanities',
    (department == 'LOC') ~ 'STEM',
    (department == 'Music') ~ 'Humanities',
    (department == 'Spanish') ~ 'Humanities'
  )))


# load sentiment dictionary
nrc_lex <- get_sentiments("nrc") # many sentiments
afinn_lex <- get_sentiments('afinn') # <-5,5>
bing_lex <- get_sentiments('bing') # {negative, positive}
loughran_lex <- get_sentiments('loughran') # many sentiments
all_stop_words <- stop_words %>% select(-lexicon) # long list of stop words

# one word per row
comment.words <- gender.comments.dept.df %>%
  unnest %>%
  unnest_tokens(word, comments)

# get rid of stop words
comment.words.interesting <- comment.words %>% anti_join(all_stop_words)

# get sentiment for each words, possibly multiple sentiments for
# each word when using nrc, which creates multiple rows
comment.words.sentiment <- comment.words.interesting %>% left_join(afinn_lex)

# used for categorical sentiment, e.g. nrc lexicon
# add a column of raw counts (count.sentiment)
# add a column that divides the raw counts by the totals for that month (freq.sentiment)
# sentiment.count <- comment.words.sentiment %>% 
#   filter(!is.na(sentiment)) %>% 
#   group_by(department, instructor_gender, sentiment) %>% 
#   summarize(count.sentiment=n()) %>%
#   mutate(freq.sentiment = count.sentiment/sum(count.sentiment))

# used for continuous sentiment scores, e.g. afinn
sentiment.mean <- comment.words.sentiment %>%
  filter(!is.na(score)) %>%
  group_by(department) %>%
    mutate(dept.mean = sum(score)/n()) %>%
    ungroup() %>%
  group_by(instructor_gender) %>%
    mutate(gender.mean = sum(score)/n()) %>%
    ungroup() %>%
  group_by(instructor) %>%
    mutate(instructor.mean = sum(score)/n()) %>%
    ungroup() %>%
  group_by(department, instructor_gender) %>%
    mutate(dept.gender.mean = sum(score)/n()) %>%
    ungroup()

# sanity check to remove NAs
# sentiment.count <- na.omit(sentiment.count)
sentiment.mean <- na.omit(sentiment.mean)

# get categorical means
aggregate(sentiment.mean$score, by=list(sentiment.mean$instructor_gender), FUN=mean)
aggregate(sentiment.mean$score, by=list(sentiment.mean$department), FUN=mean)
aggregate(sentiment.mean$score, by=list(sentiment.mean$category), FUN=mean)

# plot mean sentiment by gender/department
ggplot(sentiment.mean, aes(x=instructor_gender, y=score)) +
  geom_boxplot(aes(color=instructor_gender)) +
  facet_wrap(~department) +
  NULL

ggplot(sentiment.mean, aes(x=instructor_gender, y=score)) +
  geom_jitter(aes(color=instructor_gender),
              alpha = 0.2) +
  geom_boxplot(aes(fill=instructor_gender),
               alpha = 0.6) +
  facet_wrap(~department) +
  NULL

# plot by gender/category
ggplot(sentiment.mean, aes(x=instructor_gender, y=score)) +
  geom_jitter(aes(color=instructor_gender),
              alpha = 0.2) +
  geom_boxplot(aes(fill=instructor_gender),
               alpha = 0.6) +
  facet_wrap(~category) +
  NULL


# plot sentiment proportions by gender only
# ggplot(sentiment.count, aes(x=sentiment, y=freq.sentiment)) +
#   geom_boxplot(aes(color=instructor_gender)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot sentiment proportions by gender and department
# ggplot(sentiment.count, aes(x=sentiment, y=freq.sentiment)) +
#   geom_boxplot(aes(color=instructor_gender)) +
#   facet_wrap(~department) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### significance tests
ggplot(sentiment.mean, aes(score)) +
  geom_bar() +
  facet_wrap(~instructor_gender)

t.test(score ~ instructor_gender, alternative='two.sided', data=sentiment.mean)
summary(aov(score ~ instructor_gender*department, data=sentiment.mean))
