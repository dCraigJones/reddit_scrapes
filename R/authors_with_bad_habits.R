library(tidyverse)
library(jsonlite)
library(tidytext)

# Read JSON from Reddit API

link <- "https://www.reddit.com/r/books/comments/ndfktm/i_looked_up_to_michael_crichton_until_now.json?limit=10000"

raw <- read_json(link)

# Extract 1st Tier Responses and store in data.frame

n <- length(raw[[2]]$data$children)

data <- data.frame(
      id = raw[[2]]$data$children[[1]]$data$id
    , body = raw[[2]]$data$children[[1]]$data$body
    , score = raw[[2]]$data$children[[1]]$data$score
)


for (i in 2:(n-1)) {
  tmp <- data.frame(
      id = raw[[2]]$data$children[[i]]$data$id
    , body = raw[[2]]$data$children[[i]]$data$body
    , score = raw[[2]]$data$children[[i]]$data$score
  )

  data <- rbind(data, tmp)

}

# Remove Stop Words

data("stop_words")

text <- data %>%
  unnest_tokens(word, body) %>%
  anti_join(stop_words)

index <- match(text$word, parts_of_speech$word)
pos <- parts_of_speech$pos[index]

text <- cbind(text, pos)

count(text)

# Identify and Filter Nouns

text <- text %>%
  filter(pos=="Noun" | is.na(pos))

nrow(text)

# Identify and Filter Names

library(genderdata)

ssa_national %>% distinct(name) -> name_list

isName <- !is.na(match(text$word, name_list$name))

text <- cbind(text, isName) %>% filter(isName==TRUE)

nrow(text)

# Remove Common Words

library(qdapDictionaries)

isFry <- !is.na(match(text$word, Fry_1000))

text <- cbind(text, isFry) %>% filter(isFry==FALSE)

author_list <- text %>% count(word) %>% arrange(desc(n))

# <<This doesn't work>>

data %>%
  mutate(author=case_when(
      str_detect(tolower(body), "crichton") ~ "Michael Crichton"
    , str_detect(tolower(body), "orson") ~ "Orson Scott Card"
    , str_detect(tolower(body), "adams") ~ "Scott Adams"
    , str_detect(tolower(body), "atwood") ~ "Margaret Atwood"
    , str_detect(tolower(body), "sergei") ~ "Sergei Lukyanenko"
    , str_detect(tolower(body), "goodkind") ~ "Terry Goodkind"
    , str_detect(tolower(body), "deary") ~ "Terry Deary"
    , str_detect(tolower(body), "simmons") ~ "Dan Simmons"
    , str_detect(tolower(body), "ayn rand") ~ "Ayn Rand"
    , str_detect(tolower(body), "clancy") ~ "Tom Clancy"
    , str_detect(tolower(body), "clayton") ~ "Clayton Crain"
    , str_detect(tolower(body), "wallace") ~ "David Foster Wallace"
    , str_detect(tolower(body), "eddings") ~ "David Eddings"
    , str_detect(tolower(body), "demille") ~ "Nelson Demille"
    , str_detect(tolower(body), "finn") ~ "AJ Finn"
    , str_detect(tolower(body), "hubbard") ~ "L Ron Hubbard"
    , str_detect(tolower(body), "kerouac") ~ "Jack Kerouac"
    , str_detect(tolower(body), "unteweger") ~ "Jack Unteweger"
    , str_detect(tolower(body), "marion") ~ "Marion Simmer Bradley"
    , str_detect(tolower(body), "king") ~ "Stephen King"
    , str_detect(tolower(body), "christie") ~ "Agatha Christie"
    , str_detect(tolower(body), "ambrose") ~ "Stephen Ambrose"
    , str_detect(tolower(body), "dickens") ~ "Charles Dickens"
    , str_detect(tolower(body), "dahl") ~ "Roald Dahl"
    , str_detect(tolower(body), "k dick") ~ "Philip K Dick"
    , str_detect(tolower(body), "waugh") ~ "Evelyn Waugh"
    , str_detect(tolower(body), "pournelle") ~ "Jerry Pournelle"
    , str_detect(tolower(body), "herbert") ~ "Frank Herbert"
    , str_detect(tolower(body), "mccarthy") ~ "Joe McCarthy"
    , str_detect(tolower(body), "niven") ~ "Larry Niven"
    , str_detect(tolower(body), "piers") ~ "Piers Anthony"
    , str_detect(tolower(body), "mayne") ~ "William Mayne"
    , str_detect(tolower(body), "lovecraft") ~  "HP Lovecraft"
    , str_detect(tolower(body), "heinlein") ~  "Richard Heinlein"
    , str_detect(tolower(body), "banks") ~  "Iain M Banks"
    , str_detect(tolower(body), "koontz") ~  "Dean Koontz"
    , str_detect(tolower(body), "sriduangkae") ~  "Benjamin Sriduangkae"
    , str_detect(tolower(body), "cruise") ~  "Tom Cruise"
    , str_detect(tolower(body), "etzold") ~  "Veit Etzold"
    , str_detect(tolower(body), "rowling") ~  "JK Rowling"
  )) %>%
  mutate(author=ifelse(is.na(author), "Michael Crichton", author)) %>%
  group_by(author) %>%
  summarize(replies=n(), score=sum(score)) %>%
  arrange(desc(replies)) -> result

write.csv(result, "result.csv")






tmp %>%
  mutate(stop_words=str_detect(bigram, "york")) %>% filter(stop_words==TRUE)


# bigram test

data %>%
  unnest_tokens(bigram, body, token="ngrams", n=3, n_min=2) -> tmp


tmp %>%
  mutate(stop_words=str_detect(bigram, "orson")) %>% filter(stop_words==TRUE)

tmp %>%
  mutate(stop_words=str_detect(bigram, "adams")) %>% filter(stop_words==TRUE)

tmp %>%
  mutate(stop_words=str_detect(bigram, "crichton")) %>% filter(stop_words==TRUE)

tmp %>%
  mutate(stop_words=str_detect(bigram, "atwood")) %>% filter(stop_words==TRUE)

tmp %>%
  mutate(stop_words=str_detect(bigram, "sergei")) %>% filter(stop_words==TRUE)

tmp %>%
  mutate(stop_words=str_detect(bigram, "goodkind")) %>% filter(stop_words==TRUE)

tmp %>%
  mutate(stop_words=str_detect(bigram, "deary")) %>% filter(stop_words==TRUE)

tmp %>%
  mutate(stop_words=str_detect(bigram, "simmons")) %>% filter(stop_words==TRUE)

tmp %>%
  mutate(stop_words=str_detect(bigram, "hamilton")) %>% filter(stop_words==TRUE)
# also, anita and blake

tmp %>%
  mutate(stop_words=str_detect(bigram, "ayn rand")) %>% filter(stop_words==TRUE)



