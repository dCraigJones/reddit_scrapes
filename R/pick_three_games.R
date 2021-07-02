library(tidyverse)
library(jsonlite)
library(tidytext)
library(readxl)

# Read JSON from Reddit API

# Constants ---------------------------------------------------------------

games <- c(
  "Super Mario"
  , "The Legend of Zelda"
  , "Kirby"
  , "Pokemon"
  , "Metroid"
  , "Sonic the Hedgehog"
  , "Mega Man"
  , "Animal Crossing"
  , "Pikmin"
  , "Fire Emblem"
  , "F-Zero"
  , "Golden Sun"
  , "Donkey Kong"
  , "Advance Wars"
  , "Xenoblade Chronicles"
  , "Ratchet and Clank"
  , "Jak and Daxter"
  , "Sly Cooper"
  , "God of War"
  , "Shin Megami Tensei: Persona"
  , "Final Fantasy"
  , "Resident Evil"
  , "Silent Hill"
  , "Uncharted"
  , "Halo"
  , "Street Fighter"
  , "Mortal Kombat"
  , "Gears of War"
  , "Devil May Cry"
  , "Castlevania"
  , "Crash Bandicoot"
  , "Spyro"
  , "Metal Gear Solid"
  , "Doom"
  , "Bioshock"
  , "Kingdom of Hearts"
  , "Fallout"
  , "Monster Hunter"
  , "Grand Theft Auto"
  , "Dark Souls"
)


# Import from Reddit ------------------------------------------------------

link <- "https://www.reddit.com/r/gaming/comments/obysq4/pokemon_gta_and_halo.json?limit=100000"

raw <- read_json(link)

# Extract 1st Tier Responses and store in data.frame

n <- length(raw[[2]]$data$children)

data <- data.frame(
      id = raw[[2]]$data$children[[1]]$data$id
    , body = raw[[2]]$data$children[[1]]$data$body
    , username = raw[[2]]$data$children[[1]]$data$author
    , score = raw[[2]]$data$children[[1]]$data$score
)


for (i in 2:(n-1)) {
  tmp <- data.frame(
      id = raw[[2]]$data$children[[i]]$data$id
    , body = raw[[2]]$data$children[[i]]$data$body
    , username = raw[[2]]$data$children[[i]]$data$author
    , score = raw[[2]]$data$children[[i]]$data$score
  )

  data <- rbind(data, tmp)

}


# Get Common Words --------------------------------------------------------

# Remove Stop Words

data("stop_words")

text <- data %>%
  unnest_tokens(word, body) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  filter(n>1) %>%
  arrange(desc(n))


# Export Common Words and Factor ------------------------------------------

write.csv(text, file="ref/text.csv")
write.csv(games, file="ref/games.csv")


fct <- read_excel("ref/pick_three_games_factored.xlsx")


# Build data frame --------------------------------------------------------

data %>%
  unnest_tokens(word, body) %>%
  anti_join(stop_words) %>%
  mutate(index=match(word, fct$word)) %>%
  mutate(game=fct$game[index]) %>%
  select(id, username, game) %>%
  filter(!is.na(game)) %>%
  unique() -> tmp


tmp %>%
  count(username) %>%
  filter(n == 3) -> good_users

tmp %>%
  left_join(good_users) %>%
  filter(!is.na(n)) %>%
  select(-n) %>% 
  arrange(username, game)-> export
