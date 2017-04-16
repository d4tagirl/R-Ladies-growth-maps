library(rtweet)

# users <- search_users(q = 'RLadies',
#                      n = 1000,
#                      parse = TRUE)

library(dplyr)
library(lubridate)
library(stringr)
library(ggmap)
library(purrr)
library(tidyr)

users_2 <- unique(users) %>%
  filter(str_detect(screen_name, "^(RLadies).*")) %>%
  filter(!screen_name %in% c("RLadies", "RLadies_LF", "RLadiesGlobal")) %>%
  #   mutate(city = str_match(location, "^([^,]+)")[,2]) %>%
  mutate(location = ifelse(screen_name == "RLadiesLx", "Lisbon", location)) %>%
  mutate(location = ifelse(screen_name == "RLadiesMTL", "Montreal", location)) %>%
  mutate(age_days = difftime(as.Date('2017-4-20'), created_at, unit = "days")) %>%
  mutate(created = format(created_at, format = "%Y-%m")) %>%
  select(screen_name, location, created_at, created, followers = followers_count, age_days) %>%
  mutate(longlat = purrr::map(.$location, geocode)) %>%
  unnest()
 
# # #Google Maps API Terms of Service: http://developers.google.com/maps/terms.
# # #Please cite ggmap if you use it: see citation('ggmap') for details.

# create frames asuming linear increasingof followers



library("maps")
library("ggthemes")

load("RLadies_growing.Rdata")

mp <- ggplot(world.cities, package = "maps") +
  borders("world", colour = "gray80", fill = "gray80") +
  theme_map() +
  geom_point(aes(x = lon, y = lat, text = paste("city: ", location),
                 size = followers,
                 frame = created,
                 cumulative = TRUE),
             data = users_2, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 10))

library(gganimate)

animation::ani.options(ani.width = 1000, ani.height = 600)
gganimate(mp, interval = .2)
gganimate(mp, interval = .2, "output.gif")

library("plotly")

ggplotly(mp, tooltip = c("text", "size", "frame"))

