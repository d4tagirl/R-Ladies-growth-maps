library(rtweet)

# users <- search_users(q = 'RLadies',
#                      n = 1000,
#                      parse = TRUE)
# 
# write.csv(users, 'users.csv')


library(readr)
library(dplyr)
users <- read_csv('users.csv') %>% 
  select(-1)

library(dplyr)
library(lubridate)
library(stringr)
library(ggmap)
library(purrr)
library(tidyr)
library(tibble)

lookup <- tibble(screen_name = c('RLadiesLx','RLadiesMTL' ,'RLadiesSeattle'), 
                     location = c('Lisbon', 'Montreal', 'Seattle'))

rladies <- unique(users) %>%
  filter(str_detect(screen_name, '^(RLadies).*') &
           !screen_name %in% c('RLadies', 'RLadies_LF', 'RLadiesGlobal')) %>%
  add_row(                              # add Taipei chapter (info from Meetup)
    screen_name = 'RLadiesTaipei',
    location = 'Taipei',
    created_at = as.Date('2014-11-15'),
    followers_count = 347) %>%
  add_row(
    screen_name = 'RLadiesWarsaw',
    location = 'Warsaw',
    created_at = as.Date('2016-11-15'),
    followers_count = 80) %>%
  mutate(created_at = as.Date(created_at),
         age_days = difftime(as.Date('2017-5-15'), created_at, unit = 'days')) %>%
  left_join(lookup, by = 'screen_name') %>%
  mutate(location = ifelse(is.na(location.y), location.x, location.y)) %>%
  select(screen_name, location, created_at, followers = followers_count, age_days) %>%
  mutate(longlat = purrr::map(.$location, geocode)) %>%
  unnest()

# saveRDS(rladies, 'rladies_longlat.rds')
# # write.csv(rladies, 'rladies.csv')
rladies <- read_csv('rladies.csv') %>% 
  select(-1)
 
# # #Google Maps API Terms of Service: http://developers.google.com/maps/terms.
# # #Please cite ggmap if you use it: see citation('ggmap') for details.

library('maps')
library('ggthemes')

# load('RLadies_twitter_growth.Rdata')

#··················
# plotly

library('plotly')

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()

map <- world +
  geom_point(aes(x = lon, y = lat,
                 text = paste('city: ', location,
                              '<br /> created : ', created_at),
                 size = followers),
             data = rladies, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

ggplotly(map, tooltip = c('text', 'size'))

#··············
# gganimate 

# print just the map without any animation
map <- world +
  geom_point(aes(x = lon, y = lat,
                 size = followers),
             data = rladies, 
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

# init point to show empty map in the beggining
ghost_point <- rladies %>%
  add_row(
    created_at = as.Date('2011-09-01'),
    followers = 0,
    lon = 0,
    lat = 0,
    .before = 1) %>%
  slice(1)

map <- world +
  geom_point(aes(x = lon, y = lat,
                 size = followers,
                 frame = created_at,
                 cumulative = TRUE),
             data = rladies, colour = 'purple', alpha = .5) +
  geom_point(aes(x = lon, y = lat,  # this is the transparent frame
                 size = followers,
                 frame = created_at,
                 cumulative = TRUE),
             data = ghost_point, alpha = 0) +
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers') 

library(gganimate)

animation::ani.options(ani.width = 750, ani.height = 450)
gganimate(map, interval = .2)

#··············
# gganimate - with intermediate points!

library(tibble)

dates <- as_tibble(seq(as.Date(min(rladies$created_at)), 
                       as.Date('2017-04-25'), 
                       by = 'days')) %>% 
  filter(day(value) %in% c(1, 5, 10, 15, 20, 25))

rladies_frames <- rladies %>% 
  nest(-screen_name) %>% 
  expand(screen_name, date = dates$value) %>%
  right_join(rladies, by = 'screen_name') %>% 
  filter(date > created_at) %>% 
  mutate(date = format(date, format = '%Y-%m-%d'),
         age_total = as.numeric(age_days, units = 'days'),
         age_at_date = as.numeric(difftime(date, created_at, unit = 'days'), units = 'days'),
         est_followers = ((followers - 1) / age_total) * age_at_date)

# modify init point to show empty map in the beggining
ghost_point <-  ghost_point %>% 
  mutate(date = format(created_at, format = '%Y-%m-%d'),
         est_followers = 0)

map_frames <- world +
  geom_point(aes(x = lon, y = lat,
                 size = est_followers,
                 frame = date),
             data = rladies_frames, colour = 'purple', alpha = .5) +
  geom_point(aes(x = lon, y = lat,  #print init point
                 size = est_followers,
                 frame = date),
             data = ghost_point, colour = 'blue', alpha = 0) +
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500, 750, 1000)) + 
  labs(size = 'Followers')
             
animation::ani.options(ani.width = 750, ani.height = 450)
gganimate(map_frames, interval = .05)

#··············
# gganimate - with less intermediate points!

library(lubridate)

dates <- as_tibble(seq(floor_date(as.Date(min(rladies$created_at)), 
                                  unit = "month"),
                       as.Date('2017-05-15'),
                       by = 'days')) %>%
  filter(day(value) %in% c(1, 10, 20))

library(tidyr)
  
rladies_frames <- rladies %>%
  select(screen_name) %>%
  expand(screen_name, date = dates$value) %>%
  right_join(rladies, by = 'screen_name') %>%
  filter(date > created_at) %>%
  mutate(date = format(date, format = '%Y-%m-%d'),
         age_total = as.numeric(age_days, units = 'days'),
         age_at_date = as.numeric(difftime(date, created_at, unit = 'days'), 
                                  units = 'days'),
         est_followers = ((followers - 1) / age_total) * age_at_date)

map_frames <- world +
  geom_point(aes(x = lon, y = lat,
                 size = est_followers,
                 frame = date),
             data = rladies_frames, colour = 'purple', alpha = .5) +
  geom_point(aes(x = lon, y = lat,
                 size = est_followers,
                 frame = date),
             data = ghost_point, alpha = 0) +
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

gganimate(map_frames, interval = .05)

#··············
# gganimate - with intermediate points - leaving some frames before London creation out

rladies_less_frames <- rladies_frames %>%
  filter((day(date) == 1 & month(date) %% 6 == 0) |
           date >= rladies$created_at[rladies$screen_name == 'RLadiesLondon'])

map_less_frames <- world +
  geom_point(aes(x = lon, y = lat,
                 size = est_followers,
                 frame = date),
             data = rladies_less_frames, colour = 'purple', alpha = .5) +
  geom_point(aes(x = lon, y = lat,
                 size = est_followers,
                 frame = date),
             data = ghost_point, alpha = 0) +
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')

animation::ani.options(ani.width = 1125, ani.height = 675)
gganimate(map_less_frames, interval = .15, "rladies_growth.gif")

save.image('RLadies_twitter_growth.RData')
