library(rtweet)

# users <- search_users(q = 'RLadies',
#                      n = 1000,
#                      parse = TRUE)

# saveRDS(users, 'users.rds')
# write.csv(users, 'users.csv')

users <- readRDS('users.rds')

library(dplyr)
library(lubridate)
library(stringr)
library(ggmap)
library(purrr)
library(tidyr)

# rladies <- unique(users) %>%
#   filter(str_detect(screen_name, '^(RLadies).*') &
#            !screen_name %in% c('RLadies', 'RLadies_LF', 'RLadiesGlobal')) %>%
#   add_row(                              # add Taipei chapter (info from Meetup)
#     screen_name = 'RLadiesTaipei',
#     location = 'Taipei',
#     created_at = as.Date('2014-11-15'),
#     followers_count = 347) %>%
#   add_row(
#     screen_name = 'RLadiesWarsaw',
#     location = 'Warsaw',
#     created_at = as.Date('2016-11-15'),
#     followers_count = 80) %>%
#   mutate(created_at = format(as.Date(created_at), format = '%Y-%m-%d'),
#          age_days = difftime(as.Date('2017-4-25'), created_at, unit = 'days'),
#          location = ifelse(screen_name == 'RLadiesLx', 'Lisbon',
#                            ifelse(screen_name == 'RLadiesMTL', 'Montreal', location))) %>%
#   select(screen_name, location, created_at, followers = followers_count, age_days) %>%
#   mutate(longlat = purrr::map(.$location, geocode)) %>%
#   unnest()

# saveRDS(rladies, 'rladies.rds')
# # write.csv(rladies, 'rladies.csv')
rladies <- readRDS('rladies.rds')
 
# # #Google Maps API Terms of Service: http://developers.google.com/maps/terms.
# # #Please cite ggmap if you use it: see citation('ggmap') for details.

library('maps')
library('ggthemes')

# load('RLadies_twitter_growth.Rdata')

#··················
# plotly

library('plotly')

map <- ggplot(world.cities, package = 'maps') +
  borders('world', colour = 'gray80', fill = 'gray80') +
  theme_map() +
  geom_point(aes(x = lon, y = lat, text = paste('city: ', location),
                 size = followers,
                 frame = created_at),
             data = rladies, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500, 750, 1000)) + 
  labs(size = 'Followers')

ggplotly(map, tooltip = c('text', 'size', 'frame'))

#··············
# gganimate 

map <- ggplot(world.cities, package = 'maps') +
  borders('world', colour = 'gray80', fill = 'gray80') +
  theme_map() +
  geom_point(aes(x = lon, y = lat, text = paste('city: ', location),
                 size = followers,
                 frame = created_at,
                 cumulative = TRUE),
             data = rladies, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500, 750, 1000)) + 
  labs(size = 'Followers')

library(gganimate)

# animation::ani.options(ani.width = 1000, ani.height = 600)
# # gganimate(map, interval = .3)
# gganimate(map, interval = .3, filename = 'rladies.gif')

animation::ani.options(ani.width = 750, ani.height = 450)
gganimate(map, interval = .3, filename = 'rladies.gif')

#··············
# gganimate, adding one transparent geom_point frame at the beggining

# init point to show empty map in the beggining
ghost_point <- rladies %>%
  add_row(
    created_at = format(as.Date('2012-09-01'), format = '%Y-%m-%d'),
    followers = 0,
    lon = 0,
    lat = 0,
    .before = 1) %>% 
  slice(1)

map_ghost <- map + 
  geom_point(aes(x = lon, y = lat, text = paste('city: ', location), #print init point
                 size = followers,
                 frame = created_at,
                 cumulative = TRUE),
             data = ghost_point, colour = 'blue', alpha = 0) + 
  labs(size = 'Followers')

# animation::ani.options(ani.width = 1000, ani.height = 600)
# # gganimate(map_ghost, interval = .3)
# gganimate(map_ghost, interval = .3, filename = 'rladies_ghost.gif')

animation::ani.options(ani.width = 750, ani.height = 450)
gganimate(map_ghost, interval = .3, filename = 'rladies_ghost.gif')

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

map_frames <- ggplot(world.cities, package = 'maps') +
  borders('world', colour = 'gray80', fill = 'gray80') +
  theme_map() +
  geom_point(aes(x = lon, y = lat, text = paste('city: ', location),
                 size = est_followers,
                 frame = date),
             data = rladies_frames, colour = 'purple', alpha = .5) +
  geom_point(aes(x = lon, y = lat, text = paste('city: ', location), #print init point
                 size = est_followers,
                 frame = date),
             data = ghost_point, colour = 'blue', alpha = 0) +
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500, 750, 1000)) + 
  labs(size = 'Followers')
             
# animation::ani.options(ani.width = 1000, ani.height = 600)
# # gganimate(map_frames, interval = .2)
# gganimate(map_frames, interval = .2, filename = 'rladies_frames.gif')

animation::ani.options(ani.width = 750, ani.height = 450)
gganimate(map_frames, interval = .2, filename = 'rladies_frames.gif')

#··············
# gganimate - with intermediate points - leaving some frames before London creation out

rladies_less_frames <- rladies_frames %>% 
  filter((day(date) == 1 & month(date) %% 6 == 0) |
           date >= rladies$created_at[rladies$screen_name == 'RLadiesLondon'])

map_less_frames <- ggplot(world.cities, package = 'maps') +
  borders('world', colour = 'gray80', fill = 'gray80') +
  theme_map() +
  geom_point(aes(x = lon, y = lat, text = paste('city: ', location),
                 size = est_followers,
                 frame = date),
             data = rladies_less_frames, colour = 'purple', alpha = .5) + 
  geom_point(aes(x = lon, y = lat, text = paste('city: ', location), #print init point
                 size = est_followers,
                 frame = date),
             data = ghost_point, colour = 'blue', alpha = 0) +
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500, 750, 1000)) + 
  labs(size = 'Followers')

# animation::ani.options(ani.width = 1000, ani.height = 600)
# # gganimate(map_less_frames, interval = .2)
# gganimate(map_less_frames, interval = .2, filename = 'rladies_less_frames.gif')

animation::ani.options(ani.width = 750, ani.height = 450)
gganimate(map_less_frames, interval = .2, filename = 'rladies_less_frames.gif')

#··············
# gganimate -  leaving some frames before London creation out - faster!

dates <- as_tibble(seq(min(rladies$created_at), 
                       as.POSIXlt('2017-04-25'), 
                       by = 'days')) %>% 
  filter(day(value) %in% c(1, 10, 20))

rladies_frames <- rladies %>% 
  nest(-screen_name) %>% 
  expand(screen_name, date = dates$value) %>%
  right_join(rladies, by = 'screen_name') %>% 
  filter(date > created_at) %>% 
  mutate(date = format(date, format = '%Y-%m-%d'),
         age_total = as.numeric(age_days, units = 'days'),
         age_at_date = as.numeric(difftime(date, created_at, unit = 'days'), units = 'days'),
         est_followers = ((followers - 1) / age_total) * age_at_date)

rladies_faster <- rladies_frames %>% 
  filter((day(date) == 1 & month(date) %% 6 == 0) |
           date >= rladies$created_at[rladies$screen_name == 'RLadiesLondon'])

map_faster <- ggplot(world.cities, package = 'maps') +
  borders('world', colour = 'gray80', fill = 'gray80') +
  theme_map() +
  geom_point(aes(x = lon, y = lat, text = paste('city: ', location),
                 size = est_followers,
                 frame = date),
             data = rladies_faster, colour = 'purple', alpha = .5) +
  geom_point(aes(x = lon, y = lat, text = paste('city: ', location), #print init point
                 size = est_followers,
                 frame = date),
             data = ghost_point, colour = 'blue', alpha = 0) +
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500, 750, 1000)) + 
  labs(size = 'Followers')

# animation::ani.options(ani.width = 1000, ani.height = 600)
# # gganimate(map_less_frames_fast, interval = .2)
# gganimate(map_less_frames_fast, interval = .2, filename = 'rladies_less_frames_fast.gif')

animation::ani.options(ani.width = 750, ani.height = 450)
gganimate(map_faster, interval = .2, filename = 'rladies_faster.gif')

save.image('RLadies_twitter_growth.RData')
