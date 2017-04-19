library(rtweet)

# users <- search_users(q = 'RLadies',
#                      n = 1000,
#                      parse = TRUE)

library(dplyr)
library(lubridate)
# library(stringr)
library(ggmap)
library(purrr)
library(tidyr)

saveRDS(users, "users.rds")

users <- readRDS("users.rds")

save.image()
load("RLadies_growing.Rdata")

rladies <- unique(users) %>%
  filter(str_detect(screen_name, "^(RLadies).*")) %>%
  filter(!screen_name %in% c("RLadies", "RLadies_LF", "RLadiesGlobal")) %>%
  #   mutate(city = str_match(location, "^([^,]+)")[,2]) %>%
  mutate(location = ifelse(screen_name == "RLadiesLx", "Lisbon", location)) %>%
  mutate(location = ifelse(screen_name == "RLadiesMTL", "Montreal", location)) %>%
  add_row(                              # add Taipei chapter
    screen_name = "RLadiesTaipei",
    location = "Taipei",
    created_at = as.Date("2016-10-23 01:00:00"),
    followers_count = 347) %>% 
  mutate(age_days = difftime(as.Date('2017-4-25'), created_at, unit = "days")) %>%
  mutate(created = format(created_at, format = "%Y-%m")) %>%
  select(screen_name, location, created_at, created, followers = followers_count, age_days) %>%
  mutate(longlat = purrr::map(.$location, geocode)) %>%
  unnest()
 
# # #Google Maps API Terms of Service: http://developers.google.com/maps/terms.
# # #Please cite ggmap if you use it: see citation('ggmap') for details.

library("maps")
library("ggthemes")

# load("RLadies_growing.Rdata")

#··············
# gganimate 

map <- ggplot(world.cities, package = "maps") +
  borders("world", colour = "gray80", fill = "gray80") +
  theme_map() +
  geom_point(aes(x = lon, y = lat, text = paste("city: ", location),
                 size = followers,
                 frame = created,
                 cumulative = TRUE),
             data = rladies, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 12))

library(gganimate)

animation::ani.options(ani.width = 1000, ani.height = 600)
gganim <- gganimate(map, interval = .2)
gganimate(map, interval = .2, "rladies.gif")

#··············
# gganimate, adding one transparent geom_point frame at the beggining

# init point to show empty map in the beggining
ghost_point <- rladies %>%
  add_row(
    created_at = as.Date("2012-10-01 01:00:00"),
    created = format(created_at, format = "%Y-%m"),
    followers = 0,
    lon = 0,
    lat = 0,
    .before = 1) %>% 
  slice(1)

map_ghost <- map + 
  geom_point(aes(x = lon, y = lat, text = paste("city: ", location), #print init point
                 size = followers,
                 frame = created,
                 cumulative = TRUE),
             data = ghost_point, colour = 'blue', alpha = 0) 

animation::ani.options(ani.width = 1000, ani.height = 600)
gganim_ghost <- gganimate(map_ghost, interval = .2)
gganimate(map_ghost, interval = .2, "rladies_ghost.gif")

#··············
# gganimate - with intermediate points!

library(tibble)

# dates <- as_tibble(seq(min(rladies$created_at), 
#     as.POSIXlt('2017-04-25'), 
#     by = "15 day"))

dates <- as_tibble(seq(min(rladies$created_at), 
                       as.POSIXlt('2017-04-25'), 
                       by = "days")) %>% 
  filter(day(value) %in% c(1,15))

rladies_frames <- rladies %>% 
  nest(-screen_name) %>% 
  expand(screen_name, date = dates$value) %>%
  right_join(rladies, by = "screen_name") %>% 
  filter(date > created_at)
  
rladies_frames <- rladies_frames %>% 
    mutate(age_total = as.numeric(age_days, units = "days"),
           age_at_date = as.numeric(difftime(date, created_at, unit = "days"), units = "days"),
           est_followers = ((followers - 1) / age_total) * age_at_date)

# modify init point to show empty map in the beggining

ghost_point <-  ghost_point %>% 
  mutate(date = created_at,
         est_followers = 0)

map_frames <- ggplot(world.cities, package = "maps") +
  borders("world", colour = "gray80", fill = "gray80") +
  theme_map() +
  geom_point(aes(x = lon, y = lat, text = paste("city: ", location),
                 size = est_followers,
                 frame = date#,
                 # cumulative = TRUE
                 ),
             data = rladies_frames, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 10)) + 
  geom_point(aes(x = lon, y = lat, text = paste("city: ", location), #print init point
                 size = est_followers,
                 frame = date#,
                 # cumulative = TRUE
                 ),
             data = ghost_point, colour = 'blue', alpha = 0) 
             
animation::ani.options(ani.width = 1000, ani.height = 600)
gganim_frames <- gganimate(map_frames, interval = .08)
gganimate(map_frames, interval = .2, "rladies_frames.gif")

#··············
# gganimate - with intermediate points - leave frames out from the SF ch

rladies_wo_SFframes <- rladies_frames %>% 
  filter((day(date) == 1 & month(date) %% 3 == 0)|
           date >= rladies$created_at[rladies$screen_name == "RLadiesLondon"])

map_wo_SFframes <- ggplot(world.cities, package = "maps") +
  borders("world", colour = "gray80", fill = "gray80") +
  theme_map() +
  geom_point(aes(x = lon, y = lat, text = paste("city: ", location),
                 size = est_followers,
                 frame = date#,
                 # cumulative = TRUE
  ),
  data = rladies_wo_SFframes, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 15)) + 
  geom_point(aes(x = lon, y = lat, text = paste("city: ", location), #print init point
                 size = est_followers,
                 frame = date#,
                 # cumulative = TRUE
  ),
  data = ghost_point, colour = 'blue', alpha = 0) 

animation::ani.options(ani.width = 1000, ani.height = 600)
gganim_wo_SFframes <- gganimate(map_wo_SFframes, interval = .2)

gganimate(map_wo_SFframes, interval = .1, "rladies_wo_SFframes.gif")

#··················
# plotly

library("plotly")

ggplotly(map, tooltip = c("text", "size", "frame"))

