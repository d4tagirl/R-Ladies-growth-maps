---
  layout: post
title:  How to plot animated maps with gganimate
date: "2017-05-10 10:11:29 UYT"
published: true
tags: [rstats, r, gganimate, maps, gif]
description: How to plot an animated map using the gganimate package, and produce a .gif file to share it!
  ---
  Here I show how to plot an animated map using the `gganimate` package, and produce a .gif file to share it!
  
  <!--more-->
  
  This is the third one of the 3-posts-series, where I go from fetching Twitter users and preparing the data to visualizing it (If I wanted to show everything I've done in a single post, it would be almost as long as my first one! And I didn't want that `r emo::ji("stuck_out_tongue_closed_eyes")` ):
  
  * [How to fetch Twitter users with R]: the title is kind of self explanatory...
* [How to deal with ggplotly huge maps]: where I go through the details of why I chose not to use `ggplotly` and use `plot_geo` instead to generate HTML.
* How to plot animated maps with gganimate: this one. Again, pretty obvious subject.

Finally [I present my favourite visualization here]({% post_url 2017-05-10-visualizing-r-ladies-growth %}).

<br />
  ## The data
  
  Let's take a look at the R-Ladies' chapters' Twitter accounts dataframe (`rladies`) I produced in the first post of this series:

```{r load_data, echo = FALSE, message = FALSE, warning = FALSE}
# You can find everything I use here:
# https://github.com/d4tagirl/R-Ladies-growth-maps

library(knitr)

# This I took (with a few tweaks courtesy of my amazing hubby <3) from gganimate documentation https://github.com/dgrtwo/gganimate/blob/master/README.Rmd

library(animation)
ani.options(autobrowse = FALSE, ani.width = 850, ani.height = 510)

opts_knit$set(animation.fun = function(x, options, format = "gif") {
x = c(knitr:::sans_ext(x), knitr:::file_ext(x))
fig.num = options$fig.num
format = sub("^[.]", "", format)
base = sub(paste0(fig.num, '$'), '', x[1])
fig.fname = paste0(sub(paste0(fig.num, "$"), "*", x[1]),
".", x[2])
mov.fname = paste0(sub('-$', '', base), '.', format)

# order correctly
figs <- Sys.glob(fig.fname)
figs <- figs[order(as.numeric(stringr::str_match(figs, paste0("(\\d+)\\.", x[2]))[, 2]))]

animation::im.convert(figs, output = mov.fname)
original_path <- basename(mov.fname)
file.rename(original_path, mov.fname)
file.remove(figs)

sprintf("![%s](%s)", options$label, paste0(opts_knit$get("base.url"), mov.fname))
})

knitr::opts_chunk$set(dpi = 150, fig.align = 'center', screenshot.force = FALSE, fig.height = 4, fig.cap = "")
# options(width = 80, dplyr.width = 150)
```

```{r  head_users, message = FALSE, warning = FALSE}
library(readr)
library(dplyr)

url_csv <- 'https://raw.githubusercontent.com/d4tagirl/R-Ladies-growth-maps/master/rladies.csv'
rladies <- read_csv(url(url_csv)) %>% 
select(-1)

library(DT)

datatable(rladies, rownames = FALSE,
options = list(pageLength = 5))
```

<br />
## Plotting the map using ggplot2

The goal is to produce a map where each chapter is plotted according to its location, with the point's size indicating the amount of Twitter followers. 

I use the `maps` package to get the world map, using the `ggplot2::ggplot` and `ggthemes::theme_map` functions for plotting it nicely. Then I plot the chapters choosing the purple color, obviously!
  
  ```{r message = FALSE, warning = FALSE}
library(ggplot2)
library(maps)
library(ggthemes)

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

map <- world +
  geom_point(aes(x = lon, y = lat,
                 size = followers),
             data = rladies, 
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers')
```

```{r static_chapters_map, echo = FALSE, message = FALSE, warning = FALSE}
map
```

The `range` parameter is what controls the scale of the points' size.
<br />
## Animating the map using gganimate

Now let's animate the map! The core thing here is that I want every chapter appearing following the creation timeline, to somehow tell a story with the map. Lets start by animating `map`: the `ggplot` object I just created. I have to make a few changes for `gganimate` to work:
  
  - `gganimate` requires a `frame` aesthetic: I'll use the `created_at` variable. You set this as a new aesthetic in `ggplot` which is ignored by it (as shown in the warning messages), but `gganimate` recognizes and uses it;

- I also add the `cumulative = TRUE`, an additional aesthetic  (same comment about `ggplot` ignoring it), so once the chapter appears on the map, it keeps showing in all the following frames.

Following [my good friend Bruno](https://www.linkedin.com/in/bruno-chiesa-gispert-b1a6b942)'s suggestion, I add an empty frame at the beginning so that the first frame you see is just the empty map. I generate a dataframe with the same structure than the original one, with some random data, except for the `created_at` field that should be filled with a date prior to the first chapter creation for it to appear at the beginning.

And I add some empty frames at the end as well, to be able to see the final composition of chapters for a bit longer.

```{r}
rladies <- rladies  %>% 
  slice(7:9) %>% 
  mutate(frame = reorder(as.character(created_at), created_at, ordered = TRUE)) 

ghost_points_ini <- tibble(
  frame = reorder('before', as.Date('2011-09-01')),
  followers = 0, lon = 0, lat = 0)

ghost_points_fin <- tibble(
  created_at = seq(max(rladies$created_at) + days(1),
                   as.Date('2017-05-20'),
                   by = 'days'),
  followers = 0, lon = 0, lat = 0) %>% 
  mutate(frame = reorder(rep('today', times = length(created_at)), 
                         created_at, ordered = TRUE))

levels = unlist(list(ghost_points_ini$frame, 
                     rladies$frame, 
                     ghost_points_fin$frame))

levels <- c(as.character(levels))

levels(rladies$frame) = c(levels(rladies$frame), levels)
levels(ghost_points_ini$frame) = c(levels(ghost_points_ini$frame), levels)
levels(ghost_points_fin$frame) = c(levels(ghost_points_fin$frame), levels)

```

Then I add an extra layer to the `ggplot`: the second `geom_point`, with the `alpha` parameter set to `0` so the point will not show in the plot.

```{r ani_map, fig.show = "animate", fig.align = "center", message = FALSE}
map <- world +
  geom_point(aes(x = lon, y = lat,
                 size = followers,
                 frame = frame,
                 cumulative = TRUE),
             data = rladies, colour = 'purple', alpha = .5) +
  geom_point(aes(x = lon, y = lat,  # this is the init transparent frame
                 size = followers,
                 frame = frame,
                 cumulative = TRUE),
             data = ghost_points_ini, alpha = 0) +
  geom_point(aes(x = lon, y = lat,  # this is the final transparent frames
                 size = followers,
                 frame = frame,
                 cumulative = TRUE),
             data = ghost_points_fin, alpha = 0) +
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Followers') 

library(gganimate)
ani.options(interval = 0.2)
gganimate(map)
```