# Circle packing visualisations
# Code based on R bloggers post: http://www.r-bloggers.com/circle-packing-in-r-again/

# Load packages ----
library(ggart)
library(gganimate)
library(ggforce)
library(packcircles)
library(tweenr)
library(viridis)
library(tidyverse)

# Make reproducible ---- 
set.seed(1)

# Parameters ----
n <- 750 # number of circles
m <- 9 # number of facets
animate <- TRUE # animate?

# Function for making pack of circles ----
make_pack <- function(n, dir = 1, rep = 1) {
  rep(1:n, times = 5)
  r <- 1:n
  if(dir < 0) {
    circleProgressiveLayout(rev(r)) %>% mutate(fill = runif(n), facet = 1)
  } else {
    circleProgressiveLayout(r) %>% mutate(fill = runif(n), facet = 2)
  }
}

# Combine into data frame ----
if(animate) {
  df1 <- make_pack(n, dir = 1)
  df2 <- make_pack(n, dir = -1)
  df <- list(df1, df2, df1)
  tf <- tween_states(df, tweenlength = 2.5, statelength = 0.5, ease = "exponential-out", nframes = 25)
} else {
  df <- 1:m %>%
    map_df(~make_pack(runif(1, 0.75 * n, n), runif(1, -1, 1), runif(1, n, n)), .id = "facet")
}


# Make plot ----
p <- ggplot() +
  geom_circle(aes(x0 = x, y0 = y, r = radius, alpha = radius, fill = radius, frame = .frame),
              tf, size = 0.5,
              n = 90) +
  coord_equal() +
  scale_fill_gradient(low = "#efedf5", high = "#3f007d") +
  #facet_wrap(~facet) +
  theme_blankcanvas()

# Save plot ----
if(animate) {
  animation::ani.options(interval = 1/30)
  gganimate(p, "gifs/gif0001.gif", title_frame = FALSE, ani.width = 600, ani.height = 600)
} else {
  ggsave("plots/plot001.png", p, width = 40, height = 40, units = c("in"))
}
