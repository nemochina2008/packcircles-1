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
m <- 300 # number of facets
animate <- FALSE # animate?

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

make_pack2 <- function(X, Y, n) {
  df <- data.frame(x = rep(X, times = n), y = rep(Y, times = n), r = c(1000, rep(10, times = n-1))) %>%
    mutate(x = jitter(x), y = jitter(y))
  temp <- circleRepelLayout(df, c(X - 5, X + 5), c(Y - 5, Y + 5))
  temp$layout
}

df <- 1:m %>%
  map_df(~make_pack2(runif(1, -50, 50), runif(1, -50, 50), 6), .id = "facet")

test <- (circleRepelLayout(df %>% select(x, y, radius)))$layout %>%
  mutate(dist = sqrt(x^2 + y^2)) #%>%
  #filter(dist <= 50)

#df <- df$layout

# Make plot ----
p <- ggplot() +
  geom_circle(aes(x0 = x, y0 = y, r = radius, alpha = dist),
              test %>% mutate(fill = runif(nrow(.), 0, 1)), size = 0.55,
              n = 4, fill = "black") +
  coord_equal() +
  #scale_fill_viridis(option = "B") +
  #facet_wrap(~facet) +
  theme_blankcanvas()
#p

# Save plot ----
if(animate) {
  animation::ani.options(interval = 1/30)
  gganimate(p, "gifs/gif0001.gif", title_frame = FALSE, ani.width = 600, ani.height = 600)
} else {
  ggsave("plots/plot003.png", p, width = 18, height = 18, units = c("in"))
}
