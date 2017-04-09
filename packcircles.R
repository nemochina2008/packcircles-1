# Circle packing visualisations
# Code based on R bloggers post: http://www.r-bloggers.com/circle-packing-in-r-again/

# Load packages ----
library(ggart)
library(ggforce)
library(packcircles)
library(viridis)
library(tidyverse)

# Make reproducible ---- 
set.seed(1)

# Parameters ----
n <- 750 # number of circles
m <- 9 # number of facets

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
df <- 1:m %>%
  map_df(~make_pack(runif(1, 0.75 * n, n), runif(1, -1, 1), runif(1, n, n)), .id = "facet")

# Make plot ----
p <- ggplot() +
  geom_circle(aes(x0 = x, y0 = y, r = radius, alpha = radius, fill = radius), df, size = 1,
              n = 90) +
  coord_equal() +
  scale_fill_gradient(low = "#efedf5", high = "#3f007d") +
  facet_wrap(~facet) +
  theme_blankcanvas()

# Save plot ----
ggsave("plots/plot001.png", p, width = 40, height = 40, units = c("in"))
