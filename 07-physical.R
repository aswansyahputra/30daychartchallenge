# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(countrycode)
library(ggridges)
library(ggfx)
library(gganimate)

# Import data -------------------------------------------------------------

prayers_time <-
  read_csv("data/prayers_time.csv")

# Preprocess data to plot -------------------------------------------------

to_plot <-
  prayers_time %>%
  filter(country != "Svalbard and Jan Mayen") %>%
  anti_join(
    filter(., maghrib - fajr < dhours(5))
  ) %>%
  mutate(
    continent = countryname(country, destination = "continent"),
    duration = (as.numeric(maghrib - fajr)),
    continent = fct_reorder(continent, duration)
  ) %>%
  drop_na()

# Create plot -------------------------------------------------------------

anim <-
  to_plot %>%
  ggplot(aes(duration, continent)) +
  as_reference(
    geom_text(
      data = to_plot %>%
        summarise(
          x_pos = 9,
          y_pos = unique(continent),
          continent = toupper(y_pos)
        ),
      aes(x_pos, y_pos, label = continent),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 0,
      family = "Assistant",
      fontface = "bold",
      colour = "#93E5AB",
      size = 13
    ),
    id = "text"
  ) +
  with_blend(
    geom_density_ridges(
      rel_min_height = 0.001,
      fill = "#E3DBDB",
      colour = "#034C3C"
    ),
    bg_layer = "text",
    blend_type = "xor"
  ) +
  labs(
    caption = "{format(as.Date(frame_time), format = 'DAY %d')}"
  ) +
  annotate(
    geom = "text",
    x = 21,
    y = 1.9,
    label = "Fasting duration\nduring Ramadhan 2021",
    family = "Titillium Web",
    colour = "#E3DBDB",
    size = 5,
    hjust = 1,
    lineheight = 0.9
  ) +
  annotate(
    geom = "text",
    x = 9,
    y = 1,
    label = "Data source: aladhan.com | Visualized by Muhammad Aswan Syahputra",
    family = "Titillium Web",
    colour = "#E3DBDB",
    size = 3,
    hjust = 0,
    vjust = 5
  ) +
  scale_y_discrete(expand = c(0, 0.05)) +
  scale_x_continuous(breaks = c(12, 15, 18), labels = function(x) paste(x, "hours"), position = "top", expand = c(0.05, 0)) +
  theme_minimal(
    base_family = "Assistant"
  ) +
  theme(
    plot.background = element_rect(fill = "#034C3C", colour = NA),
    panel.background = element_rect(fill = "#034C3C", colour = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted", size = 0.1, colour = "#C5E0D8"),
    panel.grid.major.y = element_blank(),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0.95, vjust = 5.25, face = "bold", colour = "#FCD0A1", size = rel(4.25)),
    axis.text.x = element_text(colour = "#C5E0D8"),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(25, 5, 0, 5)
  ) +
  coord_cartesian(clip = "off") +
  transition_time(hijri_date) +
  ease_aes()

# Save animation ----------------------------------------------------------

anim_save(
  "outfile/07-physical.gif",
  animation = anim,
  duration = 15,
  fps = 10,
  width = 8,
  height = 5,
  units = "in",
  device = "png",
  type = "cairo-png",
  res = 90,
  renderer = gifski_renderer(loop = TRUE)
)
