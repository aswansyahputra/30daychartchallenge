# Load packages -----------------------------------------------------------

library(tidyverse)
library(magick)
library(imager)
library(packcircles)
library(scales)
library(gganimate)

#' Codes are adapted from https://chichacha.netlify.app/2018/12/22/bubble-packed-chart-with-r-using-packcircles-package/

# Preprocess and load image file ------------------------------------------
rid_logo_img <-
  image_read("asset/r-indonesia.png")

tmp <- tempfile(fileext = ".png")

rid_logo_img %>%
  image_background(color = "#E5E5E3") %>%
  image_write(tmp)

rid_logo <-
  load.image(tmp) %>%
  as.data.frame(wide = "c") %>%
  transmute(
    img_x = x,
    img_y = y,
    colour = rgb(c.1, c.2, c.3)
  )

# Calculate layout --------------------------------------------------------

headcount <- 3319 * 0.95 # take 5% of as bot :p
iconsize <- rbeta(headcount, 1, 0.7)

pack_layout <-
  circleProgressiveLayout(
    iconsize,
    sizetype = "area"
  ) %>%
  mutate(
    img_x = floor(rescale(x, to = range(rid_logo$img_x))),
    img_y = floor(rescale(y, to = range(rid_logo$img_y)))
  ) %>%
  rowid_to_column("id") %>%
  inner_join(rid_logo, by = c("img_x", "img_y"))

# Create dataset for plot -------------------------------------------------

to_plot <-
  circleLayoutVertices(
    pack_layout,
    xysizecols = c("x", "y", "radius")
  ) %>%
  inner_join(
    select(pack_layout, id, colour),
    by = "id"
  ) %>%
  group_by(id) %>%
  summarise(
    x = mean(x),
    y = mean(y),
    colour = unique(colour)
  ) %>%
  mutate(
    icon = sample(
      c(letters, LETTERS),
      size = n(),
      replace = TRUE
    ),
    frame = sample(
      seq.Date(
        from = as.Date("2016-08-13"),
        to = as.Date("2021-04-06"),
        length.out = 7
      ),
      size = n(),
      replace = TRUE
    )
  )

# Create animation --------------------------------------------------------

anim <-
  to_plot %>%
  ggplot(aes(x, y, colour = colour)) +
  geom_text(aes(label = icon), size = 10, family = "Wee People") +
  scale_y_reverse() +
  scale_colour_identity() +
  labs(
    title = "You are truly magical!",
    subtitle = "a tribute to Indonesian #rstats family",
    caption = "Headcount of member in Komunitas R Indonesia group\nVisualized by Muhammad Aswan Syahputra"
  ) +
  theme_void(base_family = "Public Sans") +
  theme(
    plot.background = element_rect(fill = "#F2F2F0", colour = NA),
    panel.background = element_rect(fill = "#F2F2F0", colour = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(hjust = 0.5, size = rel(2.75), family = "BF Tiny Hand", colour = "gray10"),
    plot.subtitle = element_text(hjust = 0.5, size = rel(1.5), family = "Homemade Apple", colour = "gray30"),
    plot.caption = element_text(hjust = 0.5, size = rel(1.25), colour = "gray30"),
    plot.margin = margin(30, 25, 10, 25)
  ) +
  coord_equal() +
  transition_manual(frame, cumulative = TRUE) +
  enter_appear() +
  ease_aes("circular-in")

# Save animation ----------------------------------------------------------

anim_save(
  "outfile/04-magical.gif",
  animation = anim,
  duration = 15,
  fps = 30,
  width = 8,
  height = 9.21,
  units = "in",
  device = "png",
  type = "cairo-png",
  res = 90,
  renderer = gifski_renderer(loop = TRUE)
)
