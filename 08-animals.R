# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggfx)

# Supply some data --------------------------------------------------------

nthen <- 230000
nnow <- 104700
ygap <- 100

# Prepare data for visualization ------------------------------------------

to_plot <-
  seq.int(from = nthen, to = nnow, length.out = ygap) %>%
  enframe(name = "year", value = "n") %>%
  mutate(
    xmin = -1,
    xmax = 1
  )

# Create plot -------------------------------------------------------------

p <-
  to_plot %>%
  ggplot() +
  as_reference(
    annotate(
      geom = "richtext",
      x = -0.1,
      y = 0,
      label = "<img src='asset/orangutan.png' height='510'/>",
      fill = NA,
      label.colour = NA,
      hjust = 0.5,
      vjust = 0
    ),
    id = "img"
  ) +
  with_blend(
    geom_segment(
      aes(x = xmin, xend = xmax, y = year, yend = year, colour = n),
      size = 2.35,
      show.legend = FALSE
    ),
    bg_layer = "img",
    blend_type = "atop"
  ) +
  geom_vline(xintercept = 0.6, size = 0.1, colour = "#C5E0D8") +
  annotate(
    geom = "segment",
    x = 0.6,
    xend = 0.65,
    y = c(0, 60, 100),
    yend = c(0, 60, 100),
    size = 0.1,
    colour = "#C5E0D8"
  ) +
  annotate(
    geom = "text",
    x = 0.675,
    y = c(0, 60, 100),
    label = c(
      "About 230,000 orangutans\nlived a century ago",
      "Since four decades ago,\n2,000-3,000 Bornean \norangutans were killed\nevery year!",
      "Now it's only 104,700\nBornean orangutans left"
    ),
    family = "Public Sans Thin",
    fontface = "italic",
    colour = "#C5E0D8",
    size = 3,
    hjust = 0,
    vjust = 0.5
  ) +
  annotate(
    geom = "text",
    x = -1.1,
    y = 100,
    label = "ORANGUTAN",
    angle = 90,
    family = "Public Sans",
    fontface = "bold",
    colour = "#DAFF7D",
    size = 27.5,
    hjust = 1,
    vjust = 1
  ) +
  with_blur(
    annotate(
      geom = "text",
      x = -0.85,
      y = 100,
      label = "don't let them fade ",
      angle = 90,
      family = "Public Sans",
      colour = "#DAFF7D",
      alpha = 0.3,
      size = 13,
      hjust = 1,
      vjust = 1
    ),
    sigma = 5
  ) +
  annotate(
    geom = "text",
    x = -0.1,
    y = 0,
    label = "Visualized by Muhammad Aswan Syahputra\nData from OFI and WWF\nImage credit to freepik.com",
    family = "Public Sans Thin",
    colour = "#C5E0D8",
    size = 2.25,
    vjust = 0.5
  ) +
  scale_color_gradient(low = "#034C3C", high = "#FEB95F") +
  theme_minimal(
    base_family = "Assistant"
  ) +
  theme(
    plot.background = element_rect(fill = "#034C3C", colour = NA),
    panel.background = element_rect(fill = "#034C3C", colour = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  coord_cartesian(clip = "off")

ggsave(
  "outfile/08-animals.png",
  plot = p,
  width = 8,
  height = 8,
  dpi = 300,
  type = "cairo-png"
)
