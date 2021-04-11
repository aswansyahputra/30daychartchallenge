# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggforce)
library(scales)
library(ggpomological)

# Load dataset ------------------------------------------------------------

onepiece_chapters <- read_csv("data/onepiece_chapters.csv")
onepiece_arcs <- read_csv("data/onepiece_arcs.csv")

# Preprocess data for plot ------------------------------------------------

onepiece_arcs_long <-
  onepiece_arcs %>%
  rowwise() %>%
  transmute(
    arc,
    saga,
    chapter = toString(seq.int(from = chapter_from, to = chapter_to, by = 1))
  ) %>%
  ungroup() %>%
  separate_rows(chapter, sep = ", ", convert = TRUE)

to_plot <-
  onepiece_chapters %>%
  left_join(onepiece_arcs_long) %>%
  group_by(
    year = year(release_date),
    week = isoweek(release_date)
  ) %>%
  summarise(
    saga = unique(saga),
    saga = fct_inorder(saga),
    chapters = toString(chapter),
    n_chapters = n(),
    pages = sum(pages),
    avg_pages = pages / n_chapters
  ) %>%
  ungroup()

# Create plot -------------------------------------------------------------

p <-
  to_plot %>%
  ggplot(aes(week, year)) +
  annotate(
    geom = "segment",
    x = 1,
    xend = 1,
    y = -Inf,
    yend = Inf,
    colour = "white"
  ) +
  geom_point(
    aes(
      size = as.character(n_chapters),
      fill = saga
    ),
    shape = 21,
    colour = "white",
    position = position_jitter(width = 0.2, height = 0, seed = 123)
  ) +
  geom_text(
    data = ~ .x %>%
      filter(n_chapters == 2) %>%
      slice_tail(n = 1) %>%
      mutate(label = "2 chapters\nin a week!"),
    aes(
      label = label
    ),
    family = "Comic Sans MS",
    fontface = "italic",
    colour = "#035E7B",
    size = 2.25,
    hjust = 1.35
  ) +
  geom_text(
    data = ~ .x %>%
      slice_tail(n = 1) %>%
      mutate(label = str_glue("Latest:\nCh. {chapters}, {avg_pages} pages\n5 April 2021")),
    aes(
      label = label
    ),
    family = "Comic Sans MS",
    fontface = "italic",
    colour = "#035E7B",
    size = 2.25,
    hjust = 0,
    nudge_y = 0.6
  ) +
  geom_mark_circle(
    aes(
      filter = chapters == "1",
      label = "ROMANCE DAWN",
      description = "Debut on 19 July 1997, 53 pages"
    ),
    expand = 0,
    alpha = 0.8,
    label.fill = "#7c2834",
    label.family = "Comic Sans MS",
    label.colour = "gray90",
    label.fontsize = 8,
    con.colour = "gray15"
  ) +
  annotate(
    geom = "text",
    x = 8,
    y = 2021,
    label = "*and more years\nto come",
    hjust = 0,
    vjust = -3.5,
    family = "Comic Sans MS",
    colour = "gray40",
    size = 3
  ) +
  scale_x_continuous(
    breaks = c(1, 10, 20, 30, 40, 50),
    labels = function(x) {
      if_else(x == 1, paste("Week", x, " ⟶"), paste0("w", x))
    }
  ) +
  scale_y_continuous(
    labels = function(x) {
      if_else(x == 2020, paste("Year", x), paste0(x))
    },
    guide = guide_axis(check.overlap = TRUE)
  ) +
  scale_size_manual(values = c("1" = 3, "2" = 6), guide = FALSE) +
  scale_fill_pomological(
    name = NULL,
    guide = guide_legend(nrow = 3, override.aes = list(size = 4))
  ) +
  labs(
    title = "23 years, 8 months, 16 days!*",
    subtitle = "A dot marks weekly One Piece manga chapter release",
    caption = "Data source: One Piece Wiki\nVisualized by: Muhammad Aswan Syahputra"
  ) +
  coord_polar(theta = "x", clip = "off") +
  theme_minimal(base_family = "Comic Sans MS") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(hjust = 0.5, size = rel(2.5), colour = "#7c2834", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = rel(1.2), colour = "gray20", face = "italic"),
    plot.caption = element_text(hjust = 0.5, colour = "gray30"),
    legend.text = element_text(colour = "gray30", size = rel(1)),
    legend.position = "bottom",
    plot.background = element_rect(fill = "#FFF8E7", colour = NA),
    panel.background = element_rect(fill = "#FFF8E7", colour = NA),
    panel.spacing.y = unit(2.5, "points"),
    axis.title = element_blank(),
    axis.ticks.y = element_line(),
    plot.margin = margin(30, 25, 10, 25)
  )

# Save plot ---------------------------------------------------------------

ggsave(
  "outfile/03-historical.png",
  plot = p,
  width = 8,
  height = 9.4,
  dpi = 300,
  type = "cairo-png"
)
