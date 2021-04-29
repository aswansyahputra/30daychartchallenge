# Load packages -----------------------------------------------------------

library(datasauRus)
library(tidyverse)
library(scales)
library(ggtext)
library(gganimate)

# Calculate statistical summaries for labelling ---------------------------

datasaurus_label <-
  datasaurus_dozen %>%
  group_by(dataset) %>%
  summarise(
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x = sd(x),
    sd_y = sd(y),
    r = cor(x, y)
  ) %>%
  mutate(
    across(
      mean_x:r,
      list(
        p1 = ~ str_sub(number(.x, accuracy = 0.0000001), start = 1, end = 5),
        p2 = ~ str_sub(number(.x, accuracy = 0.0000001), start = 6, end = 10)
      )
    ),
    label = str_glue(
      "
    **X Mean:** {mean_x_p1}<span style='color:gray60;'>{mean_x_p2}</span><br>
    **Y Mean:** {mean_y_p1}<span style='color:gray60;'>{mean_y_p2}</span><br>
    **X S.D.:** {sd_x_p1}<span style='color:gray60;'>{sd_x_p2}</span><br>
    **Y S.D.:** {sd_y_p1}<span style='color:gray60;'>{sd_y_p2}</span><br>
    **Corr. :** {r_p1}<span style='color:gray60;'>{r_p2}</span>
      "
    )
  )

# Create plot -------------------------------------------------------------

anim <-
  datasaurus_dozen %>%
  ggplot(aes(x, y)) +
  annotate(
    geom = "segment",
    x = seq.int(0, 100, by = 25),
    xend = seq.int(0, 100, by = 25),
    y = 0,
    yend = 100,
    colour = "gray60",
    size = 0.15,
    linetype = "dotted"
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = 100,
    y = seq.int(0, 100, by = 25),
    yend = seq.int(0, 100, by = 25),
    colour = "gray60",
    size = 0.15,
    linetype = "dotted"
  ) +
  annotate(
    geom = "text",
    x = -50,
    y = 90,
    label = "TRUST\nONLY\nTHE\nSTATS?",
    family = "Fira Code",
    fontface = "bold",
    size = 14,
    colour = "#DAFF7D",
    hjust = 0,
    vjust = 1,
    lineheight = 0.8,
  ) +
  annotate(
    geom = "text",
    x = -50,
    y = 0,
    label = "Data: Datasaurus Dozen\nViz: Muhammad Aswan Syahputra",
    family = "Fira Code Light",
    size = 2.5,
    colour = "gray70",
    hjust = 0,
    vjust = 1,
    lineheight = 0.9,
  ) +
  geom_point(colour = "#7CFFCB") +
  geom_text(
    aes(x = 100, y = 0, label = str_replace(dataset, "_", " ")),
    family = "Fira Code Light",
    size = 5,
    hjust = 1,
    vjust = 0,
    colour = "#D5ECD4"
  ) +
  geom_textbox(
    data = datasaurus_label,
    aes(x = -50, y = 10, label = label),
    inherit.aes = FALSE,
    family = "Fira Code",
    colour = "gray15",
    hjust = 0,
    vjust = 0,
    halign = 0,
    valign = 0.5,
    lineheight = 0.8,
    size = 4,
    fill = "#D5ECD4",
    box.colour = NA
  ) +
  scale_x_continuous(breaks = seq.int(0, 100, by = 25), expand = c(0.05, 0.05)) +
  scale_y_continuous(breaks = seq.int(0, 100, by = 25), expand = c(0.05, 0.05), position = "right") +
  theme_minimal(base_family = "Fira Code") +
  theme(
    plot.background = element_rect(fill = "#034C3C", colour = NA),
    panel.background = element_rect(fill = "#034C3C", colour = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(family = "Fira Code Light", colour = "gray70"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_cartesian(clip = "off") +
  transition_states(dataset) +
  ease_aes("sine-in-out") +
  enter_appear() +
  exit_disappear()

# Save animation ----------------------------------------------------------

anim_save(
  "outfile/09-statistics.gif",
  animation = anim,
  duration = 15,
  fps = 30,
  width = 8,
  height = 5,
  units = "in",
  device = "png",
  type = "cairo-png",
  res = 150,
  renderer = gifski_renderer(loop = TRUE)
)
