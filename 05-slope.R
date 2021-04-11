# Load packages -----------------------------------------------------------

library(tidyverse)
library(scales)
library(ggtext)
library(ggrepel)
library(ggforce)

# Load data ---------------------------------------------------------------

onepiece_chapters <- read_csv("data/onepiece_chapters.csv")

# Preprocess data for plot ------------------------------------------------

to_plot <-
  onepiece_chapters %>%
  add_row(chapter = 1010, release_date = as.Date("2021-04-11")) %>%
  count(
    group = if_else(
      between(chapter, 1, 500),
      "Chapter\n1-500",
      "Chapter\n501-1009"
    ),
    ndays = dplyr::lead(release_date, 1) - release_date
  ) %>%
  filter(!is.na(ndays)) %>%
  complete(group, ndays, fill = list(n = 0)) %>%
  mutate(
    ndays_label = str_glue("{ndays} days"),
    ndays = as.numeric(ndays),
    .after = ndays
  )

# Create plot -------------------------------------------------------------

p <-
  to_plot %>%
  ggplot(aes(group, n, group = ndays_label)) +
  annotate(
    geom = "text",
    x = rep(1:2, each = 4),
    y = rep(c(0, 1, 10, 100), 2),
    label = rep("-----", 8),
    family = "Fira Code",
    colour = "gray80"
  ) +
  annotate(
    geom = "text",
    x = 2.1,
    y = c(0, 1, 10, 100),
    label = c(0, 1, 10, 100),
    family = "Fira Code",
    colour = "gray70"
  ) +
  geom_line(colour = "gray80") +
  geom_point(
    size = 7,
    shape = 21,
    fill = "#004E89",
    colour = "#e5e5e3"
  ) +
  annotate(
    geom = "text",
    x = 1.5,
    y = 150,
    label = "Need to be more patient",
    family = "Fira",
    fontface = "bold",
    colour = "gray15",
    hjust = 0.5,
    vjust = 0.5,
    size = 6
  ) +
  annotate(
    geom = "text",
    x = 1.5,
    y = 80,
    label = str_wrap(
      "We usually need to wait for one week for the next manga chapter of One Piece. However, it seems that in the future we need to hold our curiosity about the story a little longer",
      width = 35
    ),
    family = "Fira Code",
    colour = "gray30",
    hjust = 0.5,
    vjust = 0.5,
    size = 3.5,
    lineheight = 0.75
  ) +
  annotate(
    geom = "richtext",
    x = 1.5,
    y = -Inf,
    label = "<span style='color:#004E89;'>●</span> shows number of occurences",
    fill = NA,
    label.color = NA,
    family = "Fira Code",
    colour = "gray30",
    hjust = 0.5,
    vjust = 0.5,
    size = 2.75,
    lineheight = 0.75
  ) +
  geom_text_repel(
    data = ~ .x %>%
      pivot_wider(names_from = group, values_from = n) %>%
      arrange(ndays) %>%
      mutate(
        label = if_else(
          `Chapter\n1-500` == max(`Chapter\n1-500`),
          str_glue("{ndays_label} pause\nwas {`Chapter\n1-500`} times then\n{`Chapter\n501-1009`} times since Ch. 501"),
          str_glue("{ndays_label} ({`Chapter\n1-500`}->{`Chapter\n501-1009`})")
        )
      ) %>%
      group_by(`Chapter\n1-500`) %>%
      summarise(label = paste0(label, collapse = "\n")),
    aes(x = "Chapter\n1-500", y = `Chapter\n1-500`, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 0.5,
    nudge_x = -0.05,
    family = "Fira Code",
    colour = "gray30",
    size = 3,
    direction = "y",
    lineheight = 0.75,
    segment.colour = NA
  ) +
  geom_mark_circle(
    data = tibble(
      group = "Chapter\n501-1009",
      n = 1,
      label = "The longest wait...",
      description = "was between chapter 597 ('3D2Y') and 598 ('Two Years Later') that took 35 days. The pause marked the transition between 'Summit War Saga' and 'Fish-Man Island Saga'"
    ),
    aes(group, n, label = label, description = description),
    inherit.aes = FALSE,
    label.family = "Fira Code",
    label.fontface = c("bold", "italic"),
    label.colour = c("gray15", "gray30"),
    label.lineheight = 0.8,
    label.fontsize = c(8.5, 7.5),
    label.fill = "#F8F8F8",
    label.width = unit(55, "mm"),
    label.buffer = unit(20, "mm"),
    con.colour = "gray10"
  ) +
  scale_x_discrete(
    name = NULL,
    expand = c(0.52, 0.05),
    position = "bottom"
  ) +
  scale_y_continuous(
    name = NULL,
    breaks = 0,
    trans = pseudo_log_trans()
  ) +
  labs(caption = "Data source: One Piece Wiki\nVisualized by: Muhammad Aswan Syahputra") +
  theme_minimal(base_family = "Fira Code") +
  theme(
    plot.background = element_rect(fill = "#e5e5e3", colour = NA),
    panel.background = element_rect(fill = "#e5e5e3", colour = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0.5, size = rel(0.75)),
    panel.grid.major.x = element_line(colour = "gray80"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x.top = element_line(),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  coord_cartesian(clip = "off")

# Save plot ---------------------------------------------------------------

ggsave(
  "outfile/05-slope.png",
  plot = p,
  width = 8,
  height = 8,
  dpi = 300,
  type = "cairo-png"
)
