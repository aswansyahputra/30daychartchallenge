# Load packages -----------------------------------------------------------

library(httr)
library(tidyverse)
library(ggraph)
library(tidyverse)
library(ggrepel)
library(ggtext)

# Fetch data from Open Data Jawa Barat ------------------------------------

resp <-
  GET(
    "https://data.jabarprov.go.id/api-coredata/bkd/od_jumlah_pns_daerah_otonom_berdasarkan_jenis_kelamin"
  )

opd <- 
  resp %>% 
  content(as = "parsed", simplifyVector = TRUE) %>% 
  pluck("data") %>% 
  as_tibble() %>% 
  mutate(
    across(
      where(is.character),
      ~ str_to_title(.x)
    )
  ) %>% 
  filter(tahun == 2017)

# Preprocess data for plotting --------------------------------------------

to_plot <-
  opd %>%
    rowwise() %>%
    transmute(
      gender = recode(
        jenis_kelamin,
        "Pria" = "Male", 
        "Wanita" = "Female"
      ),
      gender = list(rep.int(gender, times = jumlah))
    ) %>% 
    ungroup() %>% 
    unnest_longer(gender) %>%
    drop_na() %>% 
    mutate(
      n = 1,
      pack = as_tibble(pack_circles(n))
    ) %>% 
  unpack(pack) %>% 
  rename(x = V1, y = V2) %>% 
  filter(gender != "")

# Create plot -------------------------------------------------------------

p <-
  to_plot %>% 
  ggplot(aes(x, y, colour = gender)) +
  geom_point(show.legend = FALSE) +
  geom_text_repel(
    data = ~ .x %>% 
      summarise(
        x = 0.6 * min(x),
        y = 0.7 * max(y),
        label = "A dot represents\none person"
      ),
    aes(x, y, label = label),
    inherit.aes = FALSE,
    min.segment.length = 0,
    nudge_x = -25,
    nudge_y = 10,
    arrow = arrow(length = unit(0.01, "npc")),
    segment.curvature = -0.2,
    segment.colour = "gray40",
    family = "Roboto Condensed",
    fontface = "italic",
    colour = "gray30"
  ) +
  annotate(
    geom = "richtext",
    x = -100,
    y = -80,
    label = "<strong style='color:#8700f9;font-size:57.5px;'>Women</strong><br>in West Java<br>Government Office | **2017**",
    fill = NA,
    label.color = NA,
    family = "Roboto Condensed",
    colour = "gray30",
    size = 9,
    hjust = 0,
    vjust = 0
  ) +
  scale_colour_manual(
    values = c("Male" = "#00c4aa", "Female" = "#8700f9")
  ) +
  labs(
    caption = "Data source: Open Data Jawa Barat\nVisualized by: Muhammad Aswan Syahputra"
  ) +
  coord_equal(clip = "off") +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(fill = "#FFF8E7", colour = NA),
    panel.background = element_rect(fill = "#FFF8E7", colour = NA),
    plot.caption = element_text(colour = "gray40"),
    plot.margin = margin(20, 20, 20, 20)
  )

# Save plot ---------------------------------------------------------------

ggsave(
  "outfile/01-part_to_whole.png",
  plot = p,
  width = 8,
  height = 8,
  dpi = 300,
  type = "cairo-png"
)
