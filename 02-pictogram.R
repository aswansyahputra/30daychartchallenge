# Load packages -----------------------------------------------------------

library(httr)
library(tidyverse)
library(waffle)
library(ggtext)
library(ggpomological)

# Fetch dataset -----------------------------------------------------------

resp <-
  GET("https://data.jabarprov.go.id/api-coredata/distanhor/od_jumlah_produksi_umbi_berdasarkan_komoditi?limit=10000")

tubers <-
  resp %>%
  content(as = "parsed", simplifyVector = TRUE) %>%
  pluck("data") %>%
  count(
    year = tahun,
    commodity = recode(
      jenis_komoditi,
      "UBI KAYU" = "Cassava",
      "UBI JALAR" = "Sweet potato"
    ),
    wt = jumlah_produksi,
    name = "yield"
  )

# Create plot -------------------------------------------------------------

tubers_plot <-
  tubers %>%
  mutate(
    yield = round(yield / 1e5),
    img = if_else(
      commodity == "Cassava",
      "<img src='asset/cassava.png' width='22'/>",
      "<img src='asset/sweet_potato.png' width='22'/>"
    )
  ) %>%
  ggplot(aes(fill = commodity, values = yield, label = img)) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  stat_waffle(geom = "richtext", fill = NA, label.color = NA, flip = TRUE, n_rows = 5) +
  labs(
    title = "Cassava <img src='asset/cassava.png' width='20'/> and Sweet Potato <img src='asset/sweet_potato.png' width='20'/> Yields in West Java",
    subtitle = "One symbol equals to about 100 kilotonnes of commodity",
    caption = "Data source: Open Data Jawa Barat\nVisualized by: Muhammad Aswan Syahputra\nImage: freepik.com"
  ) +
  theme_classic(base_family = "Homemade Apple") +
  theme(
    plot.background = element_rect(fill = "#FFF8E7", colour = NA),
    panel.background = element_rect(fill = "#fffeea", colour = "#855A5C"),
    strip.background = element_rect(fill = "#855A5C", colour = NA),
    plot.title = element_markdown(colour = "#855A5C", hjust = 0.5, face = "bold", size = rel(1.7)),
    plot.subtitle = element_text(colour = "#855A5C", hjust = 0.5),
    plot.caption = element_text(colour = "#855A5C"),
    strip.text = element_text(colour = "white", size = rel(1.25)),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

# Apply image filter and save plot ----------------------------------------

tubers_plot %>%
  paint_pomological(
    width = 2400,
    height = 1500,
    res = 300,
    outfile = "outfile/02-pictogram.png"
  )
