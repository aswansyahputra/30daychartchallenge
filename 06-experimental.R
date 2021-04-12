# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidycode)
library(treemapify)
library(ggfittext)
library(ggtext)
library(scales)
library(paletteer)

# List path of rscripts withing project directories -----------------------

rpaths <- 
  list.files(
    path = "use/your/directory/path",
    pattern = "*\\.R$", 
    recursive = TRUE, 
    full.names = TRUE
  )

# Read rscripts as dataframe ----------------------------------------------

rcodes <- 
  map_dfr(rpaths, possibly(read_rfiles, otherwise = NULL))
    
# Classify function used in rscripts --------------------------------------

rcodes_class <-
  rcodes %>% 
  filter(str_detect(file, "utils", negate = TRUE)) %>% 
  unnest_calls(expr) %>% 
  inner_join(
    get_classifications(
      lexicon = "crowdsource", 
      include_duplicates = TRUE
    )
  ) %>% 
  anti_join(get_stopfuncs()) %>% 
  select(-args)

# Prepare data for visualization ------------------------------------------

to_plot <- 
  rcodes_class %>% 
  mutate(
    classification = if_else(
      func == "GET",
      "import",
      classification
    ),
    classification = recode(
      classification,
      "data cleaning" = "transformation"
    )
  ) %>% 
  count(classification) %>% 
  left_join(
    treemapify(
      .,
      area = "n",
      subgroup = "classification",
      xlim = c(0, 10),
      ylim = c(0, 10)
    )
  ) %>% 
  mutate(
    pct = n / sum(n),
    label = percent(pct, accuracy = 0.1),
    txtcolour = case_when(
      pct < 0.1 ~ "#2e8db0",
      TRUE ~ "#e5e5e3"
    )
  ) 

# Create plot -------------------------------------------------------------

p <- 
  to_plot %>% 
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  treemapify:::geom_rrect(
    aes(fill = pct),
    radius = unit(15, "pt"),
    colour = "#e5e5e3",
    size = 5,
    show.legend = FALSE
  ) +
  geom_fit_text(
    aes(label = label, colour = txtcolour),
    place = "bottomright",
    family = "Arial Narrow",
    padding.x = unit(4, "mm"),
    padding.y = unit(4, "mm"),
    grow = TRUE
  ) +
  geom_fit_text(
    aes(label = classification, colour = txtcolour, angle = if_else(classification %in% c("export", "communication"), 0, 90)),
    min.size = 10,
    place = "topright",
    family = "Arial Narrow",
    fontface = "bold",
    padding.x = unit(4, "mm"),
    padding.y = unit(4, "mm"),
    reflow = TRUE,
    show.legend = FALSE
  ) +
  labs(
    caption = "<b style='font-size:35pt;color:grey15'>What are these codes for?</b><br>Classification of my #rstats codes within 25 data analysis projects at work<br><br><i style='font-size:10pt;'><br>﹋﹋﹋﹋﹋﹋﹋﹋﹋﹋<br>Data and visualization by Muhammad Aswan Syahputra</i>"
  ) +
  scale_fill_paletteer_c("ggthemes::Blue-Teal") +
  scale_colour_identity() +
  theme_void(base_family = "Arial Narrow") +
  theme(
    plot.background = element_rect(fill = "#e5e5e3", colour = NA),
    panel.background = element_rect(fill = "#e5e5e3", colour = NA),
    plot.caption.position = "plot",
    plot.caption = element_markdown(hjust = 0.5, colour = "gray25", size = rel(1.2), lineheight = 0.8),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  coord_cartesian(clip = "off")

# Save plot ---------------------------------------------------------------

ggsave(
  "outfile/06-experimental.png",
  plot = p,
  width = 8,
  height = 8,
  dpi = 300,
  type = "cairo-png"
)



