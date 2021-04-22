# Load packages -----------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)

# List countries and their city capitals ----------------------------------

countrycapitals <- 
  read_json(
    path = "https://raw.githubusercontent.com/samayo/country-json/master/src/country-by-capital-city.json",
    simplifyVector = TRUE
  ) %>% 
  as_tibble()

# Define function to access prayers time ----------------------------------

#' Here we set param to gather prayers time during Ramadhan 1442 H (year 2021)

get_prayer_times <- function(country, city) {
  resp <- GET(
    str_glue(
      "http://api.aladhan.com/v1/hijriCalendarByCity?city={url_encode(city)}&country={url_encode(country)}&method=3&month=09&year=1442"
    )
  )
  
  if (status_code(resp) != "200") {
    stop("Cannot retrieve data")
    res <- NULL
  } else if (status_code(resp) == "200") {
    res_raw <-
      resp %>%
      content(as = "parsed", simplifyVector = TRUE) %>%
      pluck("data")
    res <-
      bind_cols(
        tibble(country = country,
               city = city),
        res_raw %>%
          pluck("meta") %>%
          select(latitude, longitude),
        res_raw %>%
          pluck("date") %>%
          jsonlite::flatten() %>%
          select(gregorian.date, hijri.date),
        res_raw %>%
          pluck("timings")
      ) %>%
      clean_names()
  }
  return(res)
}

safely_get_prayer_times <- safely(get_prayer_times)

# Gather prayers time for all countries -----------------------------------

prayers_time_raw <- 
  countrycapitals %>% 
  mutate(
    city = case_when(
      country == "Sri Lanka" ~ "Colombia", 
      country == "Belgium" ~ "Brussel", 
      country == "Finland" ~ "Helsinki", 
      country == "Luxembourg" ~ "Luxembourg", 
      country == "Myanmar" ~ "Rangoon", 
      TRUE ~ city
    )
  ) %>% 
  drop_na() %>% 
  group_by(
    rowid = row_number()
  ) %>% 
  group_map(
    ~ safely_get_prayer_times(country = .x$country, city = .x$city)
  )

# Clean up! ---------------------------------------------------------------

prayers_time <- 
  prayers_time_raw %>% 
  map_dfr("result") %>% 
  mutate(
    across(
      fajr:midnight,
      ~ paste(
        gregorian_date,
        str_extract(.x, '\\d{2}:\\d{2}')
      ) %>% 
        dmy_hm()
    ),
    across(
      ends_with("date"),
      ~ dmy(.x)
    )
  )

# Save data into the project ----------------------------------------------

prayer_times %>% 
  write_csv("data/prayers_time.csv")

