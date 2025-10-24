library(tidyverse)

df <- read_csv("/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/news_cs_hc_joined_data.csv")
coordinates <- read_csv('/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/country-coord.csv')

df <- df %>%
  mutate(
    dest_list = str_split(cleaned_dest_list, ",\\s*") %>%
      map(~ str_squish(tolower(.x)))
  )

dest_split_df <- df %>%
  unnest_longer(dest_list, values_to = "indiv_destination", keep_empty = FALSE)

coordinates <- coordinates %>%
  select(Country, `Latitude (average)`, `Longitude (average)`) %>%
  mutate(Country = tolower(Country)) %>%
  rename(country = Country, latitude = `Latitude (average)`, longitude = `Longitude (average)`)

coordinates <- coordinates %>%
  mutate(
    country = recode(
      country,
      "viet nam" = "vietnam",
      "korea, democratic people's republic of" = "south korea",
      "lao people's democratic republic" = "laos",
      "libyan arab jamahiriya" = "libya",
      "bolivia, plurinational state of" = "bolivia",
      "brunei darussalam" = "brunei",
      "congo, the democratic republic of the" = "congo",
      "côte d'ivoire" = "ivory coast",
      "falkland islands (malvinas)" = "falkland islands",
      "iran, islamic republic of" = "iran",
      "macedonia, the former yugoslav republic of" = "macedonia",
      "moldova, republic of" = "moldova",
      "palestinian territory, occupied" = "palestine",
      "saint kitts and nevis" = "st. kitts-nevis",
      "saint lucia" = "st. lucia",
      "taiwan, province of china" = "taiwan",
      "tanzania, united republic of" = "tanzania",
      "venezuela, bolivarian republic of" = "venezuela",
      "virgin islands, british" = "virgin islands",
      "macedonia" = "north macedonia",
    )
  )

coords_joined <- left_join(
  dest_split_df,
  coordinates,
  by = join_by(indiv_destination == country)
)

coords_joined <- coords_joined %>%
  mutate(latitude = if_else(indiv_destination == "africa", 8.79, latitude)) %>%
  mutate(latitude = if_else(indiv_destination == "asia", 34.05, latitude)) %>%
  mutate(latitude = if_else(indiv_destination == "caribbean", 21.47, latitude)) %>%
  mutate(latitude = if_else(indiv_destination == "europe", 54.53, latitude)) %>%
  mutate(latitude = if_else(indiv_destination == "hawaii", 19.90, latitude)) %>%
  mutate(latitude = if_else(indiv_destination == "kosovo", 42.60, latitude)) %>%
  mutate(latitude = if_else(indiv_destination == "north america", 54.53, latitude)) %>%
  mutate(latitude = if_else(indiv_destination == "pacific", 22.74, latitude)) %>%
  mutate(latitude = if_else(indiv_destination == "south america", 8.78, latitude)) %>%
  mutate(latitude = if_else(indiv_destination == "the americas", 54.53, latitude)) %>%
  mutate(latitude = if_else(indiv_destination == "the middle east", 22.30, latitude)) %>%
  mutate(latitude = if_else(indiv_destination == "worldwide", 0, latitude)) %>%
  mutate(longitude = if_else(indiv_destination == "africa", 34.51, longitude)) %>%
  mutate(longitude = if_else(indiv_destination == "asia", 100.62, longitude)) %>%
  mutate(longitude = if_else(indiv_destination == "caribbean", 78.66, longitude)) %>%
  mutate(longitude = if_else(indiv_destination == "europe", 15.26, longitude)) %>%
  mutate(longitude = if_else(indiv_destination == "hawaii", 155.67, longitude)) %>%
  mutate(longitude = if_else(indiv_destination == "kosovo", 20.90, longitude)) %>%
  mutate(longitude = if_else(indiv_destination == "north america", 105.26, longitude)) %>%
  mutate(longitude = if_else(indiv_destination == "pacific", 140.02, longitude)) %>%
  mutate(longitude = if_else(indiv_destination == "south america", 55.49, longitude)) %>%
  mutate(longitude = if_else(indiv_destination == "the americas", 105.56, longitude)) %>%
  mutate(longitude = if_else(indiv_destination == "the middle east", 42.55, longitude)) %>%
  mutate(longitude = if_else(indiv_destination == "worldwide", 0, longitude))

na_health <- coords_joined[is.na(coords_joined$latitude),]

na_health_count <- na_health %>%
  group_by(indiv_destination) %>%
  summarise(count = n())

df_write <- coords_joined


