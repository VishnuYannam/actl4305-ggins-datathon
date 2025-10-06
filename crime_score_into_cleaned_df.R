library("tidyverse")

cleaned <- read_csv('/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/actl4305-ggins-datathon/standardised_freely_quote_data_v2.csv')

cleaned <- cleaned %>%
  mutate(
    dest_list = str_split(destinations, ";\\s*") %>%
      map(~ str_squish(tolower(.x)))
  )

dest_split_df <- cleaned %>%
  mutate(quote_id = row_number()) %>%
  unnest_longer(dest_list, values_to = "indiv_destination", keep_empty = FALSE)

dest_split_df <- dest_split_df %>%
  mutate(indiv_destination = str_remove(indiv_destination, "\\s*\\(.*$"))

dest_split_df <- dest_split_df %>%
  mutate(indiv_destination = str_remove(indiv_destination, "^all of\\s+"))

dest_split_df <- dest_split_df %>%
  mutate(
    indiv_destination = str_squish(indiv_destination),
    indiv_destination = recode(
      indiv_destination,
      "korea" = "south korea",
      "usa" = "united states",
      "us" = "united states",
      "united states of america" = "united states",
      "america" = "united states",
      "uk" = "united kingdom",
      "great britain" = "united kingdom",
      "england" = "united kingdom",
      "britain" = "united kingdom",
      "gbr" = "united kingdom",
      "united kingdoms" = "united kingdom",
      "domestic cruise" = "australia",
      "holland" = "netherlands",
      "quebec" = "canada",
      "siem reap" = "cambodia",
      "tasmania" = "australia",
      "northern ireland" = "united kingdom",
      "republic of ireland" = "ireland",
      "bosnia" = "bosnia and herzegovina",
      "herzegovina" = "bosnia and herzegovina",
      "scotland" = "united kingdom",
      "lice springs" = "australia",
      "arizona" = "united states",
      "dominican rep." = "dominican republic",
      "denpasar" = "indonesia",
      "alabama" = "united states",
      "california" = "united states",
      "broome" = "australia",
      "bhutan" = "asia",
      "british columbia" = "canada",
      "burkina faso" = "africa",
      "cape verde" = "africa",
      "central african republic" = "africa",
      "chad" = "africa",
      "colorado" = "united states",
      "connecticut" = "united states",
      "dubrovnik" = "croatia",
      "eritrea" = "africa",
      "espana" = "spain",
      "noumea" = "new caledonia",
      "macau" = "china",
      "macedonia" = "north macedonia",
      "airlie beach" = "australia",
      "tahiti" = "french polynesia",
      "port vila" = "vanuatu",
      "antarctica-sightseeing flight" = "antarctica",
      "queensland" = "australia",
      "western samoa" = "samoa",
      "south west pacific cruise" = "pacific",
      "the pacific" = "pacific",
      "norfolk island" = "pacific",
      "mystery island" = "pacific",
      "antartica" = "antarctica",
      "virginia" = "united states",
      "tennessee" = "united states",
      "ubud" = "indonesia",
      "faroe islands" = "europe",
      "solomon islands" = "pacific",
      "cayman islands" = "caribbean",
      "koh samui" = "thailand",
      "lombok" = "indonesia",
      "queenstown" = "new zealand",
      "kangaroo island" = "australia",
      "lifou" = "pacific",
      "whitsundays" = "pacific",
      "western australia" = "australia",
      "hamilton island" = "australia",
      "lapland" = "finland",
      "rarotonga" = "cook islands",
      "lord howe island" = "australia",
      "palau" = "pacific",
      "south australia" = "australia",
      "sunshine coast" = "australia",
      "alice springs" = "australia",
      "glasglow" = "united kingdom",
      "indiana" = "united states",
      "sapporo" = "japan",
      "salzburg" = "austria",
      "port villa" = "vanuatu",
      "nuremburg" = "germany",
      "nova scotia" = "canada",
      "minnesota" = "united states",
      "michigan" = "united states",
      "massachusetts" = "united states",
      "marrakech" = "morocco",
      "malvinas" = "falkland islands",
      "loyalty islands" = "new caledonia",
      "kosovo" = "kosovo (disputed territory)",
      "isle of pines" = "pacific",
      "guernsey" = "united kingdom",
      "great barrier reef" = "australia",
      "fraser island" = "australia",
      "florida" = "united states",
      "estados unidos" = "united states",
      "venice" = "italy",
      "utah" = "united states",
      "oregon" = "united states",
      "noosa" = "australia",
      "new jersey" = "united states",
      "nebraska" = "united states",
      "ibiza" = "spain",
      "alberta" = "canada",
      "uluru" = "australia",
      "east timor" = "timor-leste",
      "texas" = "united states",
      "tenerife" = "spain",
      "netherlands antilles" = "caribbean",
      "kuta" = "pacific",
      "kl" = "malaysia",
      "northern territory" = "australia",
      "north carolina" = "united states",
      "new mexico" = "united states",
      "netherlands antilles" = "caribbean",
      "nadi" = "fiji",
      "port douglas" = "australia",
      "suva" = "fiji",
      "wales" = "united kingdom",
      "tibet" = "china",
      "vatican city" = "italy",
      .default = indiv_destination,
      .missing = indiv_destination
    )
  )

# exploration purposes not relevant to cleaning
destinations <- dest_split_df %>%
  select(indiv_destination) %>%
  unique()

crime_countries <- read_csv('/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/actl4305-ggins-datathon/crime-rate-by-country-2025.csv')

crime_countries <- crime_countries %>%
  mutate(country = tolower(country)) %>%
  rename(crime_index = NumbeoCrimeIndex_2024,
         safety_index = NumbeoSafetyIndex_2024)

# semi-important caveat, this is 2022 data
kaggle_world_crime_index <- read_csv('/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/actl4305-ggins-datathon/World Crime Index .csv')

crime_cities <- kaggle_world_crime_index %>%
  mutate(
    city    = tolower(str_trim(str_extract(City, "^[^,]+"))),
    country = tolower(str_trim(str_extract(City, "[^,]+$")))
  ) %>%
  select(-City) %>%
  rename(crime_index_city = `Crime Index`,
         safety_index_city = `Safety Index`)

crime_countries_list_1 <- crime_countries %>%
  pull(country) %>%
  unique()

crime_countries_list_2 <- crime_cities %>%
  pull(country) %>%
  unique()

# exploration purpose only code
# setdiff(crime_countries_list_1, crime_countries_list_2)
# setdiff(crime_countries_list_2, crime_countries_list_1)

# differences are only hong kong and bosnia and herz. Technically hong kong a city so will keep it there
# just put bosnia into country dataset

avgs <- crime_cities %>%
  filter(country == "bosnia and herzegovina") %>%
  summarise(avg_crime_index = mean(crime_index_city, na.rm = TRUE),
            avg_safety = mean(safety_index_city, na.rm = TRUE))

# average crime 40, safety 60, impute into crime_countries df

new_row <- tibble(
  flagCode = "BA",
  country = "bosnia and herzegovina",
  crime_index = 40.0,
  safety_index = 60.0,
)

crime_countries <- bind_rows(
  slice(crime_countries, 1:92),
  new_row,
  slice(crime_countries, 93:n())
)

crime_countries_joined <- left_join(
  dest_split_df,
  select(crime_countries, country, crime_index, safety_index),
  by = join_by(indiv_destination == country)
)

crime_cities_cleaned <- crime_cities[crime_cities$city != "singapore" & 
                                       !(crime_cities$city == "london" & crime_cities$country == "canada"),]

tmp <- left_join(
  crime_countries_joined,
  select(crime_cities_cleaned, city, crime_index_city, safety_index_city),
  by = join_by(indiv_destination == city)
)

tmp$crime_index  <- coalesce(tmp$crime_index,  tmp$crime_index_city)
tmp$safety_index <- coalesce(tmp$safety_index, tmp$safety_index_city)

crime_joined <- select(tmp, -crime_index_city, -safety_index_city)

# taken from numbero website, couple of main african countries.
africa_main_crime_indices <- c(74.6, 66.3, 66.2, 66.1, 65.5, 63.6, 63.2, 61.2, 61.0, 56.4,
                               55.8, 55.7, 54.1, 53.4, 52.9, 50.9, 50.4, 49.4, 47.5, 46.9,
                               45.8, 45.6, 45.6, 45.0, 26.4)

africa_avg <- mean(africa_main_crime_indices)

# central, east, south, and south east asia
asia_main_crime_indices <- c(51.7, 26.9, 23.5, 22.7, 21.5, 18.2, 17.0, 61.5, 54.1, 44.2, 42.4, 42.3, 36.1, 51.6,
                             50.2, 48.6, 46.1, 43.4, 40.4, 36.8, 29.4, 22.6, 52.0, 45.6, 26.6)

asia_avg <- mean(asia_main_crime_indices)

europe_main_crime_indices <- c(55.6, 49.5, 49.1, 48.6, 48.4, 48.1, 47.2, 47.0, 46.4, 44.7,
                               44.2, 43.0, 41.4, 41.2, 41.1, 39.6, 38.4, 37.2, 37.2, 36.6,
                               35.9, 35.0, 34.0, 33.7, 33.0, 32.8, 32.6, 32.4, 31.0, 28.7,
                               28.3, 26.7, 26.6, 26.5, 26.0, 25.8, 25.8, 25.4, 24.7, 24.4,
                               23.5, 20.9, 15.2)

europe_avg <- mean(europe_main_crime_indices)

north_america_main_crime_indices <- c(49.2, 45.8)
north_america_avg <- mean(north_america_main_crime_indices)

south_america_main_crime_indices <- c(80.5, 66.7, 66.4, 64.6, 64.2, 63.3, 62.6, 61.0, 60.5, 60.1, 52.3)
south_america_avg <- mean(south_america_main_crime_indices)

americas_main_crime_indices <- c(81.0, 80.5, 71.9, 71.0, 67.4, 66.7, 66.4, 64.6, 64.2, 63.3,
                                 62.6, 61.3, 61.0, 60.5, 60.4, 60.1, 59.2, 57.7, 56.9, 56.7,
                                 53.7, 53.2, 52.3, 52.1, 50.8, 49.2, 45.8, 44.9, 42.7, 35.4,
                                 30.6)
americas_avg <- mean(americas_main_crime_indices)

# png, fiji, french polynesia, cook islands, samoa, vanuatu, new caldonia, tonga, solomon islands, nauru, guam, hawaii
pacific_main_crime_indices <- c(80.7, 56.9, 20.96, 30.92, 42.06, 41.65, 64.17, 60.98, 63.87, 47.16, 63.66, 47.74)
pacific_avg <- mean(pacific_main_crime_indices)

worldwide_avg <- mean(crime_countries$crime_index)

# low estimate given news and general crime rate in antartica
antarctica_avg <- 10

# aruba, cuba, gibraltar, antigua and barbuda, dominica, martinique, anguilla, st. lucia,
# st. kitts-nevis, san marino, virgin islands, haiti, trinidad and tobago, jamaica, puerto rico, dominican republic
caribbean_main_crime_indices <- c(30.81, 33.4, 20.23, 51.10, 53.58, 62.86, 24.93, 60.83, 40.02, 22.35,
                                  59.17, 77.9, 70.8, 68.1, 61.7, 60.5)
caribbean_avg <- mean(caribbean_main_crime_indices)

# syria, UAE, qatar, oman, saudi, israel, jordan, turkey, iraq, egypt, iran
middle_east_main_crime_indices <- c(69.1, 15.6, 16.0, 19.0, 26.9, 32.4, 40.4, 40.9, 44.7, 47.3, 49.5)
middle_east_avg <- mean(middle_east_main_crime_indices)

# country specific --> https://www.numbeo.com/crime/country_result.jsp?country=New+Caledonia (new caledonia is example)
# continent based --> https://www.numbeo.com/crime/rankings_by_country.jsp?title=2025-mid&region=150 (europe is example)

# honolulu crime index is used as a proxy for hawaii
# anchorage is proxy for alaska
crime_joined <- crime_joined %>%
  mutate(crime_index = if_else(indiv_destination == "africa", africa_avg, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "asia", asia_avg, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "europe", europe_avg, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "vanuatu", 41.65, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "new caledonia", 64.17, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "north america", north_america_avg, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "south america", south_america_avg, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "the americas", americas_avg, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "vanuatu", 36.28, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "tajikistan", 51.06, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "turkmenistan", 47.32, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "hawaii", 47.74, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "cook islands", 30.92, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "french polynesia", 20.96, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "phuket", 39.51, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "laos", 36.28, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "samoa", 42.06, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "american samoa", 45.59, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "aruba", 30.81, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "gibraltar", 20.23, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "pacific", pacific_avg, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "worldwide", worldwide_avg, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "antarctica", antarctica_avg, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "alaska", 63.42, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "greenland", 40.42, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "antigua and barbuda", 51.10, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "anguilla", 24.93, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "madagascar", 62.25, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "reunion", 46.07, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "tonga", 60.98, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "canary islands", 46.67, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "seychelles", 37.70, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "senegal", 	50.12, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "falkland islands", 44.36, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "madeira", 17.83, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "benin", 42.00, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "st. lucia", 60.83, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "st. kitts-nevis", 40.02, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "san marino", 22.35, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "malawi", 56.11, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "liechtenstein", 6.41, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "guadeloupe", 35.09, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "timor-leste", 45.74, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "nauru", 47.16, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "mali", 47.08, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "liberia", 78.28, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "lesotho", 65.92, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "togo", 45.16, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "gambia", 60.71, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "dominica", 53.58, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "swaziland", 43.55, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "martinique", 62.86, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "kosovo (disputed territory)", 41.59, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "togo", 45.16, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "bermuda", 36.45, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "jersey", 27.29, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "macao", 18.21, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "sierra leone", 60.44, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "south sudan", 80.15, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "virgin islands", 59.17, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "caribbean", caribbean_avg, crime_index)) %>%
  mutate(crime_index = if_else(indiv_destination == "the middle east", middle_east_avg, crime_index))

na_crime <- crime_joined[is.na(crime_joined$crime_index),]

na_crime_count <- na_crime %>%
  group_by(indiv_destination) %>%
  summarise(count = n())

crime_joined <- crime_joined %>%
  mutate(
    across(where(is.logical), ~ as.numeric(.x))
  )

df_write <- crime_joined %>%
  select(quote_id, crime_index) %>%
  mutate(crime_index = as.character(crime_index)) %>%
  group_by(quote_id) %>%
  summarise(
    crime_index = paste(unique(na.omit(crime_index)), collapse = ", "),
    .groups = "drop"
  )

write.csv(df_write,"/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/cleaned_crime_indices.csv", row.names = FALSE)

# 
# smart_traveller_data <- read_csv('/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/smartraveller_destinations.csv')
# 
# smart_traveller_data <- smart_traveller_data[-nrow(smart_traveller_data),]
# 
# # higher level means more danger/less positive advice to travel there
# smart_traveller_data <- smart_traveller_data %>%
#   select(country, region, overall_advice) %>%
#   mutate(
#     level = as.numeric(factor(
#       overall_advice,
#       levels = c("Exercise normal safety precautions", "Exercise a high degree of caution",
#                  "Reconsider your need to travel", "Do not travel"),
#       ordered = TRUE
#     ))) %>%
#   mutate(country = if_else(country == "Türkiye", "Turkey", country))
# 
# smart_traveller_countries <- smart_traveller_data %>%
#   select(country, level) %>%
#   mutate(country = tolower(country))
# 
# smart_traveller_regions <- smart_traveller_data %>%
#   select(region, level) %>%
#   mutate(region = tolower(region)) %>%
#   group_by(region) %>%
#   summarise(level = median(level, na.rm = TRUE)) %>%
#   ungroup()
# 
# # smart traveller = ST
# 
# crime_st_country_data <- left_join(
#   crime_joined,
#   smart_traveller_countries,
#   by = join_by(indiv_destination == country)
# )
#   
# table(is.na(crime_st_country_data$level))
# 
# crime_st_country_and_regions_data <- left_join(
#   crime_joined,
#   smart_traveller_regions,
#   by = join_by(indiv_destination == region)
# )
# 
# table(is.na(crime_st_country_and_regions_data$level))
# 
