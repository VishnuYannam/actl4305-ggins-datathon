library("tidyverse")

cleaned <- read_csv('/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/actl4305-ggins-datathon/cleaned_destinations_data.csv')
hc_index <- read_csv('/Users/michaelwang/Downloads/country_metrics.csv')

cleaned <- cleaned %>%
  mutate(
    cleaned_dest_list_list = str_split(cleaned_dest_list, ",\\s*") %>%
      map(~ str_squish(tolower(.x)))
  )

dest_split_df <- cleaned %>%
  mutate(quote_id = row_number()) %>%
  unnest_longer(cleaned_dest_list_list, values_to = "indiv_destination", keep_empty = FALSE)

hc_index <- hc_index %>%
  select(Country, Metric1) %>%
  mutate(Country = tolower(Country)) %>%
  rename(country = Country,
         healthcare_index = Metric1)


hc_countries_joined <- left_join(
  dest_split_df,
  hc_index,
  by = join_by(indiv_destination == country)
)


africa_main_health_indices <- c(68.9, 67.4, 66.5, 63.2, 60.3, 58.7, 57.2, 57.1, 56.6, 48.1, 47.5, 45.8, 45.3, 44.9)
africa_avg <- mean(africa_main_health_indices)

asia_main_health_indices <- c(87.0, 82.8, 80.2, 68.7, 66.5, 71.4, 65.5, 59.4, 57.7, 52.7, 41.7, 60.7, 77.3, 71.8, 70.7, 66.8, 61.9, 61.2, 51.6)
asia_avg <- mean(asia_main_health_indices)

middle_east_main_health_indices <- c(73.5, 73.4, 71.3, 70.7, 65.5, 63.7, 63.3, 61.9, 60.0, 58.4, 56.5, 56.4, 49.1, 46.3, 35.2)
middle_east_avg <- mean(middle_east_main_health_indices)

europe_main_health_indices <- c(80.6, 78.5, 78.3, 77.4, 77.3, 77.2, 75.8, 75.8, 75.5, 75.1, 74.8, 74.7, 72.5, 72.0, 71.7, 70.8, 68.6, 67.9, 66.3, 64.9, 64.8, 62.9, 61.7, 58.5, 58.2, 58.2, 58.0, 56.8, 55.8, 55.3, 54.9, 54.1, 52.8, 52.3, 51.5, 49.3, 48.2, 43.9)
europe_avg <- mean(europe_main_health_indices)

north_america_main_health_indices <- c(68.6, 67.5)
north_america_avg <- mean(north_america_main_health_indices)

south_america_main_health_indices <- c(78.0, 68.9, 68.5, 67.9, 63.7, 59.2, 56.7, 39.6)
south_america_avg <- mean(south_america_main_health_indices)

americas_main_health_indices <- c(78.0, 72.7, 68.9, 68.6, 68.5, 67.9, 67.5, 67.2, 65.1, 63.7, 61.2, 59.2, 59.1, 58.4, 56.7, 54.3, 39.6)
americas_avg <- mean(americas_main_health_indices)

pacific_main_health_indices <- c(40.51, 61.11, 88.19, 62.27, 73.1, 68.1, 55.56)
pacific_avg <- mean(pacific_main_health_indices)

worldwide_avg <- mean(hc_index$healthcare_index)

antarctica_avg <- 50

caribbean_main_health_indices <- c(59.1, 58.4, 54.3, 79.17, 73.26, 49.90, 50.00, 50.00)
caribbean_avg <- mean(caribbean_main_health_indices)

hc_countries_joined <- hc_countries_joined %>%
  mutate(healthcare_index = if_else(indiv_destination == "africa", africa_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "asia", asia_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "europe", europe_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "vanuatu", 40.51, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "new caledonia", 61.11, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "north america", north_america_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "south america", south_america_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "the americas", americas_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "tajikistan", 65.33, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "turkmenistan", 49.83, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "hawaii", 72.84, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "cook islands", pacific_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "french polynesia", 88.19, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "phuket", 75.71, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "laos", 34.44, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "samoa", 55.56, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "american samoa", pacific_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "aruba", 79.17, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "gibraltar", 73.26, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "pacific", pacific_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "worldwide", worldwide_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "antarctica", antarctica_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "alaska", 61.47, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "greenland", 51.39, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "antigua and barbuda", 50.00, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "anguilla", 77.08, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "madagascar", 39.32, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "reunion", 71.30, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "tonga", 62.27, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "canary islands", 73.15, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "seychelles", 63.19, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "senegal", 	47.73, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "falkland islands", caribbean_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "madeira", 71.95, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "benin", 51.39, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "st. lucia", 49.90, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "st. kitts-nevis", 50.00, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "san marino", 83.33, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "malawi", 37.50, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "liechtenstein", 70.37, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "guadeloupe", 35.09, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "timor-leste", 93.06, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "nauru", pacific_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "mali", 18.52, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "liberia", 40.56, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "lesotho", 55.56, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "togo", 54.17, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "gambia", 66.67, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "dominica", 36.57, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "swaziland", 61.11, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "martinique", 75.00, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "kosovo", 68.78, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "bermuda", 68.85, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "jersey", 53.94, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "macao", 51.00, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "sierra leone", 31.11, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "south sudan", 23.61, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "virgin islands", 45.37, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "caribbean", caribbean_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "the middle east", middle_east_avg, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "fiji", 48.45, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "hong kong", 66.53, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "cambodia", 51.56, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "papua new guinea", 23.86, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "maldives", 45.94, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "mauritius", 63.55, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "tanzania", 43.64, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "montenegro", 48.50, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "zimbabwe", 45.27, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "bolivia", 46.23, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "nicaragua", 61.21, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "botswana", 61.12, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "uzbekistan", 65.16, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "belize", 47.37, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "el salvador", 51.87, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "bahamas", 39.52, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "kyrgyzstan", 57.48, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "uganda", 49.83, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "namibia", 65.07, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "cuba", 59.30, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "monaco", 75.00, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "armenia", 59.19, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "jamaica", 51.52, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "honduras", 37.01, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "andorra", 75.56, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "mongolia", 51.63, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "ghana", 56.98, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "afghanistan", 24.77, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "brunei", 72.97, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "myanmar", 48.39, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "zambia", 56.82, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "bahrain", 66.60, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "angola", 36.16, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "barbados", 71.88, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "ethiopia", 52.70, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "rwanda", 74.49, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "paraguay", 64.53, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "cameroon", 45.53, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "ivory coast", 42.06, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "mozambique", 40.74, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "guyana", 49.46, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "haiti", 32.72, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "moldova", 52.21, healthcare_index)) %>%
  mutate(healthcare_index = if_else(indiv_destination == "yemen", 24.03, healthcare_index))
  
na_health <- hc_countries_joined[is.na(hc_countries_joined$healthcare_index),]

na_health_count <- na_health %>%
  group_by(indiv_destination) %>%
  summarise(count = n())

df_write <- hc_countries_joined %>%
  select(quote_id, healthcare_index) %>%
  mutate(healthcare_index = as.character(healthcare_index)) %>%
  group_by(quote_id) %>%
  summarise(
    healthcare_index = paste(unique(healthcare_index), collapse = ", "),
    .groups = "drop"
  )

write.csv(df_write,"/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/healthcare_indices.csv", row.names = FALSE)

