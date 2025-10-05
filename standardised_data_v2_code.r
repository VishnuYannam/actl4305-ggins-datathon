library("tidyverse")

original <- read_csv('/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/Freely_quote_data_clean.csv')

original <- original[-nrow(original),]

df <- original %>%
  mutate(
    trip_start_date = dmy(trip_start_date),
    trip_end_date = dmy(trip_end_date)
  )

df[df$trip_start_date > df$trip_end_date, ]

# quote date + time format, trip length/lead length, ages of travelers, traveler groups, 
df <- df %>%
  mutate(
    quote_create_time = ydm_hms(quote_create_time),
    quote_date = ydm(format(quote_create_time, "%Y-%d-%m")),
    quote_time = hms(format(quote_create_time, "%H:%M:%S")),
    quote_hour = str_extract(quote_time, "^[0-9]*") %>% as.integer(),
  ) %>%
  mutate(
    trip_length = trip_end_date - trip_start_date,
    lead_length = trip_start_date - quote_date
  ) %>%
  rowwise %>%
  mutate(
    ages = list(as.numeric(str_split(traveller_ages, ";")[[1]])),
    num_travellers = length(ages),
    max_age = if (num_travellers) max(ages) else NA_real_,
    min_age = if (num_travellers) min(ages) else NA_real_,
    age_range = if (num_travellers) max_age - min_age else NA_real_,
    mean_age = if (num_travellers) mean(ages) else NA_real_,
    median_age = if (num_travellers) median(ages) else NA_real_,
    has_child_U12 = any(ages <= 12),
    has_teen_O13 = any(ages >= 13 & ages <= 17),
    has_adult_O18 = any(ages >= 18 & ages <= 64),
    has_senior_O65 = any(ages >= 65),
    generations = sum(c(has_child_U12, has_teen_O13, has_adult_O18, has_senior_O65)),
    is_family = num_travellers >= 3 & !is.na(age_range) & age_range > 20,
    
    group_type = case_when(
      num_travellers == 1 & max_age < 30 ~ "single_young_U30",
      num_travellers == 1 & between(max_age,30,59) ~ "single_middle_U60",
      num_travellers == 1 & max_age >= 60 ~ "single_old_O60",
      
      num_travellers == 2 & max_age < 30 ~ "young_couple_U30",
      num_travellers == 2 & between(max_age,30,59) ~ "middle_couple_U60",
      num_travellers == 2 & max_age >= 60 ~ "old_couple_O60",
      
      !is_family & num_travellers >= 3 & median_age < 30 ~ "young_peer_group_U30",
      !is_family & num_travellers >= 3 & between(median_age,30,59) ~ "middle_peer_group_U60",
      !is_family & num_travellers >= 3 & median_age >= 60 ~ "older_peer_group_O60",
      
      is_family & has_senior_O65 & has_child_U12 ~ "grandparents_plus_kids",
      is_family & !has_child_U12 & !has_teen_O13 & max_age >= 60 ~ "older_parents_family",
      is_family & max_age < 60 & has_child_U12 ~ "parents_plus_kids",
      is_family & !has_child_U12 & has_teen_O13 ~ "parents_plus_teens",
      is_family ~ "mixed_generation_family",
      TRUE                                               ~ "Other"
    )
  ) %>%
  ungroup()

# initialise boost number column
df$boost_num <- NA_integer_

# loop to count boost number
for (i in 1:nrow(df)) {
  boost_num <- 0
  for (j in 1:8) {
    name <- paste0("boost_", j, "_name")
    if (!is.na(df[[name]][i])) {
      boost_num <- j
    }
  }
  df$boost_num[i] <- boost_num
}

# str squish to remove leading/trailing whitespaces, didn't work without it.
# map applies the function to all the values of the list provided by str_split
df <- df %>%
  mutate(
    dest_list = str_split(destinations, ";\\s*" %>%
                   map(~ str_squish(tolower(.x))))
  )

# named list/dictionary
region_map <- list(
  "Africa" = c(
    "All of Africa","South Africa","Egypt","Kenya","Morocco","Namibia","Tanzania",
    "Botswana","Zimbabwe","Zambia","Uganda","Rwanda","Ghana","Ethiopia","Senegal",
    "Madagascar","Mauritius","Seychelles","Cape Verde","Cabo Verde","Reunion",
    "Algeria","Tunisia","Angola","Cameroon","Mozambique","Lesotho","Swaziland",
    "Liberia","Benin","Burkina Faso","Chad","Ivory Coast","Cote d'Ivoire",
    "Johannesburg","Cape Town","Nairobi","Cairo",
    "Mali","Sierra Leone","Eritrea","Malawi","Nigeria","South Sudan",
    "Gambia","Togo","Ethiopia","Seychelles","Mauritius","Madagascar",
    "Marrakech","Trinidad and Tobago","Central African Republic"
  ),
  
  "Europe" = c(
    "All of Europe","France","Paris","Lyon","Nice","United Kingdom","England","Scotland","Wales",
    "Republic of Ireland","Ireland","Dublin","Germany","Berlin","Munich","Frankfurt","Cologne",
    "Netherlands","Holland","Amsterdam","Belgium","Brussels","Luxembourg","Switzerland","Zurich","Geneva",
    "Austria","Vienna","Salzburg","Czech Republic","Czechia","Prague","Spain","Madrid","Barcelona","Ibiza","Tenerife",
    "Portugal","Lisbon","Italy","Rome","Venice","Milan","Florence","Greece","Athens",
    "Denmark","Copenhagen","Finland","Helsinki","Sweden","Norway","Iceland","Slovenia","Slovakia","Poland",
    "Hungary","Montenegro","Albania","Macedonia","Kosovo","Serbia","Bosnia","Andorra","Monaco","San Marino",
    "Liechtenstein","Gibraltar","Malta","Estonia","Latvia","Lithuania","Strasbourg","Liverpool","Birmingham","Cardiff",
    "Dubrovnik","London","Edinburgh","Budapest","Stockholm","Lapland","Manchester","Italy (Italia)",
    "Bosnia and Herzegovina","Herzegovina","Croatia","Romania","Moldova","Bulgaria",
    "Georgia","Armenia","Azerbaijan","Northern Ireland","All of UK","All of UK (United Kingdom)",
    "All of UK (Great Britain)","All of UK (GBR)","All of UK (Isle of Man)","Isle of Man",
    "Guernsey","Jersey","Vatican City","Faroe Islands","Greenland",    # (Greenland often classed with NA; keep here if you prefer move to NA)
    "Madeira","Espana","Nuremburg","Nuremberg","Belgrade","Glasglow","Glasgow",
    "Netherlands (The Netherlands)","Vienna","Nice"
  ),
  
  "Oceania" = c(
    "Australia","Sydney","Melbourne","Perth","Adelaide","Brisbane","Hobart","Canberra","Darwin",
    "Queensland","Western Australia","Northern Territory","Tasmania","Gold Coast","Sunshine Coast",
    "Cairns","Port Douglas","Broome","Fraser Island","Great Barrier Reef","Lord Howe Island","Kangaroo Island","Whitsundays",
    "New Zealand","New Zealand (NZ)","Auckland","Queenstown","Christchurch",
    "Fiji","Suva","Nadi","Vanuatu","Port Vila","Mystery Island","Port Villa",
    "New Caledonia","Noumea","Lifou","Isle of Pines","Loyalty Islands",
    "Cook Islands","Rarotonga","Samoa","Western Samoa","American Samoa",
    "Papua New Guinea","Papua New Guinea (PNG)","Norfolk Island","French Polynesia","Tahiti","Alice Springs",
    "Palau","Tonga","Nauru","Solomon Islands","Hamilton Island","Airlie Beach","Noosa","Canary Islands",
    "Uluru","South Australia","South West Pacific Cruise","Australia (Domestic Cruise)","Domestic Cruise"
  ),
  
  "North_America" = c(
    "All of North America",
    "United States","USA","United States of America","America","Estados Unidos",
    "New York","California","Los Angeles","Chicago","Miami","Florida","Texas",
    "Arizona","Utah","Oregon","Alaska","Alabama","Nebraska","Massachusetts",
    "North Carolina","Virginia","New Jersey","Connecticut","Colorado","Indiana",
    "Minnesota","Tennessee","Michigan","New Mexico",
    "Hawaii","Honolulu","Las Vegas","San Francisco",
    "Canada","Vancouver","Toronto","Calgary","Ottawa","Quebec","Nova Scotia",
    "Alberta","British Columbia",
    "Mexico","Mexico City","Cancun"
  ),
  
  "Central_America" = c(
    "Puerto Rico","Bahamas","Barbados","Dominican Rep.","Cayman Islands","Jamaica",
    "Bermuda","Guadeloupe","Martinique","Aruba","Anguilla","Antigua and Barbuda",
    "St. Lucia","St. Kitts-Nevis","Virgin Islands","Cuba",
    "Panama","Panama City","Costa Rica","Nicaragua","Honduras","Guatemala","Belize",
    "El Salvador","Salvador","Netherlands Antilles","Dominica","Haiti"
  ),
  
  "South_America" = c(
    "All of South America","Brazil","Rio de Janeiro","Argentina","Buenos Aires","Chile","Santiago",
    "Peru","Lima","Ecuador","Colombia","Uruguay","Paraguay","Bolivia","Venezuela",
    "Brasilia","Falkland Islands","Malvinas","All of South America (Patagonia)","Guyana"
  ),
  
  "Middle_East" = c(
    "All of the Middle East","United Arab Emirates","United Arab Emirates (UAE)","United Arab Emirates (U.A.E.)","Dubai","Abu Dhabi",
    "Qatar","Doha","Saudi Arabia","Oman","Jordan","Israel","Lebanon","Bahrain","Kuwait","Turkey","Istanbul","Cyprus","Yemen","Afghanistan"
  ),
  
  "Central_Asia" = c(
    "Central Asia","Kazakhstan","Uzbekistan","Turkmenistan","Kyrgyzstan","Tajikistan","Mongolia"
  ),
  
  "South_East_Asia" = c(
    "South East Asia","Thailand","Bangkok","Phuket","Chiang Mai","Koh Samui",
    "Malaysia","Kuala Lumpur","KL","Penang","Singapore",
    "Vietnam","Ho Chi Minh City","Hanoi",
    "Indonesia","Bali","Jakarta","Denpasar","Kuta","Lombok","Ubud",
    "Philippines","Cebu","Manila","Cambodia","Siem Reap","Laos","Myanmar (Burma)","Brunei","East Timor","Timor-Leste","Pattaya"
  ),
  
  "South_Asia" = c(
    "India","Pakistan","Sri Lanka","Sri Lanka (SriLanka)","Nepal","Bhutan","Bangladesh","Maldives","Delhi","Mumbai","Kathmandu","Dhaka"
  ),
  
  "East_Asia" = c(
    "China","Beijing","Shanghai","Shenzhen","Hong Kong","Hong Kong (HK)","Hong Kong (Hongkong)","Macau","Macao",
    "Taiwan","Taipei","Japan","Tokyo","Osaka","Sapporo",
    "South Korea","Korea (south)","Korea (south) (South Korea)","Korea (south) (Republic of Korea)","Seoul","Tibet"
  ),
  
  "Multi_Region" = c(
    "Worldwide","All of the Pacific","All of the Pacific (Pacific Islands)","All of the Americas","All of the Americas (Central America)", "All of Asia",
    "All of Europe (Scandinavia)","All of South America (Patagonia)","All of Asia (exclude Nepal)",
    "Antarctica (Cruising)","Antarctica-Sightseeing Flight"
  ),
  
  "Antarctica" = c("Antarctica","Antarctica (Cruising)","Antarctica-Sightseeing Flight")
)

# lapply --> always return a list, apply function to every element of list. sapply --> wrapper around lapply to try and simplify
# things if, e.g. every element is a number, then return a vector not a list.
region_map_lower <- lapply(region_map, function(x) tolower(str_squish(x)))

domestic_cruise_regex <- "domestic\\s*cruise"
cruise_regex <- "cruise"

# grepl is logical grep, grep = command originating from shell used to search within something
# (here it is a list) for a certain regex match
df <- df %>%
  mutate(
    Domestic_Cruise = as.integer(
      # apply function onto every element of dest_list and creates a new list, of booleans since we are running the any() function
      # converted to integer for one hot encoding
      sapply(dest_list, function(x)
        any(grepl(domestic_cruise_regex, x, ignore.case = TRUE))
      )
    ),
    International_Cruise = as.integer(
      sapply(dest_list, function(x) {
        has_cruise  <- grepl(cruise_regex, x, ignore.case = TRUE)
        is_domestic <- grepl(domestic_cruise_regex, x, ignore.case = TRUE)
        any(has_cruise & !is_domestic)
      })
    )
  )

# loop through all the names of the regon map, i.e. the continents. For each name, extract the list that is part of that continent
# then for each destination on the dest_list column of the dataframe for each row, check if it is in the list. if it is in, any will
# return true. as.integer will change to 1, and 1 will be assigned to the new continent column that has just been created in df[[name]]
for (name in names(region_map_lower)) {
  list <- region_map_lower[[name]]
  df[[name]] <- sapply(df$dest_list, function(dest) as.integer(any(dest %in% list)))
}

# rearranging the continents/regions better.
flag_cols <- c("Africa","Europe","Oceania","North America", "Central America","South America",
               "Middle East","Central Asia","South East Asia","South Asia","East Asia",
               "Domestic Cruise","International Cruise","Multi Region","Antarctica")

# rearrange column order
df <- df %>%
  relocate(any_of(flag_cols), .after = dest_list)

# boosts into one hot encoded variables
df <- df  %>% 
  mutate(
    snowsports = as.integer(if_any(starts_with("boost_"), ~ str_detect(.x, "Snow Sports"))),
    adventure_activities = as.integer(if_any(starts_with("boost_"), ~ str_detect(.x, "Adventure Activities"))),
    cruise_cover = as.integer(if_any(starts_with("boost_"), ~ str_detect(.x, "Cruise Cover"))),
    medical_conditions = as.integer(if_any(starts_with("boost_"), ~ str_detect(.x, "Existing Medical Condition(s)"))),
    gadget_cover = as.integer(if_any(starts_with("boost_"), ~str_detect(.x, "Gadget Cover"))),
    motorcycle_cover = as.integer(if_any(starts_with("boost_"), ~str_detect(.x, "Motorcycle Cover"))),
    rental_vehicle_excess = as.integer(if_any(starts_with("boost_"), ~str_detect(.x, "Rental Vehicle Insurance Excess"))),
    specified_items = as.integer(if_any(starts_with("boost_"), ~str_detect(.x, "Specified Items")))
  ) %>% 
  mutate(
    snowsports = if_else(is.na(snowsports), 0, 1),
    adventure_activities = if_else(is.na(adventure_activities), 0, 1),
    cruise_cover = if_else(is.na(cruise_cover), 0, 1),
    medical_conditions = if_else(is.na(medical_conditions), 0, 1),
    gadget_cover = if_else(is.na(gadget_cover), 0, 1),
    motorcycle_cover = if_else(is.na(motorcycle_cover), 0, 1),
    rental_vehicle_excess = if_else(is.na(rental_vehicle_excess), 0, 1),
    specified_items = if_else(is.na(specified_items), 0, 1)
  ) %>%
  mutate(
    extra_cancellation = if_else(is.na(extra_cancellation), 0, extra_cancellation)
  )

# since we need to compute a summary within each row rather than rely on vectorised whole column operations, need to use rowwise
# in essence groups up data per row, so each row gets its own operation rather than a vectorised whole column operation
df <- df %>%
  rowwise() %>%
  mutate(
    num_adults = sum(ages >= 18, na.rm = TRUE)
  ) %>%
  ungroup()

# remove boost name columns since we one hot encoded it above
df <- df %>%
  select(-matches("^boost_[1-8]_name$"))

# use boost start and end dates to calculate length
for (i in 1:8) {
  start_col <- paste0("boost_", i, "_start_date")
  end_col   <- paste0("boost_", i, "_end_date")
  len_col   <- paste0("boost_", i, "_length")
  
  df[[start_col]] <- as.Date(df[[start_col]], format = "%d/%m/%Y")
  df[[end_col]]   <- as.Date(df[[end_col]], format = "%d/%m/%Y")
  
  df[[len_col]] <- as.numeric(df[[end_col]] - df[[start_col]])
}

# check to see if any lengths are negative - they shouldn't.
negative_length_check <- df %>%
  filter(if_any(ends_with("_length"), ~ .x < 0))


cols <- c("Africa", "Europe", "Oceania", "North_America", "Central_America", "South_America", 
          "Middle_East", "Central_Asia", "South_East_Asia", "South_Asia", 
          "East_Asia", "Multi_Region", "Antarctica")

# check to see if any quote does not fall into any region, i.e. error rows.
region_check <- df %>%
  filter(rowSums(across(all_of(cols))) < 0)

# list of all destinations planned to be visited by every quote - pull extracts a column as a list/vector, unlist flattens
# it and drops names, then unique gets only non-duplicate locations.
destination_list <- df %>%
  pull(dest_list) %>%
  unlist(use.names = FALSE) %>%
  unique()
print(destination_list)

# create a lookup list that by flattening the list in region_map_lower and drops all the names so
# its JUST the locations, not the regions/continents.
lookup <- unlist(region_map_lower, use.names = FALSE)

# check to see if any country in the destinations list, is not in the lookup list, i.e. the large list of locations that gpt gave
# for a mapping and was augmented/edited by me to fill in the errors.
# looking at the results variable I then manually fixed the locations that gpt didn't catch/do properly.
results <- tibble(
  country = destination_list,
  in_dictionary = destination_list %in% lookup
)

# function doesn't let me write list based columns so remove for writing into csv
df_write <- df %>%
  select(-ages, -dest_list)

write.csv(df_write,"/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/standardised_freely_quote_data_v2.csv", row.names = FALSE)
