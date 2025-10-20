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
      "pattaya" = "thailand",
      "auckland"="new zealand","christchurch"="new zealand","cairns"="australia",
      "brisbane"="australia","melbourne"="australia","perth"="australia",
      "sydney"="australia","gold coast"="australia","darwin"="australia",
      "adelaide"="australia","canberra"="australia","hobart"="australia",
      "bangkok"="thailand","phuket"="thailand","chiang mai"="thailand",
      "kuala lumpur"="malaysia","penang"="malaysia","jakarta"="indonesia",
      "bali"="indonesia","hanoi"="vietnam","ho chi minh city"="vietnam",
      "manila"="philippines","cebu"="philippines","singapore"="singapore",
      "tokyo"="japan","osaka"="japan","seoul"="south korea","taipei"="taiwan",
      "beijing"="china","shanghai"="china","shenzhen"="china",
      "kathmandu"="nepal","mumbai"="india","delhi"="india","dhaka"="bangladesh",
      "dubai"="united arab emirates","abu dhabi"="united arab emirates","doha"="qatar",
      "kuwait"="kuwait",
      "london"="united kingdom","manchester"="united kingdom","liverpool"="united kingdom",
      "amsterdam"="netherlands","paris"="france","lyon"="france","nice"="france",
      "strasbourg"="france","madrid"="spain","barcelona"="spain","lisbon"="portugal",
      "rome"="italy","milan"="italy","florence"="italy","athens"="greece",
      "vienna"="austria","prague"="czech republic","budapest"="hungary","belgrade"="serbia",
      "brussels"="belgium","berlin"="germany","munich"="germany","frankfurt"="germany",
      "cologne"="germany","zurich"="switzerland","geneva"="switzerland",
      "copenhagen"="denmark","stockholm"="sweden","helsinki"="finland",
      "dublin"="ireland","istanbul"="turkey",
      "birmingham"="united kingdom","edinburgh"="united kingdom","cardiff"="united kingdom",
      "cairo"="egypt","nairobi"="kenya","johannesburg"="south africa",
      "cape town"="south africa",
      "new york"="united states","los angeles"="united states","san francisco"="united states",
      "chicago"="united states","miami"="united states","las vegas"="united states",
      "honolulu"="united states","vancouver"="canada","toronto"="canada","calgary"="canada",
      "ottawa"="canada","mexico city"="mexico","cancun"="mexico","panama city"="panama",
      "lima"="peru","santiago"="chile","buenos aires"="argentina","rio de janeiro"="brazil",
      "salvador"="brazil","brasilia"="brazil",
      "monaco"="monaco","san marino"="san marino",
      "gibraltar"="united kingdom","jersey"="jersey",
      "reunion"="france","madeira"="portugal","martinique"="france","guadeloupe"="france",
      "bermuda"="united kingdom","puerto rico"="united states","american samoa"="united states",
      "falkland islands"="united kingdom","alaska"="united states","greenland"="denmark","jersey"="united kingdom",
      "canary islands"="spain", 
      .default = indiv_destination,
      .missing = indiv_destination
    )
  )

dest_collapsed <- dest_split_df %>%
  group_by(quote_id) %>%
  summarise(
    destinations = paste(indiv_destination, collapse = ", "),
    .groups = "drop"
  )

one_per_quote <- dest_split_df %>%
  group_by(quote_id) %>%
  slice_head(n = 1) %>%           # or dplyr::first_row() in newer dplyr
  select(-indiv_destination) %>%
  ungroup()

df_back <- one_per_quote %>%
  left_join(dest_collapsed, by = "quote_id")

df_back <- df_back %>% relocate(destinations.y, .after = destinations.x)

df_write <- df_back %>% rename(cleaned_dest_list = destinations.y)

write.csv(df_write,"/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/cleaned_destinations_data.csv", row.names = FALSE)
