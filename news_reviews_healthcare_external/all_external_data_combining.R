library("tidyverse")

google_news <- read_csv('/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/google_news_sentiment_attached_data_may_to_oct.csv')
healthcare_indices <- read_csv('/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/healthcare_indices.csv')
crime_indices <- read_csv('/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/actl4305-ggins-datathon/cleaned_crime_indices.csv')

df <- left_join(google_news, healthcare_indices, by = join_by(quote_id == quote_id))
df <- left_join(df, crime_indices, by = join_by(quote_id == quote_id))

write.csv(df,"/Users/michaelwang/Desktop/unsw/unsw actl/4305/Assignment/news_cs_hc_joined_data.csv", row.names = FALSE)
