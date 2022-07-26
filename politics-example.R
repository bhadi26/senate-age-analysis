# Senate vs. American Age Comparison 

library(rvest)
library(tidyverse)

senators_url <- "https://en.wikipedia.org/wiki/List_of_current_United_States_senators" 
american_pop_url <- "https://en.wikipedia.org/wiki/Demographics_of_the_United_States#Age_and_sex_distribution"

# Return a data frame of list of senators from the wikipedia page url for senators
df <- senators_url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="senators"]') %>%
  html_table(fill = TRUE)

#Remove the duplicated column
df <- df[, -4]

# Return a data frame of list of US population characteristics from the wikipedia page url for the U.S population
american_df <- american_pop_url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table(fill = TRUE)


# filter out independent 
df <- df %>% filter(!Party %in% c('Independent[a]', 'Republican[d]', 'Democrat[d]')) %>%
  mutate(Age = as.numeric(difftime(
    Sys.Date(),
    lubridate::as_date(str_extract(string = Born, pattern = "\\d+-\\d+-\\d+")),
    units = "days"
  ))/365)

# plot age 
ggplot(data=df, aes(x=Party, y=Age)) + 
  geom_boxplot(col=c("blue","red")) + 
  theme_bw() + 
  ggtitle("Senator Age Range  by Party")


# calculate proportion based on 21 + 
calc_df <- american_df %>% filter(!`age (years)` %in% c('0', '< 5', '< 15', '15-17', '18-20', 'all', '100+'))

#Remove commas from text thats supposed to me numbers
calc_df$`total (in thousands)` <- gsub(",", "", calc_df$`total (in thousands)`)
calc_df$`males (in thousands)` <- gsub(",", "", calc_df$`males (in thousands)`)
calc_df$`females (in thousands)` <- gsub(",", "", calc_df$`females (in thousands)`)


#Convert the numbers to text
calc_df[,c(2,4,5,8)] <- lapply(calc_df[,c(2,4,5,8)],as.numeric)

# calculate percent of total 
denom <- sum(calc_df$`total (in thousands)`)

calc_df$pct_total <- calc_df$`total (in thousands)`/ denom

# plot for american
ggplot(data=calc_df, aes(x=`age (years)`, y=pct_total)) +
  geom_bar(stat='identity',fill='darkgreen') + 
  theme_bw() + 
  ggtitle("How old are Americans? (21+)")


# bucket based on criteria 
df$age_bucket <- 
  as.character(cut(
    df$Age,
    c(21,44,64,Inf),
    labels=c("21-44", "45-64","65+")))

# aggregate and calculate percent of total 
senator_age_df <- df %>% count(age_bucket)
party_age_df <- df %>% group_by(Party) %>% count(age_bucket)

senator_age_df$pct_total <- senator_age_df$n / sum(senator_age_df$n)

dem_total <- 48
rep_total <- 50 

dem_age_df <-  df %>% filter(Party == 'Democratic') %>% count(age_bucket)
rep_age_df <-  df %>% filter(Party == 'Republican') %>% count(age_bucket)

dem_age_df$pct_total <- (dem_age_df$n / dem_total)
rep_age_df$pct_total <- (rep_age_df$n / rep_total)

# by party 

# plot 
# plot for senators 
ggplot(data=dem_age_df, aes(x=age_bucket, y=pct_total)) +
  geom_bar(stat='identity',fill="blue") + 
  theme_bw() + 
  ggtitle("Democratic Senator Age by Bucket")

# plot 
# plot for senators 
ggplot(data=senator_age_df, aes(x=age_bucket, y=pct_total)) +
  geom_bar(stat='identity') + 
  theme_bw() + 
  ggtitle("Senator Age by Bucket (Both Parties)")

# republican 
ggplot(data=rep_age_df, aes(x=age_bucket, y=pct_total)) +
  geom_bar(stat='identity',fill='red') + 
  theme_bw() + 
  ggtitle("Republican Senator Age by Bucket")

