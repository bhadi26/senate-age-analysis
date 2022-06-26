# Senate vs. American Age Comparison 

library(tidyverse)
# politican data 
df <- data.frame(read.csv('wiki_list_of_senator.csv'))
american_df <- data.frame(read.csv('wiki_census_data.csv'))

# filter out independent 
df <- df %>% filter(Party != 'Independent[a]')

# plot age 
ggplot(data=df, aes(x=Party, y=Age)) + 
  geom_boxplot(col=c("blue","red")) + 
  theme_bw() + 
  ggtitle("Senator Age Range  by Party")


# calculate proportion based on 21 + 
calc_df <- american_df %>% filter(!age..years. %in% c('< 15','15-17','18-20','all'))

# calculate percent of total 
denom <- sum(calc_df$total..in.thousands.)

calc_df$pct_total <- calc_df$total..in.thousands. / denom

# plot for american
ggplot(data=calc_df, aes(x=age..years., y=pct_total)) +
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









