install.packages("tidyverse")
install.packages('skimr')
install.packages('janitor')
library(skimr)
library(janitor)

booking_df <- read_csv("hotel_bookings (1).csv")

head(booking_df)
str(booking_df)
colnames(booking_df)
glimpse(booking_df)

new_df <- select(booking_df, 'adr', 'adults')
mutate(new_df, total = adr/adults)


trimmed_df <- booking_df %>% 
  select('hotel', 'is_canceled', 'lead_time')

trimmed_df <- booking_df %>% 
  select('hotel', 'is_canceled', 'lead_time') %>% 
  rename(hotel_type = hotel)

example_df <- booking_df %>% 
  select(arrival_date_month, arrival_date_year) %>% 
  unite(arrival_month_year, c(arrival_date_month, arrival_date_year), sep = ' ')

example_df <- booking_df %>% 
  mutate(guest = adults + children + babies)
head(example_df)
View(example_df)

example_df <- booking_df %>%
  summarize(number_calceled = sum(is_canceled), average_lead_time = mean(lead_time))
head(example_df)


arrange(booking_df, lead_time)

arrange(booking_df,desc(lead_time))

hotel_bookings <- arrange(booking_df,desc(lead_time))
head(hotel_bookings)
max(hotel_bookings$lead_time)
min(hotel_bookings$lead_time)
mean(hotel_bookings$lead_time)


hotel_booking_city <- filter(hotel_bookings, hotel_bookings$hotel == 'City Hotel')

head(hotel_booking_city)
mean(hotel_booking_city$lead_time)

hotel_summary <- hotel_bookings %>% 
  group_by(hotel) %>% 
  summarise(average_lead_time = mean(lead_time),
            max_lead_time = max(lead_time),
            min_lead_time = min(lead_time))
head(hotel_summary)

ggplot(data=hotel_bookings)+
  geom_point(mapping = aes(x= lead_time, y= children))

ggplot(data=hotel_bookings)+
  geom_point(mapping = aes(x=stays_in_weekend_nights, y=children))

ggplot(data = hotel_bookings)+
  geom_bar(mapping = aes(x=distribution_channel))
ggplot(data = hotel_bookings)+
  geom_bar(mapping = aes(x=distribution_channel,fill=deposit_type))
ggplot(data = hotel_bookings)+
  geom_bar(mapping = aes(x=distribution_channel,fill=market_segment))

ggplot(data = hotel_bookings)+
  geom_bar(mapping = aes(x=distribution_channel))+
  facet_wrap(~deposit_type)
ggplot(data = hotel_bookings)+
  geom_bar(mapping = aes(x=distribution_channel))+
  facet_wrap(~market_segment)
ggplot(data = hotel_bookings)+
  geom_bar(mapping = aes(x=distribution_channel))+
  facet_grid(~deposit_type)
ggplot(data = hotel_bookings)+
  geom_bar(mapping = aes(x=distribution_channel))+
  facet_wrap(~deposit_type~market_segment)

ggplot(data = hotel_bookings)+
  geom_bar(mapping=aes(x=market_segment))+
  facet_wrap(~hotel)

min(hotel_bookings$arrival_date_year)
max(hotel_bookings$arrival_date_year)
mindate <- min(hotel_bookings$arrival_date_year)
maxdate <- max(hotel_bookings$arrival_date_year)

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       caption=paste0("Data from: ", mindate, " to ", maxdate),
       x="Market Segment",
       y="Number of Bookings")

ggsave('hotel_booking_chart.png')