This project is part of Google Data Analytics Capstone: Complete a Case Study course. Out of the 3 available case study options, I chose case study 1. In this case study, I will be performng data analysis for a fictional bike-share company in order to help them attract more riders. Along the way, I’ll perform typical tasks of a junior data analyst while following the steps of the data analysis process: __Ask, Prepare, Process, Analyze, Share, and Act__.

# Introduction and my task
I am a junior data analyst at Cyclistic, a bike-share company in Chicago. My manager, Lily Moreno (the director of marketing), believes that the company should focus on converting casual riders into annual members. To achieve this, my team needs to analyze how both types of riders use the bikes and present data-driven recommendations to the company executives.

# Ask
1. How do annual members and casual riders use Cyclistic bikes differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?

# Prepare
I downloaded [Cyclistic’s historical trip data](https://divvy-tripdata.s3.amazonaws.com/index.html) to analyze and identify trends. In my case, the data from June 2023 to May 2024. I renamed each file to be shorter and easier to identify.
Does your data __ROCCC__? I will be using the __ROCCC__ system to see if there are issues with bias or credibility in this data.

__RELIABLITY:__ The dataset, comprising over 5 million annual records, offers a high degree of reliability.

__ORIGINALITY:__ The data originates directly from the primary source.

__COMPREHENSIVENESS:__ The dataset provides extensive information on ridership, excluding any personally identifiable details due to privacy issues.

__CURRENT:__ The dataset is up-to-date and refreshed monthly with the latest data.

__CITED:__ The data has been made available by Motivate International Inc. under this [licence](https://divvybikes.com/data-license-agreement)

# Process
I did the first part of my process phase in Excel. I added a column named ride_length where the values were calculated by subtracting started_at values from ended_at values. day_of_week column was also added where the values are shown as a number with no decimals, noting that 1 = Sunday and 7 = Saturday. This was done in all 12 datasets.

I then moved on to R Studio for the rest of the project.

I first installed necessary packages including one I used for a visualization.

```R
# Installed necessary packages:
install.packages('tidyverse')
install.packages('janitor')
install.packages("skimr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggpubr")
```

I then imported the 12 datasets.

```R
Jun23 <- read_csv("filepath here")
Jul23 <- read_csv("filepath here")
Aug23 <- read_csv("filepath here")
...
```
and so on for the remaining 9 files.

Then everything was merged into 1 dataframe, which I name __trips_merged__.

```R
trips_merged <- bind_rows(Jun23, Jul23, Aug23, Sep23, Oct23, Nov23, Dec23, Jan24, Feb24, Mar24, Apr24, May24)
```

I then started cleaning the dataframe. First, I removed columns that may be unnecessary for this analysis. (ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng)

I removed any rows or columns from the dataframe where all values are empty or missing. Other removed things include columns with NA values, duplicate rows, rows where ride_length values don’t make sense. ( value of 0 or a negative value) and rows where ride_length exceeds 24 hours.

```R
# Removal of rows or columns where all values are empty or missing
trips_merged <- remove_empty(trips_merged, which = c("rows", "cols"), cutoff = 1)

# Removal of columns with NA values
trips_merged <- trips_merged %>% drop_na()

# Removal of duplicate rows
trips_merged <- trips_merged %>% distinct()

# Removal of rows where ride_length values don’t make sense. ( value of 0 or a negative value)
trips_merged <- trips_merged[!(trips_merged['started_at'] == trips_merged['ended_at']),]

# Removal of rows where ride_length exceeds 24 hours.
trips_merged2 <- trips_merged %>% filter(trips_merged$ride_length <= hours(24))
```
This is the end my cleaning process and started the analysis. Also note that at the final cleaning step, I created a new dataframe named trips_merged2. This is so that I can keep a cleaned dataframe seperate from the original.

- Calculated the mean of ride_length for each member type

```R
 trips_merged2 %>%
     group_by(member_casual) %>%
     summarize(mean_duration = mean(ride_length))
# A tibble: 2 × 2
  member_casual mean_duration
  <chr>         <drtn>       
1 casual        793.8144 secs
2 member        637.7006 secs
```

- Calculated the max of ride_length for each member type

```R
  trips_merged2 %>%
     group_by(member_casual) %>%
     summarize(max_duration = max(ride_length))
# A tibble: 2 × 2
  member_casual max_duration
  <chr>         <drtn>      
1 casual        2171 secs   
2 member        2171 secs   
```

- Calculated the mode of day_of_week (Note that 1 = Sunday and 7 =Saturday)

```R
   mode_day_of_week <- trips_merged2 %>%
     group_by(member_casual) %>%
     summarize(day_of_week_mode = names(which.max(table(day_of_week))))
 
 print(mode_day_of_week)
# A tibble: 2 × 2
  member_casual day_of_week_mode
  <chr>         <chr>           
1 casual        7               
2 member        5    
```


__Overall__:

These findings paint a picture of two distinct user groups:

Casual Riders: More likely to take longer, less frequent rides, primarily on weekends.

Members: More likely to take shorter, more frequent rides, primarily on weekdays.

Next, I move on to visualization for a deeper analysis of the data.

Outliers were removed to make visualization easier. ride_length column was converted to seconds and I also split started_at and ended_at into 2 separate columns each - to show start date, end date, start time and end time in their own columns. Start times and end times in these new columns are in also seconds in order to make calculations and some visualizations easier. With the removal of outliers, a new dataframe was also created which I named trips_merged3.



__1. Boxplot that shows ride length distribution between each member type__

![1](https://github.com/user-attachments/assets/968c5564-0769-479f-b399-de75e4149f6e)

```R
   ggplot(trips_merged3, aes(x = ride_length)) +
    geom_histogram(binwidth = 300) +  # Adjust binwidth for desired granularity
    facet_wrap(~member_casual, ncol = 1) +  # Separate plots for each member type
    labs(title = "Distribution of Ride Lengths by User Type",
         x = "Ride Length", 
         y = "Number of Rides") +
    theme_minimal()

ggplot(trips_merged3, aes(x = ride_length, fill = member_casual)) +
    geom_density(alpha = 0.5) +  # Adjust alpha for transparency
    labs(title = "Density Plot of Ride Lengths by User Type",
         x = "Ride Length", 
         y = "Density") +
    theme_minimal()

ggplot(trips_merged3, aes(x = member_casual, y = ride_length, fill = member_casual)) +
    geom_boxplot() +
    labs(
        title = "Ride Length Distribution",
        subtitle = "Distribution of Ride Times: Insights for Marketing Strategies",
        x = "User Type",
        y = "Ride Length (seconds") +
    theme_minimal()  
```

The boxplot illustrates the distribution of ride times for Cyclistic bike-share users, categorized by their membership type: casual or member. The ride times are measured in seconds. The graph suggests that there's a distinct difference in how casual riders and members use Cyclistic bikes. Casual riders generally take longer rides with more variability in duration, while members tend to have shorter and more consistent ride times.


__2. Stacked bar chart that shows rideable type preference by user type__

![2](https://github.com/user-attachments/assets/4ebbc426-9ddf-427e-ad8a-e635c2a85c5f)

```R
 ggplot(rideable_type_counts, aes(x = member_casual, y = proportion, fill = rideable_type)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)), position = position_stack(vjust = 0.5), color = "white") +  
    labs(
        title = "Rideable Type Preference by User Type",
        subtitle = "Bike Preferences: A Closer Look at Bike Choices Among Cyclistic Users", 
        x = "User Type", 
        y = "Proportion of Rides",
        fill = "Rideable Type"
    ) +
    scale_y_continuous(labels = scales::percent_format()) + 
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") 
```

The stacked bar chart illustrates the proportion of each bike type used by casual riders and members. Classic bikes are the most popular choice for both casual riders and members. This suggests that the classic bike design is versatile and appeals to a wide range of users.


__3. Heatmap that shows top 10 stations__

![3](https://github.com/user-attachments/assets/2f5cf3dc-bd36-46de-9aa2-adb2edd9168f)

```R
 ggplot(heatmap_data, aes(x = start_station_name, y = end_station_name, fill = ride_count)) +
    geom_tile(color = "white") + # Add white borders to each tile
    facet_wrap(~member_casual, nrow = 2) +
    labs(title = "Ride Frequency Heatmap by User Type: Top 10 Stations",
         subtitle = "Popular Routes for Casual Riders and Members",
         x = "Start Station", y = "End Station", fill = "Ride Count") +
    scale_fill_viridis(option = "magma") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major = element_line(color = "lightgray", size = 0.2)  # gridlines
    ) 
```

The heatmap reveals distinct patterns in bike trip frequency between casual riders and members, with casual riders concentrating on a few specific routes around tourist attractions and members exhibiting a wider range of trips, including commutes.


__4. Stacked bar chart that shows time of day preference by member type__

![5](https://github.com/user-attachments/assets/e2221406-64d8-4374-8c39-6629db0c0a8a)

```R
 ggplot(time_of_day_counts, aes(x = time_of_day, y = ride_proportion, fill = member_casual)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(aes(label = scales::percent(ride_proportion, accuracy = 0.1)), position = position_fill(vjust = 0.5)) +
    labs(title = "Time of Day Preference by Member Type",
    subtitle = "Proportion of Rides by Time of Day for Member and Casual Riders",
         x = "Time of Day",
         y = "Proportion of Rides",
         fill = "Member Type") +
    scale_y_continuous(labels = scales::percent)
```

The stacked bar chart illustrates the distribution of bike rides throughout the day, revealing that casual riders primarily use the service during the afternoon while member riders show a more balanced usage with a preference for the morning and afternoon. Notably, both member and casual riders rarely use the service during the night.


__5. Line chart that shows monthly usage trends for members and casual riders (2023-2024)__

![6](https://github.com/user-attachments/assets/b4dca552-454a-45aa-abba-3eff9a5e874a)

```R
 ggplot(monthly_usage, aes(x = month, y = number_of_rides, color = member_casual, group = member_casual)) +
    geom_line() +
    geom_point() +
    facet_wrap(~year, ncol = 2) +
    labs(title = "Monthly Usage Trends for Members and Casual Riders (2023-2024)",
    subtitle = "Comparing Total Rides per Month for Each Rider Type",
         x = "Month",
         y = "Number of Rides") +
    scale_y_continuous(labels = scales::comma) + 
    theme(axis.text.x = element_text(angle = -45, hjust = 0))
```

The line chart illustrates that both members and casual riders exhibit a similar seasonal trend in bike usage throughout 2023, with ridership peaking in the summer months and declining in the winter months.

__6. Grouped bar chart that displays number of rides by day of week for each user type across two years (2023 and 2024).__

![7](https://github.com/user-attachments/assets/a900a6d9-3f6e-41d9-8191-7b756d5f001f)

```R
 # Extract year
trips_merged3$year <- year(trips_merged3$start_date)

# Filter for 2023 and 2024
trips_merged4_filtered <- trips_merged3 %>% 
    filter(year == 2023 | year == 2024)

day_of_week_counts <- trips_merged4_filtered %>%
    group_by(member_casual, day_of_week, year) %>%
    summarise(number_of_rides = n(), .groups = 'drop')

# Convert `day_of_week` from numeric to day names
day_of_week_counts$day_of_week <- factor(
    day_of_week_counts$day_of_week, 
    levels = 1:7, 
    labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
)

ggplot(day_of_week_counts, aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
    geom_bar(stat = "identity", position = "dodge") + 
    facet_grid(year ~ member_casual) +  # Create facets for year and member_casual
    labs(title = "Number of Rides by Day of Week for Each User Type and Year",
         x = "Day of Week",
         y = "Number of Rides") +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = -45, hjust = 0))
```

The chart displays the number of Cyclistic bike rides by day of the week, comparing casual riders and members across two years (2023 and 2024). Casual riders show consistent usage throughout the week in both years, with a slight increase towards the weekend while members have a clear preference for weekdays.


# Analyze

__Key Differences Between Casual Riders and Members:__

•	Ride Duration: Both groups have similar median ride times, but casual riders show more variability in ride lengths.

•	Trip Patterns: Casual riders focus on popular tourist routes, while members' trips are more diverse and include commuting.

•	Seasonality: Both groups show similar seasonal trends, with higher usage in summer and lower in winter. However, 2024 data indicates a stronger increase in member ridership compared to casual riders.

•	Time of Day: Casual riders use the service more in the afternoon, while members have a more balanced usage across morning and afternoon.

•	Day of Week: Casual riders use bikes more on weekends, while members increasingly prefer weekdays in 2024.

__Summary__

Casual riders tend to be more leisure-oriented, using bikes for shorter, tourist-focused trips on weekends, primarily in the afternoon. Members, on the other hand, exhibit a more practical pattern, using bikes for commuting and various other purposes throughout the week, with a preference for mornings and afternoons.


# Act

__Recommendation for Cyclistic Marketing Strategy__

__1. Targeted Marketing for Casual Riders:__
   
-	Weekend Promotions: Since casual riders predominantly use bikes on weekends, offer special weekend rates or package deals to entice them to become members.

-	Tourist-Focused Packages: Develop special passes or discounts for tourists who frequent the popular routes identified in the analysis.

-	Membership Conversion Campaigns: Create targeted marketing campaigns aimed at converting casual riders into members by highlighting the benefits of membership (e.g., discounted rates for longer rides, access to special events, etc.).

__2. Enhance Member Engagement:__

-	Morning Commute Incentives: As members primarily use bikes during the morning commute, offer early bird discounts or loyalty rewards for frequent morning rides to further incentivize their usage.

-	Weekday Deals: Introduce weekday promotions or discounts for members to encourage more consistent usage throughout the week.

-	Expand Station Network: Consider expanding the network of bike stations to cater to the wider range of trips taken by members, particularly in areas with high commuter traffic.

__3. Optimize Pricing Strategies:__
   
- Dynamic Pricing: Explore the possibility of implementing dynamic pricing based on demand. This could involve higher rates for popular routes during peak tourist season and discounted rates for members during off-peak hours.

- Subscription Options: Introduce different membership tiers to cater to different usage patterns, such as a basic commuter plan for weekdays and a premium plan for weekend and leisure riders.

__4. Data-Driven Decision Making:__

-	Continue Tracking: Continuously monitor the usage patterns of both casual riders and members to identify emerging trends and adjust marketing strategies accordingly.

-	Conduct Surveys: Gather feedback from both casual riders and members to understand their motivations, preferences, and pain points, and use this information to inform future initiatives.

End of project.


# References for ideas

- Assyifaziza's work was great for reference who unlike me, used tableau to create great visualizations and dashboard. Check out Assyifaziza work [here](https://github.com/Assyifaziza/RStudio-Google-Data-Analytic-Study-Case-1/tree/main)

- Shumail Sajjad's work gave a lot of ideas as well because he did his work entirely in R. Check out Shumail Sajjad's work [here](https://rpubs.com/Shumail/1049889)
