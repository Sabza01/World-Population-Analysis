#Read the files
w_data <- read.csv('World_pop_data_2023-11-14 11_56_30.csv')
metadata <- read.csv('country_metadata_2023-11-14 11_59_06.csv')

#Explore the files
head(w_data)
head(metadata)

#Check out column names of the files
colnames(w_data)
colnames(metadata)

#We need to select the columns we want only
new_w_data <- w_data[c(-2,-3)]
new_metadata <- metadata[c(-4)]

#We want to join the two data frames
library(dplyr)
pop_data <- merge(x = new_metadata, y = new_w_data, by = "Country.Code")

#Rename Column four to Country
colnames(pop_data)[4] <- "CountryName"

#Clean the data
#Check rows with null values. 
any(is.null(pop_data))

# The null values are coded as "null" so they do not show up when applying the function
# We find the rows that contain the value "null" a different way

null_values <- pop_data%>%filter_all(any_vars(.%in%c('null')))

#Remove the rows with the null values in region as these are not countries
pop_data <- subset(pop_data, Region!="null")
head(pop_data)

#Now that we cleaned the data, we can do some analysis
library(ggplot2)

# Remove the scientific notation
options(scipen = FALSE)

# Stack the columns of the different years. This can also be achieved
# using the melt() function

#Bind the first four columns
# Stack the rest of the columns

stacked_pop_data <- cbind(pop_data[1:4], stack(pop_data[5:65]))


# Remove the X on the Years data
stacked_pop_data$ind <- substr(stacked_pop_data$ind, 2, 5)


# Change the column heading from ind to Year

colnames(stacked_pop_data)[6] <- 'Year'

head(stacked_pop_data)

# Check the structure of the dataframe
str(stacked_pop_data)

# Change the data type of Year from string to number
stacked_pop_data$Year <- as.numeric(stacked_pop_data$Year)

# Create a date column Using lubridate
library(lubridate)
stacked_pop_data$Date <- ymd(stacked_pop_data$Year, truncated = 2L)
head(stacked_pop_data)

# Check the structure of the dataframe again
str(stacked_pop_data)

#Output the data frame as csv
#write.csv(stacked_pop_data , "C:\\Users\\Sabelo\\Documents\\Grad School\\GIES\\MSBA\\Learning\\Word Population Project\\WorldPop\\Population_data.csv", row.names=FALSE)

# Scale the population values for more clear presentation
stacked_pop_data$values <- stacked_pop_data$values/1000000
stacked_pop_data <- na.omit(stacked_pop_data)
sum(is.na(stacked_pop_data))
head(stacked_pop_data)

#Plot a line graph to show changes to population for the chosen country
ggplot(filter(stacked_pop_data, CountryName=='United States'), 
  aes(x=Date, y=values)) + 
    geom_line() + 
      ggtitle("United States Population Growth from 1960-2020") +
        xlab("Time (in Years)") +
          ylab("Population in Millions")

#Plot a line graph to show changes to population for the chosen countries
ggplot(filter(stacked_pop_data, CountryName %in% c("United States", "China", "India")), 
       mapping=aes(x=Date, y=values, group=CountryName, color=CountryName)) + 
  geom_line() + 
        scale_colour_manual("", 
                      breaks = c("United States", "China", "India"),
                      values = c("blue", "red", "green")) +
          ggtitle("Comparison of Population Changes of United States, China 
                  and India from 1960-2020") +
              xlab("Time (in Years)") +
                ylab("Population in Millions")

# How did the population in different parts of the world change over time?
# we will use the stat_summary function and specify geom="line"

ggplot(stacked_pop_data, mapping=aes(x=Date, y=values, color=Region, group=Region)) + 
         stat_summary(fun=sum, geom = "line") +
  ggtitle("Comparison of Population Changes in different
          regions of the world from 1960-2020") +
    xlab("Time (in Years)") +
      ylab("Population in Millions")

# Which country or countries have experienced the highest increase/decrease in population over time?

new_d<- stacked_pop_data %>%
  group_by(CountryName) %>%
  mutate(lagged_values = values - lag(values,1))
View(new_d)

new_d %>% 
  filter(Date>="1961-01-01") %>%
  group_by(CountryName) %>%
  summarise(sum_diff=sum(lagged_values, na.rm=T)) %>%
    arrange(desc(sum_diff)) %>%
    slice_head(n=5)

new_d %>% 
  filter(Date>="1961-01-01") %>%
  group_by(CountryName) %>%
  summarise(sum_diff=sum(lagged_values, na.rm=T)) %>%
  arrange(desc(sum_diff)) %>%
  slice_tail(n=5)


# Which country or countries have been experiencing the highest increase/decrease in population in the last five (or ten) years?
new_d %>% 
  filter(Date>="2015-01-01") %>%
  group_by(CountryName) %>%
  summarise(sum_diff=sum(lagged_values, na.rm=T)) %>%
  arrange(desc(sum_diff)) %>%
  slice_head(n=5)

new_d %>% 
  filter(Date>="2015-01-01") %>%
  group_by(CountryName) %>%
  summarise(sum_diff=sum(lagged_values, na.rm=T)) %>%
  arrange(desc(sum_diff)) %>%
  slice_tail(n=5)


# How many people were born in your country (or any other country) during your birth year?
new_d %>%
  filter(Date=="1994-01-01") %>%
  filter(CountryName=="Eswatini") %>%
  group_by(CountryName) %>%
  summarise(num_born=mean(lagged_values, na.rm=T)*1000000)
  

# How does income group affect a country's population growth?

ggplot(stacked_pop_data, mapping=aes(x=Date, y=values, color=IncomeGroup, group=IncomeGroup)) + 
  stat_summary(fun=sum, geom = "line") +
  ggtitle("Comparison of Population Changes in different
          income groups from 1960-2020") +
  xlab("Time (in Years)") +
  ylab("Population in Millions")
  






