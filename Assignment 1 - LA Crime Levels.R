# Assignment 1: Exploring Crime Data in Los Angeles ----

# In this assignment, you'll be exploring a data set containing information
# about crimes in Los Angeles. The goal is to analyze and summarize key aspects
# of the data to better understand crime patterns.

# Start with a few simple ideas:

#1) What are the unique crimes happening in LA?
#2) Which type of crime is occurring the most?
#3) Which neighborhoods are experiencing the highest crime rates?
#4) Create 3 visuals from the data. Keep in mind the best practices we learned from
#Tufte and Berinato. Make sure all your graphs are customized.

# Deliverables:
# An R Script file.
# Screenshots of your graphs with a brief description of what you created. In your document 
# attach your visual with the caption explain what you accomplished with each chart.  

# Note: Each graph must have at least 4 modifications (more is better :) ) from the default settings to 
# receive full credit. You can include elements such as color, labels, axis names, titles, captions, or even remove
# unnecessary elements to enhance clarity. Be creative and ensure your graphs are informative
# and appealing.

# Helpful Reading Resources:

#Data Visualization: A Practical Introduction by Kieran Healy https://socviz.co/
#Chapter 3: Make a plot and
#Chapter 4: Show the right numbers

# Section 1: Read in required packages and libraries needed ----

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)


# Section 2: Load data, review data, and format ----
crime_tbl <- read.csv("crime_data.csv")

# Check for for NAs in entire dataset
any(is.na(crime_tbl))

# Remove Nas from data
crime_tbl <- na.omit(crime_tbl)

# sanity check
is.na(crime_tbl)

# see column names
names(crime_tbl)

# rename columns for ease
crime_tbl <- crime_tbl %>%
  rename(
    "Report_Number"          = DR_NO,              
    "Date_of_Occurrence"     = DATE.OCC,       
    "Time_of_Occurrence"     = TIME.OCC,     
    "Crime_Code_Description" = Crm.Cd.Desc,
    "Crime_Code"             = Crm.Cd,  
    "Premise_Description"    = Premis.Desc,
    "Premise_Code"           = Premis.Cd,
    "Victim_Race"            = Vict.Descent,
    "Victim_Sex"             = Vict.Sex,
    "Victim_Age"             = Vict.Age,    
    "Neighborhood"           = AREA.NAME,
    "Neighborhood_Code"      = AREA,  
  )

# Convert date column "Date_of_Occurrence" to just date since we already have a time column 
crime_tbl$Date_of_Occurrence <- as.Date(
  crime_tbl$Date_of_Occurrence, format = "%m/%d/%Y")

# Sanity reformatted column
head(crime_tbl)

# Section 3: Wrangling crime data ----
# Data Manipulation

# Aggregate data by month
crime_by_month <- crime_tbl %>%
  mutate(Month = format(Date_of_Occurrence, "%Y-%m")) %>%
  group_by(Month) %>% # Group crimes by month
  summarise(Total_Crimes = n()) # n() counts the number of rows in each group, how many times each type of crime occurs.

# Convert Month back to Date to prepare date for plotting
crime_by_month$Month <- as.Date(paste0(crime_by_month$Month, "-01"))

# Create Line Chart showing total crimes per month ----
ggplot(crime_by_month, aes(x = Month, y = Total_Crimes)) +
  
  # Canvas
  geom_line(color = "blue", size = 1) +  # Line plot for trend
  geom_point(color = "red", size = 2) +  # Add points for emphasis
  theme_minimal() +
  labs(title = "Crime Is Declining in Los Angeles",
       caption = "Source: Data.gov",
       x = "",
       y = "Total Crimes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#1) What are the unique crimes happening in LA?
colnames(crime_tbl)
unique(crime_tbl$Crime_Code_Description)
unique(crime_tbl$Crime_Code)
  #There are 96 unique crimes happening in LA

colnames(crime_tbl)

#2) Which type of crime is occurring the most?
crime_tbl %>% count(Crime_Code_Description, sort = TRUE)
crime_tbl %>% count(Crime_Code, sort = TRUE)
  #A Stolen vehicle which is a 510 occured the most at 1101 times.

#3) Which neighborhoods are experiencing the highest crime rates?
crime_tbl %>% count(Neighborhood, sort = TRUE)
crime_tbl %>% count(Neighborhood_Code, sort = TRUE)
  #Central Neighborhood, code 1, experiences the most crime with 686 crimes occurring over 4 years.

#4) Graphs

#Visualizing the neighborhood that has the most crime over the 4 year period.
ggplot(data = crime_tbl, aes(x = Neighborhood)) +
  geom_bar(aes(fill = ifelse(Neighborhood == "Central", "red", "#00CEC8"))) + #Highlighting a specific neighborhood
  scale_fill_identity() +
  ylab("Number of Crimes") +
  xlab("Neighborhoods") +
  ggtitle ("Crime Levels in LA Neighborhoods (2020-2024)") +
  theme_classic() +
  coord_flip() +
  annotate("text", x = "Central", y = 570, label = "Most Dangerous Neighborhood", 
           size = 3, color = "red", vjust = -1)

#Visualizing at what time a vehicle is most likely to be stolen
  #Aggregate the data by Time of Occurrence and Crime code description
stolen_vehicle_time <- crime_tbl %>%
  filter(Crime_Code_Description == "VEHICLE - STOLEN") %>%
  select(Time_of_Occurrence) %>%
  group_by(Time_of_Occurrence) %>%
  summarize(count = n())

  #Looking at the new data
stolen_vehicle_time %>% arrange(desc(count))

  #Line chart
ggplot(stolen_vehicle_time, aes(x = Time_of_Occurrence, y = count)) +
  geom_line(color = "darkblue", size = .5) +
  labs(title = "Times When Vehicles Are Stolen In LA (2020-2024)", 
       subtitle = "When are you most likely to have your car stolen?",
       x = "Time (Military)", 
       y = "Number of Vehicles Stolen",
       caption = "Your are most likely to have your car stolen between 5pm and 10pm") +
  annotate("rect", xmin = 1625, xmax = 2250, ymin = 40, ymax = 65, 
           alpha = 0.5, fill = "red")+ 
  theme_classic() +
  theme (
    panel.grid.major.x = element_line(color = "grey", size = 0.25),
    panel.grid.minor.x = element_line(color = "grey", size = 0.25),
    title = element_text(face = "bold", color = "darkblue"))

#Looking at the distribution of the age of victim 
  #Looking at the data
crime_tbl$Victim_Age
  
  #Removing victims ages listed as -1, 0
remove <- c(-1,0)
age <- crime_tbl %>%
  filter(!Victim_Age %in% remove)

  #Checking my work
age$Victim_Age
  
  #Looks at my future X and Y values
age %>% count(Victim_Age, sort = TRUE)

  #Hisogram
ggplot(age, aes(Victim_Age)) + 
  geom_histogram(bins = 50, fill = "orange", color = "black") +
  labs(title = "Ages of Victims in LA (2020-2024)",
       x = "Victim Age",
       y = "Number of Victims",
       caption = "People between the ages of 25 and 40 are more likey to be a victim of a crime.") +
  theme_bw() +
  theme(
    title = element_text(face = "bold", color = "black")) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 450, by = 50))
