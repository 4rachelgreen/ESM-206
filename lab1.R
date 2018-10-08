#######################
#lab 1 10/3/18
###########################

#Install tidyverse

install.packages("tidyverse")

##############################
#Part 1. Loading the tidyverse
###########################

library(tidyverse)

######################################################
#Part 2. Read in National Parks data (csv)
#########################

np_visits<-read_csv("National Parks Visitation Data.csv")
View(np_visits)

#######################
#Part 3. 
######################

names(np_visits) #Reports all column names
dim(np_visits) #dimensions
np_names <- names(np_visits)
np_names

head(np_visits, 10) #Report the last 10 lines
tail(np_visits, 14) # Report the last 14 lines

##########################
#Part 4. Basic data wrangling in dplyr
###########################

#select(): select specific columns of a data frame
#filter(): create subsets of ROWS based on conditions
#arrange(): sort data (alphabetically and/or numerically)
#mutate(): add a new column based on calculations from existing columns

#Create a subset of the np_visits data frame that only has columns from state through YearRaw

df1 <- select(np_visits, State:YearRaw)
View(df1)

#Filter df1 to only retain observations for California National Parks for years since 1950

df2 <- filter(df1, State == "CA" & Type == "National Park" & YearRaw >= 1950)
View(df2)

#Arrange data alphabetically by park Code, and then numberically by YearRaw

df3 <- arrange(df2, Code, YearRaw)
View(df3)

#Add a new column that is Visitors/1000 (so we can make a graph with thousands of visitors)

df4 <- mutate(df3, kVis = Visitors/1000)
View(df4)

#Use the filter functions to only keep rows where the 'YearRaw' column is not matching "Total"

df5 <- filter(df4, YearRaw != "Total") #!= does not equal
View(df5)

class(df5$YearRaw) #currenlty "character", want to change to "numeric"

#Coerce the YearRaw variable into a numeric class

df5$YearRaw <- as.numeric(df5$YearRaw)
class(df5$YearRaw) #now numeric


#Part 5. More elegant code for data wrangling with piping

#pipe operator (%>%) (shortcut is Command + Shift + m) means "and then"

#to wrange on the original np_vsits dataset, and we want to:
#1. Only select parks in UT that are National Parks
#2. Select only columns Name, Type, Visitors,a nd YearRaw
#3. Add a column that is millions of visitors
#4. Arrange by Name, then by millions of visitors column

utah_parks <- np_visits %>% 
  filter(State == "UT", Type == "National Park") %>% 
  select(Name, Type, Visitors, YearRaw) %>% 
  mutate(m_vis = Visitors/1000000) %>% 
  arrange(Name, m_vis)


View(utah_parks)

#############################
#Part 6. Create a scatterplot of California National Park Visitation using the data frame (df5)
#############################

#1. Using ggplot()
#2. What data we're using, including what our x & y variables are (if relevent)
#3. What type of graph we're creating

np_graph <- ggplot(df5, aes(x = YearRaw, y = kVis)) +
  geom_point(aes(color = Code)) + #adds different color for each variable
  labs(x = "Year", y = "Annual Visitors (thousands)",
       title = "Rachel Green")


windows() #opens new window
np_graph

