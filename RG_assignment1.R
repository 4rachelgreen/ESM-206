#================================================#
# ESM 206 
#Assignment 1 
# Rachel Green
# 10/8/2018
# California kindergarten vaccinations
#================================================#

#Problem 3

#================================================#
# 1. Load the tidyverse package
#================================================#
library(tidyverse) # This loads the tidyverse pkg.


#================================================#
# 2. Read in the ca_vacc.csv data (using read_csv)
#================================================#
CA_kid_vac = read_csv("ca_vacc.csv") # Read in California kindergarten vaccinations .csv file


#================================================#
# 3. Create a new data frame:
#================================================#
SB_kid_vac = CA_kid_vac %>% 
  filter(COUNTY == "SANTA BARBARA") %>% 
  mutate(prop_vacc = UpToDate/Enrolled) %>% 
  arrange(prop_vacc)


#================================================#
# 3. Graph
#================================================#
ggplot(SB_kid_vac, aes(x = prop_vacc)) +
  geom_histogram(breaks = seq(0,1, by = .1), #binwidth is by rice rule
                 col="darkgray",
                 fill='gray') +
  labs(title = 'Proportions of up-to-date vaccinated kindergartners in SB Countyn\Rachel Green') +
  labs(x = 'Proportion of kids vaccinated', y = 'Number of kids') +
  scale_x_continuous(breaks=seq(0,1,.1)) +
  theme_classic()


#================================================#

#Problem 4

#================================================#
# 1. Load the tidyverse package
#================================================#
library(tidyverse) # This loads the tidyverse pkg.


#================================================#
# 2. Read in the glacial_volume_loss.csv data (using read_csv)
#================================================#
glacial = read_csv("glacial_volume_loss.csv", skip = 32)
names(glacial)[11] = "annual"
names(glacial)[10] = "cum"

#================================================#
# 3. Histogram
#================================================#
ggplot(glacial, aes(x = annual)) +
  geom_histogram(breaks = seq(-.14,.98,.14), #binwidth is by rice rule
                 col="darkgray",
                 fill='gray') +
  labs(title = 'Annual Sea Level Rise from 1961-2003\nRachel Green') +
  labs(x = 'Annual Sea Level Rise (nm)', y = 'Number of Observations') +
  scale_x_continuous(breaks=seq(-.14,.98,.14)) +
  theme_classic()


#================================================#
# 4. Scatterplot
#================================================#
ggplot(glacial, aes(x = Year, y = cum)) +
  geom_line(lty = 3, colour = "gray19") +
  geom_point(colour = "gray35", size = 2, alpha = 0.9) +
  labs(x = "Year",
       y = expression(paste("Volume Change", (km^3))),
       title ="Cumulative Global Glacial Volume Change \n(1961 - 2003)\nRachel Green") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        text = element_text(family = "Times New Roman")) +
  scale_x_continuous(breaks = c(seq(1961, 2003, by = 6)), limits = c(1961,2003))
  