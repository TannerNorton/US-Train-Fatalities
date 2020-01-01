# Tanner Norton
# Semester Project: Train Data

#### Data is current as Sep 2019 ####

# Look at deaths per total miles since 2017 with inception of Brighline (BLF) what are other dangerous ones

# What has been trend of deaths over time (Sex, Age, )

# Distribution in deaths across rail class (1,2,3)

# In depth look at California

# Distribution in deaths by month

# Show a timeline with major US rail accidents


# Load libraries
pacman::p_load(tidyverse, readxl, stringr, stringi,blscrapeR, forcats, skimr, rio, lubridate, riem, 
               dygraphs,epitools, tidyquant, quantmod, timetk, DT, scales, USAboundaries, USAboundariesData,
               ggrepel,sf, maps, geofacet, ggplot2, maptools, buildings, rnaturalearth, rvest, leaflet, 
               prettydoc, magrittr, cdlTools, htmltools, viridis, RColorBrewer)


# Read in 2000 casualty data
casualty_2000 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2000.csv") %>% 
  mutate(IYR = as.numeric(IYR)) %>% 
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD)
  
# Read in 2001 casualty data
casualty_2001 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2001.csv") %>% 
  mutate(IYR = as.numeric(IYR)) %>% 
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD)

# Read in 2002 casualty data
casualty_2002 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2002.csv") %>% 
  mutate(IYR = as.numeric(IYR)) %>% 
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD)

# Read in 2003 casualty data
casualty_2003 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2003.csv") %>% 
  mutate(IYR = as.numeric(IYR)) %>% 
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD)

# Read in 2004 casualty data
casualty_2004 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2004.csv") %>% 
  mutate(IYR = as.numeric(IYR)) %>% 
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD)

# Read in 2005 casualty data
casualty_2005 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2005.csv") %>% 
  mutate(IYR = as.numeric(IYR)) %>% 
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD)

# Read in 2006 casualty data
casualty_2006 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2006.csv") %>% 
  mutate(IYR = as.numeric(IYR)) %>% 
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD)

# Read in 2007 casualty data
casualty_2007 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2007.csv") %>% 
  mutate(IYR = as.numeric(IYR)) %>% 
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD)

# Read in 2008 casualty data
casualty_2008 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2008.csv") %>% 
  mutate(IYR = as.numeric(IYR)) %>% 
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD)

# Read in 2009 casualty data
casualty_2009 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2009.csv") %>% 
  mutate(IYR = as.numeric(IYR)) %>% 
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD)

# Read in 2010 casualty data
casualty_2010 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2010.csv") 
# Read in 2011 casualty data
casualty_2011 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2011.csv") 
# Read in 2012 casualty data
casualty_2012 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2012.csv") 
# Read in 2013 casualty data
casualty_2013 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2013.csv")
# Read in 2014 casualty data
casualty_2014 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2014.csv") 
# Read in 2015 casualty data
casualty_2015 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2015.csv") 
# Read in 2016 casualty data
casualty_2016 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2016.csv") 
# Read in 2017 casualty data
casualty_2017 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2017.csv") 
# Read in 2018 casualty data
casualty_2018 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2018.csv")
# Read in 2019 casualty data
casualty_2019 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2019.csv") 



# Join the data from 2000 to 2019 together
all_data <- full_join(casualty_2019, casualty_2018) %>% 
  full_join(casualty_2017) %>% full_join(casualty_2016) %>% full_join(casualty_2015) %>% full_join(casualty_2014) %>% 
  full_join(casualty_2013) %>% full_join(casualty_2012) %>% full_join(casualty_2011) %>% full_join(casualty_2010) %>%
  full_join(casualty_2009) %>% full_join(casualty_2008) %>% full_join(casualty_2007) %>% full_join(casualty_2006) %>% 
  full_join(casualty_2005) %>% full_join(casualty_2004) %>% full_join(casualty_2003) %>% full_join(casualty_2002) %>% 
  full_join(casualty_2001) %>% full_join(casualty_2000) %>% 
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD) %>% 
  mutate(Fatal = case_when(CASFATAL == "Y" ~ 1,
                           CASFATAL == "N" ~ 0),
         state = fips(STATE, to = "Name"),
         state_name = str_replace_all(state,"Deleware","Delaware"),
         Age_group = case_when(as.numeric(AGE) %in% seq(1,9,1) ~ "0-9",
                               AGE %in% seq(10,19,1) ~ "10-19",
                               AGE %in% seq(20,29,1) ~ "20-29",
                               AGE %in% seq(30,39,1) ~ "30-39",
                               AGE %in% seq(40,49,1) ~ "40-49",
                               AGE %in% seq(50,59,1) ~ "50-59",
                               AGE %in% seq(60,69,1) ~ "60-69",
                               AGE %in% seq(70,79,1) ~ "70-79",
                               AGE %in% seq(80,89,1) ~ "80-89",
                               AGE %in% seq(90,99,1) ~ "90-99"))


# Write new data set to CSV
# write_csv(all_data, "all_data.csv")


# Read in all_data
all_data <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/all_data.csv")


# Join the data from 2010 to 2019 together
all_data_2010 <- full_join(casualty_2019, casualty_2018) %>% 
  full_join(casualty_2017) %>% full_join(casualty_2016) %>% full_join(casualty_2015) %>% full_join(casualty_2014) %>% 
  full_join(casualty_2013) %>% full_join(casualty_2012) %>% full_join(casualty_2011) %>% full_join(casualty_2010) %>% 
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD) %>% 
  mutate(Fatal = case_when(CASFATAL == "Y" ~ 1,
                           CASFATAL == "N" ~ 0),
         state = fips(STATE, to = "Name"),
         state_name = str_replace_all(state,"Deleware","Delaware"),
         Age_group = case_when(as.numeric(AGE) %in% seq(1,9,1) ~ "0-9",
                               AGE %in% seq(10,19,1) ~ "10-19",
                               AGE %in% seq(20,29,1) ~ "20-29",
                               AGE %in% seq(30,39,1) ~ "30-39",
                               AGE %in% seq(40,49,1) ~ "40-49",
                               AGE %in% seq(50,59,1) ~ "50-59",
                               AGE %in% seq(60,69,1) ~ "60-69",
                               AGE %in% seq(70,79,1) ~ "70-79",
                               AGE %in% seq(80,89,1) ~ "80-89",
                               AGE %in% seq(90,99,1) ~ "90-99"))


# Write new data set to CSV
# write_csv(all_data_2010, "all_data_2010.csv")

# Read in all_data
all_data_2010 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/all_data_2010.csv")








# All deaths 2000-2018
all_deaths <- all_data %>% 
  filter(Fatal == 1,
         YEAR4 < 2019) %>% 
  mutate(C_date = paste(DAY, IMO, YEAR4, sep = "-")) 


# All deaths 2010-2018
all_deaths_2010 <- all_data_2010 %>% 
  filter(Fatal == 1,
         YEAR4 < 2019) %>% 
  mutate(C_date = paste(DAY, IMO, YEAR4, sep = "-")) 



# States geometry data
states <- us_boundaries(map_date = NULL, type = c("state", "county", "congressional"),
                        resolution = c("low", "high")) %>% 
  filter(!state_abbr %in% c("PR","HI"))

# State total deaths 2000-2018
state_deaths <- all_data %>% 
  filter(YEAR4 < 2019) %>% 
  group_by(state_name, YEAR4, REGION) %>% 
  summarise(total_deaths = sum(Fatal)) %>% 
  full_join(states) %>% 
  filter(!state_name == "Hawaii")

# Face geo of US deaths over time 2000 - 2019
ggplot(state_deaths, aes(x = YEAR4, y = total_deaths)) +
  geom_line() +
  facet_geo(~ state_name, scales = "free") +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  labs(title = "US Train Fatalities", subtitle = "2010-2019", x = "Year", y = "Deaths")
  

# Look at deaths by Region over time
ggplot(state_deaths, aes(x = as.factor(YEAR4), y = total_deaths)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ REGION, scales = "free") +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Regional Train Fatalities", subtitle = "2010 - 2019", x = "Year", y = "Deaths" )


# Look at age distribution of deaths by every age 2000 - 2019

ggplot(all_deaths, aes(x = AGE)) +
  geom_histogram(stat = "count")



# Create a dataset with just death observations from California
california_death <- all_deaths %>% 
  filter(state_name == "California") %>% 
  mutate(Month = case_when(as.numeric(IMO) %in% c(12,1,2) ~ "Winter",
                           as.numeric(IMO) %in% c(3,4,5) ~ "Spring",
                           as.numeric(IMO) %in% c(6,7,8) ~ "Summer",
                           as.numeric(IMO) %in% c(9,10,11) ~ "Fall"),
         week_day = dmy(C_date),
         wday = ymd(week_day) %>% 
           wday(label = TRUE))

# Look at age group distribution of deaths 
ggplot(california_death, aes(x = Age_group)) +
  geom_bar(stat = "count") +
  facet_wrap(~ YEAR4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "California deaths by Age Group", x = "Age Group", y = "Count")


# Look at age group distribution of deaths in California
ggplot(california_death, aes(x = as.factor(YEAR4), y = as.numeric(AGE))) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Casualty Age", x = "Year", y = "Age")



# deaths by season in California
california_death %>% 
  group_by(Month, YEAR4) %>% 
  summarise(month_deaths = sum(n())) %>% 
ggplot(aes(x = fct_reorder(Month, month_deaths, .fun = median, .desc = TRUE), y = month_deaths)) +
  geom_boxplot() +
  labs(title = "California Deaths by Season", subtitle = "2000 - 2018", x = "Season", y = "Number of Deaths")



# Data california deaths since 
cali <- all_data_2010 %>% 
  filter(state_name == "California",
         Fatal == 1) %>% 
  mutate(Narrative = paste(NARR1, NARR2, sep = "."))

# Create death Icon for map
deathicon <- iconList(Y = makeIcon("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/death_icon.jpg",
                                   "C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/death_icon.jpg", 24,24))


# Create a map to show all of California deaths
leaflet(cali) %>% setView(lng = -120, lat = 40, zoom = 4.5) %>% addTiles() %>% 
  addMarkers(lng = ~LONGITUD, lat = ~LATITUDE, icon = ~deathicon[CASFATAL], label = ~htmlEscape(Narrative),
             labelOptions = labelOptions(textsize = "10px")) %>% 
  addPopups(-117.384544, 33.198039, popup = "My near fatal accident",
            options = popupOptions(closeButton = FALSE),
            labelOptions(textsize = "10px")
  )




# Show timeline of major train accidents
# https://en.wikipedia.org/wiki/List_of_American_railroad_accidents link to major accidents

# All deaths 2000-2019 grouped by date to show major wrecks in all US
Major_accidents <- all_data %>% 
  filter(Fatal == 1) %>% 
  mutate(C_date = paste(DAY, IMO, YEAR4, sep = "-"),
         day = dmy(C_date)) %>% 
  group_by(day,RAILROAD ) %>% 
  summarise(total = sum(n()))  


Worst6_accidents <- all_data %>% 
  filter(Fatal == 1) %>% 
  mutate(C_date = paste(DAY, IMO, YEAR4, sep = "-"),
         day = dmy(C_date)) %>% 
  group_by(day,RAILROAD ) %>% 
  summarise(total = sum(n())) %>% 
  filter(total > 7)


ggplot(Major_accidents, aes(x = day, y = total)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0,30, by = 2)) +
  geom_vline(data=Worst6_accidents, mapping=aes(xintercept=date(day)), color= date(Worst6_accidents$day))
  
  
# All deaths 2000-2019 grouped by date to show major wrecks in California only
Major_accidents_cal <- all_data %>% 
  filter(Fatal == 1,
         state_name == "California") %>% 
  mutate(C_date = paste(DAY, IMO, YEAR4, sep = "-"),
         day = dmy(C_date)) %>% 
  group_by(day,RAILROAD ) %>% 
  summarise(total = sum(n())) 

Worst_accidents_cal <- all_data %>% 
  filter(Fatal == 1,
         state_name == "California") %>% 
  mutate(C_date = paste(DAY, IMO, YEAR4, sep = "-"),
         day = dmy(C_date)) %>% 
  group_by(day,RAILROAD ) %>% 
  summarise(total = sum(n())) %>% 
  filter(total > 3)


# Timeline of major accidents in california
ggplot(Major_accidents_cal, aes(x = day, y = total)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0,30, by = 2)) +
  geom_vline(data=Worst_accidents_cal, aes(xintercept=date(day), color = factor(RAILROAD,
             labels = c("Amtrak", "Metrolink (California)", "Union Pacific"))), linetype = "dashed") +
  theme(legend.position = "bottom") +
  guides(color=guide_legend("Railroad")) +
  labs(title = "Timeline of Major Accidents", subtitle = "California: 2000 - 2019", x = "Year", y = "Total Deaths") +
  geom_label(data = Worst_accidents_cal,
            aes(x = day , y = Worst_accidents_cal$total, label = day),
            nudge_x = 4,
            nudge_y = 3) 


# Create Heatmap of time of death in California
california_death %>% 
  mutate(Time = paste(TIMEHR, AMPM, sep = " ")) %>% 
  group_by(AMPM, TIMEHR) %>% 
  summarise(Fatalities = n()) %>% 
ggplot(aes(x = as.factor(TIMEHR), y = AMPM, fill = Fatalities)) +
  geom_tile(color = "white", size = .1, stat = "identity") +
  scale_fill_viridis() +
  labs(title = "When do deaths occur?", x = "Hour of day", y = "")


# Create barplot of week day deaths in California
california_death %>% 
  group_by(wday) %>% 
  summarise(Fatalities = n()) %>% 
  ggplot(aes(x = wday, y = Fatalities)) +
  geom_bar(stat = "identity") +
  labs(title = "Deaths by Day", subtitle = "2000 - 2018", x = " ", y = "Deaths")


























# Look at deaths per total miles since 2017 with inception of Brighline (BLF) what are other dangerous ones
# Display all BLF deaths on a spatial map
# Download operational data for railroads since 2017
Op_data_2017 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/Op_2017.csv") 

Op_data_2018 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/Op_2018.csv") 

Op_data_2019 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/Op_2019.csv") 


casualty_2017 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2017.csv") 
# Read in 2018 casualty data
casualty_2018 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2018.csv")
# Read in 2019 casualty data
casualty_2019 <- read_csv("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/Proj_data/casualty_2019.csv") 



# Join operation data from 2017-2019 together  
Op_2017_2019 <- full_join(Op_data_2017,Op_data_2018) %>% 
  group_by(RAILROAD) %>%
  summarise(miles = sum(TOTMI))

death_2017_2019 <- full_join(casualty_2019,casualty_2018) %>% 
  full_join(casualty_2017) %>%
  select(IYR,IMO,RAILROAD,TYPPERS,AGE,STATE,TYPRR,REGION,CASFATAL,DAY,YEAR4,TIMEHR,TIMEMIN,AMPM,COUNTY,CNTYCD,STCNTY,
         HZMEXPOS,TERMINAT,NARR1,NARR2,LATITUDE,LONGITUD) %>% 
  mutate(Fatal = case_when(CASFATAL == "Y" ~ 1,
                           CASFATAL == "N" ~ 0),
         STATE_NAME = fips(STATE, to = "Name")) %>% 
  filter(Fatal == 1) %>% 
  group_by(RAILROAD) %>% 
  summarise(total_deaths = sum(Fatal)) 


# Join operation and death data
deadly <- left_join(Op_2017_2019, death_2017_2019) %>% 
  filter(total_deaths > 0) %>% 
  mutate(death_rate = (miles/total_deaths))

# Data with only Brightline deaths
brightline <- all_data_2010 %>% 
  filter(RAILROAD == "BLF",
         YEAR4 > 2016,
         Fatal == 1) %>% 
  mutate(Narrative = paste(NARR1, NARR2, sep = "."))


# Create death Icon for map
deathicon <- iconList(Y = makeIcon("C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/death_icon.jpg",
                                   "C:/Users/User/Documents/Fall 2019/Math 335/M335_FA19_Norton_Tann/Semester_Project/death_icon.jpg", 24,24))

# Create a map to show all of Brightlines deaths
# Some of the points are off
leaflet(brightline) %>% setView(lng = -81, lat = 27, zoom = 5.5) %>% addTiles() %>% 
  addMarkers(lng = ~LONGITUD, lat = ~LATITUDE, icon = ~deathicon[CASFATAL], label = ~htmlEscape(Narrative),
             labelOptions = labelOptions(textsize = "10px"))























