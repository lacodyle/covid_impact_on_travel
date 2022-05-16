### DSC 465 --- Group Project ---
## Final Code 

#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("scales")
#install.packages("gridExtra")
#install.packages("ggridges")
#install.packages("ggpubr")
#install.packages("gapminder")
#install.packages("gganimate")
#install.packages("mapproj")
#install.packages("mapdata")
#install.packages("maps")
#install.packages("magrittr")
#install.packages("usdata")
#install.packages("reshape2")
#install.packages("ggforce")
#install.packages("cowplot")
#install.packages("ggtext")
#install.packages("geosphere")
#install.packages("sp")
#install.packages("gifski")
#install.packages("mapcan")
#install.packages("dplyr")

# General use libraries
library(tidyverse)
library(lubridate)

# Ridgeline/Violin plots
library(scales)
library(gridExtra) 
library(ggridges)
library(ggpubr)

# Animated Choropleth
library(gapminder)
library(gganimate)
library(mapproj)
library(mapdata)
library(maps)
library(magrittr)
library(usdata)
library(reshape2)
library(ggforce)
library(cowplot)
library(ggtext)
library(geosphere)
library(sp)
library(gifski)
library(mapcan)
library(dplyr)

### --- Dataframe setup --- ### 

# import the dataset
filename ='covid_impact_on_airport_traffic.csv'
df <- read_csv(filename,col_names=TRUE, show_col_types = FALSE)

# see the structure
str(df)
# view the dataframe
View(df)

# Extract Dates/Date Parts
df <- df %>%
  mutate(DateX = as.POSIXct(Date, format = '%Y-%m-%d'),
         Week = floor_date(Date,unit='week'),
         Month = factor(month(Date,TRUE)),
         MonthX = factor(month(Date,label=TRUE,abbr=FALSE),
                         levels = c("March", "April", "May", "June",
                                    "July", "August","September",
                                    "October", "November", "December"))
  )

# Add State/Province abbreviations
df <- df %>%
  mutate(stateAbb = str_sub(ISO_3166_2,-2),
         cityState = paste(City,stateAbb,sep=","))


### --- Ridgeline Plot --- ### 

# Create Ridgeline Plot for U.S. and Canada:
ggplot(df, aes(x = PercentOfBaseline, y = MonthX)) +
  geom_density_ridges(data=filter(df, Country=='United States of America (the)'), aes(fill='darkturquoise'), color = 'turquoise1', alpha=0.8, rel_min_height = 0.003, scale = 1.4) +
  geom_density_ridges(data=filter(df, Country=='Canada'), aes(fill='salmon1'), color = 'lightsalmon1', alpha=0.3, rel_min_height = 0.005, scale = 1.4) +
  scale_x_continuous(limits = c(0, 115), labels = scales::percent_format(scale = 1)) +
  scale_y_discrete(limits=rev(levels(df$MonthX)),
                   labels=c('Dec', 'Nov', 'Oct', 'Sep', 'Aug', 'Jul', 'Jun', 'May', 'Apr', 'Mar')) +
  labs (x ='Percentage of Airport Travel Relative to Pre-Covid Baseline Period',
        y = 'Month in 2020') +
  scale_fill_identity(name = 'Country',
                      breaks = c('darkturquoise', 'salmon1'),
                      labels = c('United States', 'Canada'),
                      guide = guide_legend(override.aes = list(color=c('darkturquoise', 'salmon1')))) +
  ggtitle(label='Level of Airport Travel in the United States and Canada in 2020 during COVID-19',
          subtitle='Distribution of Percentage of Airport Travel Over Time Across all Airports ') +
  theme(plot.title = element_text(face='bold'),
        axis.text = element_text(family='Arial', size = rel(1.25))) +
  theme_ridges(font_size = 16, font_family = 'Arial')


### --- Heat Map --- ### 

# custom palette - diverging red-yellow-blue, colorblind safe
cpal = c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')

# filter -> re-code -> ggplot
pCity <- df %>%
  filter(Country %in% c('United States of America (the)','Canada')) %>%
  mutate(Country = recode_factor(Country,
                                 'United States of America (the)' = 'U.S.A.'
  ),
  cityState= recode_factor(cityState,
                           'South San Francisco,CA' = 'S San Fran,CA',
                           'Urban Honolulu,HI'= 'Honolulu,HI')
  ) %>%
  ggplot(aes(x=Week, 
             y=reorder(cityState,PercentOfBaseline,fun=average),       
             fill=PercentOfBaseline))

# customize ggplot
plot_CityHeatMap <-pCity + geom_tile(alpha=0.4) + 
  # scale
  scale_x_date(breaks='1 month',date_labels = '%b') +
  # labels
  labs(title = 'COVID-19 Impacts on Airport Travel by City in the United States and Canada',
       subtitle = 'Comparing average airport traffic relative to baseline traffic by city, by week',
       caption = 'Dates covered: 2020-03-16 to 2020-12-02, summarized by week',
       x = NULL,
       y = NULL,
       fill='Traffic % \nof Baseline') +
  # theme
  theme_bw() + 
  scale_fill_gradientn(
    colors=cpal,
    values = NULL,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  ) + 
  theme(axis.title.y = element_text(face='bold',size = rel(1.1)),
        axis.text.x= element_text(face='bold',size = rel(1.1)),
        plot.title = element_text(face='bold',lineheight=0.9),
        plot.caption = element_text(hjust=0, face = 'italic'),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        strip.background = element_rect(fill = 'grey90',
                                        color = 'black',
                                        size=0.25),
        strip.text = element_text(face='bold', size = rel(1.2)),
        panel.grid.major = element_blank(),
        panel.grid.minor = NULL
  ) +
  # facet
  facet_grid(Country ~ .,
             scales = 'free_y')

# show the visualization
plot_CityHeatMap

# save the visualization
#ggsave('COVIDCityHeatmap.png',plot_CityHeatMap,width=9,height=5, scale=2,dpi=300)


### --- Violin Plots --- ### 

# Create DF for U.S. Airports:
us_airports <- df %>%
  filter(Country == 'United States of America (the)')

# Create DF for Canada Airports:
can_airports <- df %>%
  filter(Country == 'Canada')


# Manual Scale of Colors for States:
us_pal = c('#ece2f0', '#d0d1e6', '#a6bddb', '#a6bddb', '#a6bddb', '#67a9cf', '#67a9cf', '#67a9cf', '#67a9cf', '#3690c0', '#3690c0', '#02818a', '#02818a', '#02818a', '#016450') 
can_pal = c('#67a9cf', '#3690c0', '#02818a', '#02818a', '#016450', '#016450')

# Create Violin Plot Ordered by Percent of Baseline (US):
us_dist = us_airports %>%
  mutate(State = fct_reorder(State, PercentOfBaseline)) %>%
  ggplot(aes(y=reorder(State, PercentOfBaseline), x=PercentOfBaseline, fill=State)) +
  geom_violin(alpha = 0.8) +
  geom_boxplot(color = 'azure1', alpha=0.7, width=0.15, coef = 0, outlier.color = NA) +
  scale_x_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = us_pal)+
  labs (x ="Percentage of Airport Travel \n Relative to Pre-Covid Baseline Period",
        y = '') +
  ggtitle('Distribution of Percentage of Airport Travel in the U.S.') +
  theme(plot.title = element_text(face='bold', family = 'Arial', hjust=0.5),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = 'gray90'),
        panel.grid.minor = element_line(color = 'gray95'),
        axis.text.y = element_text(face="bold", family='Arial'),
        axis.text.x = element_text(face='bold', family='Arial', size=rel(1.2)),
        legend.position="bottom")


# Create Violin Plot Ordered by Percent of Baseline (Canada):
can_dist = can_airports %>%
  mutate(State = fct_reorder(State, PercentOfBaseline)) %>%
  ggplot(aes(y=reorder(State, PercentOfBaseline), x=PercentOfBaseline, fill=State)) +
  geom_violin(alpha = 0.8) +
  geom_boxplot(color = 'azure1', alpha=0.7, width=0.1, coef = 0, outlier.color = NA) +
  scale_x_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(name = 'Province', values = can_pal) +
  labs (x ="Percentage of Airport Travel \n Relative to Pre-Covid Baseline Period \n",
        y = '') +
  ggtitle('Distribution of Percentage of Airport Travel in Canada') +
  theme(plot.title = element_text(face='bold', family = 'Arial', hjust=0.5),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid.major = element_line(color = 'gray90'),
        panel.grid.minor = element_line(color = 'gray95'),
        axis.text.y =  element_text(face="bold", family='Arial'),
        axis.text.x = element_text(face='bold', family='Arial', size=rel(1.2)),
        legend.position="bottom")


# Display U.S. and Canada Distribution Side-By-Side:
grid.arrange(us_dist, can_dist, ncol = 2, 
             top=text_grob('Impact of COVID-19 on Airport Travel by State/Province in the United States and Canada in 2020',
                           face = 'bold', size = '16'))


### --- Animated Choropleth --- ### 
covidbyweek <- df %>%
  group_by(State,Country,Week) %>%
  summarise(POB = mean(PercentOfBaseline)) %>%
  ungroup() %>%
  mutate(WN = format.Date(Week, format='%m.%d.%y'),
         WN_2 = format.Date(Week, format='%m.%d.%y')
  )

names(covidbyweek)[3] <- "WeekNumber"

#US mainland map data
mainland <- map_data("state") %>%
  mutate(subregion = toupper(state2abbr(region)))

#covid_by_week is the dataset that will be used for the chloropleths
#abbreviating state data in covid, this is only for the US
covid_by_week <- covidbyweek %>%
  mutate(State_revised = state2abbr(State))

#map data for Hawaii
maps <- map_data("world")
hawaii <- subset(maps, subregion=="Hawaii") %>%
  mutate(region=subregion) %>%
  mutate(region=state2abbr(region))

#HAWAII COVID
covidHI = hawaii %>%
  left_join(covid_by_week ,by=c("region" = "State_revised"), all=TRUE)
covidHI <- na.omit(covidHI)

#MAINLAND COVID
covidUS = mainland %>%
  left_join(covid_by_week ,by=c("subregion" = "State_revised"), all=TRUE)
covidUS <- covidUS %>%
  mutate(WN = format.Date(WeekNumber, format='%m.%d.%y')) %>%
  filter(subregion != "UT")
covidUS <- covidUS %>%
  mutate(WN_2 = format.Date(WeekNumber, format='%m.%d.%y'))
covidUS <- na.omit(covidUS)

#Creating a seperate Chloropleth for Hawaii, and then using the draw_plot() feature to have it more visible in the mainland graphic

hawaii_map <- ggplot(aes(long,lat,group=group, fill = POB), data = covidHI) +
  geom_polygon(colour = "black") + theme_void() +
  panel_border(color = "black") +
  scale_fill_gradient2(low =  "#ece2f0", mid = "#a6bddb", high = "#1c9099", midpoint = mean(covidHI$POB)) +
  ggtitle("HI", ) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

#Lines 87 - 103 related to adding labels to the U.S. Map
#Read different articles/forums on how to approach this, original idea was to use the centroid points given in the dataset, however the points were pretty random
#Eventually saw an example on how to find the centroid for multiple factors
#First attempt can be found in lines ......


#Here, a function is created to find the centroid for all US states using the Polygon() function
statenames <- function(State)
{
  Polygon(State[c('long','lat')])@labpt
}

centroids <- by(covidUS, covidUS$State, statenames)
centroids2 <- do.call("data.frame", centroids)
centroids3 <- t(centroids2) %>%
  as.data.frame()

centroids_final <- rownames_to_column(centroids3, "State")
names(centroids_final)[2] <- "long"
names(centroids_final)[3] <- "lat"

centroids_final$short_state <- state2abbr(centroids_final$State)


#chloropleth of the U.S. mainland
us_map <- covidUS %>%
  ggplot(aes(long,lat,group=group, fill = POB)) +
  borders("state", colour = "black", fill = "whitesmoke") +
  geom_polygon(colour = "black") + theme_bw() +
  scale_fill_gradient2(low =  "#ece2f0", mid = "#a6bddb", high = "#1c9099", midpoint = mean(covidUS$POB)) +
  with(centroids_final, annotate(geom="text", x = long, y= lat, label = short_state, size = 3.25, fontface = "bold" ))

us_map

#USANIMATION is the mainland map with the Hawaii plot on the lower left hand corner, transition time is the variable WN_2
#from the cowplot library, using draw_plot so Hawaii is included with the states while also not compromising size
USANIMATION <- us_map + draw_plot(hawaii_map, width = 13, height = 7, x = -125, y = 24) +
  #transition_time(WN_2)+
  labs(title = "Airport Travel by State in the United States in 2020 during COVID-19",
       subtitle = "Average Percent of Baseline at: {frame_time}",
       fill = "Percentage of Airport Traffic Relative to Baseline",
       caption = "Note: States that are not represented in the data have been shaded in light grey") +
  theme(axis.text.x = element_blank(), #axes changes begins
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        text = element_text("Arial"), #adjusting font type
        plot.title = element_text(face = "bold", size = 21), #title is made bold
        plot.caption = element_markdown(face = "italic", vjust = 1,size = 12),
        plot.title.position = "plot",
        legend.position = "bottom", #legend changes
        legend.key.size = unit(1,'cm'),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 10),
        plot.subtitle = element_text(size = 19.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
#??valign
USANIMATION
typeof(USANIMATION)
#animating USANIMATION
USANIMATION_FINAL <- gganimate::animate(USANIMATION, nframes = 450, height = 600, width = 900,
                                        renderer=file_renderer())
warnings()
#saving animation
anim_save(filename = "US_Animation.gif", animation = USANIMATION_FINAL)


#CANADA ANIMATION PROCESS
#Discovered a library in R, mapcan that provides Canada's map with the provinces outlined
library(mapcan)
library(dplyr)
Canada_States <- mapcan(boundaries = province, type = standard)

CAN <- Canada_States %>%
  mutate(region = pr_english) %>%
  mutate(subregion = pr_alpha) %>%
  as.data.frame() %>%
  mutate(as.numeric(Canada_States$group))

CAN1 <- subset(CAN, select = c("long","lat","group","order","region","subregion"))
CANADA_BORDER <- CAN1 %>%
  filter(region == "Saskatchewan")
ggplot(aes(x=long, y=lat, group = group), data = CANADA_BORDER) + geom_polygon(colour = "black")
CANcovid <- CAN1 %>%
  left_join(covidbyweek, by=c("region" = "State"), all=TRUE)
CANcovid <- na.omit(CANcovid)

canada_centroids <- by(CANcovid, CANcovid$region, statenames)
canada_centroids2 <- do.call("data.frame", canada_centroids)
canada_centroids3 <- t(canada_centroids2) %>%
  as.data.frame()
canada_centroids4 <- rownames_to_column(canada_centroids3)
names(canada_centroids4) <- c("Province", "long","lat")
#adjusting british columbia and nova scotia

canada_centroids4$Province [canada_centroids4$Province == "British.Columbia"] = "British Columbia \nBC"
canada_centroids4$Province [canada_centroids4$Province == "Alberta"] = "Alberta \nAB"
canada_centroids4$Province [canada_centroids4$Province == "Manitoba"] = "Manitoba \nMB"
canada_centroids4$Province [canada_centroids4$Province == "Ontario"] = "Ontario \nONT"
canada_centroids4$Province [canada_centroids4$Province == "Quebec"] = "Quebec \nQC"
canada_centroids4$Province [canada_centroids4$Province == "Nova.Scotia"] = "NS"

#Chloropleth for Canada
CANmap <- ggplot(CANcovid, aes(x = long, y=lat, group=group, fill = POB)) +
  geom_polygon(colour = "black") + #transition_time(WN_2) +
  labs(title = "Airport Travel by Province in Canada in 2020 during COVID-19",
       subtitle = "Average Percent of Baseline at: {frame_time}",
       fill = "Percentage of Airport Traffic Relative to Baseline",
       x = "",
       y = "",
       caption = "Note: States that are not represented in the data have been shaded in light grey") +
  theme_bw() +
  scale_fill_gradient2(low =  "#ece2f0", mid = "#a6bddb", high = "#1c9099", midpoint = mean(CANcovid$POB)) +
  theme(text = element_text("Arial"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", size = 19.5),
        plot.subtitle = element_text(size = 19),
        legend.position = "bottom", #legend changes
        legend.key.size = unit(1,'cm'),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.caption = element_text(face = "italic", hjust = 1, vjust = 0.5, size = 12)) +
  with(canada_centroids4, annotate(geom = "text", x = long, y = lat, label = Province, size = 3, fontface = "bold")) +
  with(CANADA_BORDER, annotate(geom = "polygon", x = long, y = lat, group = group, fill = "whitesmoke", colour = "black"))

CAN_ANIMATION <- gganimate::animate(CANmap, nframes = 450, height = 600, width = 900)

#saving animation
anim_save(filename = "CAN_Amination.gif", CAN_ANIMATION)

