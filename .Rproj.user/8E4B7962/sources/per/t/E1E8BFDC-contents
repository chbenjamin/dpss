library(tidycensus)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(sf)
library(gganimate)
library(gifski)
library(png)
library(plotly)
library(maps)
library(maptools)
library(mapproj)

# Getting data from census and FBI
var <- load_variables(2016,"acs5")
crimes <- read_xls("Table_12_Agency_Hate_Crime_Reporting_by_State_2017.xls")
crimes <- mutate(crimes, crimes_per = number/Population_covered * 100000)
data <- get_acs(
  geography = "state",
  year = 2016,
  variables = c(unemployed_pop = "B27011_014",
                median_income = "B19013_001",
                total_pop = "B01003_001", 
                hs_pop = "B06009_003",
                white_poor_pop = "B17001A_002",
                gini = "B19083_001",
                white_pop = "B01001A_001",
                non_citizen = "B05001_006",
                non_citizen_total = "B05001_001",
                hs_pop_total = "B06009_001",
                unemployed_pop_total = "B27011_002",
                white_poor_pop_total = "B17001_001"
                ),
  geometry = TRUE,
  shift_geo = TRUE)
data <- data %>%
  select(-moe) %>%
  spread(key = "variable", value = "estimate") %>%
  mutate(
    unemployed_per = unemployed_pop/unemployed_pop_total,
    hs_per = hs_pop/hs_pop_total,
    white_poor_per = white_poor_pop/white_poor_pop_total,
    non_white_per = 1 - (white_pop/total_pop),
    non_citizen_per = non_citizen/non_citizen_total
  ) %>%
  left_join(crimes, by = c("NAME" = "Participating_state")) %>%
  filter(NAME != "Puerto Rico" & NAME != "Hawaii") 



slave <- read.csv("50_us_states_all_data.csv")
data <- left_join(data, slave, by = c("NAME" = "state"))
lgbt <- read_xls("LGBT.xls", col_names = FALSE)
data <- left_join(data, lgbt, by = c("NAME" = "...1"))
reg <- lm(crimes_per ~ median_income + unemployed_per + hs_per + white_poor_per + non_white_per +non_citizen_per + gini + if_slave + ...2, data = data)
summary(reg)

cor.test(data$...2, data$if_slave.x)


# Plotting map
data <- mutate(data, qtile = factor(ntile(crimes_per, 5)))
data %>%
  ggplot()+
  geom_sf(aes(fill = qtile)) +
  theme_void() +
  scale_fill_brewer(palette = "YlGnBu", name = "")+
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )+
  labs(
    title = "HATE CRIMES IN THE U.S.A",
    caption = "Source: census",
    subtitle = "Average hate crimes per 100,000 residents, 2017"
  )

# Plotting dynamic map
year <- 2008:2017
for (i in year) {
  if (i == 2008){
    crimes_year <- read_xls("2008.xls", range = "A5:E56", col_names = FALSE)
    crimes_year <- crimes_year %>%
      select(...1, ...3, ...5)
    names(crimes_year)[1:3] <- c("states","2008pop","2008crimes")
  }
  else{
    later <- read_xls(paste(i,".xls", sep = ""), range = "A5:E56", col_names = FALSE)
    later <- later %>%
      select(...1, ...3, ...5)
    names(later)[1:3] <- c("states",paste(i, "pop", sep = ""),paste(i, "crimes", sep = ""))
    crimes_year <- left_join(crimes_year, later, by = "states")
  }
}
for (i in 1:10) {
    crimes_year[21+i] <- (crimes_year[1+2*i]/crimes_year[2*i])*100000
    names(crimes_year)[21+i] <- paste(i+2007)
}
crimes_year <- crimes_year %>%
  select(1, 22:31) 
crimes_year <- gather(crimes_year, key = "year", value = "number", -states)
crimes_year <- filter(crimes_year, !is.na(number))
geometry <- select(data, NAME, geometry)
geometry <- left_join(geometry, crimes_year, by = c("NAME" = "states"))
geometry$year <- as.numeric(geometry$year)

class(geometry$year)

plot_maker <- function(x) {
geometry %>%
  filter(year == x) %>%
  ggplot() +
  geom_sf(aes(fill = number)) +
    theme_void() +
    scale_fill_distiller(palette = "Spectral", 
                         name = "", 
                         breaks = c(2.5,5,7.5),
                         limits = c(0, 10)
                         )+
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "italic")
    )+
    labs(
      title = "HATE CRIMES IN THE U.S.A",
      caption = "Source: FBI",
      subtitle = paste("Average hate crimes per 100,000 residents ", x)
    )
  ggsave(str_c("plot", x, ".png"))
}
  
map(2008:2017, plot_maker)


# line plot
year <- 2008:2017
for (i in year) {
  if (i == 2008){
    crimes_year_total <- read_xls("2008.xls", range = "C4:E4", col_names = FALSE)
    crimes_year_total <- crimes_year_total %>%
      select(...1, ...3)
    names(crimes_year_total)[1:2] <- c("2008pop","2008crimes")
  }
  else{
    later_total <- read_xls(paste(i,".xls", sep = ""), range = "C4:E4", col_names = FALSE)
    later_total <- later_total %>%
      select(...1, ...3)
    names(later_total)[1:2] <- c(paste(i, "pop", sep = ""),paste(i, "crimes", sep = ""))
    crimes_year_total <- cbind(crimes_year_total, later_total)
  }
}
for (i in 1:10) {
  crimes_year_total[20+i] <- (crimes_year_total[2*i]/crimes_year_total[2*i-1])*100000
  names(crimes_year_total)[20+i] <- paste(i+2007)
}
crimes_year_total <- select(crimes_year_total, 21:30)
crimes_year_total <- gather(crimes_year_total, key = "year", value = "number")
crimes_year_total$year <- as.numeric(crimes_year_total$year)
crimes_year_total %>%
  ggplot()+
  geom_line(aes(x = year, y = number), color = "#7593d9", size = 1.5) +
  geom_point(aes(x = year, y = number), size = 2, color = "#0e3388") +
  scale_x_continuous(breaks = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))+
  theme_bw(base_size = 11)+
  labs(
    title = "Hate Crimes in the United States",
    subtitle = "Average hate crimes per 100'000 people in each year", 
    caption = "Source: FBI  "
  )

######################### non-map plot
data$crimes_per[which(data$crimes_per >= 1)] <- 'big'
data$crimes_per[which(data$crimes_per < 1)] <- 'small'
data$crimes_per <- as.factor(data$crimes_per)
df4166
p <- plot_ly(data, x = ~gini, y = ~median_income, z = ~lgbt, color = ~crimes_per, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'gini'),
                      yaxis = list(title = 'median income'),
                      zaxis = list(title = 'lgbt')))
p



data$crimes_per[which(data$crimes_per >= 1.968)] <- 'big'
data$crimes_per[which(data$crimes_per < 1.968)] <- 'small'
data$crimes_per <- as.factor(data$crimes_per)
plot <- plot_ly(data, x = ~gini, y = ~median_income, z = ~lgbt, color = ~crimes_per, colors = c('#800000','#0062ac'),text = ~paste('state:', NAME, '<br>non white:', non_white_per, '<br>non citizen', non_citizen_per, '<br>high school:', hs_per, '<br>unemployment:', unemployed_per))%>%   
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'gini',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwidth = 2),
                      yaxis = list(title = 'median income',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwidth = 2),
                      zaxis = list(title = 'lgbt',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwidth = 2)),
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)',
         title = "50 States hate crimes per 100k population")
plot




map("World", fill = TRUE)
x=readShapePoly('bou2_4p.shp')

library(ggplot2)

library(mapproj)
