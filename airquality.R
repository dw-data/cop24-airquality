setwd("/Users/lc_admin.schachtk/Documents/DW/086_088_Air quality")
library(tidyverse)
#source("../Templates/chart_template.R")

#air = read.csv("aap_air_quality_database_2018_v13_latest_pm10.csv", sep=";", stringsAsFactors = F)
air = read.csv("data/aap_air_quality_database_2018_v13_latest_pm10.csv", sep=",", stringsAsFactors = F)

#write list of cities to file
air %>% group_by(region = dw_region, country, city) %>% summarise() %>% write.csv("air_quality_2018_cities_list.csv", row.names = F, na="")


##### Function for creating strip plot #####

#airdata:     Data frame with air quality data
#hightlight:  Character vector with city names to hightlight in the chart
#katowice:    TRUE or FALSE. Should Katowice be highlighted?
#file_ending: Character. Is attached to file name to explain which part of the data is shown
makeStripPlot = function(airdata, highlight, katowice = T,
                         file_ending = "all", ylimit = ceiling(max(tmp$pm10.mean)/100)*100){
  
  #Highlight Katowice
  if(katowice) { highlight = c(highlight, "Katowice") }
  #Mark cities to highlight in dataset
  tmp = airdata %>% select(c(1:3,6,7,9)) %>% filter(city != c("Tetovo")) %>% 
    mutate(group = ifelse(city %in% highlight, "col","grey"))
  
  #Create chart
  ggplot(tmp) +
    #Make grey strips
    geom_segment(data = tmp %>% filter(group == "grey"),
                 aes(x=1, xend = 2, y = pm10.mean, yend = pm10.mean), color = dw_grey[12], size = 0.5) +
    #Make hightlighted strips
    geom_segment(data = tmp %>% filter(group == "col"),
                 aes(x=1, xend = 2, y = pm10.mean, yend = pm10.mean), color = dw_info[2], size = 2) +
    #Annotate highlighted strips
    geom_text(aes(x = 2, y = pm10.mean, label = ifelse(group=="col",city,"")), size = 25, hjust = 0, nudge_x = 0.25) +
    #Set limits and breaks for y axis
    scale_y_continuous(limits = c(0,ylimit+30), breaks = c(20,seq(0,ylimit,100))) +
    scale_x_continuous(limits = c(1, 3.5))# + 
    #Theme settings
    #theme_dw()
  #Save
  ggsave(paste0("plots/original/airquality_strip_",file_ending,".svg"), device = "svg", scale = 10, width= 70, height= 80, units="mm")

}




#### Global chart #####
# Display all cities, with megacities highlighted
megacities = c("Beijing", "Buenos Aires", "Delhi", "Dhaka", "Greater Cairo", "Istanbul", "Mexico City", "Mumbai", "Sao Paulo", "Shanghai")
makeStripPlot(air, megacities, katowice = T,
              file_ending = "all_2", ylimit = 300)


#### By continent or region ####
air %>% group_by(continent,country) %>% summarise() %>% View
air %>% group_by(continent) %>% summarise(n = n())


# One chart per continent ####
cities_continents = list(
  "Europe" = c("Paris","London","Madrid","Rome","Kyiv"),
  "Asia" = c("Beijing","Delhi","Dhaka","Shanghai","Mumbai","Peshawar","Rawalpindi"),
  "Americas" = c("Mexico City", "Sao Paulo","Lima", "Brasilia", "Santiago", "Caracas", "Buenos Aires"),
  "Oceania" = c("Sydney","Canberra","Wellington"),
  "Africa" = c("Greater Cairo","Johannesburg","Casablanca","Accra","Tunis")
)

for(i in 1:length(cities_continents)){
  tmp = air %>% filter(city == "Katowice" | continent == names(cities_continents)[i])
  makeStripPlot(tmp, cities_continents[[i]], katowice = T, file_ending = paste0("continent_",names(cities_continents)[i]))
}

#Pro DW Region eine Grafik ####
cities_dwregions = list(
  "Eastern Europe" = c("Moscow","Kyiv", "Bucharest","Minsk","Budapest","Warsaw"),
  "Western and Southern Europe"  = c("Paris","London","Madrid","Rome"),
  "Middle East" = c("Tehran","Istanbul", "Baghdad","Riyadh","Amman", "Hamad Town"),
  "Americas" = c("Mexico City", "Sao Paulo","Lima", "Bogota", "Santiago", "Caracas", "Buenos Aires"),
  "Oceania" = c("Sydney","Canberra","Wellington"),
  "Asia" = c("Beijing","Delhi","Dhaka","Shanghai","Mumbai","Peshawar","Rawalpindi"),
  "Africa" = c("Greater Cairo","Johannesburg","Casablanca","Accra","Tunis")
)

for(i in 1:length(cities_dwregions)){
  tmp = air %>% filter(city == "Katowice" | dw_region == names(cities_dwregions)[i])
  makeStripPlot(tmp, cities_dwregions[[i]], katowice = T,
                file_ending = paste0("region_",names(cities_dwregions)[i]))
}; rm(i)



##### Special requests #####

#Nur Städte in der Türkei
cities_turkey = c("Mus", "Sirnak","Ankara","Izmir", "Istanbul")
tmp = air %>% filter(city == "Katowice" | country == "Turkey")
makeStripPlot(tmp, cities_turkey, katowice = T, file_ending = "turkey")


#Strip plots nach Kontinent
tmp = air %>% select(c(1,6,7,9)) %>% mutate(group = ifelse(city %in% c(megacities, "Katowice"), "col","grey"))
ggplot(tmp) +
  geom_segment(aes(x=1, xend = 2, y = pm10.mean, yend = pm10.mean, color = group), size = 2) +
  facet_grid(cols = vars(continent)) +
  geom_text(aes(x = 2, y = pm10.mean, label = ifelse(group=="col",city,"")), size = 20, hjust = 0, nudge_x = 0.25) +
  scale_y_continuous(limits = c(0,320), breaks = c(20,100,200,300)) +
  scale_x_continuous(limits = c(1, 3.5)) + 
  scale_color_manual(values = unname(c(dw_info[1], dw_grey[12]))) +
  theme_dw()
ggsave("plots/original/airquality_strip_by_continent.svg", device = "svg", scale = 10, width= 70, height= 80, units="mm")
