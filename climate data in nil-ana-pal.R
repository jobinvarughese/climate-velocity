
setwd("H:/Drive 2/iiser/4th Sem/Vijay Ramesh/2020_in prep_historical climate and landscape-20200729T171046Z-001/2020_in prep_historical climate and landscape/data/shapefiles")

## Analyzing climate data across the Western Ghats (Data from IIT-Guwahati - Vimal Mishra)



### Steps to follow



#### Step 1. Load the Nil_Ana_Pal shapefile ####



library(raster)

library(sf)

library(rgdal)

library(dplyr)



WG <- st_read("Nil_Ana_Pal.shp") 




## Making sense of the data using ggplot

library(ggplot2)

library(cowplot)



(vis <- ggplot(data = WG) +
   
   geom_sf(fill = "antiquewhite1") +
   
   coord_sf(xlim = c(76.25, 78), ylim = c(9.75,11.75)) +
   
   xlab("Longitude")+ ylab("Latitude")+
   
   theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                         
                                         size = 0.5), panel.background = element_rect(fill = "aliceblue"), 
         
         panel.border = element_rect(fill = NA)))


#### Step 2: Plot the points first and then overlay the grid ####

#### These were coordinates for locations sent to me by Vimal Mishra

#### Are these points locations of weather stations? 



points <- data.frame(long = c(76.75,77.25,76.75,77.25,77.75,76.75,76.25,76.25,77.25,76.75,76.25,76.75,77.25),
                     
                     lat = c(11.25,10.75,10.25,10.25,10.25,11.75,11.25,10.75,11.25,10.75,10.25,9.75,9.75)) %>% st_as_sf(coords=c('long','lat'), crs=4326)


# Create 0.5 degree grid

# Note: I made a grid by extending the bounds of your points out by half a cell size in all directions:

cellsize = 0.5

grid_05 <- st_make_grid(st_as_sfc(
  
  st_bbox(points) + 
    
    c(-cellsize/2, -cellsize/2,
      
      cellsize/2, cellsize/2)),
  
  what="polygons", cellsize=cellsize) %>% st_sf(grid_id = 1:length(.))


# Create labels for each grid_id

grid_lab <- st_centroid(grid_05) %>% cbind(st_coordinates(.))


# View the sampled points, polygons and grid

ggplot() +
  
  geom_sf(data = WG, fill = 'white', lwd = 0.05) +
  
  geom_sf(data = points, color = 'red', size = 1.7) + 
  
  geom_sf(data = grid_05, fill = 'transparent', lwd = 0.3) +
  
  geom_text(data = grid_lab, aes(x = X, y = Y, label = grid_id), size = 2) +
  
  coord_sf(datum = 4326)  +
  
  labs(x = "") +
  
  labs(y = "")



# which grid square is each point in?

points %>% st_join(grid_05, join = st_intersects) %>% as.data.frame


library(tidyverse)

library(readr)


## Data comes in the following format:

## Precipitation (mm), Max.Temperature (Celsius), Min.temperature (Celsius), Wind (m/s)



library(data.table)

library(tools)
library(readr)
# Setting the path where the directory of files exist
#setwd("H:/Drive 2/iiser/4th Sem/Vijay Ramesh/2020_in prep_historical climate and landscape-20200729T171046Z-001/2020_in prep_historical climate and landscape/data/climate/data-westernGhats-IITG")

#file_list <- list.files(full.names=F)
file_list <- list.files("H:/Drive 2/iiser/4th Sem/Vijay Ramesh/2020_in prep_historical climate and landscape-20200729T171046Z-001/2020_in prep_historical climate and landscape/data/climate/data-westernGhats-IITG_2", full.names=T)
# Looping through the data and reading in the files as a list of lists

dataset <- NULL

for (i in 1:length(file_list)){
  
  dataset[[i]] <- read_table2(file_list[i], col_names = FALSE)
  
}  


# Naming each dataset by lat/long as the ID

names(dataset) <- basename(file_path_sans_ext(file_list)) # Using a function from the tools package

# Naming each column for each dataframe within a list

colnames <- c("Precipitation","Max.Temp","Min.Temp","Wind") 

  
dataset <- lapply(dataset, setNames, colnames)
setwd("H:/Drive 2/iiser/4th Sem/Vijay Ramesh/2020_in prep_historical climate and landscape-20200729T171046Z-001/2020_in prep_historical climate and landscape/data/climate/data-Nilgiris-Anamalais-IITG")

dates <- read_table2("dates.csv",col_names=F)

colnames(dates) <- c("Year","Month","Day")

dataset <- lapply(dataset,bind_cols, dates)
rm(dates)


# Creating a summary list of lists that takes the Mean Max.Monthly temp, Mean Min Monthly Temp and Monthly Sum of Precipitation
summary <-  NULL
a <- names(dataset)

for(i in 1:length(dataset)){
  summary[[i]] <- dataset[[a[i]]] %>% group_by(Year,Month) %>% summarise(Mean_Max_Temp = mean(Max.Temp), Mean_Min_Temp = mean(Min.Temp)
                                                                         ,Monthly_Precip = sum(Precipitation), Ann_Min_Temp=min(Min.Temp), Ann_Max_Temp=max(Max.Temp))
}
rm(dataset)


# Naming each dataset by lat/long as the ID
names(summary) <- basename(file_path_sans_ext(file_list)) # Using a function from the tools package

#### Step 4. Visualizations for Climate data - Exploratory Data Analysis ####

# Before we make visualizations, it is easier to bind_rows with a location identified column

data <- NULL
for(i in 1:length(summary)){
  a <- summary[[i]] %>% mutate(location = basename(file_path_sans_ext(file_list[i])))
  data <- bind_rows(a,data)
}
rm(a,summary)


# Renaming location name for ease of plotting

# Eastern Locations
data$location[data$location=="data_11.25_76.75"] <- "Central Nilgiris & Eastern Spur_11.25_76.75"
data$location[data$location=="data_10.75_76.75"] <- "Lower Coonoor + Palghat Gap_10.75_76.75"
data$location[data$location=="data_11.75_76.75"] <- "Top of Nilgiris(Mysore)_11.75_76.75"
data$location[data$location=="data_11.25_77.25"] <- "Further East of Nilgiris_11.25_77.25"
data$location[data$location=="data_10.75_77.25"] <- "Top of Anamalais(Plains)_10.75_77.25"
data$location[data$location=="data_10.25_77.25"] <- "Central Anamalais_10.25_77.25"
data$location[data$location=="data_10.25_77.75"] <- "Palanis_10.25_77.75"

# Western Locations
data$location[data$location=="data_11.25_76.25"] <- "Western Nilgiris (Parts of O Valley)_11.25_76.25"
data$location[data$location=="data_9.75_77.25"]<- "MunnarIdukki_9.75_77.25"
data$location[data$location=="data_10.75_76.25"] <- "Western Spur of Nelliampathies_10.75_76.25"
data$location[data$location=="data_10.25_76.25"] <- "Western Coast_Anamalais_10.25_76.25"
data$location[data$location=="data_10.25_76.75"] <- "Valparai_10.25_76.75"
data$location[data$location=="data_9.75_76.75"] <- "South of Valparai_9.75_76.75"

## 1. Line plots of Temp (Min and Max) and Precipitation for each month across all locations over 1870-2018 

## Min and Max Temp plots

for(i in 1:12){
  a <- ggplot(filter(data, Month==i), aes(x=Year)) +
    geom_line(aes(y=Mean_Min_Temp), colour="green") +
    geom_line(aes(y=Mean_Max_Temp), colour="brown") +
    geom_line(aes(y=Ann_Min_Temp), colour="blue") +
    geom_line(aes(y=Ann_Max_Temp), colour="red") +
    geom_smooth(aes(y=Mean_Min_Temp, x=Year))+
    geom_smooth(aes(y=Mean_Max_Temp, x=Year))+
    geom_smooth(aes(y=Ann_Min_Temp, x=Year))+
    geom_smooth(aes(y=Ann_Max_Temp, x=Year))+
    facet_wrap(~location, scales = "free") +
    scale_x_continuous(breaks=seq(1870,2018,5)) +
    theme_bw() + theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "") +
    labs(y = "")
  
  dirname <- "H:\\Drive 2\\iiser\\4th Sem\\Vijay Ramesh\\Temp\\"
  
  png(filename= file.path(dirname, paste("Month",i, ".png", sep = "")), 
      units="px", 
      width=1920, 
      height=1137,
      res=96,
      type="cairo")
  
  print(a)
  dev.off()
}

## Precipitation plots

for(i in 1:12){
  a <- ggplot(filter(data, Month==i), aes(x=Year)) +
    geom_line(aes(y=Monthly_Precip), colour="black") +
    geom_smooth(aes(y=Monthly_Precip, x=Year))+
    facet_wrap(~location, scales = "free") +
    scale_x_continuous(breaks=seq(1870,2018,5)) +
    theme_bw() + theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "") +
    labs(y = "")
  
  dirname <- "H:\\Drive 2\\iiser\\4th Sem\\Vijay Ramesh\\Prec\\"
  
  png(filename= file.path(dirname, paste("Month",i, ".png", sep = "")), 
      units="px", 
      width=1920, 
      height=1137,
      res=96,
      type="cairo")
  
  print(a)
  dev.off()
}

#to get lat long as column for each place in data
data1<-data
data1%>%separate(location,c("name","lat","long"),"_")
data1
data1<-data1%>%separate(location,c("name","lat","long"),"_")
data1
data1<-data1[-6]#remove the column 6
head(data1)

data1$lat<-as.numeric(data1$lat)
data1$long<-as.numeric(data1$long)
head(data1)
unique(data1$lat)
max(data1$lat)
min(data1$lat)

# to get annual min temp of 1988 and 2019 barplot


for(i in 1:12){
  b <- ggplot(filter(data%>%filter(Year%in%c("1988","2018")), Month==i), aes(x=as.factor(Year))) +
    geom_boxplot(aes(y=Ann_Min_Temp), colour="black")
  facet_wrap(~location, scales = "free") 
  theme_bw() + theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "") +
    labs(y = "")
  dirname <- "H:\\Drive 2\\iiser\\4th Sem\\Vijay Ramesh\\Practice\\"
  
  png(filename= file.path(dirname, paste("Month",i, ".png", sep = "")), 
      units="px", 
      width=900, 
      height=500,
      res=96,
      type="cairo")
  
  print(b)
  dev.off()
}



data[,1]
data[1,]
data[data$Year==c(1988),]

str(dat)
dat
view(data[data$Year==c(2018),])
dat<-data%>%filter(Year%in%c("2018","1988"))


#grouping by seasons

sum_season <-  NULL
a <- names(dataset)

for(i in 1:length(dataset)){
  sum_season[[i]] <- dataset[[a[i]]] %>% group_by(Year) %>% summarise(Max_Summer = max(Max.Temp), Min_Winter = min(Min.Temp) )
}

names(sum_season) <- basename((file_list))
dat_season <- NULL
for(i in 1:length(sum_season)){
  a <- sum_season[[i]] %>% mutate(location = basename(file_path_sans_ext(file_list[i])))
  dat_season <- bind_rows(a,dat_season)
}


## season temp plots

ggplot(dat_season,aes(x=Year)) +
  geom_smooth(aes(y=Min_Winter), colour="blue") +
  geom_smooth(aes(y=Max_Summer), colour="red") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "") +
    labs(y = "")
  
  dirname <- "H:\\Drive 2\\iiser\\4th Sem\\Vijay Ramesh\\Prec\\"
  
  png(filename= file.path(dirname, paste("Month",i, ".png", sep = "")), 
      units="px", 
      width=1920, 
      height=1137,
      res=96,
      type="cairo")
  
  print(a)
  dev.off()
