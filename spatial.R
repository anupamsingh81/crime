# load level 1 india data (india with states)
# If we want to get and map district level data then we need to use the level 2 data as follows :
# To move to the district, sub-division ( or taluk) level we need to use the level three data file

library(sp)
library(RColorBrewer)
library(tidyverse)

# http://visual.yantrajaal.com/2015/05/using-r-for-maps-of-india-state.html

ind= readRDS("IND_adm1.rds")


spplot(ind, "NAME_1", scales=list(draw=T), colorkey=F, main="India")

ind$NAME_1


testd = tibble(states=as.character(mr13$V1),murder=as.numeric(as.character(mr13$V2)))

tester = tibble(states=ind$NAME_1)

#test

tester$states
tester$states = toupper(tester$states) # upper case ensures match

testd$states

tester %>% filter(states %in% testd$states) # check

library(tidyr)
testy = right_join(testd,tester,by="states") %>%  # keep only smaller subset of testd by right join
  replace_na(list(murder=0)) # replace_na in urder by zero

ind$NAME_1 = as.factor(ind$NAME_1)
ind$fake.data=testy$murder

spplot(ind,"NAME_1",  col.regions=rgb(0,ind$fake.data,0), colorkey=T, main="Indian States")
# doesnt work as rgb for random

#colouring the states with  with range of colours acc

col_no = as.factor(as.numeric(cut(ind$fake.data, c(0,500,1000,1500,2000,5500)))) # create a variable colno with cut off can be done with dplyr ntile
levels(col_no) = c("<20%", "20-40%", "40-60%","60-80%", ">80%") # levels
ind$col_no = col_no
library(RColorBrewer)
myPalette = brewer.pal(5,"Greens")

#spplot(ind,"NAME_1",  col.regions=rgb(0,ind$fake.data,0), colorkey=T, main="Indian States")


spplot(ind, "col_no", col=grey(.9), col.regions=myPalette, main="Indian States")


##ggplot2

# the dataframe is in ind@data

download.file("http://biogeo.ucdavis.edu/data/diva/adm/IND_adm.zip", 
              destfile = "IND_adm.zip")
unzip("IND_adm.zip", overwrite = TRUE)

getwd()
library(maptools)
library(rgdal)
states.shp <- readOGR("IND_adm1.shp")
class(states.shp)

library(sf)

getwd()
nc <- st_read('IND_adm1.shp')

library(ggplot2)

ggplot(nc) +
  geom_sf(aes(fill = AREA)) +
  scale_fill_viridis("Area") +
  ggtitle("Area of counties in North Carolina") +
  theme_bw()

## very important resource http://strimas.com/r/tidy-sf/
#geom_sf removes need of fortify

india = ind@data