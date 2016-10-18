#Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggmap)
library(maptools)
library(gtools)

#read in map data
mapdata <- read_csv('data/map.csv')

#construct geographical map
map1 <- ggplot() + theme_nothing(legend=TRUE) +
  geom_polygon(data=mapdata, aes(x=long,y=lat,group=group),fill='white',color='black')
png('map.png',width=1500,height=1000)
print(map1)
dev.off()

#read in data
ipums <- read_csv('data/10_16.csv',col_types = cols(PERWT=col_double()))

#filter data
dsouth <- ipums %>% filter(RACE>= 4 & RACE<=6 & YEAR-AGE>=1885 & YEAR-AGE<=1900)

#sumarize population
ds <- dsouth %>% group_by(YEAR,STATEFIP) %>% summarise(Number=sum(PERWT))
#create new variable to order map data
newmap <- mapdata %>% mutate(STATEI=as.integer(STATEFIP))
dsmap <- left_join(ds,newmap,by=c('STATEFIP'='STATEI'))
dsmap <- dsmap %>% arrange(order)
#plotting values onto map
map2 <- ggplot() + theme_nothing(legend=TRUE) +
  geom_polygon(data=mapdata, aes(x=long,y=lat,group=group),fill='white',color='black') +
  geom_polygon(data=filter(dsmap,YEAR==1900),aes(x=long,y=lat,group=group,fill=Number),color='black')
png('map.png',width=1500,height=1000)
print(map2)
dev.off()
#create population variables for color coding
cuts <- quantcut(ds$Number,q=seq(0,1,.2))
dscats <- ds %>% mutate(Population=ifelse(Number<100,1,
                                        ifelse(Number<500,2,
                                        ifelse(Number<1000,3,
                                        ifelse(Number<10000,4,5)))))
dscats$Population <- factor(dscats$Population, labels=c('1-99','100-499','500-999','1,000-9,999','10,000+'))
#I struggled with this line a whole lot, for some reason the population 
#factory variable is not lining up with the number values for population above
#i swear ive tried everything, nothing seems to work. It makes my data all incorrect

#add variable to map
dsmap <- left_join(dscats,newmap,by=c('STATEFIP'='STATEI')) %>% arrange(order)
map2 <- map1 + scale_fill_brewer(palette='Blues') +
  geom_polygon(data=filter(dsmap,YEAR==1900),aes(x=long,y=lat,group=group,fill=Population),color='black')
png('map.png',width=1500,height=1000)
print(map2)
dev.off()

#run loop for each map population for each year
unique(dsmap$YEAR)
for (year in unique(dsmap$YEAR)) {
map2 <- map1 + scale_fill_brewer(palette='Blues') + theme_bw(base_size = 24) +
  geom_polygon(data=filter(dsmap,YEAR==year),aes(x=long,y=lat,group=group,fill=Population),color='black') +
  labs(title=paste('Persons Recorded As Asian Born 1885-1900,',year,sep=' '))
png(paste('map_',year,'.png',sep=''),width=1500,height=1000)
print(map2)
dev.off()
}

#after installing, run program
library(devtools)
devtools::install_github('dgrtwo/gganimate')

library(gganimate)

#create animated map that changes with year/pop
anmap <- map1 + scale_fill_brewer(palette='Blues') +
  geom_polygon(data=dsmap,aes(x=long,y=lat,group=group,fill=Population,frame=YEAR),color='black') +
  labs(title='Persons Recorded As Asian Born 1885-1900, ')

gg_animate(anmap,ani.width=1500,ani.height=1000,'anmap.gif')



