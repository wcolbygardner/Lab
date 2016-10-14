#load packages
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggmap)
library(maptools)
library(gtools)
#change

mapdata <- read_csv('sata/map.csv')

map1 <- ggplot() + theme_nothing(legend=TRUE) +
  geom_polygon(data=mapdata, aes(x=long,y=lat,group=group),fill='white',color='black')
png('map.png',width=1500,height=1000)
print(map1)
dev.off()


ipums <- read_csv('data/10_13.csv',col_types = cols(PERWT=col_double()))

dsouth <- ipums %>% filter(BTL %in% c(1,12,13,22,28,45) & 
                             YEAR-AGE>=1885 & YEAR-AGE<=1900)
ds <- dsouth %>% group_by(YEAR,STATEFIP) %>% summarise(Number=sum(PERWT))

newmap <- mapdata %>% mutate(STATEI=as.integer(STATEFIP))

dsmap <- left_join(ds,mapdata,by=c('STATEFIP'='STATEI'))

      
cuts <- quantcut(ds$Number,q=seq(0,1,.2))

dscats <- ds %>% mutate(Population=factor(ifelse(Number<500,1,
                                         ifelse(Number<1500,2,
                                         ifelse(Number<6000,3,
                                         ifelse(Number<30000,4,5))))))
levels(dscats$population) <- c('1-499','500-1,499','1,500-5,999','6,000-29,999','30,000+')

for (year in unique(dsmap$YEAR)) {
map2 <- map1 +scale_fill_brewer(palette='Blues') +
    geom_polygon(data=filter(dsmap,YEAR=1900), aes(x=long,y=lat,group=group,fill=Number),color='black')
png(paste('map_'),year,'.png',sep=''),width=1500,height=1000)
print(map1)
dev.off()
}

#run this code once
install.packages('devtools')
library(devtools)
devtools::install_github('dgrtwo/gganimate')

library(gganimate)

anmap <- map1 + scale_fill_brewer(paletter='Blues') +
  geom_polygon(data=dsmap, aes(x=long,y=lat,group=group,fill=Population,frame=YEAR),color='black') +
  labs(title='Persons Born 1885-1900 in the Deep South, ')

gg_animate(anmao,ani.width=1500,ani.height=100,'anmap.gif')


 
