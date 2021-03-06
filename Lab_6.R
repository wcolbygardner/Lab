#read packages
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)

#read in data and create regions excluding alaska and hawaii
ipums1 <- read_csv('data/10_23.csv', col_types = cols(PERWT = col_double(), HHWT = col_double())) %>% filter(YEAR<2000 & !(STATEFIP %in% c(2,15)))
ipums2 <- ipums1 %>% mutate(Region=factor
       (ifelse(STATEFIP %in% c(4,6,8,16,30,32,35,41,49,53,56), 4,
      ifelse(STATEFIP %in% c(17,18,19,20,26,27,29,31,38,39,46,55),3,
  ifelse(STATEFIP %in% c(9,23,25,33,34,36,42,44,50),1,2))),
    labels=c('Northeast','South','Midwest','West')))
#filter out group quarters, select household head, and summarise for total households
heads <- ipums2 %>% filter(GQ==1 & RELATE==1) %>% 
    rename(Hrace=RACE)
spouces <- ipums2 %>% filter(RELATE==2) %>%
  select(YEAR,SERIAL,PERNUM,RACE) %>% rename(Wrace=RACE)
#join heads and spouses
couples <- left_join(heads,spouces,by=c('YEAR','SERIAL','SPLOC'='PERNUM'))
couples2 <- couples %>% mutate(Race=(ifelse(SPLOC==0, 'Unmarried',
         ifelse(Hrace==Wrace,'Same Race', 'Mixed Race'))))
#summarize totals in each category
households <- couples2 %>% group_by(YEAR,Race,Region) %>% summarise(Number1=sum(HHWT))
headcount <- heads %>% group_by(YEAR,Region) %>% summarise(Number2=sum(HHWT))
headcountpct <- left_join(households,headcount)
#create Percentage
pctrace <- headcountpct %>% mutate(pct=Number1/Number2*100)

printplot <- function(plot,file) {
  png(file,height=500,width=1000)
  print(plot)
  dev.off()
}
#Plot data
graph1 <- ggplot(data=pctrace,aes(x=Region,y=pct/100,fill=Race)) +
  geom_bar(stat='identity', aes(width=rescale(Number2,c(.1,1)))) +
  labs(x='Region',y='Percent of Households',fill='Head and Spouse',title='Percentage of Households Headed By Same and Different Races, 1900-1990') +
  scale_y_continuous(labels=scales::percent) +
  #scale_x_continuous(breaks=c('Northeast','South','Midwest','West')) +
  scale_fill_brewer(palette='Set1',guide=guide_legend(reverse=TRUE))+
  facet_wrap(~YEAR,nrow=2) +
  theme_bw() +
  geom_text(label=ifelse(pctrace$Race=='Mixed Race',paste('Different=',round(pctrace$pct,2),'%',sep=''),''),angle=90,y=.5)
printplot(graph1,'graph1.png')
#read in data again
ipums1 <- read_csv('data/10_23.csv', col_types = cols(PERWT = col_double(), HHWT = col_double())) %>% filter(YEAR<2000 & !(STATEFIP %in% c(2,15)))
ipums2 <- ipums1 %>% mutate(Region=factor
                            (ifelse(STATEFIP %in% c(4,6,8,16,30,32,35,41,49,53,56), 4,
                             ifelse(STATEFIP %in% c(17,18,19,20,26,27,29,31,38,39,46,55),3,
                             ifelse(STATEFIP %in% c(9,23,25,33,34,36,42,44,50),1,2))),
                            labels=c('Northeast','South','Midwest','West')))
#filter for children and their parents
children <- ipums2 %>% filter(AGE<18)
mothers <- ipums2 %>% filter(SEX==2 & NCHILD>0) %>% rename(Mrace=RACE)
fathers <- ipums2 %>% filter(SEX==1 & NCHILD>0) %>% rename(Frace=RACE)
#join children and their paretns
a <- left_join(children,mothers,by=c('YEAR','SERIAL','PERNUM'='MOMLOC'))%>% 
  left_join(fathers,by=c('YEAR','SERIAL','PERNUM'='POPLOC'))
b <- a %>% mutate(Race=(ifelse(MOMLOC==0 | POPLOC==0, 'One Parent', 
               ifelse(Mrace==Frace,'Same Race', 'Mixed Race')))) 
#summarise
c <- b %>% group_by(YEAR,Race,Region) %>% summarise(Number3=sum(PERWT))
headcount1 <- ipums2 %>% group_by(YEAR,Region) %>% summarise(Number4=sum(PERWT))
headcount1pct <- left_join(c,headcount1)
#create perecntage
pctrace1 <- headcount1pct %>% mutate(pct=Number3/Number4*100)
#plot graph
graph2 <- ggplot(data=pctrace1,aes(x=Region,y=pct/100,fill=Race)) +
  geom_bar(stat='identity', aes(width=rescale(Number4,c(.1,1)))) +
  labs(x='Region',y='Percent of Households',fill='Parents',title='Percentage of Childrens Households Headed By Same and Different Races, 1900-1990') +
  scale_y_continuous(labels=scales::percent) +
  #scale_x_continuous(breaks=c('Northeast','South','Midwest','West')) +
  scale_fill_brewer(palette='Set1',guide=guide_legend(reverse=TRUE))+
  facet_wrap(~YEAR,nrow=2) +
  theme_bw() +
  geom_text(label=ifelse(pctrace1$Race=='Mixed Race',paste('Different=',round(pctrace$pct,2),'%',sep=''),''),angle=90,y=.5)
printplot(graph2,'graph2.png')
