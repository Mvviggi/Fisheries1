#official script to fish abundance in time series
#uplaod all packages you will need
getwd()
setwd("C:/Users/mviggiano/Documents/Github/Fisheries")
library(lubridate)
library(ggplot2)
library(neonUtilities)
library(tidyr)
library(dplyr)
library(devtools)
library(tidyverse)
library(dplyr)
library(stringr)
library(stats)
#bring the data product into your directory
fish <-loadByProduct(dp="DP1.20107.001", site=c("CUPE", "GUIL"), 
                     startdate="2017-01", enddate="2023-04",
                     package= 'basic', check.size=T)

#create the dataframes - only need to do once the write.csv, then only need for read.csv
#dataframe PER FISH
#read.csv("fish_length.csv")
write.csv(fish$fsh_perFish, "C:/Users/mviggiano/Documents/Github/Fisheries/fish_length.csv", row.names=F) 
lenweightMain <- read.csv("fish_length.csv") %>%
    select(siteID, namedLocation, passStartTime, eventID, passNumber, taxonID, fishTotalLength, fishWeight) 
colnames(lenweightMain) <- c("site", "location", "start", "eventID", "pass", "taxon", "length", "weight")
summary(lenweightMain)
#dataframe PER PASS
#read.csv("fish_pass.csv")
write.csv(fish$fsh_perPass,"C:/Users/mviggiano/Documents/Github/Fisheries/fish_pass.csv", row.names=F) 
pass <- read.csv("fish_pass.csv") %>%
    select(siteID, passStartTime, passNumber, reachID, eventID, efTime)
colnames(pass) <- c("site", "start", "pass", "reach", "eventID", "EF")
pass<- pass[!duplicated(pass), ] #Pass df has duplicates, eliminate them

#dataframe PER BULK
#read.csv("fish_bulkCount.csv")
write.csv(fish$fsh_bulkCount, "C:/Users/mviggiano/Documents/Github/Fisheries/bulkcount.csv",row.names=F) 
bulk <-read.csv("bulkcount.csv") %>%
    select( siteID, namedLocation, passStartTime,passNumber, eventID, taxonID, bulkFishCount)
colnames(bulk)<-c("site", "location", "start", "pass","eventID", "taxon", "bulkcount")

#Convert and add columns for season, dates, bouts
###create column for Year-Month for each dataframe
lenweightMain$start <-as.Date(lenweightMain$start)
pass$start <-as.Date(pass$start)
bulk$start <-as.Date(bulk$start)
lenweightMain$collectDate <- format(as.Date(lenweightMain$start, format = "%y/%m/%d"), "%Y-%m")
pass$collectDate <- format(as.Date(pass$start, format = "%y/%m/%d"), "%Y-%m")
bulk$collectDate <- format(as.Date(bulk$start, format = "%y/%m/%d"), "%Y-%m")
    
###separate collectDate  into month and year- will come back here if want to keep this column
lenweight1<- separate(lenweightMain, col = collectDate, into = c("year", "month"), sep = "-") 
pass1<- separate(pass, col = collectDate, into = c("year", "month"), sep = "-")                      
bulk1<- separate(bulk, col = collectDate, into = c("year", "month"), sep = "-")

###convert factor variable into numerit
bulk1 <- bulk1 %>% mutate (month= as.numeric(as.character(month)))
pass1 <- pass1 %>% mutate (month= as.numeric(as.character(month)))
lenweight1 <- lenweight1 %>% mutate (month= as.numeric(as.character(month)))

str(lenweight1)
###generate a new season column based on month
bulk1 <-bulk1 %>% mutate(season = case_when(
  month < 3 ~ "spring",
  month < 9 ~ "summer",
  month < 13 ~ "fall",
  TRUE ~ NA_character_
))
pass1 <-pass1 %>% mutate(season = case_when(
  month < 3 ~ "spring",
  month < 9 ~ "summer",
  month < 13 ~ "fall",
  TRUE ~ NA_character_
))    
lenweight1 <-lenweight1 %>% mutate(season = case_when(
  month < 3 ~ "spring",
  month < 9 ~ "summer",
  month < 13 ~ "fall",
  TRUE ~ NA_character_
))

###if desired  join Year and season
lenweight1<- unite(lenweight1, col= "bout", c('year', 'season'), sep='-')
bulk1 <- unite(bulk1, col ="bout", c('year', 'season'), sep= '-')
pass1 <- unite(pass1, col ="bout", c('year', 'season'), sep= '-')


###separate collectDate  into month and year- will come back here if want to keep this column
lenweight1<- separate(lenweightMain, col = collectDate, into = c("year", "month"), sep = "-") 
pass1<- separate(pass, col = collectDate, into = c("year", "month"), sep = "-")                      
bulk1<- separate(bulk, col = collectDate, into = c("year", "month"), sep = "-")

#create column only with year
lenweight1$start <-as.Date(lenweight1$start)
pass1$start <-as.Date(pass1$start)
bulk1$start <-as.Date(bulk1$start)
lenweight1$Year <- format(as.Date(lenweight1$start, format = "%y/%m/%d"), "%Y")
pass1$year <- format(as.Date(pass1$start, format = "%y/%m/%d"), "%Y")
bulk1$Year <- format(as.Date(bulk1$start, format = "%y/%m/%d"), "%Y")    
    
##create column for reach
lenweight1<- separate(lenweight1, col = location, 
            into = c("one", "two","three", "four", "reach"))
pass1<- separate(pass1, col = reach, 
        into = c("one", "two", "reach"))
bulk1 <- separate(bulk1, col = location, 
        into = c("one", "two","three", "four", "reach"))
#remove unnecessary columns created from above lines 
lenweight1<- lenweight1 %>% select(-c(one, two, three, four))
pass1 <- pass1 %>% select(-c(one, two))
bulk1<- bulk1 %>% select(-c(one, two, three, four))

#make the column bout to keep unique order.
lenweight1$bout<-factor(lenweight1$bout, levels= unique(lenweight1$bout))
bulk1$bout<-factor(bulk1$bout, levels= unique(bulk1$bout))
pass1$bout<-factor(pass1$bout, levels= unique(pass1$bout))

#Count individuals by event, by season, by species
Totalperfish <- lenweight1 %>%
  group_by(site, reach, eventID, pass, taxon, Year, season, bout) %>%
  count()

#Sum total fish per eventID, no species, missing bulk data
sum_event <- Totalperfish %>%
  group_by(site, reach, eventID, pass, Year, season, bout) %>%
  tally(n)
write.csv(sum_event,"C:/Users/mviggiano/Documents/Github/Fisheries/sum_event.csv" )

sum_event <- read.csv("sum_event.csv")
sum_event <-sum_event %>% mutate(term = case_when(
  season == "spring" ~ "Dry",
  season == "fall" ~ "Wet",
  TRUE ~ NA_character_
))
##create a table  replacing Sicydium to SPP and GOBD

#########GRAPHING for Length and Weight analysis###########
LmedianXspecies<- ggplot(lenweightbout, aes(x=bout, y= length)) +
  geom_boxplot()+
  facet_wrap(~site, ncol = 2, scale = "free")
LmedianXspecies

###Section working with Effort time : convert EF from seconds to hours
pass1 <- pass1 %>%
  mutate(EFhr= EF/3600)

#create new unique id
#merging per fish and bulk
counteventtaxaID<- Totalperfish %>%
  mutate(eventaxa = paste(eventID, taxon, sep = "."))
bulkeventtaxaID <-bulk1 %>%
  mutate(eventaxa = paste(eventID, taxon, sep = "."))

#merging per fish and bulk using above unique id
allspeciescount <- merge.data.frame(counteventtaxaID, bulkeventtaxaID, by = "eventaxa", all = TRUE)
allspeciescount$start <- NULL
#replacing NA in bulkcount and n columns with 0
allspeciescount["n"][is.na(allspeciescount["n"])] <- 0
allspeciescount["bulkcount"][is.na(allspeciescount["bulkcount"])] <- 0
allspeciescount$bout.x<-as.character(allspeciescount$bout.x)
#replacing NA with values from other columns
allspeciescount$site.y[is.na(allspeciescount$site.y)] <- allspeciescount$site.x[is.na(allspeciescount$site.y)]
allspeciescount$season.y[is.na(allspeciescount$season.y)] <- allspeciescount$season.x[is.na(allspeciescount$season.y)]
allspeciescount$eventID.y[is.na(allspeciescount$eventID.y)] <- allspeciescount$eventID.x[is.na(allspeciescount$eventID.y)]
allspeciescount$taxon.y[is.na(allspeciescount$taxon.y)] <- allspeciescount$taxon.x[is.na(allspeciescount$taxon.y)]
allspeciescount$pass.y[is.na(allspeciescount$pass.y)] <- allspeciescount$pass.x[is.na(allspeciescount$pass.y)]
allspeciescount$reach.y[is.na(allspeciescount$reach.y)] <- allspeciescount$reach.x[is.na(allspeciescount$reach.y)]
allspeciescount$bout.y[is.na(allspeciescount$bout.y)] <- allspeciescount$bout.x[is.na(allspeciescount$bout.y)]
allspeciescount$Year.y[is.na(allspeciescount$Year.y)] <- allspeciescount$Year.x[is.na(allspeciescount$Year.y)]

#sum the bulk by taxon with all per fish by taxon
totalspeciescount <- allspeciescount %>%
  mutate(total = n + bulkcount)

#remove all duplicate and unnecessary columns
totalspeciescount<- totalspeciescount%>% 
  select(-c(site.x, reach.x, eventID.x, pass.x, taxon.x, Year.x, season.x, bout.x))
write.csv(totalspeciescount, "C:/Users/mviggiano/Documents/Github/Fisheries/totalspeciestally.csv")
speciestotal <- read.csv("totalspeciestally.csv")
colnames(speciestotal) <- c("x", "exentaxa", "n", "site", "reach", "pass", "eventID", "taxa", "bulkcount", "bout",
                            "season", "year", "total")
sumspecies<- speciestotal %>%
  group_by(site, taxa, bout, season, year) %>%
  tally(total)
sumspecies <-sumspecies %>% mutate(term = case_when(
  season == "spring" ~ "Dry",
  season == "fall" ~ "Wet",
  TRUE ~ NA_character_
))
sumspecies <- sumspecies  %>%
  mutate(boutyear = paste(year, term, sep = "."))



sumspecies$boutyear<-factor(sumspecies$boutyear, levels = unique(sumspecies$boutyear))
str(sumspecies)
write.csv(sumspecies,"C:/Users/mviggiano/Documents/Github/Fisheries/sumspecies_boutyear.csv" )
#keep records in order by bout
totalspeciescount$bout<-factor(totalspeciescount$bout, levels= unique(totalspeciescount$bout))
#change column names for unique id match the pass_ef df
colnames(totalspeciescount)<-c("eventtaxa", "n", "site", "reach", "pass", 
                               "eventID", "taxon", "bulkcount", "bout", "season","year", "total")
##create a df for total fish per pass
totalfish <- totalspeciescount %>%
  group_by(site, reach, eventID, season, year, pass, bout) %>%
  tally(total)

#merging tables total fish and PER PASS

EFtotal <- merge(totalfish, pass1, by = "eventID", all = TRUE)
EFtotal$bout.y <- as.character(EFtotal$bout.y)
#replacing NA to 0 for total in fish_EF
  
EFtotal["n"][is.na(EFtotal["n"])] <- 0
EFtotal$site.x[is.na(EFtotal$site.x)] <- EFtotal$site.y[is.na(EFtotal$site.x)]
EFtotal$season.x[is.na(EFtotal$season.x)] <- EFtotal$season.y[is.na(EFtotal$season.x)]
EFtotal$eventID.x[is.na(EFtotal$eventID.x)] <- EFtotal$eventID.y[is.na(EFtotal$eventID.x)]
EFtotal$pass.x[is.na(EFtotal$pass.x)] <- EFtotal$pass.y[is.na(EFtotal$pass.x)]
EFtotal$reach.x[is.na(EFtotal$reach.x)] <- EFtotal$reach.y[is.na(EFtotal$reach.x)]
EFtotal$bout.x[is.na(EFtotal$bout.x)] <- EFtotal$bout.y[is.na(EFtotal$bout.x)]
EFtotal$year.x[is.na(EFtotal$year.x)] <- EFtotal$year.y[is.na(EFtotal$year.x)]

write.csv(EFtotal, "C:/Users/mviggiano/Documents/Github/Fisheries/EFtotal.csv")
EFtotal<- read.csv("EFtotal.csv")
#remove duplicate columns
EFtotal<- EFtotal %>% 
  select(-c(year.y, reach.y, pass.y, site.y, bout.y, season.y))
#convert passs column into categorical variable
totalfish$pass.x <- as.factor(totalfish$pass.x)
EFtotal$pass.x <- as.factor(EFtotal$pass.x)
EFtotal$bout <- as.factor(EFtotal$bout)
str(EFtotal)

##CPUE
CPUE <- EFtotal %>%
  mutate(CPUE = n/EFhr)
write.csv(CPUE, "C:/Users/mviggiano/Documents/GitHub/Fisheries/CPUE2.csv")
read.csv("CPUE2.csv")
colnames(CPUE)<-c("n", "eventID", "site","reach","season" , "year", "pass", "bout",
                              "total" , "start" , "EF", "month", "EFhr", "CPUE" )
CPUE <- read.csv("CPUE2.csv")
CPUE <-CPUE %>% mutate(term = case_when(
  season == "spring" ~ "Dry",
  season == "fall" ~ "Wet",
  TRUE ~ NA_character_
))
#paste year and season dry wet
CPUE<- CPUE %>%
  mutate(boutyear = paste(year, term, sep = "."))

#keep records in order by bout
CPUE$boutyear<-factor(CPUE$boutyear, levels= unique(CPUE$boutyear))
totalspeciescount$bout<- factor(totalspeciescount$bout, levels= unique(totalspeciescount$bout))
totalfish$bout <- factor(totalfish$bout, levels = unique(totalfish$bout))
speciestotal$bout.y<- factor(speciestotal$bout.y, levels= unique(speciestotal$bout.y))


##separate CPUE table by site:kkl;;;;;;;;;;;;;;
#create dataframe by site
cupeyesCPUE <- CPUE[CPUE$site== "CUPE",]
yahuecasCPUE <- CPUE[CPUE$site== "GUIL",]
#Remove NA values for CUPE
CPUE<- CPUE[-2,]
CPUEmedian<- CPUEmedian[-3,]
##calculate mean values of CPUE by pass, by site, by boutyear

CPUEmedian <- CPUE %>%
  group_by(site, year, pass, boutyear, term) %>%
  summarise_at(vars(total, CPUE), list(median = median))
CPUEmedian <- CPUE2 %>%
  group_by(site, year, pass, boutyear, term) %>%
  summarise_at(vars(total, CPUE), list(median = median))
CPUEmedian$boutyear<- factor(CPUEmean$boutyear, levels= unique(CPUEmean$boutyear))

CPUEmedian1<- CPUEmedian %>%
  filter(c(pass =="1"))%>%
  ggplot(aes(x= year, y = CPUE_median, color= term, group=term)) +
  geom_point(size=2)+
  geom_line(size=1)+
  facet_wrap(~site, nrow=1, scale="free")+
  scale_x_discrete()+
  scale_color_manual(values=c("#D95F02", "blue"))+
  labs(title= "Catch per unit effort over the years", 
       x= "Bout by years",
       y = "Mean total CPUE")
CPUEmedian1
CPUEmedian1 +  theme(plot.title = element_text(hjust = 0.5))
+ theme(axis.text=element_text(size=16),
                  axis.title=element_text(size=18))+
  guides(color = guide_legend(override.aes = list(size = 12)))

 CUPE_CPUE<- meancupeyes %>%
   filter(pass == c("1")) %>%
   ggplot(aes(x= bout2, y =CPUE_mean)) +
   geom_boxplot()+
   labs(x= "Year",
        y = "Mean total CPUE per bout")
 CUPE_CPUE + ggtitle("CUPE mean single pass Catch per unit effort over time")
 
 ## mean total fish per site
meancupeyes <- cupeCPUE %>%
  group_by(bout2, year, weather, reach, pass)%>%
  summarise_at(vars(total, CPUE),list(mean = mean))
meanyahuecas <- yahuecasCPUE %>%
  group_by(bout, year, weather, pass)%>%
  summarise_at(vars(total, CPUE),list(mean = mean))



cupeCPUE<- cupeCPUE %>%
  mutate(bout2 = paste(weather, year, sep = "."))

meancupeyes<- unite(meancupeyes, col ="bout2", c('year', 'weather'), sep= '.')
meanyahuecas<- unite(meanyahuecas, col ="bout2", c('year', 'weather'), sep= '.')
CUPE_CPUE<- meancupeyes %>%
  filter(pass == c("1")) %>%
  ggplot(aes(x= bout2, y =CPUE_mean)) +
  geom_boxplot()+
labs(x= "Year",
     y = "Mean total CPUE per bout")
CUPE_CPUE + ggtitle("CUPE mean single pass Catch per unit effort over time")
meancupeyes$bout2<- factor(meancupeyes$bout2, levels= unique(meancupeyes$bout2))

meanguil<- meanyahuecas %>%
 ggplot(aes(x= bout2, y= CPUE_mean, fill = pass)) +
 geom_point(size = 3) +
  labs(x= "Bout by years",
     y = "Mean total CPUE per bout", ylim= 2017, 2023)
meanguil + ggtitle("GUIL site mean Catch per unit effort over the years")
meanguil
lenweight1<- separate(lenweight1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ight1, col = location, 
                      into = c("one", "two","three", "four", "reach"))

#total CPUE for single pass
CPUEpass1 <- CPUE %>%
    filter(pass == c("1")) %>%
  ggplot(aes(x = boutyear, y= CPUE, fill = term)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = "dodge")+
  stat_summary(fun.data = mean_se, geom = "bar", position = "dodge")+
  facet_wrap(~site, nrow= 2, scale = "free") +
  labs(title = " Single pass Catch per unit effort at both sites",
       x= NULL,
       y = "Mean CPUE per single pass ")+
  scale_fill_manual(values=c("#D95F02", "blue")) +
  theme(plot.title = element_text(hjust = 0.5))
CPUEpass1 + theme(axis.text=element_text(size=14),
                    axis.title=element_text(size=16))+
guides(color = guide_legend(override.aes = list(size = 12)))
str(CPUE)
#CPUE for all passes
meanCPUE_all<- CPUE %>%
  filter(site == c("CUPE"), reach.x = "03")
  ggplot(aes(x = bout.x, y= CPUE)) +
  geom_boxplot()+
  facet_wrap(~site.x, nrow= 2, scale = "free") +
  labs(x= NULL,
       y = "CPUE per bout by reach")
meanCPUE_all 


#plotting for specific species
AGOMON <- totalspeciescount %>%
  filter(taxon == c("AGOMON")) %>%
  ggplot(aes(x= bout, y= total, fill = season)) +
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  stat_summary(fun.data = mean_se, geom = "bar")
AGOMON +ggtitle("Agonostomus monticola  abundance by bout at CUPE")
AGOMON <- speciestotal %>%
  filter(taxon.y == c("AGOMON")) %>%
  ggplot(aes(x= bout.y, y= total, fill = season.y)) +
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  stat_summary(fun.data = mean_se, geom = "bar") +
labs(x = "bouts",
     y = "total")
AGOMON +ggtitle("Agonostomus monticola  abundance by bout at CUPE") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold")) +
  scale_fill_manual(values=c("#D95F02", "blue"))

sumspecies$term<- factor(sumspecies$term, levels= unique(sumspecies$term))
AGOMON <- sumspecies %>%
  filter(taxa == c("AGOMON")) %>%
  ggplot(aes(x= boutyear, y= n, fill = term)) +
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  stat_summary(fun.data = mean_se, geom = "bar") +
  labs(x = "bouts",
       y = "total")
AGOMON +ggtitle("Agonostomus monticola  abundance by bout at CUPE") +
  theme(axis.text=element_text(size=14),
              axis.title=element_text(size=16,face="bold")) +
  scale_fill_manual(values=c("#D95F02", "blue"))

XIPHEL <- totalspeciescount %>%
  filter(taxon == c("XIPHEL")) %>%
  ggplot(aes(x= bout, y= total, fill = season)) +
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  stat_summary(fun.data = mean_se, geom = "bar") 
XIPHEL + ggtitle("Xiphophorus eleri abundance by bout at GUIL")

# geom point plotting fro total fish by bout and pass
ggplot(totalfish, aes(bout, total)) +
  geom_point(aes(color = pass, shape = pass))+
  facet_wrap(~site, nrow =2, scales= "free")
str(totalfish1)
# ggplot stacked bar chart
ggplot(totalfish1, aes(x = eventID, y =total, color = pass)) +
  geom_point()+
  facet_wrap(~site)
ggplot(totalfish, aes(x= bout, y =n, fill = season)) +
  geom_col()+
  facet_wrap(~site, nrow =2, scale= "free")
ggplot(Totalfish_bout2, aes(x= collectDate, y =n, col = season)) +
  geom_col()+
  facet_wrap(~site)

##plotting CPUE 
CPUE1<- ggplot(CPUE, aes(x = bout, y= CPUE, fill = season)) +
  geom_boxplot() +
  facet_wrap(~site, nrow=2, scale= "free")

CPUEweather<- CPUE %>% mutate(weather = case_when(
  season.x = "spring" ~ "dry",
  season.x = "fall" ~ "rainy",
  TRUE ~ NA_character_
))
str(CPUE)

#length vs weight
cupelw <- lenweightMain[lenweightMain$site== "CUPE",]
guillw <- lenweightMain[lenweightMain$site== "GUIL",]
plot(cupelw$length, cupelw$weight,
     type='p',  #p=points, l=lines, b=both, o=both overplotted ...
     col=as.factor(cupelw$taxon), ##<----this line sets color to taxon. taxon must be factor
     pch=16, #marker type - 16 is solid circle, there are lots more
     main='CUPE Fish Weight vs Length', #plot title
     sub=paste('data downloaded from NEON Portal on',Sys.Date()), #subtitle - bottom
     cex.sub=0.6, #change font size of sub. 1 is normal so 0.6 is smaller, 1.6 would be bigger
     xlab='Length (mm)', #x axis label
     ylab='Weight (g)',) #y axis label)
plot(guillw$length, guillw$weight,
     type='p',  #p=points, l=lines, b=both, o=both overplotted ...
     col=as.factor(guillw$taxon), ##<----this line sets color to taxon. taxon must be factor
     pch=16, #marker type - 16 is solid circle, there are lots more
     main='GUIL Fish Weight vs Length', #plot title
     sub=paste('data downloaded from NEON Portal on',Sys.Date()), #subtitle - bottom
     cex.sub=0.6, #change font size of sub. 1 is normal so 0.6 is smaller, 1.6 would be bigger
     xlab='Length (mm)', #x axis label
     ylab='Weight (g)',) #y axis label))

outliers<-cupelw[(cupelw$weight>200 & cupelw$length<400),] #find outliers
points(outliers$length,outliers$weight,pch=22,col='black',bg='red',cex=1.1)
legend('topleft', #location of legend, can also tell it x,y coords
       legend=levels(as.factor(cupelw$taxon)), #the names used for the legend
       col=1:length(levels(as.factor(cupelw$taxon))), #colors used in legend
       pch=16, #marker - using same as plot
       bg='gray', #you can set a background color - white without this
       cex=0.7, #font size
       title='TaxonIDs',
       ncol=3)
#get data for AGOMON cupe
cupelw2<-cupelw[!(cupelw$weight>20 & cupelw$length<50),] #don't include the outliers
cupelw2<-cupelw[!(cupelw$weight==0),] #remove fish where weight = 0
cupelw2<-cupelw2[cupelw2$taxon=='AGOMON',]
a<-a[a$length>300,] #remove one outlier
#regression
fit<-lm(log10(cupelw2$weight)~log10(cupelw2$length)) #data not linear, log transform
a<-fit$coefficients[1]
b<-fit$coefficients[2]
p2<-ggplot(data=cupelw2, aes(x=length, y=weight))+
  geom_point(color='turquoise4',
             size=1.5,
             shape='circle'
  )+
  labs(title='cupe Fish',
       y='Weight (g)',
       x='Length (mm)'
  )+
  scale_y_log10()+
  scale_x_log10()+
  geom_smooth(method='lm',formula=y~x,se=FALSE)+
  geom_hline(yintercept=40,color=rgb(100,12,77,max=255))+
  geom_vline(xintercept=50,color='#1B9E77')
grid.arrange(p1,p2,ncol=2)
p2


cvst<- ggplot(CPUE, aes(x=CPUE, y= total, color= term, shape= site))+
  geom_point()
  
cvst + facet_grid(vars(boutyear), scales = "free")

cpue2<- read.csv("CPUE2.csv")
mediancpue<- cpue2%>%
  group_by(site.x, bout.x)%>%
  summarise_at(vars(CPUE), list(median=median))