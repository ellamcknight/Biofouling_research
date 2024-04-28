#Creating a data script for the Plymouth ecological study
# Ella McKnight
# phd research - Plymouth marine biofouling data

# import libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(lessR)
library(scales)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(grid)
library(vegan)
library(mgcv)
library(tidymv)

#### stage one - environmental data ####
# Import data
# Plymouth precipitation data 
weatherPly <- read.csv("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2017-04-24T00%3A00%3A00&endDateTime=2019-04-25T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=uk&locationMode=single&key=H9Y87NDNNXKGQQR7AMY8E8SKP&dataElements=default&locations=plymouth%20")
glimpse(weatherPly)
weatherPly <- as.data.frame(weatherPly)# extract and save weatherdate
write.csv(weatherPly, "weatherPly.csv", row.names = FALSE)

# convert date column to date class
weatherPly$Date <- as.Date(weatherPly$Date.time,
                           format = "%m/%d/%Y")
class(weatherPly$Date)
head(weatherPly$Date)

plymRain <- weatherPly %>% select(Date, Precipitation)# extract the rain data
range(plymRain$Date)
#environmental data loaded from hobo software
QABTempSal <- read_csv("0. QAB/QABTempSal.csv")
MBTempSal <- read_csv("0. MB/MBTempSal.csv")
QABLight <- read_csv("0. QAB/QABLight.csv", col_types = cols(time_sample = col_skip()))

# adding precipdata to both sites
QABenv <- merge(QABTempSal, plymRain, by="Date", all = TRUE)
MBenv <- merge(MBTempSal, plymRain, by="Date", all = TRUE)
# adding the light data measurements for QAB due to the missing data from the HOBO sal metre at QAB .
QABenv <- merge(QABenv, QABLight, by="Date", all = TRUE)

# creating a metric per day for the parameters
##  grouping by date
MBPerDay <- MBenv %>%
  select(Date, Temp, Salinity, Precipitation) %>% 
  arrange(Date) %>% 
  group_by(Date) %>% 
  summarize(tot_precip =max(Precipitation),
            temp_mean =mean(Temp), temp_max = max(Temp), Sal_min = min(Salinity))

QABPerDay <- QABenv %>%
  select(Date, Temp, Salinity, Precipitation, Temp2) %>% 
  arrange(Date) %>% 
  group_by(Date) %>% 
  summarize(tot_precip =max(Precipitation),
            temp_mean =mean(Temp), temp_max = max(Temp), Sal_min = min(Salinity), temp2_max= max(Temp2))

#### plotting the environmental parameters ####
#some plotting set up
# Defining a ggplot2 theme 
theme_coding <- function(){theme_bw()+                          
    theme(axis.text.x=element_text(size=12,colour="black", angle=45, vjust=1, hjust=1),
          axis.text.y=element_text(size=12,colour="black"),
          text = element_text(colour="black",size=14),
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.subtitle = element_text(size=14, colour = "black"),
          plot.caption = element_text(size=14),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = , "cm"),
          plot.title = element_text(size=14, vjust=1, hjust=0.5),
          legend.text = element_text(size=10, face="italic"),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank(),
          legend.position=c(0.9, 0.9))}

ylab <- expression(Temperature ~ (degree*C))
# salinity:
ggplot() +
  geom_line(data=MBPerDay, aes(x = Date, y = Sal_min), cex =1, color="grey60", linetype="solid") +
  geom_line(data=QABPerDay, aes(x = Date, y = Sal_min), cex =1, color="lightblue", linetype="solid") +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %y",expand=c(0,0))+
  scale_y_continuous(breaks = seq(10,36,5), limits = c(8,36))+
  labs(x = "", y = "Salinity (psu)")+ 
  theme_coding()+ theme(axis.text.x = element_blank())
## drift is vizable

# temp
ggplot() +
  geom_line(data=MBPerDay, aes(x = Date, y = temp_max),cex=1, color="grey60") +
  geom_point(data=QABPerDay, aes(x = Date, y = temp2_max),cex=1,color="red") +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %y",expand=c(0,0))+
  scale_y_continuous(breaks = seq(5,20,5), limits = c(5,20))+
  labs(x = "", y = ylab)+ 
  theme_coding()+theme(plot.margin = unit(c(0.3, 0.1, 0.1, 0.1), units = , "cm"))

## annomalies detected in QAB temp data - know to have occurred when the HOBO tech was taken out of the water to be cleaned
QABenv %>% filter(Date >= as.Date("2019-03-01") &  Date <=  as.Date("2019-05-30")) %>% 
  ggplot(aes(x = Date, y = Temp2))+
  geom_point()+
  theme_coding()+labs(x="2017", y="Temp")
## check data from online sources : https://seatemperature.info/october/united-kingdom/plymouth-water-temperature.html
## checked the data from VisualCrossing
weatherPly %>% filter(Date >= as.Date("2019-01-01") &  Date <=  as.Date("2019-05-30")) %>% 
  ggplot(aes(x = Date, y = Maximum.Temperature))+
  geom_point()+
  theme_coding()+labs(x="", y="Temp")

## data to be removed from QABenv temp 2 (the light metre)
QABLight_clean <- QABLight %>% filter(!(Date >= as.Date("2017-10-25") & Date <=  as.Date("2017-10-31")))
QABLight_clean <- QABLight_clean %>% filter(!(Date >= as.Date("2018-06-15") & Date <=  as.Date("2018-06-18")))
QABLight_clean <- QABLight_clean %>% filter(!(Date >= as.Date("2019-04-25") & Date <=  as.Date("2019-05-10")))                                  
 
#remove QABLight
QABenv <- QABenv[-c(5:6)]

# adding the new cleaned light data measurements for QAB due to the missing data from the HOBO sal metre.
QABenv <- merge(QABenv, QABLight_clean, by="Date", all = TRUE)
# regrouping
QABPerDay <- QABenv %>%
  select(Date, Temp, Salinity, Precipitation, Temp2) %>% 
  arrange(Date) %>% 
  group_by(Date) %>% 
  summarize(tot_precip =max(Precipitation),
            temp_mean =mean(Temp), temp_max = max(Temp), Sal_min = min(Salinity), temp2_mean= mean(Temp2))
# final temp figure afetr cleaning
ggplot() +
  geom_line(data=MBPerDay, aes(x = Date, y = temp_mean),cex=1, color="grey60") +
  geom_line(data=QABPerDay, aes(x = Date, y = temp2_mean),cex=1,color="red") +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %y",expand=c(0,0))+
  scale_y_continuous(breaks = seq(5,20,5), limits = c(5,20))+
  labs(x = "", y = ylab)+ 
  theme_coding()+theme(plot.margin = unit(c(0.3, 0.1, 0.1, 0.1), units = , "cm"))

# precip - looks good no errors 
ggplot() +
  geom_flame(data=weatherPly, stat="identity",aes(x = Date, y = Precipitation,y2 = Precipitation),cex=1.5,color ="darkblue") +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y",expand=c(0,0))+
  scale_y_continuous(breaks = seq(0,50,10),expand = c(0,0), limits = c(0,55))+
  labs(x = "", y = "Precipitation (mm)")+
  theme_coding()

# all 3 grahs together
# sal
p1 <- ggplot() +
  geom_line(data=MBPerDay, aes(x = Date, y = Sal_min), cex =1, color="grey60", linetype="solid") +
  geom_line(data=QABPerDay, aes(x = Date, y = Sal_min), cex =1, color="lightblue", linetype="solid") +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %y",expand=c(0,0))+
  scale_y_continuous(breaks = seq(10,36,5), limits = c(8,36))+
  labs(x = "", y = "Salinity (psu)")+ 
  theme_coding()+ theme(axis.text.x = element_blank())
# temp
p2 <- ggplot() +
  geom_line(data=MBPerDay, aes(x = Date, y = temp_mean),cex=1, color="grey60") +
  geom_line(data=QABPerDay, aes(x = Date, y = temp2_mean),cex=1,color="red") +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %y",expand=c(0,0))+
  scale_y_continuous(breaks = seq(5,20,5), limits = c(5,20))+
  labs(x = "", y = ylab)+ 
  theme_coding()+theme(plot.margin = unit(c(0.3, 0.1, 0.1, 0.1), units = , "cm"))
# precip
p3 <- ggplot() +
  geom_flame(data=weatherPly, stat="identity",aes(x = Date, y = Precipitation,y2 = Precipitation),cex=1.5,color ="darkblue") +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y",expand=c(0,0))+
  scale_y_continuous(breaks = seq(0,50,10),expand = c(0,0), limits = c(0,55))+
  labs(x = "", y = "Precipitation (mm)")+
  theme_coding()
ggarrange(p1, p2,p3,ncol=1,labels = c("a","b","c"), align = "v", hjust = -1.5, heights = c(.8,.8,1.1))

#### salinity has drift - fix it with the residuals ####
### QAB
# first remove NA from data
complete.dat <- QABPerDay %>% 
  select(Date, Sal_min) %>% 
  na.omit(Sal_min)
# fit linear model  
fit <- lm(Sal_min~Date, data = complete.dat)
#extract and add the residuals to the data
complete.dat$res <- residuals(fit) 
# add this data back into the QAB perday data
QABPerDay <- merge(QABPerDay, complete.dat, by="Date", all = TRUE)
QABPerDay <- QABPerDay[,-7]
QABPerDay <- QABPerDay %>% rename(Sal_min = Sal_min.x)

### MB
# first remove NA from data
complete.dat <- MBPerDay %>% 
  select(Date, Sal_min) %>% 
  na.omit(Sal_min)
# fit linear model  
fit <- lm(Sal_min~Date, data = complete.dat)
#extract and add the residuals to the data
complete.dat$res <- residuals(fit) 
# add this data back into the QAB perday data
MBPerDay <- merge(MBPerDay, complete.dat, by="Date", all = TRUE)
MBPerDay <- MBPerDay[,-6]
MBPerDay <- MBPerDay %>% rename(Sal_min = Sal_min.x)#

####  Plot of residuals  ####
p1 <- ggplot() +
  geom_line(data=MBPerDay, aes(x = Date, y = res),cex=1) +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %y")+
  labs(x = "", y = "Salinity residuals")+ 
  theme_coding()+ theme(axis.text.x = element_blank())

p2 <- ggplot() +
  geom_line(data=QABPerDay, aes(x = Date, y = res),cex=1) +
  scale_x_date(date_breaks = "2 month",date_labels = "%b %y")+
  labs(x = "", y = "Salinity residuals")+ 
  theme_coding()

ggarrange(p1, p2,nrow=2,labels = c("(a)","(b)"),align = "v",
          heights = c(.9,1))

#### finding the differences in the environmental paramenters ####
## getting the differences in the environmental factors between the sample points
# added a zero time point to account for a time leading up to the first photo taken (time smaple 1)

# create time_sample millbay 
mbstuff <- function(df){
  df %>% mutate(time_sample = case_when(
    Date >= as.Date("2017-04-11") &  Date <=  as.Date("2017-05-11")   ~ "0",
    Date >= as.Date("2017-05-11") &  Date <=  as.Date("2017-06-11")   ~ "1", 
    Date >=  as.Date("2017-06-11")   & Date <=  as.Date("2017-06-26")  ~ "2",
    Date >=  as.Date("2017-06-26")   & Date <=  as.Date("2017-07-30")  ~ "3",
    Date >=  as.Date("2017-07-30")   & Date <=  as.Date("2017-08-21")  ~ "4",
    Date >=  as.Date("2017-08-21")   & Date <=  as.Date("2017-10-11")  ~ "5",
    Date >=  as.Date("2017-10-11")   & Date <=  as.Date("2017-11-17")  ~ "6",
    Date >=  as.Date("2017-11-17")   & Date <=  as.Date("2017-12-21")  ~ "7",
    Date >=  as.Date("2017-12-21")   & Date <=  as.Date("2018-02-02")  ~ "8",
    Date >=  as.Date("2018-02-02")   & Date <=  as.Date("2018-04-01")  ~ "9",
    Date >=  as.Date("2018-04-01")   & Date <=  as.Date("2018-06-01")  ~ "10",
    Date >=  as.Date("2018-06-01")   & Date <=  as.Date("2018-07-05")  ~ "11",
    Date >=  as.Date("2018-07-05")   & Date <=  as.Date("2018-08-08")  ~ "12",
    Date >=  as.Date("2018-08-08")   & Date <=  as.Date("2018-10-01")  ~ "13",
    Date >=  as.Date("2018-10-01")   & Date <=  as.Date("2018-11-01")  ~ "14",
    Date >=  as.Date("2018-11-01")   & Date <=  as.Date("2018-12-06")  ~ "15",
    Date >=  as.Date("2018-12-06")   & Date <=  as.Date("2019-01-04")  ~ "16",
    Date >=  as.Date("2019-01-04")   & Date <=  as.Date("2019-04-25")  ~ "17",
  )) %>% 
    mutate(df, Site = "MB")
}

# time sampling for QAB
qabstuff <- function(df){ df  %>%
    mutate(time_sample = case_when( 
      Date >= as.Date("2017-04-11") &  Date <=  as.Date("2017-05-11")   ~ "0",
      Date >= as.Date("2017-05-11") &  Date <=  as.Date("2017-06-11")   ~ "1",  
      Date >=  as.Date("2017-06-11")   & Date <=  as.Date("2017-06-25")  ~ "2",
      Date >=  as.Date("2017-06-25")   & Date <=  as.Date("2017-07-20")  ~ "3",
      Date >=  as.Date("2017-07-20")   & Date <=  as.Date("2017-08-18")  ~ "4",
      Date >=  as.Date("2017-08-18")   & Date <=  as.Date("2017-10-25")  ~ "5",
      Date >=  as.Date("2017-10-25")   & Date <=  as.Date("2017-11-29")  ~ "6",
      Date >=  as.Date("2017-11-29")   & Date <=  as.Date("2018-01-05")  ~ "7",
      Date >=  as.Date("2018-01-05")   & Date <=  as.Date("2018-02-05")  ~ "8",
      Date >=  as.Date("2018-02-05")   & Date <=  as.Date("2018-03-05")  ~ "9",
      Date >=  as.Date("2018-03-05")   & Date <=  as.Date("2018-04-04")  ~ "10",
      Date >=  as.Date("2018-04-04")   & Date <=  as.Date("2018-05-01")  ~ "11",
      Date >=  as.Date("2018-05-01")   & Date <=  as.Date("2018-06-01")  ~ "12",
      Date >=  as.Date("2018-06-01")   & Date <=  as.Date("2018-07-05")  ~ "13",
      Date >=  as.Date("2018-07-05")   & Date <=  as.Date("2018-08-08")  ~ "14",
      Date >=  as.Date("2018-08-08")   & Date <=  as.Date("2018-10-02")  ~ "15",
      Date >=  as.Date("2018-10-02")   & Date <=  as.Date("2018-11-07")  ~ "16",
      Date >=  as.Date("2018-11-07")   & Date <=  as.Date("2018-12-06")  ~ "17",
      Date >=  as.Date("2018-12-06")   & Date <=  as.Date("2019-01-04")  ~ "18",
      Date >=  as.Date("2019-01-04")   & Date <=  as.Date("2019-04-25")  ~ "19",
    )) %>%  mutate(df, Site = "QAB")
}

# use the function to add the time samples
MBPerDay <- mbstuff(MBPerDay)
QABPerDay <- qabstuff(QABPerDay)

MBPerDay$time_sample <- as.integer(MBPerDay$time_sample)
QABPerDay$time_sample <- as.integer(QABPerDay$time_sample)
##  grouping by date and calculating sum rain, max temp, min sal
## MB 
MBPersample <- MBPerDay %>%
  select(time_sample, tot_precip,temp_max,Sal_min, res) %>% 
  group_by(time_sample) %>% 
  summarise(TotalRain =sum(tot_precip, na.rm = T), TempMax = max(temp_max, na.rm = T), SalMin = min(Sal_min, na.rm = T), SalResMin = min(res, na.rm = T)) %>% 
  arrange((time_sample))

#get the difference in total rain per sample point
MBPersample <- data.frame(MBPersample,rbind(c(0), as.data.frame(diff(MBPersample$TotalRain)))) # precip
# difference in temp max
MBPersample <- data.frame(MBPersample,rbind(c(0), as.data.frame(diff(MBPersample$TempMax)))) # temp max
# difference in salinity min
MBPersample <- data.frame(MBPersample,rbind(c(0), as.data.frame(diff(MBPersample$SalMin)))) # salinity min
# difference in salinity residuals min
MBPersample <- data.frame(MBPersample,rbind(c(0), as.data.frame(diff(MBPersample$SalResMin)))) # salinity residuals min
#rename the columns
MBPersample <- MBPersample %>% rename(RainDiff = diff.MBPersample.TotalRain.) %>% rename(TempDiff = diff.MBPersample.TempMax.) %>% rename(SalDiff = diff.MBPersample.SalMin.) %>% rename(SalResDiff = diff.MBPersample.SalResMin.)

#MBPersample <- MBPersample[,-6]

##  grouping by date and calculating sum rain, max temp, min sal
## QAB
QABPersample <- QABPerDay %>%
  select(time_sample, tot_precip,temp2_max,Sal_min, res) %>% 
  group_by(time_sample) %>% 
  summarise(TotalRain =sum(tot_precip, na.rm = T), TempMax = max(temp2_max, na.rm = T), SalMin = min(Sal_min, na.rm = T), SalResMin = min(res, na.rm = T)) %>% 
  arrange((time_sample))

# QAB has missing data and extra dates from the light metre. these have caused issues with this new dataset 
# and are removed and adjusted below:
QABPersample <- QABPersample[-21 ,]
QABPersample[sapply(QABPersample, is.infinite)] <- NA           


#get the difference in total rain per sample point
QABPersample <- data.frame(QABPersample,rbind(c(0), as.data.frame(diff(QABPersample$TotalRain)))) # precip
# difference in temp max
QABPersample <- data.frame(QABPersample,rbind(c(0), as.data.frame(diff(QABPersample$TempMax)))) # temp max
# difference in salinity min
QABPersample <- data.frame(QABPersample,rbind(c(0), as.data.frame(diff(QABPersample$SalMin)))) # salinity min
# difference in salinity residuals min
QABPersample <- data.frame(QABPersample,rbind(c(0), as.data.frame(diff(QABPersample$SalResMin)))) # salinity residuals min
#rename the columns
QABPersample <- QABPersample %>% rename(RainDiff = diff.QABPersample.TotalRain.) %>% rename(TempDiff = diff.QABPersample.TempMax.) %>% rename(SalDiff = diff.QABPersample.SalMin.) %>% rename(SalResDiff = diff.QABPersample.SalResMin.)

#QABPersample <- QABPersample[,-6]


#### Adding columns: seperate dates, ID, season ####
adding_cols <- function(df){
  df %>% mutate(year = lubridate::year(Date), month = lubridate::month(Date), day = lubridate::day(Date)) %>% 
    mutate(Season = case_when(month == 1  ~ "Winter", 
                              month == 2  ~ "Winter", 
                              month == 3  ~ "Spring", 
                              month == 4  ~ "Spring", 
                              month == 5  ~ "Spring", 
                              month == 6  ~ "Summer", 
                              month == 7  ~ "Summer", 
                              month == 8  ~ "Summer", 
                              month == 9  ~ "Autumn", 
                              month == 10  ~ "Autumn", 
                              month == 11  ~ "Autumn", 
                              month == 12  ~ "Winter")) %>% 
    mutate(DayCount = Date.number - 42897) %>% 
    mutate(DaysinWater = DayCount + 55)# this might not be right
}

# adding back cols??
# mb
newdata <- MBPerDay %>%
  select(Date, time_sample, Site)

my_dates <- as.Date(c("2017-05-11", "2017-06-11","2017-06-26", "2017-07-30", "2017-08-21", "2017-10-11", "2017-11-17", "2017-12-21", "2018-02-02",
              "2018-04-01", "2018-06-01", "2018-07-05", "2018-08-08", "2018-10-01", "2018-11-01", "2018-12-06", "2019-01-04", 
              "2019-04-25"))
df <- subset(newdata, newdata$Date %in% my_dates) 
MBPersample <- merge(df, MBPersample, by="time_sample", all = TRUE)
# QAB
newdata <- QABPerDay %>%
  select(Date, time_sample, Site)

my_dates <- as.Date(c("2017-05-11", "2017-06-11","2017-06-25","2017-07-20","2017-08-18","2017-10-25",
                      "2017-11-29","2018-01-05","2018-02-05","2018-03-05","2018-04-04","2018-05-01",
                      "2018-06-01", "2018-07-05","2018-08-08","2018-10-02","2018-11-07",
                    "2018-12-06","2019-01-04","2019-04-25"))

df <- subset(newdata, newdata$Date %in% my_dates) 
QABPersample <- merge(df, QABPersample, by="time_sample", all = TRUE)



MBPersample <- adding_cols(MBPersample)
QABPersample <- adding_cols(QABPersample)

head(MBPersample)

#  BIOLOGICAL DATA ####
# import species dataset alldata
Allspeciesdata <- read_csv("3. OriginalData/Allspeciesdata.csv")
View(Allspeciesdata)
# filter for only MB
MB <- Allspeciesdata %>% filter(Site =="Mb")
# convert into long data
MBlong <- pivot_longer(data = MB, cols = Clearspace:sponge2, names_to ="Species", values_to = "PerCover")
# for the standardization #getthe max each species occurred
d <- MBlong %>% group_by(Species)%>% summarize(max(PerCover))
print(d, n = 25)
# add this scale column to the long data
# scale species based on the maximum it covered per site
MBlong <- MBlong %>% mutate(
  Abun_scaled = round(case_when(
    Species == "Greenalgae" ~ PerCover/80, Species == "BrownRedalgae" ~ PerCover/50,
    Species == "Ciona spp" ~ PerCover/85, 
    Species == "Botrylloides violaceus" ~ PerCover/0, Species == "Ascidiella aspersa" ~ PerCover/45,
    Species == "Dendrodoa grossularia" ~ PerCover/30, Species == "Clavelina lepadiformis" ~ PerCover/40,
    Species == "Asterocarpa humilis" ~ PerCover/2, Species == "Botryllus schlosseri" ~ PerCover/16, 
    Species == "Spirorbis spp" ~ PerCover/12, Species == "Erect bryozoan spp" ~ PerCover/20,
    Species == "Bugula neritina" ~ PerCover/25, Species == "Tricellaria inopinata" ~ PerCover/15, 
    Species == "Bugulina flabellata" ~ PerCover/0, Species == "Styela clava" ~ PerCover/30,  
    Species == "BiofilmRecruit" ~ PerCover/85, Species == "Clearspace" ~ PerCover/65, 
    Species == "Watersipora subatra" ~ PerCover/35, Species == "Mytilus edulis" ~ PerCover/0,
    Species == "sponge1" ~ PerCover/40, Species == "sponge2" ~ PerCover/35, 
    Species == "Barnacle" ~ PerCover/0,  
    Species == "UnID Encrusting spp" ~ PerCover/12), digits = 3)) %>% 
  replace(is.na(.), 0)
# adding status column - note ciona is left native for this pic
add_status <- function(df){
  df %>% mutate(Status = case_when(Species == "Greenalgae" ~ "Unknown", Species == "BrownRedalgae" ~ "Unknown",
                                   Species == "Ciona spp" ~ "Ciona", Species == "Botrylloides violaceus" ~ "Non-native",
                                   Species == "Ascidiella aspersa" ~ "Native", Species == "Asterocarpa humilis" ~ "Non-native",  
                                   Species == "Dendrodoa grossularia" ~ "Native",Species == "Clavelina lepadiformis" ~ "Native", 
                                   Species == "Botryllus schlosseri" ~ "Non-native",Species == "Spirorbis spp" ~ "Unknown",
                                   Species == "Erect bryozoan spp" ~ "Unknown", Species == "Bugula neritina" ~ "Non-native",
                                   Species == "Tricellaria inopinata" ~ "Non-native", Species == "Bugulina flabellata" ~ "Native", 
                                   Species == "Mytilus edulis" ~ "Native",Species == "UnID Encrusting spp" ~ "Unknown",
                                   Species == "Watersipora subatra" ~ "Non-native",Species == "Barnacle" ~ "Unknown",
                                   Species == "sponge1" ~ "Unknown",Species == "sponge2" ~ "Unknown",
                                   Species == "Clearspace" ~ "Bare space", Species == "BiofilmRecruit" ~ "Biofilm/Larval recuits",
                                   Species == "Styela clava" ~ "Non-native"))
}

MBlong <- add_status(MBlong)

# filter for only QAB
QAB <- Allspeciesdata %>% filter(Site =="Qab")
# convert into long data
QABlong <- pivot_longer(data = QAB, cols = Clearspace:sponge2, names_to ="Species", values_to = "PerCover")
# for the standardization #getthe max each species occurred
d <- QABlong %>% group_by(Species)%>% summarize(max(PerCover))
print(d, n = 25)
# add this scale column to the long data
# scale species based on the maximum it covered per site
QABlong <- QABlong %>% mutate(
  Abun_scaled = round(case_when(
    Species == "Greenalgae" ~ PerCover/5, Species == "BrownRedalgae" ~ PerCover/30,
    Species == "Ciona spp" ~ PerCover/45, 
    Species == "Botrylloides violaceus" ~ PerCover/35, Species == "Ascidiella aspersa" ~ PerCover/62,
    Species == "Dendrodoa grossularia" ~ PerCover/2, Species == "Clavelina lepadiformis" ~ PerCover/12,
    Species == "Asterocarpa humilis" ~ PerCover/6, Species == "Botryllus schlosseri" ~ PerCover/35, 
    Species == "Spirorbis spp" ~ PerCover/12, Species == "Erect bryozoan spp" ~ PerCover/40,
    Species == "Bugula neritina" ~ PerCover/30, Species == "Tricellaria inopinata" ~ PerCover/36, 
    Species == "Bugulina flabellata" ~ PerCover/2, Species == "Styela clava" ~ PerCover/22,  
    Species == "BiofilmRecruit" ~ PerCover/50, Species == "Clearspace" ~ PerCover/85, 
    Species == "Watersipora subatra" ~ PerCover/27, Species == "Mytilus edulis" ~ PerCover/25,
    Species == "sponge1" ~ PerCover/43, Species == "sponge2" ~ PerCover/20, 
    Species == "Barnacle" ~ PerCover/4,  
    Species == "UnID Encrusting spp" ~ PerCover/20), digits = 3)) %>% 
  replace(is.na(.), 0)

QABlong <- add_status(QABlong)


# Defining a ggplot2 theme for plotting
theme_coding2 <- function(){           
  theme_bw()+                          
    theme(axis.text.x=element_text(size=12,colour="black",angle=45, vjust=1, hjust=1),
          axis.text.y=element_text(size=12,colour="black"),
          text = element_text(colour="black",size=12),
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.subtitle = element_text(size=14, colour = "black"),
          plot.caption = element_text(size=14),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = , "cm"),
          plot.title = element_text(size=14, vjust=1, hjust=0.5),
          legend.text = element_text(size=10, face="italic"),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank(),
          legend.position=c(0.9, 0.9))
}
# Species Abundance over time raw  data ####
p1 <- MBlong %>% filter(Status %in% c("Native", "Non-native")) %>% filter(PerCover>0) %>% filter(Species != "Asterocarpa humilis") %>% 
  ggplot() +
  geom_point(aes(x=Date, y=PerCover, color = factor(Status),alpha = 0.5),cex=3,pch=16) +
  theme_coding2() +
  theme(strip.text = element_text(size=12, face="bold.italic"),
        strip.background = element_rect(fill="white", colour="white",size=1), legend.position="none")+
  scale_x_date(date_breaks = "4 month",date_labels = "%b %y")+
  facet_wrap(~Species,scales="free_y",strip.position = "top")+scale_color_manual(values = c("grey50", "red"))+
  labs(x = "", y = "Species coverage (%)\n")

# QAB
QABlong2 <- QABlong[-which(QABlong$Species == "Bugulina flabellata"),]## dropping a species

p2 <- QABlong2 %>% filter(Status %in% c("Native", "Non-native")) %>% filter(PerCover>0) %>% 
  ggplot() +
  geom_point(aes(x=Date, y=PerCover, color = factor(Status),alpha = 0.5),cex=3,pch=16) +
  theme_coding2() +
  theme(strip.text = element_text(size=11, face="bold.italic"),
        strip.background = element_rect(fill="white", colour="white",size=1), legend.position="none")+
  scale_x_date(date_breaks = "4 month",date_labels = "%b %y")+
  facet_wrap(~Species,scales="free_y",strip.position = "top")+scale_color_manual(values = c("grey50", "red"))+
  labs(x = "", y = "Species coverage (%)\n") 

ggarrange(p1, p2, labels = c("a","b"),
          nrow=2) 

#####   to make a dataset with 5 biological replicates and averaging the technical reps
# d <- MBlong %>%
#   group_by(Date, Date.number,Position, Species) %>%
#   summarise(AveragperRep = mean(Abun_scaled))## ??????????

#### create nns/nat data ####
#to make a dataset with 5 biological replicates and summing the technical reps (total species cover scaled)
MBscaled <- MBlong %>%  
  group_by(Date,Date.number,Position, Species) %>% summarize(total_scale_cover = sum(Abun_scaled)) %>% ungroup()

#to make a dataset with 5 biological replicates and summing the technical reps
MBnatnis <- MBlong %>%  
  group_by(Date, Date.number, Position, Status) %>% 
  summarize(total_scale_cover = sum(Abun_scaled)) %>% 
  filter(Status %in% c("Native", "Non-native", "Ciona")) %>%  
  arrange(Status, Position) %>% ungroup()

#QAB
#to make a dataset with 5 biological replicates and summing the technical reps (total species cover scaled)
QABscaled <- QABlong %>%  
  group_by(Date,Date.number,Position, Species) %>% summarize(total_scale_cover = sum(Abun_scaled)) %>% ungroup()

#to make a dataset with 5 biological replicates and summing the technical reps
QABnatnis <- QABlong %>%  
  group_by(Date, Date.number, Position, Status) %>% #Date.number,
  summarize(total_scale_cover = sum(Abun_scaled)) %>% 
  filter(Status %in% c("Native", "Non-native", "Ciona")) %>%   
  arrange(Status, Position) %>% ungroup()


## adding columns
MBscaled <- add_status(MBscaled)
MBscaled <- adding_cols(MBscaled)
MBnatnis <- mbstuff(MBnatnis)
QABscaled <- add_status(QABscaled)
QABscaled <- adding_cols(QABscaled)
QABnatnis <- qabstuff(QABnatnis)

#plot !!! is this plot necessary ??
# MBscaled %>% filter(Status %in% c("Native", "Non-native")) %>% filter(total_scale_cover>0) %>% filter(Species != "Asterocarpa humilis") %>% 
#   ggplot() +
#   geom_point(aes(x=Date, y=total_scale_cover, color = factor(Status),alpha = 0.5),cex=3,pch=16) +
#   theme_coding2() +
#   theme(strip.text = element_text(size=12, face="bold.italic"),
#         strip.background = element_rect(fill="white", colour="white",size=1), legend.position="none")+
#   scale_x_date(date_breaks = "4 month",date_labels = "%b %y")+
#   facet_wrap(~Species,scales="free_y",strip.position = "top")+scale_color_manual(values = c("grey50", "red"))+
#   labs(x = "", y = "Total scaled cover coverage (%)\n")

# Differentiating between time points for each species ####
##### make a dataset with 1 rep per time point and averaging the total scale cover across the panels 
MBgrouped <- MBscaled %>%
  group_by(Date, Date.number, Species) %>% summarize(averageperRep = mean(total_scale_cover)) %>% ungroup()
QABgrouped <- QABscaled %>%
  group_by(Date, Date.number, Species) %>% summarize(averageperRep = mean(total_scale_cover)) %>% ungroup()

MBnatnis$time_sample <- as.integer(MBnatnis$time_sample)
QABnatnis$time_sample <- as.integer(QABnatnis$time_sample)

# adding the column for the change between species
# MBnatnis <- MBnatnis %>% arrange(Status, time_sample) %>% 
#   group_by(Status, Position) %>% 
#   mutate(Change = total_scale_cover - lag(total_scale_cover)) %>% 
#   mutate(Change = ifelse(time_sample == "1" , "0", Change)) #this changes the only sample 1 position

# adding the column for the change between status
MBnatnis <- MBnatnis %>% 
  group_by(Status, Position) %>% 
  mutate(Change = total_scale_cover - lag(total_scale_cover)) %>% 
  mutate(Change = ifelse(time_sample == "1" , "0", Change)) %>% #this changes the only sample 1 position
  mutate(Change = as.numeric(Change))
QABnatnis <- QABnatnis %>% 
  group_by(Status, Position) %>% 
  mutate(Change = total_scale_cover - lag(total_scale_cover)) %>% 
  mutate(Change = ifelse(time_sample == "1" , "0", Change)) %>% #this changes the only sample 1 position
  mutate(Change = as.numeric(Change))
# #plot
# MBnatnis %>% ggplot() +
#   geom_point(aes(x=Date, y=total_scale_cover,alpha = 0.5),cex=3,pch=16) +
#   theme_coding2() +
#   theme(strip.text = element_text(size=12, face="bold.italic"),
#         strip.background = element_rect(fill="white", colour="white",size=1), legend.position="none")+
#   scale_x_date(date_breaks = "4 month",date_labels = "%b %y")+
#   facet_wrap(~Status,scales="free_y",strip.position = "top")+scale_color_manual(values = c("grey50", "red"))+
#   labs(x = "", y = "Total scaled cover (%)\n")

# create fig S3 ####
# average nat/nns per time 
p1 <- MBnatnis %>% 
  group_by(Date, Status) %>% 
  summarise(Aveage = mean(total_scale_cover), std = sd(total_scale_cover)) %>% 
  ungroup %>% 
  pivot_wider(
    names_from = Status,
    values_from = c(Aveage, std)) %>% 
  ggplot()+
  geom_line(aes(Date, Aveage_Native,  colour = "Native"), size = 1)+ 
  geom_ribbon(aes(x=Date, ymin = Aveage_Native+std_Native*1.96, ymax = Aveage_Native-std_Native*1.96), fill = "grey50", alpha = 0.2)+ 
  geom_line(aes(Date, `Aveage_Non-native` ,colour = "Non-native"),  size = 1)+ 
  geom_ribbon(aes(x=Date, ymin = `Aveage_Non-native`+`std_Non-native`*1.96, ymax = `Aveage_Non-native`-`std_Non-native`*1.96), fill = "red", alpha = 0.2)+ 
  theme_coding()+ theme(axis.text.x = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y", expand = c(0,0))+
  labs(x = "", y = "")+theme(legend.position = "none")+
  scale_color_manual(name ="",
                     values=c("Non-native"="red", "Native" = "grey50")) 

p2 <- QABnatnis %>% 
  group_by(Date, Status) %>% 
  summarise(Aveage = mean(total_scale_cover), std = sd(total_scale_cover)) %>% 
  ungroup %>% 
  pivot_wider(
    names_from = Status,
    values_from = c(Aveage, std)) %>% 
  ggplot()+
  geom_line(aes(Date, Aveage_Native,  colour = "Native"), size = 1)+ 
  geom_ribbon(aes(x=Date, ymin = Aveage_Native+std_Native*1.96, ymax = Aveage_Native-std_Native*1.96), fill = "grey50", alpha = 0.2)+ 
  geom_line(aes(Date, `Aveage_Non-native` ,colour = "Non-native"),  size = 1)+ 
  geom_ribbon(aes(x=Date, ymin = `Aveage_Non-native`+`std_Non-native`*1.96, ymax = `Aveage_Non-native`-`std_Non-native`*1.96), fill = "red", alpha = 0.2)+ 
  theme_coding()+ 
  scale_x_date(date_breaks = "2 month",date_labels = "%b %Y", expand = c(0,0))+
  labs(x = "", y = "")+theme(legend.position = "none")+
  scale_color_manual(name ="",
                     values=c("Non-native"="red", "Native" = "grey50")) 

plot <- ggarrange(p1, p2, nrow=2,labels = c("a","b"),heights = c(1,1.2), align = "v")

grid.arrange(arrangeGrob(plot,
              left=grid::textGrob(label = expression("               Scaled species cover (%)"), 
                                  rot=90, gp= gpar(fontsize=16)))) 

##   sampling points across the time series with dominant species ####
# mb
# d <- MBlong %>%   
#   group_by(Date, Species) %>% 
#   summarise(Aveage = mean(PerCover)) %>% 
#   filter(Species %in% c("Ascidiella aspersa", "Ciona spp", "Watersipora subatra", "Tricellaria inopinata") )
  
p1 <- MBlong %>%   
  group_by(Date, Species) %>% 
  summarise(Aveage = mean(PerCover)) %>% 
  filter(Species %in% c("Ascidiella aspersa", "Ciona spp", "Watersipora subatra", "Tricellaria inopinata") ) %>%   
  ggplot() +
  geom_line(aes(x=Date, y=Aveage, color=Species), size =1) + 
  scale_x_date(date_breaks = "2 month",date_labels = "%b %y", expand = c(0,0))+
  geom_vline(xintercept= as.numeric(ymd("2017-06-26")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2017-07-30")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2017-08-21")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2017-10-11")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2017-11-17")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2017-12-21")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2018-02-02")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2018-04-01")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2018-06-01")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2018-07-05")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2018-08-08")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2018-10-01")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2018-11-01")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2018-12-06")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2019-01-04")), linetype="dashed", color = "grey80")+
  geom_vline(xintercept= as.numeric(ymd("2019-04-25")), linetype="dashed", color = "grey80")+
  labs(x = "", y = "")+ 
  theme_coding()+ theme(axis.text.x=element_text(size=14, angle=45, vjust=1, hjust=1))+
  theme_coding()+ theme(axis.text.x = element_blank())+
  scale_color_manual(name ="",
                     values=c("Ascidiella aspersa"="black","Ciona spp" = "steelblue",
                              "Watersipora subatra" = "darkred", "Tricellaria inopinata"= "orange"))+
  theme(legend.position = "top")+theme(legend.text = element_text(size=12, face="italic"))

# qab
p2 <- QABlong %>%   
  group_by(Date, Species) %>% 
  summarise(Aveage = mean(PerCover)) %>% 
  filter(Species %in% c("Ascidiella aspersa", "Ciona spp", "Watersipora subatra", "Tricellaria inopinata") ) %>%   
  ggplot() +
  geom_line(aes(x=Date, y=Aveage, color=Species), size =1) + 
  scale_x_date(date_breaks = "2 month",date_labels = "%b %y", expand = c(0,0))+
  geom_vline(xintercept= as.numeric(ymd("2017-06-11")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2017-06-25")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2017-07-20")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2017-08-18")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2017-10-25")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2017-11-29")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2018-01-05")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2018-02-05")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2018-03-05")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2018-04-04")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2018-05-01")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2018-06-01")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2018-07-05")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2018-08-08")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2018-10-02")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2018-11-07")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2018-12-06")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2019-01-04")), linetype="dashed", color = "grey50")+
  geom_vline(xintercept= as.numeric(ymd("2019-04-25")), linetype="dashed", color = "grey50")+
  labs(x = "", y = "")+
  theme_coding()+ theme(axis.text.x=element_text(size=14, angle=45, vjust=1, hjust=1))+
  #theme(axis.text.x = element_blank())+
  scale_color_manual(name ="",
                     values=c("Ascidiella aspersa"="black","Ciona spp"="steelblue",
                              "Watersipora subatra" = "darkred","Tricellaria inopinata"= "orange"))+
  theme(legend.position = "none")

# arranging big plots     ####
plot <- ggarrange(p1, p2, nrow=2,labels = c("a","b"), hjust = 0, align = "hv",
                 heights = c(1,1.1))

grid.arrange(arrangeGrob(plot,
              left=grid::textGrob(label = expression("               Species cover (%)"), 
                                  rot=90, gp= gpar(fontsize=16)))) 





## GAMM ####
#https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# see gam_plym_new_mod.R

## adding columns
MBnatnis <- adding_cols(MBnatnis)
QABnatnis <- adding_cols(QABnatnis)
# merging data ####
# MBPersample # MBnatnis
MB_test <- merge(MBnatnis, MBPersample, by=c("Date", "time_sample", "Site"))
QAB_test <- merge(QABnatnis, QABPersample, by=c("Date", "time_sample", "Site"))

alldat <- rbind(MB_test, QAB_test)
#plot
alldat %>% 
  ggplot() + 
  geom_point(aes(x=Date, y=total_scale_cover, color = Status), size =1) +
  labs(x = "", y = "")+
  theme_coding()+ theme(axis.text.x=element_text(size=14, angle=45, vjust=1, hjust=1))
  
alldat %>% 
  ggplot() + 
  geom_point(aes(x=Date, y=Change, color = Status), size =1) +
  labs(x = "", y = "")+
  theme_coding()+ theme(axis.text.x=element_text(size=14, angle=45, vjust=1, hjust=1))

# data, see file attached, I removed the first time point ??
alldat <- alldat %>% filter(Date != as.Date("2017-06-11")) #do you need to do this?? remove more?
######    plot the variables
plot(Change~TempDiff,data=alldat)
plot(Change~RainDiff,data=alldat)
plot(Change~SalDiff,data=alldat)
plot(Change~SalResDiff,data=alldat)
plot(Change~month,data=alldat)

### Assumptions?
hist(alldat$Change)
hist(alldat$TempDiff)
hist(alldat$RainDiff)
hist(alldat$SalResDiff)
hist(alldat$month)

# this is the original code:
#varibles were step wise deleted based on sibnificance
# mod <- gamm(Change ~ Site + Status + s(month, bs = "cc") + s(TempDiff) + s(SalResDiff),
#             random=list(Site=~1,Position=~1), method = 'REML',
#             correlation = corARMA(form = ~ 1|DayCount, p = 1),
#             data = alldat,
#             knots = list(month = c(1, 12)))

# Modelling all data together
mod <- gamm(Change ~ s(month, bs = "cc", k=8) + s(TempDiff) + s(SalResDiff),
            random=list(Site=~1,Position=~1), method = 'REML',
            correlation = corARMA(form = ~ 1|DayCount, p = 1),
            data = alldat, 
            knots = list(month = c(1, 12)))
summary(mod$gam)
summary(mod$lme)
par(mfrow=c(2, 2))
plot(mod$gam, residuals = TRUE, pch = 1, pages=1, all.terms = TRUE, shade = TRUE,shade.col = "lightblue",
     scale = 0, seWithMean = TRUE, shift = coef(mod$gam)[1])
#The figure shows the two splines; the one on the left is the seasonal term, the cyclic cubic spline (note how the ends join nicely!)
# check the correlation factors
layout(matrix(1:2, ncol = 2))
acf(resid(modd$lme), lag.max = 20, main = "ACF")
pacf(resid(modd$lme), lag.max = 20, main = "pACF")
layout(1)
#visualize model fit
gam.check(mod$gam,pch=19,cex=.3)
plot(fitted(mod$gam)~residuals(mod$gam))
qqnorm(residuals(mod$gam))
residuals(mod$gam)


#investigating the random effects
m <- lme(Change ~ 1,
         random=list(Position=~1), method = 'REML',
         data = alldat)
# m1 <- lme(Change ~ 1,
#           random=list(Position=~1,SampleID=~1), method = 'REML',
#           data = MB_test)
m2 <- gls(Change ~ 1,
          method = 'REML',
          data = alldat)
anova(m, m2)
summary(m)
plot(m)

#model groups seperately

# model natives change over timne vs the environs
nat <- alldat %>% filter(Status == "Native")
# data, see file attached, I removed the first time point ??
nat <- nat %>% filter(Date != as.Date("2017-06-11")) #do you need to do this?? remove more?
######    plot the variables
plot(Change~TempDiff,data=nat)
plot(Change~RainDiff,data=nat)
plot(Change~SalDiff,data=nat)
plot(Change~SalResDiff,data=nat)
plot(Change~month,data=nat)

### Assumptions?
hist(nat$Change)
hist(nat$TempDiff)
hist(nat$RainDiff)
hist(nat$SalResDiff)
hist(nat$month)

mod <- gamm(Change ~ s(month, bs = "cc", k=8) + s(TempDiff) + s(SalResDiff),
            random=list(Site=~1,Position=~1), method = 'REML',
            correlation = corARMA(form = ~ 1|DayCount, p = 1),
            data = nat, 
            knots = list(month = c(1, 12)))

summary(mod$gam)
summary(mod$lme)
par(mfrow=c(2,2))
plot(mod$gam, residuals = TRUE, pch = 1, all.terms = TRUE, shade = TRUE, shade.col = "lightblue", scale = 0,
     seWithMean = TRUE, shift = coef(mod$gam)[1])


# model non-natives change over timne vs the environs
nns <- alldat %>% filter(Status == "Non-native")
nns <- nns %>% filter(Date != as.Date("2017-06-11")) #do you need to do this?? remove more?

mod <- gamm(Change ~ s(month, bs = "cc", k=6) + s(TempDiff) + s(SalResDiff),
            random=list(Site=~1,Position=~1), method = 'REML',
            correlation = corARMA(form = ~ 1|DayCount, p = 1),
            data = nns, 
            knots = list(month = c(1, 12)))

summary(mod$gam)
summary(mod$lme)
par(mfrow=c(2,2))
plot(mod$gam, residuals = TRUE, pch = 1, all.terms = TRUE, shade = TRUE, shade.col = "lightblue", scale = 0,
     seWithMean = TRUE, shift = coef(mod$gam)[1])

# model ciona change over timne vs the environs
cio <- alldat %>% filter(Status == "Ciona")
#cio <- cio %>% filter(Date != as.Date("2017-06-11")) #do you need to do this?? remove more?

mod <- gamm(Change ~ s(month, bs = "cc", k=8) + s(TempDiff) + s(SalResDiff),
            random=list(Site=~1,Position=~1), method = 'REML',
            correlation = corARMA(form = ~ 1|DayCount, p = 1),
            data = cio, 
            knots = list(month = c(1, 12)))

summary(mod$gam)
summary(mod$lme)
par(mfrow=c(2,2))
plot(mod$gam, residuals = TRUE, pch = 1, all.terms = TRUE, shade = TRUE, shade.col = "lightblue", scale = 0,
     seWithMean = TRUE, shift = coef(mod$gam)[1])










#### Community composition and distributions ####
# data prep
# 1. merge species data(wide original?) with environmental data as a full join: all 
MB <- Allspeciesdata %>% filter(Site =="Mb")
MB <- mbstuff(MB)
MB <- adding_cols(MB)

MB_test <- merge(MB, MBPersample, by="time_sample", all = TRUE)
MB_test <- MB_test[-1,]#remove first row (sample 0)

#make species df
spp <- MB_test[,7:28]
#  environmental factors
env <- MB_test[,29:ncol(MB_test)]


env$Season <- as.factor(env$Season)
env$year <- as.factor(env$year)
env2 <- env %>% select(Temp_max ,T4_max_diff, Temp_mean, Salinity_mean, Salinity_min, S4_min_diff, RaindDiff, TotalRain) %>% # create df for overlay
  mutate(transRain= sqrt(TotalRain))
env2 <- env2[,-8]

###  one data point per time point ####
# data organisation see Community.R for ref of this
data.long2 <- Allspeciesdata %>% 
  gather(key = "species", value = "abundance", c(6:27))
# get the average species cover per timepoint
d <- data.long2 %>% group_by(Site, time_sample, species) %>% 
  summarise(ave = mean(abundance))

data.wide <- d %>% 
  spread(key = "species", value = "ave")
data.wide <- mbstuff(data.wide)

comm <- data.wide[,2:23]        
fac2 <- data.wide[,1:2] # site and time sample
fac2$Site <- as.factor(fac2$Site)


###   Method 
### ------------ Distance matrix ---
### Compute distance matrix using Bray-Curtis on double-root transformed abundances
range(comm)
range(comm^0.5)
range(comm^0.25) #fourth root transformation
# We use a double root transformation to reduce the range of the data
dist_com <- vegdist(comm^0.5, method = "bray") # creates a distance matrix
dist_com
### ------------ NMDS ---
### Run NMDS
nmds <- metaMDS(dist_com)
nmds
plot(nmds)

#extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nmds))

#add/sort columns to data frame 
data.scores$TimeSample = fac2$TimeSample
data.scores$Site = fac2$Site
levels(data.scores$Site)
head(data.scores)

library(gridExtra)

ggplot(data.scores, aes(x = NMDS1, y = NMDS2, group = Site,color = Site)) + 
  geom_path(linetype=1, size=0.5, arrow=arrow(angle=15, ends="last", type="closed"))+
  geom_point(size = 9, alpha=0.4,aes(shape = Site, color = Site))+ 
  geom_text(aes(label=TimeSample),size = 5, color= "black",fontface="bold")+
  theme_coding2()+ 
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y = "")  + theme(legend.position = "none", legend.title = element_blank())+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), units = , "cm"))+
  annotate(geom="text", x=0.18, y=0.23,label="2d Stress: 0.16",fontface = "bold")+
  scale_shape_manual(name = "", values = c(16,17)) +
  scale_colour_manual(name = "", values = c("grey40", "blue")) 

# arranging plots #
library(ggpubr)
ggarrange(p2, p1, 
          labels = c("(a)", "(b)"), 
          align = c("v"),hjust=-0.4, vjust = 1 ,
          ncol=1, nrow = 2)




