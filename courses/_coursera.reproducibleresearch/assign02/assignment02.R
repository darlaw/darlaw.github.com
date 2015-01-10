#Import libraries
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
    install.packages("ggplot2",
                     repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(ggplot2) #to plot data

if("dplyr" %in% rownames(installed.packages()) == FALSE) {
    install.packages("dplyr",
                     repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(dplyr) #to summarise data

if("RCurl" %in% rownames(installed.packages()) == FALSE) {
    install.packages("RCurl",
                     repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(RCurl) #to download file in knitr

if("grid" %in% rownames(installed.packages()) == FALSE) {
    install.packages("grid",
                     repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(grid) # required to call function unit()

source("http://peterhaschke.com/Code/multiplot.R")  #multiplot


#...........................................................................#
#Q................Loading and preprocessing the data........................#
#...........................................................................#
#set environment and variables
setwd("F:/_education/_coursera.reproducibleresearch/assignments/assign02.files/")
today <- Sys.Date() #to label the file with the download date

a.url <- "https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2"
a.zipfilename <- paste("StormData",today,"bz2", sep=".") #generate unique zip file name
a.downloadfilepath <- paste(getwd(),a.zipfilename,sep="/") 
a.containedfilename <- paste("StormData",today,sep=".") #generate file name in bz2 file
a.unziploc <- paste(getwd(),sep="/") #unzip location

#use this to set a.downloadfilepath if the source file is local
#a.downloadfilepath <- "C:/_portfolio/courses/_coursera.reproducibleresearch/assign02/StormData.2014-08-24.bz2"

download.file(a.url,a.downloadfilepath,mode = "wb")
#use this line for rmarkdown
#download.file(a.url,a.downloadfilepath,mode = "wb", method="curl")

#get the data
wdata <- read.table(bzfile(a.downloadfilepath), sep=",", 
                    na.strings="", fill=TRUE,
                    stringsAsFactors=FALSE,
                    colClasses=c(rep("character",37)),
                    header=TRUE)
    #str(wdata)
    #head(wdata)
    #tail(wdata)

#Set up new fields in table for derived data - population health and economic impact
#create working data frame for analysis
wdatatemp <- wdata 
wdatatemp["POP_HEALTH"] <- as.numeric()
wdatatemp["ECON_IMPACT"] <- as.numeric()
wdatatemp$EVTYPE <- as.factor(wdatatemp$EVTYPE)
wdatatemp$FATALITIES <- as.numeric(wdatatemp$FATALITIES)
wdatatemp$INJURIES <- as.numeric(wdatatemp$INJURIES)
wdatatemp$PROPDMG <- as.numeric(wdatatemp$PROPDMG)
wdatatemp$CROPDMG <- as.numeric(wdatatemp$CROPDMG)


#Create new field, Population Impact, as Fatalities and Injuries
wdatatemp$POP_HEALTH <- wdatatemp$FATALITIES + wdatatemp$INJURIES

#Create a fields to store the numeric multiplication factor 
#CROPDMG_FACTOR: is for crop damage
#PROPDMG_FACTOR: is for Property damage
#ECON_IMPACT: is for Economic Impact, as the sum of Property Damage and Crop Damage
wdatatemp$CROPDMG_FACTOR <- as.numeric(NA) 
wdatatemp$PROPDMG_FACTOR <- as.numeric(NA)
wdatatemp$ECON_IMPACT <- as.numeric(NA)

#populate the CROPDMG_FACTOR field
wdatatemp$CROPDMG_FACTOR[wdatatemp$CROPDMGEXP == "h" | wdatatemp$CROPDMGEXP == "H" ] <- 100 #hundreds
wdatatemp$CROPDMG_FACTOR[wdatatemp$CROPDMGEXP == "k" | wdatatemp$CROPDMGEXP == "K"] <- 1000 #thousands
wdatatemp$CROPDMG_FACTOR[wdatatemp$CROPDMGEXP == "m" | wdatatemp$CROPDMGEXP == "M" ] <- 1000000 #millions
wdatatemp$CROPDMG_FACTOR[wdatatemp$CROPDMGEXP == "b" | wdatatemp$CROPDMGEXP == "B" ] <- 1000000000 #billions
wdatatemp$CROPDMG_FACTOR[wdatatemp$CROPDMG == 0] <- 1 # if values are 0, then use default 1
wdatatemp$CROPDMG_FACTOR[wdatatemp$CROPDMGEXP %in% c("0",0,NA,"NA","?") ] <- 1 #define default value 1

#populate the PROPDMG_FACTOR field
wdatatemp$PROPDMG_FACTOR[wdatatemp$PROPDMGEXP == "h" | wdatatemp$PROPDMGEXP == "H"] <- 100 
wdatatemp$PROPDMG_FACTOR[wdatatemp$PROPDMGEXP == "k" | wdatatemp$PROPDMGEXP == "K"] <- 1000
wdatatemp$PROPDMG_FACTOR[wdatatemp$PROPDMGEXP == "m" | wdatatemp$PROPDMGEXP == "M"] <- 1000000
wdatatemp$PROPDMG_FACTOR[wdatatemp$PROPDMGEXP == "b" | wdatatemp$PROPDMGEXP == "B"] <- 1000000000
wdatatemp$PROPDMG_FACTOR[wdatatemp$PROPDMG == 0 ] <- 1 # if values are 0, then use default 1
wdatatemp$PROPDMG_FACTOR[wdatatemp$PROPDMGEXP %in% c("0",0,NA,"NA","?") ] <- 1 #define default value 1

  View(subset(wdatatemp,(is.na(wdatatemp$PROPDMG_FACTOR)|is.na(wdatatemp$CROPDMG_FACTOR))))

wdatatemp.good <- subset(wdatatemp,!(is.na(wdatatemp$PROPDMG_FACTOR)|is.na(wdatatemp$CROPDMG_FACTOR)))

  sum(is.na(wdatatemp.good$PROPDMG_FACTOR))
  sum(is.na(wdatatemp.good$CROPDMG_FACTOR))
  class(wdatatemp.good$PROPDMG)
  class(wdatatemp.good$CROPDMG)
  class(wdatatemp.good$PROPDMG_FACTOR)
  class(wdatatemp.good$CROPDMG_FACTOR)  

  wdatatemp.good$PROPDMG_FACTOR <- as.numeric(wdatatemp.good$PROPDMG_FACTOR)
  wdatatemp.good$PROPDMG_FACTOR <- as.numeric(wdatatemp.good$CROPDMG_FACTOR)

#copy values back to  wdatatemp
wdatatemp <- wdatatemp.good
wdatatemp.good <- NULL #clear memory


#calculate economic impact
wdatatemp.good$ECON_IMPACT <- (wdatatemp.good$PROPDMG * wdatatemp.good$PROPDMG_FACTOR) + (wdatatemp.good$CROPDMG * wdatatemp.good$CROPDMG_FACTOR) 


#Q1. which types of events (EVTYPE) are most harmful with 
#respect to population health 
#population health = FATALITIES + INJURIES
wdata.sub.g <- group_by(wdatatemp, EVTYPE)
w.summary.h <- summarise(wdata.sub.g,
                       mean_health = mean(POP_HEALTH),
                       total_health = sum(POP_HEALTH))

    #str(w.summary.health)

#order the summary tables, include only events with population impact
w.summary.h.total <- arrange(subset(w.summary.h, total_health != 0), desc(total_health))
w.summary.h.mean <- arrange(subset(w.summary.h, mean_health != 0), desc(mean_health))

    #head(w.summary.h.total)
    #head(w.summary.h.mean)

#top five weather events with the largest total number of injuries and deaths between 1950-2011
w.summary.h.total[1:5,c(1,3)]
#top five weather events with largest average number of injuries between 1950-2011
w.summary.h.mean[1:5,1:2]

h1 <- ggplot(w.summary.h.total[1:5,],aes(x=EVTYPE,y=total_health)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(limits=c("TORNADO","EXCESSIVE HEAT","TSTM WIND","FLOOD","LIGHTNING")) +
  ylim(0,120000) +
  geom_text(aes(x = EVTYPE, y = total_health, label=w.summary.h.total[1:5,]$total_health),
            vjust=-1, colour="black", fontface="bold") +
  ggtitle("Top 5 Events with Highest Total Number \nof Injuries and Deaths,1950-2011 ") +
  xlab("Weather Event Type") +
  ylab("Number of Injuries and Deaths") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x=element_text(angle=45,hjust = 1),
    plot.margin = unit(c(1,1,1,1), "cm"))


h2 <- ggplot(w.summary.h.mean[1:5,],aes(x=EVTYPE, y=mean_health)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(limits=c("Heat Wave","TROPICAL STORM GORDON","WILD FIRES","THUNDERSTORMW","TORNADOES, TSTM WIND, HAIL")) +
  ylim(0,100) +
  geom_text(aes(x = EVTYPE, y = mean_health, label=w.summary.h.mean[1:5,]$mean_health),
            vjust=-1, colour="black", fontface="bold") +
  ggtitle("Top 5 Events with Highest Average Number \nof Injuries and Deaths, 1950-2011") +
  xlab("Weather Event Type") +
  ylab("Number of Injuries and Deaths") +
  theme_bw(base_size = 12) +
  theme(
      axis.text.x=element_text(angle=45,hjust = 1),
      plot.margin = unit(c(1,1,1,1), "cm"))

multiplot(h1,h2,cols=2)

#Q2. which types of events have the greatest economic consequences?
#greatest economic consequences = PROPDMG + CROPDMGEXP 
wdata.sub.g <- group_by(wdatatemp, EVTYPE)
w.summary.e <- summarise(wdata.sub.g,
                        mean_econ = mean(ECON_IMPACT),
                        total_econ = sum(ECON_IMPACT))

    #str(w.summary.e)

#order the summary tables, include only events with economic impact
w.summary.e.total <- arrange(subset(w.summary.e, total_econ != 0), desc(total_econ))
w.summary.e.mean <- arrange(subset(w.summary.e, mean_econ != 0), desc(mean_econ))

#The top five weather events that resulted in the largest total economic impact due to crop and property damage between 1950-2011.
w.summary.e.total[1:5,c(1,3)]

#top five weather events that resulted in the largest average economic impact per event due to crop and property damage between 1950-2011. 
w.summary.e.mean[1:5,1:2]

e1 <- ggplot(w.summary.e.total[1:5,],aes(x=EVTYPE,y=total_econ)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(limits=c("FLOOD","HURRICANE/TYPHOON","TORNADO","STORM SURGE","HAIL")) +
  geom_text(aes(x = EVTYPE, y = total_econ,label=format(w.summary.e.total[1:5,]$total_econ,scientific=TRUE,digits = 3)),
            vjust=1,colour="white",fontface="bold") +
  ggtitle("Top 5 Events with Highest Total Economic Impact \nfrom Crop and Property Damage, 1950-2011") +
  xlab("Weather Event Type") +
  ylab("Economic Impact (U.S. dollars)") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x=element_text(angle=45,hjust = 1), 
    plot.margin = unit(c(1,1,1,1), "cm"))

e2 <- ggplot(w.summary.e.mean[1:5,],aes(x=EVTYPE,y=mean_econ)) + 
    geom_bar(stat="identity") +
    scale_x_discrete(limits=c("TORNADOES, TSTM WIND, HAIL",
                              "HEAVY RAIN/SEVERE WEATHER","HURRICANE/TYPHOON",
                              "HURRICANE OPAL","STORM SURGE")) +
  geom_text(aes(x = EVTYPE, y = mean_econ,label=format(w.summary.e.mean[1:5,]$mean_econ,scientific=TRUE,digits = 3)),
            vjust=1,colour="white",fontface="bold") +
  ggtitle("Top 5 Events with Highest Average Economic Impact \nfrom Crop and Property Damage, 1950-2011") +
  xlab("Weather Event Type") +
  ylab("Economic Impact (U.S. dollars)") +
  theme_bw(base_size = 12) +
    theme(
      axis.text.x=element_text(angle=45,hjust = 1),
      plot.margin = unit(c(1,1,1,1), "cm"))


multiplot(e1,e2,cols=2)

