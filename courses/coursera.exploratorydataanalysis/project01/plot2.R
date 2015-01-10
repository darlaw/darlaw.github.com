
#Load Libraries
if("data.table" %in% rownames(installed.packages()) == FALSE) {
  install.packages("data.table",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(data.table)

if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("ggplot2",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(ggplot2)


if("scales" %in% rownames(installed.packages()) == FALSE) {
  install.packages("scales",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(scales) #used for ggplot time series customization


#Step #1 - TODO: Change the working directory to fit your environment
#location where the source file is located and image files Will be stored
setwd("F:/_education/_coursera.exploratorydataanalysis/project01")

#Step #2 - load the data
t <- read.table("household_power_consumption.txt", sep=";", header=TRUE,
                colClasses = c("character", "character","character", "character","character","character","character","character","character"),
                na.strings=c("NA","","?"))

#Step #3 - Subset only two days of data
#CORRECTION: transposed the month and year
#Feb 1, 2007 and Feb 2, 2007 was Thursday and Friday
t.sub <- t[t$Date %in% c("1/2/2007","2/2/2007"),]

#Step #4 - Convert Date character to POSIXct, GLobal_active_power to numeric
t.sub$DateTime <- paste(t.sub$Date,t.sub$Time) #t$DateTime is new column
t.sub$DateTime <- as.POSIXct(t.sub$DateTime,format="%d/%m/%Y %H:%M:%S")
t.sub$Global_active_power <- as.numeric(t.sub$Global_active_power)

#Step 5 Option 1: Create the plot and save to a file in working directory
#this is the plot required for the project
png('plot2.png',width = 480, height = 480)
par(mfrow = c(1,1))
plot(t.sub$Global_active_power~t.sub$DateTime,type="l",
     xlab = " ",
     ylab = "Global Active Power (kilowatts)",
     yaxt="n",
     ylim=c(0,8))
dev.off()

#Option 2: Other ways to represent the data
#Lets crete a time series object and look at usage over the course of the day
png('plot2.1.png',width = 480, height = 480)
par(mfrow = c(1,1))
ts.gap <- ts(time.sub)
plot.ts(ts.gap, 
          xlab="Time of Day (Thursday, Friday)", 
          ylab="Kilowatts Used",
          main="Energy Used Through the Day (Global Active Power)",
          xaxt="n")
axis(1, at =  seq(0, 2880, 240), labels=c("12am","4am","8am","12pm","4pm","10pm","12am","4am","8am","12pm","4pm","10pm","12am"))
dev.off()


#Option 3: Other ways to represent the data
#plot the two attributes to view usage over the course of the day
png('plot2.2.png',width = 480, height = 480)
par(mfrow = c(1,1))
plot(t.sub$Global_active_power~t.sub$DateTime, type="l",
        xlab="Time of Day (Thursday, Friday)", 
        ylab="Kilowatts Used",
        main="Energy Used Through the Day (Global Active Power)")
dev.off()

       
#Option 4: Other ways to represent the data
#plot the Killowats used over time using ggplot
png('plot2.3.png',width = 480, height = 480)
par(mfrow = c(1,1))
ggplot(t.sub, aes(x=as.POSIXct(DateTime, "%m/%d/%y %I:%M%p"), y=Global_active_power)) +
  geom_line() +
  ylab("Kilowatts Used") + 
  xlab("Time of Day (Thursday, Friday)") +
  ggtitle("Energy Used Through the Day \n(Global Active Power)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x=element_text(angle=55, hjust=1,color="black",size=12),
    axis.text.y=element_text(color="black",size=12,angle=45),
    panel.background = element_rect(fill="white")) +
  scale_x_datetime(breaks = date_breaks("4 hour"), labels=date_format("%a %I:%M%p")) #version 0.9.0, the format argument has been replaced by labels. 
dev.off()

