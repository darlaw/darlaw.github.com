
#only run this if data.table is not yet installed
if("data.table" %in% rownames(installed.packages()) == FALSE) {
  install.packages("data.table",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(data.table)

#Step #1 - TODO: Change the working directory to fit your environment
#location where the source file is located and image files Will be stored
setwd("F:/_education/_coursera.exploratorydataanalysis/project01")

#Step #2 - load the data
t <- read.table("household_power_consumption.txt", sep=";", header=TRUE,
                colClasses = c("character", "character","character", "character","character","character","character","character","character"),
                na.strings=c("NA","","?"))

#Step #3 - Subset only two days of data
#CORRECTION: transposed the month and year
t.sub <- t[t$Date %in% c("1/2/2007","2/2/2007"),]

#Step #4 - Convert Date character to POSIXct 
t.sub$DateTime <- paste(t.sub$Date,t.sub$Time) #t$DateTime is new column
t.sub$DateTime <- as.POSIXct(t.sub$DateTime,format="%d/%m/%Y %H:%M:%S")


##Step #5 - convert to numeric first
#convert to numeric first
t.sub$Sub_metering_1 <- as.numeric(t.sub$Sub_metering_1)
t.sub$Sub_metering_2 <- as.numeric(t.sub$Sub_metering_2)
t.sub$Sub_metering_3 <- as.numeric(t.sub$Sub_metering_3)

#Option 1: Create the plot and save to a file in working directory
#this is the plot required for the project
png('plot3.png',width = 480, height = 480)
par(mfrow = c(1,1))
plot(t.sub$Sub_metering_1~t.sub$DateTime,type="l",
     xlab = " ",
     ylab = "Global Active Power (kilowatts)",
     yaxt="n",
     col="black",
     lwd=2,
     ylim=c(0,40))
xrange <- c(min(t.sub$DateTime), median(t.sub$DateTime+61),as.POSIXct(max(t.sub$DateTime)+61))
axis(side = 1, at=xrange, labels=format(xrange, "%a"))
axis(side = 2, at=c(0,10,20,30), labels=c(0,10,20,30))
lines(lines(t.sub$Sub_metering_2~t.sub$DateTime,col="red"))
lines(lines(t.sub$Sub_metering_3~t.sub$DateTime,col="blue"))
legend("topright", 
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
       lty=c(1,1), 
       col=c("black","red","blue"),
       cex=1,
       xjust=1)
dev.off()


#Option 2: Let's look at the electricity usage in the three areas of the house:
#Sub_metering_1, Sub_metering_2, and Sub_metering_3
#First use melt to create long format data frame
t.sub2 <- subset(t.sub, select=c("DateTime","Sub_metering_1","Sub_metering_2","Sub_metering_3"))
t.subm <- melt(t.sub2, id.vars="DateTime")  #melt to long format
t.subm$value <- as.numeric(t.subm$value) #convert to numeric

png('plot3.1.png',width = 480, height = 480)
par(mfrow = c(1,1))
ggplot(t.subm, aes(x=DateTime, y=value,group = variable, colour = variable)) +
  geom_line() +
  ylab("Kilowatts Used") + 
  xlab("Time of Day (Thursday, Friday)") +
  ggtitle("Energy Used Through the Day \n(Global Active Power)") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x=element_text(angle=55, hjust=1,color="black",size=12),
    axis.text.y=element_text(color="black",size=12),
    panel.background = element_rect(fill="white"),
    legend.position="top") +
  scale_x_datetime(breaks = date_breaks("6 hour"), labels=date_format("%a %I:%M%p")) + #version 0.9.0, the format argument has been replaced by labels. 
  scale_color_manual(name ="SubMeter Type", values=c("#000000", "#FF0000", "#0000FF"))
dev.off()

