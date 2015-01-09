
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
png('plot4.png',width = 480, height = 480)

par(mfrow = c(2,2), oma=c(0,0,0,0))
#Upper Left
t.sub$Global_active_power <- as.numeric(t.sub$Global_active_power)
plot(t.sub$Global_active_power~t.sub$DateTime,type="l",
     xlab = " ",
     ylab = "Global Active Power (kilowatts)",
     yaxt="n",
     ylim=c(0,8))

#Upper Right
plot(t.sub$Voltage~t.sub$DateTime,type="l",
     xlab = "datetime",
     ylab = "Voltage")

#Lower Left
t$Sub_metering_1 <- as.numeric(t$Sub_metering_1)
t$Sub_metering_2 <- as.numeric(t$Sub_metering_2)
t$Sub_metering_3 <- as.numeric(t$Sub_metering_3)

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

#Lower Right
plot(t.sub$Global_reactive_power~t.sub$DateTime,type="l",
     xlab = "datetime",
     ylab = "Global_reactive_power")

dev.off()