
#Import libraries
if("data.table" %in% rownames(installed.packages()) == FALSE) {
  install.packages("data.table",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(data.table)

#Step #1 - TODO: Change the working directory to fit your environment
#location where the source file is located and image files Will be stored
setwd("C:/_portfolio/courses/_coursera.exploratorydataanalysis/project01")

#Step #2 - Load the data
t <- read.table("household_power_consumption.txt", sep=";", header=TRUE,
                colClasses = c("character", "character","character", "character","character","character","character","character","character"),
                na.strings=c("NA","","?"))

#Step #3 - Subset only two days of data
#CORRECTION: transposed the month and year
t.sub <- t[t$Date %in% c("1/2/2007","2/2/2007"),]
  str(t)

#Step #4 - Convert data types: Date character to POSIXct; Global_active_power to numeric
t.sub$DateTime <- paste(t.sub$Date,t.sub$Time) #t$DateTime is new column
t.sub$DateTime <- as.POSIXct(t.sub$DateTime,format="%d/%m/%Y %H:%M:%S")
t.sub$Global_active_power <- as.numeric(t.sub$Global_active_power)


#Step 5 - Create the plot and save to a file in working directory
#Option 1: this is the plot required for the project
png('plot1.png',width = 480, height = 480)
par(mfrow = c(1,1))
hist(t.sub$Global_active_power, col ="red", 
     breaks=12,
     main="Global Active Power",
     xlab="Global Active Power (kilowatts)",
     ylab="Frequency",
     ylim=c(0,1200),
     xaxt="n",
     xlim=c(0,6),
     axes = TRUE)
axis(side = 1, at = c(0,2,4,6), labels = c(0,2,4,6))
axis(side = 2, at = c(0,200,400,600,800,1000,1200))
dev.off()

#Option 2: Other ways to represent the data
png('plot1.2.png',width = 480, height = 480)
par(mfrow = c(1,1))
ggplot(t.sub, aes(x=Global_active_power)) +
  geom_histogram(stat="bin", fill="dark grey",binwidth = 0.1) +
  theme_bw(base_size = 14) +
  ylab("Count of intervals (30 seconds each)") + 
  xlab("Kilowatts Demand") +
  ggtitle("Frequency Distribution of Global Active Power (kilowatts) \nusage during one day") +
  theme(
    axis.text.x=element_text(angle=90, hjust=1,color="black",size=12),
    axis.text.y=element_text(color="black",size=12,angle=45),
    panel.background = element_rect(fill="white"))
dev.off()


#Option 3: Other ways to represent the data
png('plot1.3.png',width = 480, height = 480)
par(mfrow = c(1,1))
ggplot(t.sub, aes(x=Global_active_power)) +
  geom_line(stat="bin", binwidth=0.05, size=1) +
  theme_bw(base_size = 14) +
  ylab("Count of intervals (30 seconds each)") + 
  xlab("Kilowatts Demand") +
  ggtitle("Frequency Distribution of Global Active Power (kilowatts) \nusage during one day") +
  theme(
    axis.text.x=element_text(angle=90, hjust=1,color="black",size=12),
    axis.text.y=element_text(color="black",size=12,angle=45),
    panel.background = element_rect(fill="white"))
dev.off()

#Option 4: Other ways to represent the data
png('plot1.4.png',width = 480, height = 480)
par(mfrow = c(1,1))
ggplot(t.sub, aes(x=Global_active_power)) +
  geom_density(alpha = 0.2, binwidth=0.5, size=1) +
  theme_bw(base_size = 14) +
  ylab("Percentage of total intervals (30 seconds each)") + 
  xlab("Kilowatts Demand") +
  ggtitle("Frequency Distribution of Global Active Power (kilowatts) \nusage during one day") +
  theme(
    axis.text.x=element_text(angle=90, hjust=1,color="black",size=12),
    axis.text.y=element_text(color="black",size=12,angle=45),
    panel.background = element_rect(fill="white"))
dev.off()

