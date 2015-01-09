#Q1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008
#group and summarise by year

#Step 1 Import libraries
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("ggplot2",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(ggplot2) #to plot data

if("dplyr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("dplyr",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(dplyr) #to summarise data

#Step 2: set the environment and import data
#Assumes data is in working directory
#FOR USER: Change working directory to your own system location
setwd("C:/_portfolio/courses/_coursera.exploratorydataanalysis/project02")
nei <- readRDS( file = "summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

#Step 3: Convert datatypes
nei$year <- factor(nei$year)

#Step 4: group and summarise data by year into tall format
nei.g <- group_by(nei, year)
nei_by_year.t <- summarise(nei.g, total_emission = sum(Emissions))

#Step 5a: Plot Option 1 and save graphic
png('plot1.1.png',width = 480, height = 480)
par(mfrow = c(1,1))
plot(total_emission~year, data=nei_by_year.t, 
     main="Total Emissions of Fine Particulate Matter, PM2.5, \nin U.S. by Year, \n rounded to nearest whole number",
     xlab="Year", 
     ylab = expression("Total Emissions (" * PM[2.5] * ")"),
     ylim=c(0,10000000))
with(nei_by_year.t, text(total_emission~year, pos = 1, labels = as.character(round(nei_by_year.t$total_emission,0))))
lines(x=nei_by_year.t$year, y=nei_by_year.t$total_emission, col="red")
dev.off()


#Step 5b: Plot Option 2 and save graphic
#summarize data by year into long format
nei_by_year.l <- sapply(split(nei$Emissions, nei$year), FUN = sum)

#Plot the long format data using barplot (Option 2)
png('plot1.2.png',width = 480, height = 480)
par(mfrow = c(1,1))
bp <- barplot(nei_by_year.l, 
              main="Total Emissions of Fine Particulate Matter, PM2.5, \nin U.S. by Year, \nrounded to nearest whole number",
              xlab="Year",
              ylab = expression("Total Emissions (" * PM[2.5] * ")"),
              ylim=c(0,8000000))
text(bp, 0, format(round(nei_by_year.l, 0),digits=5, big.mark=","),cex=1,pos=3) 
dev.off()

#Step 5c: Plot Option 3 and save graphic
#Plot Data uing ggplot (Option 2) using the tall format data frame
png('plot1.3.png',width = 480, height = 480)
par(mfrow = c(1,1))
ggplot(nei_by_year.t, aes(x=year, y=total_emission)) +
  geom_histogram(stat="identity", fill="grey", width=0.5) +
  geom_text(stat="identity", aes(label=format(round(nei_by_year.t$total_emission,0),digits=5, big.mark=","),vjust=-1)) + 
  theme_bw(base_size = 14) +
  ylim(0, (max(nei_by_year.t$total_emission)+350000)) +
  ylab(expression(paste('Total Emissions (',PM[2.5], ')'))) + 
  xlab("Year") +
  ggtitle("Total Emissions of Fine Particulate Matter, PM2.5, \n in U.S. by Year, \nrounded to nearest whole number") +
  theme(
    axis.text.x=element_text(angle=90, hjust=1,color="black",size=12),
    axis.text.y=element_text(color="black",size=12,angle=45),
    panel.background = element_rect(fill="white"))
dev.off()

#Step 5d: Plot Option 4 and save graphic
#Another way to summarize the data into tall format
TotalYearEmission <- aggregate(nei$Emissions ~ nei$year, nei, sum)
colnames(TotalYearEmission) <- c("Year","Emissions")

png('plot1.4.png',width = 480, height = 480)
par(mfrow = c(1,1))
plot(TotalYearEmission, 
     xlab="Year",
     ylab=expression(paste('Total Particulate Matter, PM', ''[2.5], ' in tons')),
     ylim=(c(0,8000000)),
     main=expression(paste('PM', ''[2.5], ' in U.S. by Year, rounded to nearest whole number')))
with(TotalYearEmission, text(Emissions~Year, pos = 3, labels = format(round(TotalYearEmission$Emissions,0),digits=5, big.mark=",")))
dev.off()


