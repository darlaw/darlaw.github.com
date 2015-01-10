#Q2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008

#Step 1: Import libraries
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("ggplot2",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(ggplot2) #to plot data

if("dplyr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("dplyr",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(dplyr) #to summarise data

#Step 2: set the environment and import data
#Assumes data file is in working directory
#FOR USER: Change working directory to your own system location
setwd("F:/_education/_coursera.exploratorydataanalysis/project02/")
nei <- readRDS( file = "summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

#Step 3: Convert datatypes
nei$year <- factor(nei$year)

#Step 4: group and summarise
nei.24510 <- subset(nei,nei$fips == "24510")
nei.24510.g <- group_by(nei.24510, year)
nei24510_by_year <- summarise(nei.24510.g, total_emission = sum(Emissions))

#Step 5a: Plot and save graphic using base plot
png('plot2.1.png',width = 480, height = 480)
par(mfrow = c(1,1))
plot(total_emission~year, data=nei24510_by_year,
     main="Total Emissions in Baltimore City by Year",
     xlab="Year",
     ylab = expression("Total Emissions (" * PM[2.5] * ")"),
     ylim=c(0,3500),
     xaxs="r",
     xaxp=c(1999,2008,3))
with(nei24510_by_year, text(total_emission~year, pos = 1, labels = as.character(round(nei24510_by_year$total_emission,2))))
lines(x=nei24510_by_year$year, y=nei24510_by_year$total_emission, col="red")
dev.off()

#Step 5a: Plot and save graphic using ggplot
png('plot2.2.png',width = 480, height = 480)
par(mfrow = c(1,1))
ggplot(nei24510_by_year, aes(x=year, y=total_emission)) +
  geom_histogram(stat="identity", fill="grey", width=0.5) +
  geom_text(stat="identity", aes(label=format(round(nei24510_by_year$total_emission,0),digits=5, big.mark=","),vjust=-1)) + 
  theme_bw(base_size = 14) +
  ylim(0, (max(nei24510_by_year$total_emission) + 500)) +
  ylab(expression(paste('Total Emissions of Fine Particulate Matter (',PM[2.5], ')'))) + 
  xlab("Year") +
  ggtitle("Total Emissions (PM2.5) in Baltimore City, Maryland \n by Year, rounded to nearest whole number") +
  theme(
    axis.text.x=element_text(angle=90, hjust=1,color="black",size=12),
    axis.text.y=element_text(color="black",size=12,angle=45),
    panel.background = element_rect(fill="white"))
dev.off()
