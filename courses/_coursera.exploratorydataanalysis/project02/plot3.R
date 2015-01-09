#Q3.  Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
#variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City

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
nei$type <- factor(nei$type)

#Step 4: group and summarise
nei.24510 <- subset(nei,nei$fips == "24510")
nei.24510.g.yt <- group_by(nei.24510, year,type)
nei24510_by_year.yt <- summarise(nei.24510.g.yt, total_emission = sum(Emissions))
nei24510_by_year.yt <- arrange(nei24510_by_year.yt,type,year)

#Step 5: Plot and save graphic
png('plot3.png',width = 480, height = 480)
par(mfrow = c(1,1))
ggplot(data = nei24510_by_year.yt, aes(x = year, y = total_emission)) + 
  facet_wrap(~ type, ncol=1,scales="free_y") +
  geom_histogram(stat="identity") +
  labs(y = expression("Total Emissions Particulate Matter  (" * PM[2.5] * ")"), x="Year") +
  ylim(0, 2500) + 
  ggtitle("Total Emissions in Baltimore City, Maryland \nby Year and Source") + 
  theme_bw(base_size = 14) +
  theme(
    axis.text.x=element_text(angle=90, hjust=1,color="black",size=12),
    axis.text.y=element_text(color="black",size=12),
    panel.background = element_rect(fill="white"),
    strip.background = element_rect(fill="white"),
    strip.text = element_text(size=12, face="bold"))
dev.off()