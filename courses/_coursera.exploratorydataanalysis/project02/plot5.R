#Q5. How have emissions from motor vehicle sources changed from 1999-2008 
# in Baltimore City? 

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
nei$SCC  <- factor(nei$SCC)

#Step 4: subset and merge data
#SCC.Level.Two has the most accurate data about motor vehicle sources
nei.24510 <- subset(nei,nei$fips == "24510")
scc.mv <- subset(scc,grepl("[V|v]ehicle",scc$SCC.Level.Two))
nei.mv <- subset(nei.24510,SCC %in% scc.mv$SCC)
nei.scc.mv <- merge(nei.mv,scc.mv,by=c("SCC")) 

#Step 5: group and summarise
nei.scc.mv.g <- group_by(nei.scc.mv, year)
nei.scc.mv.g.by.year <- summarise(nei.scc.mv.g, total_emission = sum(Emissions))

#Step 6: Plot and save graphic
png('plot5.png',width = 480, height = 480)
par(mfrow = c(1,1))
ggplot(data = nei.scc.mv.g.by.year, aes(x = year, y = total_emission)) + 
  geom_histogram(stat="identity") +
  labs(y = expression("Total Emissions (" * PM[2.5] * ")"), x="Year") +
  ggtitle("Fine Particulate Emissions in U.S. by Year for \nAll Motor Vehicle Sources in Baltimore  City") + 
  geom_text(aes(x = year, y = total_emission,label=round(nei.scc.mv.g.by.year$total_emission,2)),
            vjust=1,colour="white",fontface="bold") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x=element_text(angle=90, hjust=1,color="black",size=12),
    axis.text.y=element_text(color="black",size=12),
    panel.background = element_rect(fill="white"),
    strip.background = element_rect(fill="white"),
    strip.text = element_text(size=12, face="bold"))
dev.off()
