#Q6: Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# subset and merge data

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
#assumes data file is in working directory
#FOR USER: Change working directory to your own system location
setwd("F:/_education/_coursera.exploratorydataanalysis/project02/")
nei <- readRDS( file = "summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

#Step 3: Convert datatypes
nei$year <- as.factor(nei$year)
nei$SCC  <- as.factor(nei$SCC)

#Step 4: subset and merge data
#SCC.Level.Two has the most accurate data about motor vehicle sources
scc.sub <- subset(scc,grepl("[V|v]ehicle",scc$SCC.Level.Two)) #get rows in scc related to vehicle sources
nei.sub <- subset(nei, subset=fips %in% c("24510","06037")) #get baltimore and los angeles data from nei
nei.sub <- subset(nei.sub,SCC %in% scc.mv$SCC)#get rows in nei  with SCC values that match values in scc table

#merge nei and scc 
nei.scc.sub <- merge(nei.sub,scc.sub,by=c("SCC")) #merge data tables
nei.scc.sub$fips <- as.factor(nei.scc.sub$fips) #convert fips to factor
nei.scc.sub <- select(nei.scc.sub,year, fips, Emissions) #reduce columns in table

#Step 5: group and summarise
nei.scc.sub <- group_by(nei.scc.sub, year, fips)
nei.scc.sub.year <- summarise(nei.scc.sub, total_emission = sum(Emissions))

fips_labeller <- function(variable,value){
  fips_names <- list(
    '06037'="Los Angeles County",
    '24510'="Baltimore City"
  )
  return(fips_names[value])
}

#Step 6a: Plot Option 1 and save graphic
png('plot6.1.png',width = 480, height = 480)
par(mfrow = c(1,1))
ggplot(data = nei.scc.sub.year, aes(x = year, y = total_emission)) + 
  facet_grid(.~ fips,labeller=fips_labeller) +
  geom_histogram(stat="identity") +
  labs(y = expression("Total Emissions (" * PM[2.5] * ")"), x="Year") +
  ggtitle("Fine Particulate Emissions in U.S. by Year \nfrom All Motor Vehicle Sources \nin Baltimore City and Los Angeles County") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x=element_text(angle=90, hjust=1,color="black",size=12),
    axis.text.y=element_text(color="black",size=12),
    panel.background = element_rect(fill="white"),
    strip.background = element_rect(fill="white"),
    strip.text = element_text(size=12, face="bold"))
dev.off()

#Step 6b: Plot Option 2 and save graphic
png('plot6.2.png',width = 480, height = 480)
par(mfrow = c(1,1))
ggplot(data = nei.scc.sub.year, aes(x = year, y = total_emission)) + 
  #facet_grid(.~ fips,labeller=fips_labeller) +
  geom_histogram(stat="identity", position="dodge", aes(fill=fips)) +
  labs(y = expression("Total Emissions (" * PM[2.5] * ")"), x="Year") +
  ggtitle("Fine Particulate Emissions by Year \nfrom All Motor Vehicle Sources \n in Baltimore City and Los Angeles County") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x=element_text(angle=90, hjust=1,color="black",size=12),
    axis.text.y=element_text(color="black",size=12),
    panel.background = element_rect(fill="white"),
    strip.background = element_rect(fill="white"),
    strip.text = element_text(size=12, face="bold")) +
  scale_fill_discrete(name="Location",
                      breaks=c("06037", "24510"),
                      labels=c("Los Angeles County", "Baltimore City"))
dev.off()
