#Q4: Across the United States, how have emissions from coal combustion-related sources 
#changed from 1999-2008?

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
#look for rows where Short.Name value contains Combustion and Coal
a <- grep("[C|c]ombustion",scc$Short.Name) #46 results
b <- grep("[C|c]oal",scc$Short.Name) #239 results
ids <- intersect(a,b)

scc.coal <- subset(scc, rownames(scc) %in% ids)
nei.coal <- subset(nei,subset=(nei$SCC %in% scc.coal$SCC))
nei.scc.coal <- merge(nei.coal,scc.coal,by=c("SCC")) 

#Step 5: group and summarise
nei.scc.coal.g <- group_by(nei.scc.coal, year)
nei.scc.coal.g.by.year <- summarise(nei.scc.coal.g, total_emission = sum(Emissions))

#Step 6: Plot and save graphic
png('plot4.png',width = 480, height = 480)
par(mfrow = c(1,1))
ggplot(data = nei.scc.coal.g.by.year, aes(x = year, y = total_emission)) + 
  geom_histogram(stat="identity", fill="grey") +
  labs(y = expression("Total Emissions (" * PM[2.5] * ")"), x="Year") +
  ggtitle("Fine Particulate Emissions from Coal Combustion Sources \n in U.S. by Year, rounded to nearest whole number") + 
  geom_text(aes(x = year, y = total_emission,
                label=round(nei.scc.coal.g.by.year$total_emission,0)),
            vjust=1,colour="black",fontface="bold") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x=element_text(angle=90, hjust=1,color="black",size=12),
    axis.text.y=element_text(color="black",size=12),
    panel.background = element_rect(fill="white"),
    strip.background = element_rect(fill="white"),
    strip.text = element_text(size=12, face="bold"))
dev.off()