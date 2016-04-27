plot1 <- function()
{
setwd("C:\\Users\\Jesus\\Google Drive\\Coursera\\Data Science\\Course 4 - Exploratory Data Analysis\\Course Project 2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
  
totalEmissions <- with(NEI, tapply(Emissions, year, sum, na.rm = TRUE))
years <- names(totalEmissions)

png(file="plot1.png",width=480,height=480)
plot(years, totalEmissions, xaxt="n", cex = 2,
     main = expression("total PM"[2.5]*" emission from all sources"), xlab = "Year", ylab = "Total emission (in tons)")
axis(1, at = years)
dev.off()
}