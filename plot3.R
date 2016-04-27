plot3 <- function()
{
  library(ggplot2)
  library(dplyr)
  setwd("C:\\Users\\Jesus\\Google Drive\\Coursera\\Data Science\\Course 4 - Exploratory Data Analysis\\Course Project 2")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  BaltimoreNEI <- data.frame(subset(NEI, fips == "24510"))
    
  results <- BaltimoreNEI %>% group_by(year,type) %>% summarize(total = sum(Emissions, na.rm = TRUE))
  
  p <- ggplot(results, aes(year, total, type)) + geom_point(aes(color = type), size = 2) + scale_x_discrete(limits = results$year, expand = c(0.1, 0.9)) + 
    facet_grid(type ~ ., scales = "free_y") + labs(y ="Total emission (in tons)") + labs(title = "Total emission by type")  
  
  png(file="plot3.png",width=480,height=480)
  print(p)
  dev.off()
}