plot4 <- function()
{
  library(ggplot2)
  library(dplyr)
  setwd("C:\\Users\\Jesus\\Google Drive\\Coursera\\Data Science\\Course 4 - Exploratory Data Analysis\\Course Project 2")
  SCC <- readRDS("Source_Classification_Code.rds")

  sources <- grep("Coal",unique(SCC$EI.Sector), value = TRUE) #Get all industries that are related with coal
  sccShort <- SCC %>% filter(EI.Sector %in% sources) %>% select(SCC, EI.Sector) #filter only the codes of coal industries

  NEI <- readRDS("summarySCC_PM25.rds")
  coalNEI <- NEI %>% filter(SCC %in% sccShort$SCC) #just want scc related with coal
  
  t <- sapply(coalNEI$SCC, function(x) {sccShort[sccShort$SCC == x, 2]})
  coalNEI <- coalNEI %>% mutate(EI.Sector = t) #merge scc code with the industry
  
  #compute sum per year and EI.Sector
  results <- coalNEI %>% group_by(year,EI.Sector) %>% summarize(total = sum(Emissions, na.rm = TRUE)) #compute sum
  
  p <- ggplot(results, aes(year, total, EI.Sector)) + geom_point(aes(color = EI.Sector), size = 2) + scale_x_discrete(limits = results$year, expand = c(0.1, 0.9)) + 
    facet_grid(EI.Sector ~ ., scales = "free_y") + labs(y ="Total emission (in tons)") + labs(title = "Total emission by EI.Sector")  + theme(legend.position="bottom", legend.direction = "vertical") 
  
  png(file="plot4.png",width=480,height=480)
  print(p)
  dev.off()
}
