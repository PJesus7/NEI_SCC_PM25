plot6 <- function()
{
  library(ggplot2)
  library(dplyr)
  setwd("C:\\Users\\Jesus\\Google Drive\\Coursera\\Data Science\\Course 4 - Exploratory Data Analysis\\Course Project 2")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  sources <- grep("Vehicle",unique(SCC$EI.Sector), value = TRUE) #Get all industries that are related with coal
  sccShort <- SCC %>% filter(EI.Sector %in% sources) %>% select(SCC, EI.Sector) #filter only the codes of coal industries
  
  NEI <- readRDS("summarySCC_PM25.rds")
  vehicleBothNEI <- NEI %>% filter(SCC %in% sccShort$SCC & (fips == "24510" | fips == "06037")) #just want scc related with vehicles in baltimore
  
  t <- sapply(vehicleBothNEI$SCC, function(x) {sccShort[sccShort$SCC == x, 2]})
  vehicleBothNEI <- vehicleBothNEI %>% mutate(EI.Sector = t) #merge scc code with the industry
  head(vehicleBothNEI)

  
  results <- vehicleBothNEI %>% group_by(year,EI.Sector,fips) %>% summarize(total = sum(Emissions, na.rm = TRUE)) #compute sum
  unique(results$EI.Sector)
  
  new_names <- c(
    "Mobile - On-Road Diesel Heavy Duty Vehicles"="Diesel Heavy",
    "Mobile - On-Road Diesel Light Duty Vehicles"="Diesel Light",
    "Mobile - On-Road Gasoline Heavy Duty Vehicles"="Gasoline Heavy",
    "Mobile - On-Road Gasoline Light Duty Vehicles"="Gasoline Light"
  )
  
  p <- ggplot(results, aes(year, total, EI.Sector)) + 
    geom_point(aes(color = factor(fips, levels = c("24510", "06037"), labels = c("Baltimore City","Los Angeles County"))), size = 2) + 
    scale_x_discrete(limits = results$year, expand = c(0.1, 0.9)) + 
    facet_grid(EI.Sector ~ ., scales = "free_y", labeller = as_labeller(new_names)) + 
    labs(y ="Total emission (in tons)") + 
    labs(title = "Total emission by EI.Sector of motor vehicles") +
    labs(colour="City")

  png(file="plot6.png",width=480,height=480)
  print(p)
  dev.off()
}