#Libraries
library(ggplot2)
library("data.table")
library(base)


#Unziping data
getwd()
direccion="C:/Users/user/Downloads/exdata_data_NEI_data.zip"
unzip(direccion,exdir = "./data_emissions_usa")

#Reading data
NEI = readRDS("./data_emissions_usa/summarySCC_PM25.rds")
SCC = readRDS("./data_emissions_usa/Source_Classification_Code.rds")

NEI=as.data.table(NEI)
SCC=as.data.table(SCC)

###Plot 2
#######################
#######################

#Have total emissions from PM2.5 decreased in the Baltimore City, 
#Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008? Use the base plotting system to make a plot 
#answering this question.

names(NEI)
#Transforming to numeric
NEI[, Freq.Emissions := lapply(.SD, as.numeric), .SDcols = c("Freq.Emissions")]
#Looking for missing data
sum(is.na(NEI$Freq.Emissions))
#Counting total emissions by year
total_emissions= NEI[Freq.fips=="24510", lapply(.SD, sum, na.rm = TRUE)
                     , .SDcols = c("Freq.Emissions"), by = Freq.year]
#Plotting
png(filename='plot2.png')

barplot(total_emissions[,Freq.Emissions]/1000,names=total_emissions[,Freq.year], xlab = "Years"
        , ylab = "Total emissions when Fips=24510"
        , main = "Emissions from PM2.5 "
        ,sub = "Emissions in thousands tons" )

dev.off()

