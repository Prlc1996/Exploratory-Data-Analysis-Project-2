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

##Plot 6
###############
###############

#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor
#vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). Which city 
#has seen greater changes over time in motor vehicle emissions?

#Similar to plot 5, but now including one more category city

#Looking for the observations that have de the vehicle pattern

vehicle_two=grepl("vehicle",SCC[,SCC.Level.Two],ignore.case = TRUE)
table(vehicle_two)

#Looking for ids
vehicle_id=SCC[vehicle_two,SCC]

#Matching ids with NEI dataset to get all variables with the suited id
working_data2=NEI[NEI[,Freq.SCC] %in% vehicle_id,]

#Filttering data to baltimore city and La
balt_data=working_data2[Freq.fips=="24510",]
balt_data[,city:=c("Baltimore")]

la_data=working_data2[Freq.fips=="06037",]
la_data[,city:=c("Los Angeles")]

#Combining data
working_data3=rbind(balt_data,la_data)

#Plot 6
png(filename = "plot6.png")
ggplot(working_data3, aes(x=factor(Freq.year), y=Freq.Emissions/1000, fill=city)) +
        geom_bar(aes(fill=Freq.year),stat="identity") +
        facet_grid( .~city,scales="free", space="free") +
        labs(x="year", y="Total PM25 Emission in Thousand tons") + 
        labs(title="Motor Vehicle Total Emissions in Baltimore and Los Angeles from 1999-2008")

dev.off()
