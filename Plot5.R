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

###Plot 5
#####################
#####################
names(SCC)

#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

#Looking for the observations that have de the vehicle pattern
vehicle_two=grepl("vehicle",SCC[,SCC.Level.Two],ignore.case = TRUE)
table(vehicle_two)

#Looking for ids
vehicle_id=SCC[vehicle_two,SCC]

#Matching ids with NEI dataset to get all variables with the suited id
working_data2=NEI[NEI[,Freq.SCC] %in% vehicle_id,]

#Filttering data to baltimore city
working_data2=working_data2[Freq.fips=="24510"]

#Plot

png("plot5.png")

ggplot(working_data2,aes(factor(Freq.year),Freq.Emissions/1000)) +
        geom_bar(stat="identity") +
        labs(x="year", y="Total PM25 Emission") + 
        labs(title=expression("PM"[2.5]*" Motor Vehicle Total Emissions in Baltimore from 1999-2008"))

dev.off()