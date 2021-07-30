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


###Plot 1
################
################

#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the 
#base plotting system, make a plot showing the total PM2.5 emission from all sources for each 
#of the years 1999, 2002, 2005, and 2008.


names(NEI)
#Transforming to numeric
NEI[, Freq.Emissions := lapply(.SD, as.numeric), .SDcols = c("Freq.Emissions")]
#Looking for missing data
sum(is.na(NEI$Freq.Emissions))
#Counting total emissions by year
total_emissions= NEI[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("Freq.Emissions"), by = Freq.year]

#Plotting
png(filename='plot1.png')

barplot(total_emissions$Freq.Emissions/1000,names=total_emissions$Freq.year, xlab = "Years"
        , ylab = "Total emissions"
        , main = "Emissions from PM2.5 "
        ,sub = "Emissions in thousands tons" )

dev.off()


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


###Plot 3
##################
##################


#Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) variable,
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a 
#plot answer this question.


names(NEI)
#with 24510 for Baltimore City, to select a specific subset 

Baltimore_data=NEI[Freq.fips=="24510",]

#plot
png(filename ='plot3.png')

ggplot(Baltimore_data,aes(factor(Freq.year),Freq.Emissions,fill=Freq.type)) +
        geom_bar(stat="identity") +
        facet_grid(.~Freq.type,scales = "free",space="free") + 
        labs(x="year", y="Total PM25 Emission",title ="Total PM25 Emission in Baltimore from 1999 to 2008" ) + 
        theme_bw()

dev.off()


###Plot 4
###################
###################

#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

#Looking for coal and combustion observations in level.one and level.four variables
combustion_one=grepl("combustion",SCC[,SCC.Level.One],ignore.case = TRUE)
coal_four=grepl("coal",SCC[,SCC.Level.Four],ignore.case = TRUE)

#Looking for id for matched observations of coal and combustion
combustion_id=SCC[combustion_one & coal_four, SCC]

#Working data making match of id and original dataset
working_data=NEI[NEI[,Freq.SCC] %in% combustion_id]

#plot
png(filename = 'plot4.png')

ggplot(working_data,aes(factor(Freq.year),Freq.Emissions/1000)) +
        geom_bar(stat="identity") +
        labs(x="year", y="Total PM25 Emission",title ="Total PM25 Emission from 1999 to 2008 in thousands tons" ) + 
        theme_bw()
dev.off()



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
