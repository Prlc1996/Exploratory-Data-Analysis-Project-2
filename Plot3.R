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