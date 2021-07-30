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

###Plot 4
###################
###################

#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

names(NEI)
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

