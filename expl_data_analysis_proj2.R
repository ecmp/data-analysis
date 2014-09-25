##############################
# Code written by (J)RJM
##############################


library(ggplot2)
library(gridExtra)

NEI = readRDS("summarySCC_PM25.rds")
SCC = readRDS("Source_Classification_Code.rds")
mergedData = merge(NEI[,c(1,2,4,5,6)], SCC[,1:10], by.x = "SCC", by.y = "SCC", all=T)

yearlyEmissions = with(NEI, tapply(Emissions, year, sum))
plot(names(yearlyEmissions),yearlyEmissions, xlab="year", ylab="Total Emissions", main = "United States")
BCYearlyEmissions = with(NEI, tapply(Emissions[NEI$fips=="24510"], year[NEI$fips=="24510"],sum))
plot(names(BCYearlyEmissions),BCYearlyEmissions, xlab="year", ylab="Total Emissions", main = "Baltimore City")

BCYearlyEmissions1 = with(subset(NEI, fips=="24510" & type=="POINT"), tapply(Emissions, year, sum))
BCYearlyEmissions2 = with(subset(NEI, fips=="24510" & type=="NONPOINT"), tapply(Emissions, year, sum))
BCYearlyEmissions3 = with(subset(NEI, fips=="24510" & type=="ON-ROAD"), tapply(Emissions, year, sum))
BCYearlyEmissions4 = with(subset(NEI, fips=="24510" & type=="NON-ROAD"), tapply(Emissions, year, sum))
plot1 = ggplot(data.frame(year=names(BCYearlyEmissions1), Emissions=BCYearlyEmissions1), aes(x=year, y=Emissions)) + geom_point() + ggtitle("Point")
plot2 = ggplot(data.frame(year=names(BCYearlyEmissions2), Emissions=BCYearlyEmissions2), aes(x=year, y=Emissions)) + geom_point() + ggtitle("Nonpoint")
plot3 = ggplot(data.frame(year=names(BCYearlyEmissions3), Emissions=BCYearlyEmissions3), aes(x=year, y=Emissions)) + geom_point() + ggtitle("On-road")
plot4 = ggplot(data.frame(year=names(BCYearlyEmissions4), Emissions=BCYearlyEmissions4), aes(x=year, y=Emissions)) + geom_point() + ggtitle("Non-road")

grid.arrange(plot1,plot2,plot3,plot4)

CoalEmissions = with(subset(mergedData, EI.Sector=="Fuel Comb - Electric Generation - Coal"), tapply(Emissions, year, sum))
plot = ggplot(data.frame(year=names(CoalEmissions), Emissions=CoalEmissions), aes(x=year, y=Emissions)) + geom_point() + ggtitle("Coal Combustion")

BCMobileEmissions = with(subset(mergedData, fips=="24510" & SCC.Level.One=="Mobile Sources"), tapply(Emissions, year, sum))
LAMobileEmissions = with(subset(mergedData, fips=="06037" & SCC.Level.One=="Mobile Sources"), tapply(Emissions, year, sum))
BC = data.frame(year=names(BCMobileEmissions), Emission = BCMobileEmissions, fips=rep("24510",4))
LA = data.frame(year=names(LAMobileEmissions), Emission = LAMobileEmissions, fips=rep("06037",4))
plot = ggplot(rbind(BC,LA), aes(x=year, y=Emission, color=fips)) + geom_point() + ggtitle("Motor Vehicle Combustion") + scale_color_discrete(name="City",breaks=c("24510","06037"),labels=c("Baltimore City","Los Angeles"))

