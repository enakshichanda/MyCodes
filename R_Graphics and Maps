library(dplyr)
library(tidyverse)
library(tidyr)
# Question 1:
data<-read.csv("https://datacatalog.tusla.ie/dataset/ff6afa81-eb68-4868-abaa-88e343f94b44/resource/45d045be-ae2e-4ab2-8c54-031a81991ece/download/no-of-children-in-foster-care-general-with-an-allocated-social-worker-2018.csv",header=TRUE)
data
names(data1)
hist(data$Dec)
plot(data1$Year,data1$Dec,col="blue",xlab=("Year"),ylab="No of childerns in December")
boxplot(data1$Dec,notch = F,horizontal=T)
qqnorm(data1$Dec,
       pch=10,col="blue")

install.packages("ggplot2")
library(ggplot2)

data<-read.csv("https://datacatalog.tusla.ie/dataset/ff6afa81-eb68-4868-abaa-88e343f94b44/resource/45d045be-ae2e-4ab2-8c54-031a81991ece/download/no-of-children-in-foster-care-general-with-an-allocated-social-worker-2018.csv",header=TRUE)
data
qplot(data$RegionID, geom="histogram",binwidth=1,
      main = "Histogram for Number of childrens in Foster care", 
      xlab = "December's Data",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      ) 
data1<-read.csv("https://datacatalog.tusla.ie/dataset/ff6afa81-eb68-4868-abaa-88e343f94b44/resource/45d045be-ae2e-4ab2-8c54-031a81991ece/download/no-of-children-in-foster-care-general-with-an-allocated-social-worker-2018.csv",header=TRUE)
head(data1)
attach(data1)
names(data1)
plot(data1$RegionID,data1$Dec,pch=10,col="red",xlab="RegionID", ylab="No of childerns in December")

data1<-read.csv("https://datacatalog.tusla.ie/dataset/ff6afa81-eb68-4868-abaa-88e343f94b44/resource/45d045be-ae2e-4ab2-8c54-031a81991ece/download/no-of-children-in-foster-care-general-with-an-allocated-social-worker-2018.csv",header=TRUE)
head(data1)
str(data1)
ggplot(data1, aes(x = data1$RegionID, y = data1$Dec)) +
  geom_point() +
  geom_smooth()

ggplot(data1,aes(x=data1$RegionID, y = data1$Dec)) + 
  geom_bar(stat = "identity",aes(fill=AreaID)) + 
xlab("RegionID") +
ylab("No of childerns in December")

plot(x = data1$RegionID,y = data1$Dec,col="green",
     xlab = "RegionID",
     ylab = "No of childerns in December",		 
     main = "Foster Care December's Data"
)

#Question 2:
apartmentPrices<-read.csv("/Users/enakshi/Downloads/Second hand Appartment Prices.csv",header =TRUE)
View(apartmentPrices)
colnames(apartmentPrices)
#remove unnecessary header to show relevant data
colnames(apartmentPrices)<- c("Year", "National", "Dublin", "Cork", "Galway", "Limerick", "waterford", "Other Areas")
apartmentPrices<- apartmentPrices[-c(1),]
View(apartmentPrices)
apartmentPrices[-c(1),]
str(apartmentPrices)
summary(apartmentPrices)
#Price Comparison
plot(apartmentPrices$National,apartmentPrices$Dublin,xlab="Prices in Dublin",ylab = "National Prices",main="Price comparison National vs Dublin")
plot(apartmentPrices$National,apartmentPrices$Galway,col="pink",xlab="Prices in Galway",ylab = "National Prices",main="Price comparison National vs Galway")
plot(apartmentPrices$National,apartmentPrices$Limerick,xlab="Prices in Limerick",ylab = "National Prices",main="Price comparison National vs Limerick")
plot(apartmentPrices$National,apartmentPrices$Cork,xlab="Prices in Cork",ylab = "National Prices",main="Price comparison National vs Cork")
plot(apartmentPrices$National,apartmentPrices$waterford,xlab="Prices in Waterford",ylab = "National Prices",main="Price comparison National vs Waterford")
plot(apartmentPrices$National,apartmentPrices$`Other Areas`,xlab="Prices in Other Areas",ylab = "National Prices",main="Price comparison National vs Other Areas")



install.packages("ggfortify")
library(ggfortify)
#ts() function will convert a numeric vector into an R time series object
pricesOverTime<-ts(apartmentPrices,frequency=12,start=c(1997,1),end = c(2015,1))
View(pricesOverTime)


autoplot(pricesOverTime,facets = FALSE,stacked = TRUE)

#Plot of time series for each location
plot.ts(pricesOverTime,plot.type ="multiple",frame.plot=TRUE)


install.packages("timeSeries")
library(timeSeries)
autoplot(as.timeSeries(pricesOverTime), ts.colour = ('dodgerblue3'))

#Plotting the time series graph from 1997 to 2015 for all locations
priceDisplay<-t(pricesOverTime[,-1])
#Converting the prices into a vector
priceDisplay<-as.vector(priceDisplay)
View(priceDisplay)
View(my_ts1)
my_ts1 <- ts(c(t(priceDisplay[-1])), start = c(1997,1), end = c(2015,12),freq = 4)
plot.ts(my_ts1)
#type b for plotting both lines and points
plot.ts(my_ts1,type="b")

library(lattice)
library(plotly)

#using Lattice Package XYPlot plotting with Line and points
xyplot(ts(priceDisplay,frequency = 5,start = c(1997,4),end = c(2015,1)),type = c("l","p"))
xyplot(my_ts1, type = c("l", "p"))

#Times series plot without graphics
ts_plot(apartmentPrices,
        title = "Prices of Second Hand Apartments in Ireland from 1997-2015",
        Xtitle = "Time",
        Ytitle = "Price in Euros")


require(graphics)
class(ts_plot(pricesOverTime))
?ts_plot
#using plotly graphic to give background colour and line colours and grid colours
#to make the Time series plot more interative
ts_plot(pricesOverTime,line.mode = "lines", width=2,dash = NULL,
        slider = FALSE, type = "single",
        title = "Prices of Second Hand Apartments in Ireland from 1997-2015",
        Xtitle = "Time",
        Ytitle = "Price in Euros",
        color =  "pink",
        Xgrid = TRUE,
        Ygrid = TRUE) %>%
  layout(paper_bgcolor = "black",
         plot_bgcolor = "black",
         font = list(color = "white"),
         yaxis = list(linecolor = "#6b6b6b",
                      zerolinecolor = "#6b6b6b",
                      gridcolor= "#444444"),
         xaxis = list(linecolor = "#6b6b6b",
                      zerolinecolor = "#6b6b6b",
                      gridcolor= "#444444"))
?xyplot
#Using xyplot ploting the prices wrt year for each location  Y axis has the year and X axis has the prices
xyplot(apartmentPrices$Year~apartmentPrices$National+apartmentPrices$Dublin.+apartmentPrices$Cork+apartmentPrices$Galway+apartmentPrices$Limerick+apartmentPrices$Waterford+apartmentPrices$Other.Areas,main="Secondhand houses Prices (1997-2015)",type=c("p","g"),
       auto.key = list(x = 0.7, y = 0.5, corner = c(0, 0),text = c("National", "Dublin", "Cork","Galway", "Limerick","Waterford","Other.Areas"),
                       title = "Cities"),ylab = "Year", xlab = "Prices",pch=3)


#Question 3:
library(dplyr)
irishTowns<- read.csv('/Users/enakshi/Downloads/Irish Towns Co Ordinates.csv', header = TRUE,stringsAsFactors = FALSE)
head(irishTowns)
summary(irishTowns)
str(irishTowns)
attach(irishTowns)
names(irishTowns)

hist(irishTowns$easting,xlab="easting value",ylab="duration")
hist(irishTowns$northing,xlab="northing value",ylab="duration")
filterData<-irishTowns %>% 
  select(name,irish_name,country,county,eircode,grid_reference,easting,northing,latitude,longitude) %>%
  filter(county=="Roscommon" & country=="Republic of Ireland");
str(filterData)

library(ggmap)
ggmap(get_googlemap())
ggmap::register_google(key = "AIzaSyCvkGXBAg39h_-BoZhlZsiG8JSuFzGuMCY")
register_google(key = "AIzaSyCvkGXBAg39h_-BoZhlZsiG8JSuFzGuMCY")

LocateRoscommon<-get_map(location=c(lon=-8.71066,lat=53.30642),zoom="auto")
ggmap(LocateRoscommon)
ggmap(LocateRoscommon) +
  geom_point(data =filterData, aes(x = longitude, y = latitude,fill=eircode), shape = 21, size = 3)

install.packages("leaflet")
library(leaflet)
library(leaflet.providers)
library(widgetframe)

(cityDetails <- filterData %>%
    filter(county == "Roscommon"))
mapDisplay<-leaflet(data = cityDetails) %>%
  addTiles() %>%
  addAwesomeMarkers(label = ~name,icon = icons) %>%
  frameWidget();mapDisplay
icons <-awesomeIcons(icon = "home", library = "fa",
                     markerColor = "green", iconColor = "white",spin = TRUE,
                     extraClasses = NULL, squareMarker = FALSE, iconRotate = 0,
                     fontFamily = "monospace", text = NULL)

#Circle markers for Roscommon
leaflet(cityDetails) %>% addTiles() %>% addCircleMarkers()

#Clustering areas in Roscommon 
mapDisplay<-leaflet(data = cityDetails) %>%
  addTiles() %>%
  addAwesomeMarkers(label = ~name,clusterOptions = markerClusterOptions(),icon = icons) %>%
  frameWidget();mapDisplay

library(dplyr)
irishTowns<- filter(irishTowns, county == 'Roscommon' & country == "Republic of Ireland")
irishTowns<- select(irishTowns,-c(3,4,5,6,7,10,11,12,13,14,15,16))
plot(irishTowns$easting,irishTowns$northing)
str(irishTowns)
range(irishTowns$northing)
range(irishTowns$easting)
qplot(irishTowns$easting,irishTowns$northing,data=irishTowns,geom="text",col= irishTowns$name , main = "Towns in Roscommon",label=name)

#qplot(irishTowns$easting,irishTowns$northing,data=irishTowns,geom="text",col= irishTowns$name , main = "Towns in Roscommon",label=name)
#+coord_equal() +labs(legend.position = "none")
require(graphics)
ts.plot(apartmentPrices,col=1:4,
        gpars=list(xlab="year", ylab="Prices"))
