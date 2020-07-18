#WELCOME TO DATA ANALYTICS PROJECT USING R( R STUDIO)
#IN THIS PROJECT WE ARE GOING TO VISUALIZE THE DATA THAT IS PROVIDED AND CONCLUDE THE RESULTS
#THE DATASET WE ARE USING FOR THIS PROJECT IS WORLD HAPPINESS RECORD OF 2015.

#TO BEGIN THE PROJECT
#FIRSTLY WE NEED TO READ THE DATA 
#ASSIGN A VARIABLE data TO READ THE DATA INTO THAT VARIABLE
#NOTE: TO RUN THE STATEMENTS WE PRESS CTRL+ENTER IN R STUDIO

data<-read.csv("C:/Users/Amulya/Desktop/data analysis-r(skyfi labs)/world-happiness-data.csv")

#NOW VIEW THE DATA THAT IS READ
#TO VIEW THE DATA WE USE View() FUNCTION
View(data)

#IN THE VIEW OF DATA WE CAN SEE 158 INSTANCES WITH 12 VARIABLES

#BEFORE WE START WITH THE ANALYSIS OF THE DATA WE NEED TO CHECK
  #DIMENSION OF DATA
  #WE USE dim() FUNCTION TO VIEW
  dim(data)

  #SUMMARY OF DATA
  #WE USE summary() FUNCTION TO SUMMARISE THE DATA
  summary(data)
  #STRUCTURE OF DATA
  #WE USE str() FUNCTION TO VIEW THE DATA TYPE i.e STRUCTURE OF DATA
  str(data)

#NOW LETS GET STARTED WITH THE DATA VISUALISATION
#FOR ANY DATA TO BE VISUALISED FIRST IT IS NEEDED TO BE MANIPULATED i.e THE DATA NEEDS TO BE PRE PROCESSED TO VISUALISE THE DATA
  #AND CONCLUDE THE DATA
  
  
  #STEP-1: CHECK THE DATA IF IT HAS ANY ERRORS OR NOT. IN OUR CASE THE DATASET HAS A STANDARD ERROR WHICH IS TO BE REMOVED.FOR THAT WE USE
  data<-data[,-5]#HERE WE ARE DECLARING A VARIABLE data AND PASSING OUR data(WHERE THE DATASET IS THERE) AND ELIMINATING COLUMN 5 i.e STANDARD
   View(data)             #ERROR FROM THE COLUMN
  
  
   
   #STEP-2: WE ARE GOING CREATE A NEW COLUMN CONTINENT FOR SEEING WHICH REGION FALLS IN WHICH CONTINENT
  
  distinct(data,Region) #GIVES THE DISTINCT/UNIQUE NAMES OF THE REGIONS IN THE DATASET
  data$Continent<-NA #CREATES A NEW COLUMN
  
  #CREATE EVERY REGION FOR CONTINENT
  data$Continent[which(data$Region %in% c("Australia and New Zealand"))]<-"Australia"
  data$Continent[which(data$Region %in% c("North America"))]<-"America"
  data$Continent[which(data$Region %in% c("Southern Asia","Eastern Asia","Southeastern Asia"))]<-"Asia"
  data$Continent[which(data$Region %in% c("Western Europe","Central and Eastern Europe"))]<-"Europe"
  data$Continent[which(data$Region %in% c("Sub-Saharan Africa","Middle East and Northern Africa"))]<-"Africa"
  data$Continent[which(data$Region %in% c("Latin America and Caribbean"))]<-"South America"
  
 
   #VIEW THE DATASET AFTER THE CONTINENT IS CREATED
  View(data)
  
 
   #CALCULATE THE AGGREGATE MEAN OF DATA FOR CONTINENTS
  hp<-aggregate(data[4:11],list(data$Continent),mean) #hp IS THE VARIABLE WHCIH IS DECLARED TO AGGREGATE THE MEAN. 
  View(hp)
  #BY CALCULATING MEAN WE CAN CONCLUDE THAT HAPPIEST COUNTRIES ARE AUSTRALIA,EUROPE,AMERICA AND UNHAPPIEST COUNTRIES ARE AFRICA AND ASIA
#BY HERE WE CONCLUDE THE DATA MANIPULATION OF THE WORLD HAPPINESS RECORD 2015
  
 
  
  
   #DATA VISUALISATION
 
  
   #STEP-1: INSTALL THE LIBRARRIES
  library(dplyr)#DATA FAST AND EASY
  library(ggplot2)#PLOT THE HISTOFRAMS,BARPLOTS,ETC
  library(corrgram)#CORRELATION OF THE DATA
  library(corrplot)#CORRELATION PLOT
  
 
   #STEP-2:VIEW TOP 10 AND BOTTOM 10 COUNTRIES OF DATA
  View(head(data,10)) #MOSTLY WE SEE EUROPE REGION
  View(tail(data,10)) #MOSTLY WE SUB-SAHARAN AFRICA REGION
  
  
  
  #STEP-3:WE WILL VISUALISE CONTINENT V/S HAPPINESS SCORE w.r.t AGGREGATE MEAN(hp) DATASET
  ggplot(hp,aes(x=Group.1,y=Happiness.Score,fill=Group.1))+geom_bar(stat = "identity")+ggtitle("Continent V/S Happiness Score")+xlab("Continent")+ylab("Happiness Score")

 
  
   #STEP-4: CORRELATE THE DATA
  col<-sapply(data,is.numeric) #CHECK WHETEHR DATA IS NUMERIC OR NOT
  cor.data<-cor(data[,col])#CHECK THE DATA IN THE data DATASET
  corrplot(cor.data,method="square",type="upper")#PLOT THE CORRELATION OF THE data DATASET
  corrplot(cor.data,method = "number",type="upper")#PLOT THE CORRELATION IN NUMERIC MEANS IT SHOWS WHICH CORRELATION IS HAVING MAXIMUM 
                                                  # AND WHICH IS HAVING MINIMUM CORRELATION CAN BE SEEN CLEARLY
  

  
  #STEP-5:BOX PLOTS FOR REGION WISE
 box<-ggplot(data,aes(x=Region,y=Happiness.Score,color=Region))#ASSIGN box VARIABLE TO CREATE A ggplot FOR REGIONS
box+geom_boxplot()+geom_jitter(aes(color=Country),size=1.0)+ggtitle("Happiness Score for Regions and Countries")+coord_flip()
+theme(legend.position = "none")#ASSIGN THE box VARIABLE TO THE BOX PLOT TO VISUALISE



#STEP-6:BOX PLOTS FOR CONTINENT WISE
boxp<-ggplot(data,aes(x=Continent,y=Happiness.Score,color=Continent))#ASSIGN boxp VARIABLE TO CREATE  A ggplot CONTINENTS
boxp+geom_boxplot()+ggtitle("Happiness Score for Continents")#ASSIGN boxp TO BOX PLOT TO VISUALISE



#STEP-7: REGRESSION FOR ALL CONTINENTS
#for health life expectancy
ggplot(data,aes(x=Health..Life.Expectancy.,y=Happiness.Score))+geom_point(aes(color=Continent),size=3,alpha=0.8)
+geom_smooth(aes(color=Continent,fill=Continent),method="lm",fullrange=T)+facet_wrap(~Continent)+theme_bw()
+ggtitle("Scatter Plot for life expectancy")
#for economia gdp per capita
ggplot(data,aes(x=Economy..GDP.per.Capita.,y=Happiness.Score))+geom_point(aes(color=Continent),size=3,alpha=0.8)
+geom_smooth(aes(color=Continent,fill=Continent),method="lm",fullrange=T)+facet_wrap(~Continent)+theme_bw()
+ggtitle("Scatter Plot for Economy per Capita")
#for freedom
ggplot(data,aes(x=Freedom,y=Happiness.Score))+geom_point(aes(color=Continent),size=3,alpha=0.8)
+geom_smooth(aes(color=Continent,fill=Continent),method="lm",fullrange=T)+facet_wrap(~Continent)+theme_bw()+ggtitle("Scatter Plot for Freedom")
#for family
ggplot(data,aes(x=Family,y=Happiness.Score))+geom_point(aes(color=Continent),size=3,alpha=0.8)
+geom_smooth(aes(color=Continent,fill=Continent),method="lm",fullrange=T)+facet_wrap(~Continent)+theme_bw()+ggtitle("Scatter Plot for family")

#for trust in government
ggplot(data,aes(x=Trust..Government.Corruption.,y=Happiness.Score))+geom_point(aes(color=Continent),size=3,alpha=0.8)
+geom_smooth(aes(color=Continent,fill=Continent),method="lm",fullrange=T)+facet_wrap(~Continent)+theme_bw()+ggtitle("Scatter Plot for trust in govt")

#for Generosity
ggplot(data,aes(x=Generosity,y=Happiness.Score))+geom_point(aes(color=Continent),size=3,alpha=0.8)
+geom_smooth(aes(color=Continent,fill=Continent),method="lm",fullrange=T)+facet_wrap(~Continent)+theme_bw()
+ggtitle("Scatter Plot for generosity")

#for Dystopia Residual
ggplot(data,aes(x=Dystopia.Residual,y=Happiness.Score))+geom_point(aes(color=Continent),size=3,alpha=0.8)
+geom_smooth(aes(color=Continent,fill=Continent),method="lm",fullrange=T)+facet_wrap(~Continent)+theme_bw()
+ggtitle("Scatter Plot for dystopia residual")
#by using regression concept w.r.t health life expectancy we can say europe and south america have highest correlation and africa with neutral correlation.
#For GDP the South America has highest correlation and africa with least correlation.
#But the data is not comparable to North America and Australia
#For Freedom the europe and south america have positive correlation whereas asia with poor correlation





#STEP-8:As we can see that australia and new zealand,western europe,north america are happiest region,whereas every other are neutral happy 
#except sub saharan africa which is unhappiest
#create a classition of data based on happiest,neutral and least happy regions of data
data$happinessmeter<-NA#CREATE A COLUMN WITH EMPTY DATA
#ASSIGN THE DATA W.R.T REGIONS
data$happinessmeter[which(data$Region %in% c("Australia and New Zealand","North America","Western Europe"))]<-"HAPPIEST"
data$happinessmeter[which(data$Region %in% c("Middle East and Northern Africa","South America","Central and Eastern Europe",
                                             "Middle East and Northern Africa","Latin America and Caribbean","Eastern Asia",
                                             "Southern Asia","Southeastern Asia"))]<-"NEUTRAL HAPPY"
data$happinessmeter[which(data$Region %in% c("Sub-Saharan Africa"))]<-"UNHAPPIEST"

View(data)#SHOWS THE CREATED COLUMN WITH ITS DATA






#STEP-9:create regression lines for all the three regions(happy,unhappy,neutral)
ggplot(data,aes(x=Health..Life.Expectancy.,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)
+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)+facet_wrap(~happinessmeter)
+theme_bw()+ggtitle("Scatter Plot for life expectancy")
#for economy
ggplot(data,aes(x=Economy..GDP.per.Capita.,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)
+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)
+facet_wrap(~happinessmeter)+theme_bw()+ggtitle("Scatter Plot for Economy per capita")
#for family
ggplot(data,aes(x=Family,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)
+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)+facet_wrap(~happinessmeter)
+theme_bw()+ggtitle("Scatter Plot for family")
#for generosity
ggplot(data,aes(x=Generosity,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)
+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)+facet_wrap(~happinessmeter)
+theme_bw()+ggtitle("Scatter Plot for generosity")
#for trust government corruption
ggplot(data,aes(x=Trust..Government.Corruption.,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)
+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)+facet_wrap(~happinessmeter)
+theme_bw()+ggtitle("Scatter Plot for trust in govt")
#for dystopia residual
ggplot(data,aes(x=Dystopia.Residual,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)
+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)+facet_wrap(~happinessmeter)+theme_bw()
+ggtitle("Scatter Plot for dystopia residual")
#freedom
ggplot(data,aes(x=Freedom,y=Happiness.Score))+geom_point(aes(color=happinessmeter),size=3,alpha=0.8)
+geom_smooth(aes(color=happinessmeter,fill=happinessmeter),method="lm",fullrange=T)+facet_wrap(~happinessmeter)
+theme_bw()+ggtitle("Scatter Plot for freedom")





#STEP-10: plot gdp and health expectancy on world map
library(rworldmap) #IMPORT RWORLDMAP PACKAGE

#create a data frame
db<-data.frame(Country=data$Country,Value=data$Economy..GDP.per.Capita.)
#join the data
db1<-joinCountryData2Map(db,joinCode = "NAME",nameJoinColumn = "Country")
#map the data
mapCountryData(db1,nameColumnToPlot = "Value",mapTitle = "World Map for GDP 2015",colourPalette ="terrain")

#for health life expectancy
db<-data.frame(Country=data$Country,Value=data$Health..Life.Expectancy.)
#join the data
db1<-joinCountryData2Map(db,joinCode = "NAME",nameJoinColumn = "Country")
#map the data
mapCountryData(db1,nameColumnToPlot = "Value",mapTitle = "World Map for Health Life Expectancy 2015",colourPalette ="terrain")





  
  
  
  
  
  
   
  
  
  
  
  
  
  
  
  
  
  
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
