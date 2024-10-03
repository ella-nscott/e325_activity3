#install.packages(c("dplyr", "lubridate", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

datCO2 <- read.csv("/cloud/project/annual-co-emissions-by-region.csv")

colnames(datCO2)[4] <- "CO2"

plot(datCO2$Year, datCO2$CO2, type = "l",
     xlab = "Year", ylab = "CO2 emissions (tons)")

NA_CO <- datCO2 %>%
  filter(Entity== "United States"|
         Entity=="Mexico"| Entity== "Canada")

#aes: aesthetics- columns we want to plot
ggplot(NA_CO, aes(x=Year, y=CO2, color=Entity))+
  geom_line()+
  labs(x="Year", y= expression(paste("CO"[2], "emissions (tons)")+
  theme_classic()
#expression paste supposed to subscript the 2- can look up other formatting for characters
  
  
#in class prompts
#prompt 1: Make a plot of air temperature anomalies in the Northern 
#and Southern Hemisphere in base R and in ggplot2.

climate <- read.csv("/cloud/project/climate-change.csv")

climate$Day <- ymd(climate$Day)

NorthHem <- climate %>%
  filter(Entity == "Northern Hemisphere")

SouthHem <- climate %>%
  filter(Entity == "Southern Hemisphere")

plot(NorthHem$Day, NorthHem$temperature_anomaly,
     xlab = "Date", ylab = "Temperature Anomaly", col="darkgoldenrod3",
     pch=19, bty= "n")
points(SouthHem$Day, SouthHem$temperature_anomaly,
       xlab= "Date", ylab = "Temperature Anomaly", col="black",
       pch=19, bty= "n")

legend("topleft", 
       c("Northern Hemisphere", "Southern Hemisphere"),
       col=c("darkgoldenrod3", "black"),
       pch=19, bty= "n")

Hemispheres <- climate %>%
  filter(Entity=="Northern Hemisphere"|
         Entity=="Southern Hemisphere")
ggplot(Hemispheres, aes(x=Day| y=temperature_anomaly| color=Entity))+ 
  geom_point()+
  labs(x="Year"| y="Temperature Anomaly")+
  theme_classic()


#prompt 2: Plot the total all time emissions for the
#United States, Mexico, and Canada.

#ggplot version generated earlier in script

us <- datCO2 %>%
  filter(Entity=="United States")
mexico <- datCO2%>%
  filter(Entity=="Mexico")
canada <- datCO2 %>%
  filter(Entity=="Canada")

plot(us$Year, us$CO2,
     xlab= "Year", ylab= "CO2 emissions (tons)", col="blue", type="l")
points(mexico$Year, mexico$CO2,
       xlab = "Year", ylab="CO2 emissions (tons)", col="green", type="l") 
points(canada$Year, canada$CO2,
       xlab = "Year", ylab= "CO2 emissions (tons)", col="red", type ="l")

legend("topleft",
       c("United States", "Mexico", "Canada"),
       col=c("blue", "green", "red"),
       pch="-", bty="l")

#Homework 3

#Question 1: Make a graph that communicates about emissions from any countries 
#of your choice

#create vectors of top five emittors (us vector already made)
china <- datCO2 %>%
  filter(Entity=="China")
russia <- datCO2 %>%
  filter(Entity=="Russia")
india <- datCO2 %>%
  filter(Entity == "India")
japan <- datCO2 %>%
  filter(Entity=="Japan")

#build new data frame

top_names <- c("United States", "China", "Russia", "India", "Japan")

top_CO2_tot <- c(sum(us$CO2), sum(china$CO2), sum(russia$CO2), sum(india$CO2),
                 sum(japan$CO2))

top_five <- data.frame(Country = top_names,
                       TotalCO2 = top_CO2_tot)

#build bar graph 

ggplot(data = top_five, aes(x = Country, y = TotalCO2, fill = Country)) + 
  geom_bar(stat = "identity", color="black") + 
  ggtitle("US Has Highest CO2 Emissions of Top 5 Emitting Countries")+
  theme_light()+
  guides(fill="none")+
  labs(y="Total CO2 Emitted by 2020")


#Question 2: You are tasked with communicating the change in world air 
#temperatures and CO2 emissions to a broad audience in 2 visually appealing 
#graphs. 


#Plot global CO2 levels
world_CO2 <- datCO2 %>%
  filter(Entity=="World")

ggplot(world_CO2, aes(x=Year, y=CO2, color=Entity))+
  geom_line()+
  labs(x="Year", y="CO2 emissions (tons)")+
  ggtitle("Global CO2 Emissions Increase Exponentially Over Time")+
  theme_light()+
  theme(legend.position="none")

#Plot global temperature anomalies
world_temp <- climate %>%
  filter(Entity=="World")

ggplot(world_temp, aes(x=Day, y=temperature_anomaly, color="Entity"))+
  geom_line()+
  labs(x="Year", y="Temperature Anomaly")+
  ggtitle("Global Temperature Anomalies More Severe Over Time")+
  theme_light()+
  theme(legend.position="none")


#Question 3: Remake graph from our world in data

#read in data from owid

heatwaves <- read.csv("/cloud/project/heat_waves_fig-3.csv")

#build chart

ggplot(heatwaves, aes(x=Year, y=Heat.wave.index, color="red"))+
  geom_line()+
  geom_point()+
  labs(x="Year", y="Heat Wave Index")+
  ggtitle("Annual Heat Wave Index in the United States",
          subtitle= "This index defines a heat wave as a period lasting at least four days with an average temperature that would only be
expected to occur once every 10 years, based on the historical record. The index value for a given year depends on
how often heat waves occur and how widespread they are.")+
  theme_classic()+
  theme(legend.position="none")+
  scale_x_continuous(NULL)+
  geom_hline(yintercept = seq(from =0.0, to=1.5, by=0.25), size=0.35, color="gray", linetype=3)+
  annotate(geom = "text", x = 2029, y = 0.25, label = "United States", color="red")







