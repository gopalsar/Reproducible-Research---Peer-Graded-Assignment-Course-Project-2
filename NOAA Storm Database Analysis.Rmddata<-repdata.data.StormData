#Import the data into RStudio

repdata.data.StormData <- read.csv("C:/Users/Raymond/Desktop/repdata-data-StormData.csv")
View(repdata.data.StormData)

data<-repdata.data.StormData

#Checking data
library(plyr)
nmissing <- function(x) sum(is.na(x))
colwise(nmissing)(data)

#Transfer the EVTYPE,PROPDMGEXP and CROPDMGEXP to uppercase for aggregation
data_Transfer <- mutate(data, EVTYPE = toupper(EVTYPE), PROPDMGEXP = toupper(PROPDMGEXP), CROPDMGEXP = toupper(CROPDMGEXP), BGN_DATE = as.POSIXlt(data$BGN_DATE, format="%m/%d/%Y %H:%M:%S"))

#Sum the FATALITIES and INJURIES 
Death_Injury <- ddply(data, .(EVTYPE), summarize, Total_Harm = sum(FATALITIES + INJURIES))
Death_Injury <- Death_Injury[order(Death_Injury$Total_Harm, decreasing = T), ]
Top_Harm <- Death_Injury[1:10, ]

#Sum the PROPDMG and calculate real property damage
Property_Damage <- ddply(data, .(EVTYPE, PROPDMGEXP), summarize, PROPDMG = sum(PROPDMG))
Property_Damage <- mutate(Property_Damage, PropertyDamage = ifelse(toupper(PROPDMGEXP) =='K', PROPDMG*1000, ifelse(toupper(PROPDMGEXP) =='M', PROPDMG*1000000, ifelse(toupper(PROPDMGEXP) == 'B', PROPDMG*1000000000, ifelse(toupper(PROPDMGEXP) == 'H', PROPDMG*100, PROPDMG)))))
Property_Damage <- subset(Property_Damage, select = c("EVTYPE", "PropertyDamage"))
Property_Damage_total <- ddply(Property_Damage, .(EVTYPE), summarize, TotalPropDamage = sum(PropertyDamage))

#Sum the CROPDMG and calculate real crop damage 
Crop_Damage <- ddply(data, .(EVTYPE, CROPDMGEXP), summarize, CROPDMG = sum(CROPDMG))
Crop_Damage <- mutate(Crop_Damage, CropDamage = ifelse(toupper(CROPDMGEXP) =='K', CROPDMG*1000, ifelse(toupper(CROPDMGEXP) =='M', CROPDMG*1000000, ifelse(toupper(CROPDMGEXP) == 'B', CROPDMG*1000000000, ifelse(toupper(CROPDMGEXP) == 'H', CROPDMG*100, CROPDMG)))))
Crop_Damage <- subset(Crop_Damage, select = c("EVTYPE", "CropDamage"))
Crop_Damage_total <- ddply(Crop_Damage, .(EVTYPE), summarize, TotalCropDamage = sum(CropDamage))

#Merge the property damage data and crop damage
Damage <- merge(Property_Damage_total, Crop_Damage_total, by="EVTYPE")
Damage <- mutate(Damage, TotalDamage = TotalPropDamage + TotalCropDamage)
Damage <- Damage[order(Damage$TotalDamage, decreasing = T), ]
Top_Damage <- Damage[1:10, ]

library(ggplot2)
plot1 <- qplot(EVTYPE, Total_Harm, data = Top_Harm, fill= EVTYPE,xlab="Top 10 events",ylab="Casualties",main="Casualties due to severe weather events\nin the U.S from 1950-2011")
plot1 + theme(axis.text.x = element_text(angle = 45))
print(plot1)

plot2 <- qplot(EVTYPE, TotalDamage, data = Top_Damage, fill= EVTYPE,xlab="Top 10 events",ylab="Economic damage",main="Economic damage due to severe weather events\nin the U.S from 1950-2011")
plot2 + theme(axis.text.x = element_text(angle = 45))
print(plot2)
