install.packages("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
getwd()
vp<-read.csv("~/VP_hydrology/CCBER_Vernal_Pool_Hydrology_0.csv")
vp<-vp[,c(3,4,7,8)]
vp$Date<-as.POSIXct(vp$Date, format="%m/%d/%Y %I:%M:%S %p")
vp<-vp[vp$Location!="more_mesa" & vp$Location!="copr" & 
         vp$Location!="Ellwood_mesa"& vp$Location!="sierra_madre"
       & vp$Location!="ellwood_mesa",]
head(vp)
colnames(vp)<-c("Date.Time","Location","VP_Name","Water_Level")
vp$Date<-paste0(substr(vp$Date.Time,1,10))
vp$Date<-as.Date(vp$Date,format="%Y-%m-%d")
unique(vp$Location)
unique(vp$VP_Name)

vp<- within(vp, VP_Name[VP_Name == " V"] <- 'V')
vp<- within(vp, VP_Name[VP_Name == " PH1"] <- 'Ph1')
vp<- within(vp, VP_Name[VP_Name == "5!"] <- '5')
vp<- within(vp, VP_Name[VP_Name == "2."] <- '2')
vp<- within(vp, VP_Name[VP_Name == "2 "] <- '2')
vp<- within(vp, VP_Name[VP_Name == "U          "] <- 'U')
vp<- within(vp, VP_Name[VP_Name == "6          "] <- '6')
vp<- within(vp, VP_Name[VP_Name == " 8     "] <- '8')
vp<- within(vp, VP_Name[VP_Name == "Ph1 "] <- 'Ph1')
vp<- within(vp, VP_Name[VP_Name == "Ph 1"] <- 'Ph1')
vp<- within(vp, VP_Name[VP_Name == "PH1"] <- 'Ph1')
vp<- within(vp, VP_Name[VP_Name == "WEST pond"] <- 'West Pond')
vp<- within(vp, VP_Name[VP_Name == "West Ponder"] <- 'West Pond')
vp<- within(vp, VP_Name[VP_Name == "South vernal pool"] <- 'South')
vp<- within(vp, VP_Name[VP_Name == "South pool"] <- 'South')
vp<- within(vp, VP_Name[VP_Name == "South Pool"] <- 'South') 
vp<- within(vp, VP_Name[VP_Name == "South "] <- 'South') 
vp<- within(vp, VP_Name[VP_Name == "East S pool"] <- 'East S') 
vp<- within(vp, VP_Name[VP_Name == "West pond"] <- 'West Pond') 
vp<- within(vp, VP_Name[VP_Name == "West Pond"] <- 'West Pond')
vp<- within(vp, VP_Name[VP_Name == "Western pond"] <- 'West Pond')
vp<- within(vp, VP_Name[VP_Name == "West Pond "] <- 'West Pond')
vp<- within(vp, VP_Name[VP_Name == "West pond "] <- 'West Pond')
vp<- within(vp, VP_Name[VP_Name == "North pool "] <- 'North')
vp<- within(vp, VP_Name[VP_Name == "North "] <- 'North')
vp<- within(vp, VP_Name[VP_Name == "Santa Rosa "] <- 'Santa Rosa')
vp<- within(vp, VP_Name[VP_Name == "San Miguel "] <- 'San Miguel')
vp<- within(vp, VP_Name[VP_Name == "01"] <- '1')
vp<- within(vp, VP_Name[VP_Name == "9 "] <- '9')
vp<- within(vp, VP_Name[VP_Name == "Main "] <- 'Main')
vp<- within(vp, VP_Name[VP_Name == "U "] <- 'U')
vp<- within(vp, VP_Name[VP_Name == "Santa Barbara "] <- 'Santa Barbara')
vp<- within(vp, VP_Name[VP_Name == "Anacapa West "] <- 'Anacapa West')
vp<- within(vp, VP_Name[VP_Name == "Santa Catalina "] <- 'Santa Catalina')
vp<- within(vp, VP_Name[VP_Name == "Manzanita Wetland"] <- 'Manzanita')
vp<- within(vp, VP_Name[VP_Name == "East - North"] <- 'East N')
vp<- within(vp, VP_Name[VP_Name == "East North"] <- 'East N')
vp<- within(vp, VP_Name[VP_Name == "East - South"] <- 'East S')
vp<- within(vp, VP_Name[VP_Name == "North Pool"] <- 'North')
vp<- within(vp, VP_Name[VP_Name == "North pool"] <- 'North')
vp<- within(vp, VP_Name[VP_Name == "Main Pool"] <- 'Main')
vp<- within(vp, VP_Name[VP_Name == "Main pool"] <- 'Main')
vp<- within(vp, VP_Name[VP_Name == "Main pool "] <- 'Main')
vp<- within(vp, VP_Name[VP_Name == "main"] <- 'Main')
vp<- within(vp, VP_Name[VP_Name == "south"] <- 'South')
vp<- within(vp, VP_Name[VP_Name == "East S pool"] <- 'East S')
vp<- within(vp, VP_Name[VP_Name == "16 "] <- '16')
vp<- within(vp, VP_Name[VP_Name == "19 "] <- '19')
vp<- within(vp, VP_Name[VP_Name == "7 "] <- '7')
vp<- within(vp, VP_Name[VP_Name == "Anacapa East "] <- 'Anacapa East')
vp<- within(vp, VP_Name[VP_Name == "Reference "] <- 'Reference')
vp<- within(vp, VP_Name[VP_Name == "East "] <- 'East')
vp<- within(vp, VP_Name[VP_Name == "south "] <- 'South')
vp<- within(vp, VP_Name[VP_Name == "Chumash "] <- 'Chumash')
vp<- within(vp, VP_Name[VP_Name == "SWW "] <- 'SWW')
vp<- within(vp, VP_Name[VP_Name == "SWW 01"] <- 'SWW')
vp<- within(vp, VP_Name[VP_Name == "North pool  "] <- 'North')
vp<- within(vp, VP_Name[VP_Name == "south east "] <- 'South East')
vp<- within(vp, VP_Name[VP_Name == "Santa Cruz "] <- 'Santa Cruz')
vp<- within(vp, VP_Name[VP_Name == "Chumash Wetland"] <- 'Chumash')
vp<- within(vp, VP_Name[VP_Name == "Chumash pool"] <- 'Chumash')
vp<- within(vp, VP_Name[VP_Name == "south east"] <- 'South East')
vp<- within(vp, VP_Name[VP_Name == "north"] <- 'North')
vp<- within(vp, VP_Name[VP_Name == "8 in"] <- '8')
vp<- within(vp, VP_Name[VP_Name == "Manzanita Lake"] <- 'Manzanita')
vp<- within(vp, VP_Name[VP_Name == "SWW1"] <- 'SWW 1')
vp<- within(vp, VP_Name[VP_Name == "Catalina"] <- 'Santa Catalina')
vp<- within(vp, VP_Name[VP_Name == "Phase 1"] <- 'Ph1')
vp<- within(vp, VP_Name[VP_Name == "Mini south"] <- 'Mini South')
vp<- within(vp, VP_Name[VP_Name == "MS"] <- 'Mini South')
vp<-vp[vp$VP_Name!="3-9" &vp$VP_Name!="Created pool by trees"&
          vp$VP_Name!="Redtail 2"&vp$VP_Name!="Redtail3"&
         vp$VP_Name!="Redtail 4"&vp$VP_Name!="Tadpole 2"&vp$VP_Name!="Tadpole 6"&
         vp$VP_Name!="Tadpol 7"&vp$VP_Name!="Whitetail 2",]

vp_locations<-vp[,2:3]

vp_locations<-unique(vp_locations[,c('Location','VP_Name')])
write.csv(vp_locations,"C:/Users/ccber/Documents/VP_hydrology/vp_locations.csv")

#Get wateryear function
getYearQuarter <- function(x,
                           firstMonth=10,
                           fy.prefix='WY',
                           quarter.prefix='Q',
                           sep='-',
                           level.range=c(min(x), max(x)) ) {
  if(level.range[1] > min(x) | level.range[2] < max(x)) {
    warning(paste0('The range of x is greater than level.range. Values ',
                   'outside level.range will be returned as NA.'))
  }
  quarterString <- function(d) {
    year <- as.integer(format(d, format='%Y'))
    month <- as.integer(format(d, format='%m'))
    y <- ifelse(firstMonth > 1 & month >= firstMonth, year+1, year)
    q <- cut( (month - firstMonth) %% 12, breaks=c(-Inf,2,5,8,Inf),
              labels=paste0(quarter.prefix, 1:4))
    return(paste0(fy.prefix, substring(y,3,4)))
  }
  vals <- quarterString(x)
  levels <- unique(quarterString(seq(
    as.Date(format(level.range[1], '%Y-%m-01')),
    as.Date(format(level.range[2], '%Y-%m-28')), by='month')))
  return(factor(vals, levels=levels, ordered=TRUE))
} 
unique(vp$VP_Name)
#Run function with our data
vp$date<-format(vp$Date.Time,format="%Y-%m-%d")
vp$date<-as.POSIXct(vp$date,format="%Y-%m-%d")
vp$wtr_yr <- getYearQuarter(vp$date, firstMonth=10)
vp$wk<-format(vp$Date.Time,format="%W")

write.csv(vp,"C:/Users/ccber/Documents/VP_hydrology/vp_clean.csv")



##################################################################################
#########################Water Quality############################################


WQ<-read.csv("VP_hydrology/Vernal_Pool_Water_Quality_Monitoring_0.csv")
WQ<-WQ[,c(3,4,7,9,10,11,12,13)]
colnames(WQ)<-c("Date","Location","VP_Name","DO_mg_L","DO_per","Conductivity_us_cm",
                "Salinity_ppt","Temp_C")
WQ$Date<-as.POSIXct(WQ$Date, format="%m/%d/%Y %I:%M:%S %p")
WQ<-WQ[WQ$Location!="more_mesa" & WQ$Location!="copr" & 
         WQ$Location!="Ellwood_mesa"& WQ$Location!="sierra_madre"
       & WQ$Location!="ellwood_mesa",]

WQ<- within(WQ, VP_Name[VP_Name == "Santa Rosa (MSRA, MSRB)"] <- 'Santa Rosa')
WQ<- within(WQ, VP_Name[VP_Name == "Santa Catalina (MSCA, MSCB)"] <- 'Santa Catalina')
WQ<- within(WQ, VP_Name[VP_Name == "Santa Barbara (MSBA, MSBB)"] <- 'Santa Barbara')
WQ<- within(WQ, VP_Name[VP_Name == "San Miguel (MSMA,MSMB)"] <- 'San Miguel')
WQ<- within(WQ, VP_Name[VP_Name == "NCOS1"] <- '1')
WQ<- within(WQ, VP_Name[VP_Name == "NCOS8"] <- '8')
WQ<- within(WQ, VP_Name[VP_Name == "Pool 6"] <- '6')
WQ<- within(WQ, VP_Name[VP_Name == "Pool E"] <- 'E')
WQ<- within(WQ, VP_Name[VP_Name == "Pool W"] <- 'W')
WQ<- within(WQ, VP_Name[VP_Name == "NP9"] <- '9')
WQ<- within(WQ, VP_Name[VP_Name == "NCOS4"] <- '4')
WQ<- within(WQ, VP_Name[VP_Name == "Pool N"] <- 'N')
WQ<- within(WQ, VP_Name[VP_Name == "NCOS2"] <- '2')
WQ<- within(WQ, VP_Name[VP_Name == "NP19"] <- '19')
WQ<- within(WQ, VP_Name[VP_Name == "Santa Catalina "] <- 'Santa Catalina')

write.csv(WQ,"VP_hydrology/VP_WQ_Clean.csv")

