install.packages("lubridate")
library(lubridate)
#First upload rain data and get max and total
noaa<-read.csv("VP_hydrology/Monthly_NOAA.csv")
noaayrly<-aggregate(cbind(Precip_mm,AVG_Temp_C)~Rain.Year,noaa,FUN=max)
noaayrly<-noaayrly[noaayrly$Rain.Year>=2019,]
noaayrly$wtr_yr<-c("WY19","WY20","WY21","WY22","WY23","WY24")

#Load clean vp data
vp<-read.csv("VP_hydrology/vp_clean.csv")
vpq<-vp[vp$Water_Level!=0,]
vpmax
vpq$days<-7
vpqag<-aggregate(days~Location+VP_Name+wtr_yr,vpq,FUN=sum)
vpmax<-aggregate(Water_Level~Location+VP_Name+wtr_yr,vp,FUN=max)
vpqag
head(vpq)head(vpq)sum()


vpall<-merge(vpqag,noaayrly,by="wtr_yr",all.x=TRUE,all.y=TRUE)
vpall<-vpall[!is.na(vpall$VP_Name),]
str(vpall)
coeff<-0.2
vpall$Precip_in<-vpall$Precip_mm/25.4
vpall$Precip_cm<-vpall$Precip_mm/10
lubridate::ymd(vpall$Rain.Year, truncated = 2L)

for (i in unique(vpall$Location)){
  p<-vpall[vpall$Location==i,]
  q<- ggplot(p,aes(x=Rain.Year)) +
    geom_bar(stat="identity", position=position_dodge(),aes(y=days, fill=VP_Name))+ylab("Total Days Inundated")+xlab("Vernal Pool Name")+ 
    theme_bw()+geom_line(color="lightblue",size=1.5,aes(y=Precip_cm))+geom_point(aes(y=Precip_cm))+
    theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), 
          axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
          axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
          axis.text.y = element_text(size=18),title =element_text(size=18, face='bold'),
          strip.text.y = element_text(size = 18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.position = "right")+ggtitle(p$Location)+
    guides(fill=guide_legend(ncol=2))
  print(q)
}


vpmax<-vpmax[vpmax$Water_Level<90,]
for (i in unique(vpmax$Location)){
  p<-vpmax[vpmax$Location==i,]
  q<- ggplot(p,aes(x=wtr_yr)) +
    geom_bar(stat="identity", position=position_dodge(),aes(y=Water_Level, fill=VP_Name))+ylab("Maximum Water Level")+xlab("Vernal Pool Name")+ 
    theme_bw()+
    theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), 
          axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
          axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
          axis.text.y = element_text(size=18),title =element_text(size=18, face='bold'),
          strip.text.y = element_text(size = 18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.position = "right")+ggtitle(p$Location)+
    guides(fill=guide_legend(ncol=2))
  print(q)
}







