#install.packages("httr")
#install.packages("ggmap")
#dg_coords <- ggmap::geocode("Barcelona")
#httr::GET()

## LOAD RESULTS
setwd("~/BGSE_Classes/thesis/results/")
data = read.csv("count.csv", stringsAsFactors = FALSE, header = T, row.names = 1)


## LOAD LIBRARIES & SET WORKING DIRECTORY
library(dplyr)
setwd("~/BGSE_Classes/thesis/data/")

## EXPLORE CLIENT FIELD
data = read.csv("subset_cols.csv")
colnames(data)
web_data = filter(data,Client =="Web")
print(dim(web_data)[1]/dim(data)[1]*100)

## EXPLORE ID FIELD
data = read.csv("subset_id.csv")
colnames(data)
dim(data)
distinct(data)

## Check Market shares
# subscription data 2012, source PTS:

subs <- data.frame("Telia"=20.8,"Comhem"=18.0,"Telenor"=16.4,"Bredband2"=3.4,"Bahnhof"=3.0,"Others"=38.5)
subs[2,] <- c(5778031,1771198,2648565+1850452,383189,608838,2966735+1600599+675639)

chi <- chisq.test(as.vector(subs[2,],mode="numeric"),p=as.vector(subs[1,],mode="numeric"), rescale.p = TRUE)

print(chi) ## p-value 2.2e-16 -> does not correspond!


## Map of Sweden

library(maptools)
library(data.table)
library(rgdal)
library(rgeos)
library(sp)

#Load the shapefile
shp <- readOGR(path.expand("~/BGSE_Classes/thesis/maps/svk/riks/an_riks.shp"),"an_riks",stringsAsFactors = F)

#plot it
plot(shp)

#get data from shapefile (dbf)
dbf <- shp@data
str(dbf)

dbf$ID_1


# PLOT COUNT CHART
setwd("~/BGSE_Classes/thesis/results/")
count_data = read.csv("count.csv", stringsAsFactors = FALSE, header = T, row.names = 1)
library(ggplot2)
library(dplyr)
library(reshape)
library(scales)

count_data['Year'] <- row.names(count_data)
bardata <- melt(count_data[,c(3,2,1,4)], id="Year") 
bardata$variable <- factor(bardata$variable,levels=c('Mobile.broadband',
                                                     'Fixed.broadband_WORLD',
                                                     'Fixed.broadband_SE'))

ggplot(data=bardata, aes(x=Year, y=value, fill=variable)) +
  geom_bar(stat="identity")+
  theme_classic()+ 
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)))+
  ylab("Observations")+
  scale_y_continuous(expand = c(0, 0),breaks=seq(0,35000000,5000000),
                     labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_fill_manual(values=c("#fecb00","#a6cee3","#005293"),name="",
                    labels=c("Mobile broadband     ","Fixed broadband abroad     ","Fixed broadband Sweden"))+
  theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))+
  theme(panel.grid.major = element_line(colour="grey80", linetype="dashed",size=0.2),
        panel.grid.major.x = element_line(colour="white", linetype="dashed",size=0))+
  theme(legend.text=element_text(size=14),legend.direction="horizontal",legend.position=c(0.5,1.1),plot.margin = unit(c(6,1,1,1), "lines"))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(size = rel(1.5)))+      
  theme(axis.text.y = element_text(size = rel(1.4)))      

ggsave("Observations_year.pdf",width=898/72,height=610/72)

# PLOT SPEED CHARTS
library(ggplot2)
library(dplyr)
library(reshape)
library(scales)
setwd("~/BGSE_Classes/thesis/results/")
avg_speed <- read.csv("speeds.csv", stringsAsFactors = FALSE, header = T, row.names = 2)
median_speed <- read.csv("speeds_median.csv", stringsAsFactors = FALSE, header = T, row.names = 2)
percentile_speed <- read.csv("speeds_percentiles.csv", stringsAsFactors = FALSE, header = T, row.names = 2)
avg_speed <- avg_speed[,-1]
median_speed <- median_speed[,-1]
percentile_speed <- percentile_speed[,-1]

l = list(median_speed, avg_speed, percentile_speed)
df_speeds <- Reduce(merge, lapply(l, function(x) data.frame(x, rn = row.names(x))))

df_dwl <- df_speeds[,c(1,2,4,6)]
colnames(df_dwl) <- c('Year','Median','Avg','Percentile')

df_upl <- df_speeds[,c(1,3,5,7)]
colnames(df_upl) <- c('Year','Median','Avg','Percentile')

# DOWNLOAD
dwldata <- melt(df_dwl, id="Year") 
dwldata$variable <- factor(dwldata$variable,levels=c('Avg','Percentile','Median'))


ggplot(data=dwldata, aes(x=Year, y=value, group=variable)) +
  geom_line(aes(colour = variable),size=1.5) +
  theme_classic()+ 
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)))+
  ylab("Download speed (Mbit/s)")+
  scale_y_continuous(breaks=seq(0,60,10),
                     labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_color_manual(values=c("#a6cee3","#005293","#fecb00"),name="",
                    labels=c("Average    ","Avg. excl. percentiles    ","Median"))+
  theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))+
  theme(panel.grid.major = element_line(colour="grey80", linetype="dashed",size=0.2),
        panel.grid.major.x = element_line(colour="white", linetype="dashed",size=0))+
  theme(legend.text=element_text(size=rel(2)),legend.direction="vertical",
        legend.position=c(0.3,0.85),
        legend.key.height=unit(3,"line"),legend.key.size=unit(1,"cm"),
        legend.background = element_rect(fill="transparent"))+
  theme(axis.title.y = element_text(size = rel(2)))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.text.x = element_text(size = rel(2)))+      
  theme(axis.text.y = element_text(size = rel(2)))      

ggsave("download_speeds.pdf",width=898/72,height=610/72)

# UPLOAD
upldata <- melt(df_upl, id="Year") 
upldata$variable <- factor(upldata$variable,levels=c('Avg','Percentile','Median'))


ggplot(data=upldata, aes(x=Year, y=value, group=variable)) +
  geom_line(aes(colour = variable),size=1.5) +
  theme_classic()+ 
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)))+
  ylab("Upload speed (Mbit/s)")+
  scale_y_continuous(breaks=seq(0,60,10), limits = c(0,60))+
  scale_color_manual(values=c("#a6cee3","#005293","#fecb00"),name="",
                     labels=c("Average    ","Avg. excl. percentiles    ","Median"))+
  theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))+
  theme(panel.grid.major = element_line(colour="grey80", linetype="dashed",size=0.2),
        panel.grid.major.x = element_line(colour="white", linetype="dashed",size=0))+
  theme(legend.text=element_text(size=rel(2)),legend.direction="vertical",
        legend.position=c(0.3,0.7),
        legend.key.height=unit(3,"line"),legend.key.size=unit(1,"cm"),
        legend.background = element_rect(fill="transparent"))+
  theme(axis.title.y = element_text(size = rel(2)))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.text.x = element_text(size = rel(2)))+      
  theme(axis.text.y = element_text(size = rel(2)))      

ggsave("upload_speeds.pdf",width=898/72,height=610/72)

# PLOT SPEED BAR CHARTS
library(ggplot2)
library(dplyr)
library(reshape)
library(scales)
setwd("~/BGSE_Classes/thesis/results/")
avg_speed <- read.csv("speeds.csv", stringsAsFactors = FALSE, header = T, row.names = 2)
median_speed <- read.csv("speeds_median.csv", stringsAsFactors = FALSE, header = T, row.names = 2)
percentile_speed <- read.csv("speeds_percentiles.csv", stringsAsFactors = FALSE, header = T, row.names = 2)
avg_speed <- avg_speed[,-1]
median_speed <- median_speed[,-1]
percentile_speed <- percentile_speed[,-1]

l = list(median_speed, avg_speed, percentile_speed)
df_speeds <- Reduce(merge, lapply(l, function(x) data.frame(x, rn = row.names(x))))
speeds_comp <- data.frame('Speed'=df_speeds[2,6],'Source'='IIS')
speeds_comp <- rbind(speeds_comp, data.frame('Speed'=(1437*7.14+1034*55.91+586*35.1)/(586+1437+1034),
                                             'Source'= 'SamKnows'))
speeds_comp <- rbind(speeds_comp, data.frame('Speed'=7.843,
                                             'Source'= 'Akamai'))
speeds_comp['Year'] <- 2012
speeds_comp <- rbind(speeds_comp, data.frame('Speed'=22.31,
                                             'Source'= 'Akamai',
                                             'Year'=2016))
speeds_comp <- rbind(speeds_comp, data.frame('Speed'=df_speeds[6,6],
                                             'Source'= 'IIS',
                                             'Year'=2016))
speeds_comp <- rbind(speeds_comp, data.frame('Speed'=70.37,
                                             'Source'= 'Ookla',
                                             'Year'=2016))

speeds_comp$Source <- factor(speeds_comp$Source,levels=c('Akamai','IIS','SamKnows','Ookla'))


ggplot(data=speeds_comp, aes(x=Year, y=Speed, fill=Source)) +
  geom_bar(stat='identity',width=2.8, position = 'dodge')+
  geom_text(data=speeds_comp,aes(x=Year,y=Speed,label=sprintf("%0.1f", round(Speed, digits = 1))),
            hjust=1.2,size=6,position = position_dodge(width = 2.8),color='white')+
  coord_flip()+
  theme_classic()+ 
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)))+
  #xlab("Year")+
  ylab('Donwload speed (Mbit/s)')+
  scale_x_continuous(breaks=c(2012,2016),
                     labels=c('2012','2016'))+
  scale_y_continuous(breaks=seq(0,60,10))+
  scale_fill_brewer(palette='Paired',name="",
                    breaks=c('Ookla','IIS','Akamai','SamKnows'))+
  theme(axis.line.x = element_blank(),axis.line.y = element_blank())+
  theme(panel.grid.major = element_line(colour="grey80", linetype="dashed",size=0.2),
        panel.grid.major.y = element_line(colour="white", linetype="dashed",size=0))+
  theme(legend.text=element_text(size=14),legend.position=c(0.863,0.4),
        plot.margin = unit(c(1,3,1,1), "lines"),
        legend.key.height=unit(3,"line"),legend.key.size=unit(1,"cm"),
        legend.background = element_rect(fill="transparent"))+
  #theme(axis.title.y = element_text(size = rel(1.8),angle=0,vjust=0.5))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.text.x = element_text(size = rel(2)))+      
  theme(axis.text.y = element_text(size = rel(2)),
        axis.ticks=element_blank(),axis.title.y=element_blank())      

ggsave("comparison_speeds.pdf",width=898/72,height=610/72)


# PLOT COUNT CHART
setwd("~/BGSE_Classes/thesis/results/")
operator_data <- read.csv("byoperator.csv", stringsAsFactors = FALSE, header = T, row.names = 1)
library(ggplot2)
library(dplyr)
library(reshape)
library(scales)


for(year in 2011:2017){
  operator_data[operator_data['Year']==year,3] = 100 * operator_data[operator_data['Year']==year,3]/sum(operator_data[operator_data['Year']==year,3])
}

operator_data$Operator <- factor(operator_data$Operator,levels=c('Others',
                                                     'Bredband2',
                                                     'Bahnhof',
                                                     'Com Hem',
                                                     'Telenor',
                                                     'Telia'))

ggplot(data=operator_data, aes(x=Year, y=Count, fill=Operator)) +
  geom_bar(stat="identity")+
  theme_classic()+ 
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)))+
  ylab("% Observations")+
  scale_y_continuous(expand = c(0, 0),breaks=seq(0,100,20))+
  scale_x_continuous(breaks=seq(2011,2016,1))+
  scale_fill_brewer(palette="Paired",name="")+
  theme(legend.text=element_text(size=14),
        legend.key.height=unit(2,"line"),legend.key.size=unit(1,"cm"))+
  theme(axis.title.y = element_text(size = rel(1.5)),
        plot.margin = unit(c(1,1,1,1), "lines"),
        axis.ticks.x=element_blank())+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(size = rel(1.7)))+      
  theme(axis.text.y = element_text(size = rel(1.7)))      

ggsave("byoperator_barchart.pdf",width=898/72,height=610/72)

# Check multinomial results

subs <- data.frame("Telia"=c(37.9,38.5,38.8,39.2,39.1,37.7),
                   "Comhem"=c(17.9,18.0,17.6,18.2,19.1,19.3),
                   "Telenor"=c(17.3,16.4,17.3,20.3,19.0,18.4),
                   "Bredband2"=c(3.4,3.4,3.5,3.8,4.3,4.7),
                   "Bahnhof"=c(2.8,3.0,3.4,3.6,4.5,4.7),
                   "Others"=c(20.8,20.8,19.3,14.8,14.0,15.1))

setwd("~/BGSE_Classes/thesis/results/")
operator_data <- read.csv("byoperator.csv", stringsAsFactors = FALSE, header = T, row.names = 1)
library(XNomial)

for(i in 1:6){
  print(i+2010)
  xmonte(as.vector(operator_data[operator_data['Year']==(i+2010),3],mode="numeric"),
         as.vector(subs[i,],mode="numeric"),
         detail=2,
         histobins=T)
}

#pdf('multinomial.pdf',width=898/72,height=610/72)
xmonte(as.vector(operator_data[operator_data['Year']==2011,3],mode="numeric"),
       as.vector(subs[1,],mode="numeric"),
       detail=2,
       histobins=T)
text(350,400,"P-value = 0",cex =2)
#dev.off()

# CALCULATE WEIGHTS
row.names(subs) <- 2011:2016

for(year in 2011:2017){
  operator_data[operator_data['Year']==year,3] = 100 * operator_data[operator_data['Year']==year,3]/sum(operator_data[operator_data['Year']==year,3])
}

weight <- subs

for(i in (1:6)){
  weight[i,] <- weight[i,]/as.vector(operator_data[operator_data['Year']==(2010+i),3],mode="numeric")
}

write.csv(weight,file="weights.csv")


# PLOT CORRECTED SPEED CHARTS
library(ggplot2)
library(dplyr)
library(reshape)
library(scales)
setwd("~/BGSE_Classes/thesis/results/")
percentile_speed <- read.csv("speeds_percentiles.csv", stringsAsFactors = FALSE, header = T, row.names = 2)
percentile_speed_corrected <- read.csv("speeds_percentile_corrected.csv", stringsAsFactors = FALSE, header = T, row.names = 2)
percentile_speed <- percentile_speed[,-1]
percentile_speed_corrected <- percentile_speed_corrected[,-1]
colnames(percentile_speed_corrected) <- c('Dwnl_corr',"Upl_corr")

l = list(percentile_speed,percentile_speed_corrected)
df_speeds <- Reduce(merge, lapply(l, function(x) data.frame(x, rn = row.names(x))))

df_dwl <- df_speeds[,c(1,2,4)]
colnames(df_dwl) <- c('Year','Percentile','Percentile corrected')

df_upl <- df_speeds[,c(1,3,5)]
colnames(df_upl) <- c('Year','Percentile','Percentile corrected')

# DOWNLOAD
dwldata <- melt(df_dwl, id="Year") 
dwldata$variable <- factor(dwldata$variable,levels=c('Percentile','Percentile corrected'))


ggplot(data=dwldata, aes(x=Year, y=value, group=variable)) +
  geom_line(aes(colour = variable),size=1.5) +
  theme_classic()+ 
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)))+
  ylab("Download speed (Mbit/s)")+
  scale_y_continuous(breaks=seq(0,60,10),limits=c(0,60))+
  scale_color_manual(values=c("#a6cee3","#005293"),name="",
                     labels=c("Corrected","Normal"),breaks=c('Percentile corrected','Percentile'))+
  theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))+
  theme(panel.grid.major = element_line(colour="grey80", linetype="dashed",size=0.2),
        panel.grid.major.x = element_line(colour="white", linetype="dashed",size=0))+
  theme(legend.text=element_text(size=rel(2)),legend.direction="vertical",
        legend.position=c(0.3,0.74),
        legend.key.height=unit(3,"line"),legend.key.size=unit(1,"cm"),
        legend.background = element_rect(fill="transparent"))+
  theme(axis.title.y = element_text(size = rel(2)))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.text.x = element_text(size = rel(2)))+      
  theme(axis.text.y = element_text(size = rel(2)))      

ggsave("download_speeds_corrected.pdf",width=898/72,height=610/72)

# UPLOAD
upldata <- melt(df_upl, id="Year") 
dwldata$variable <- factor(dwldata$variable,levels=c('Percentile','Percentile corrected'))


ggplot(data=upldata, aes(x=Year, y=value, group=variable)) +
  geom_line(aes(colour = variable),size=1.5) +
  theme_classic()+ 
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)))+
  ylab("Upload speed (Mbit/s)")+
  scale_y_continuous(breaks=seq(0,60,10), limits=c(0,60))+
  scale_color_manual(values=c("#a6cee3","#005293"),name="",
                     labels=c("Normal","Corrected"))+
  theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))+
  theme(panel.grid.major = element_line(colour="grey80", linetype="dashed",size=0.2),
        panel.grid.major.x = element_line(colour="white", linetype="dashed",size=0))+
  theme(legend.text=element_text(size=rel(2)),legend.direction="vertical",
        legend.position=c(0.3,0.58),
        legend.key.height=unit(3,"line"),legend.key.size=unit(1,"cm"),
        legend.background = element_rect(fill="transparent"))+
  theme(axis.title.y = element_text(size = rel(2)))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.text.x = element_text(size = rel(2)))+      
  theme(axis.text.y = element_text(size = rel(2)))      

ggsave("upload_speeds_corrected.pdf",width=898/72,height=610/72)

# PLOT SPEED BY OPERATOR BAR CHARTS
library(ggplot2)
library(dplyr)
library(reshape)
library(scales)
setwd("~/BGSE_Classes/thesis/results/")
speed_byop <- read.csv("speeds_byoperator.csv", stringsAsFactors = FALSE, header = T)
speed_byop <- speed_byop[,-1]

speed_byop$Operator <- factor(speed_byop$Operator,
                               levels=c('Telia','others','Telenor',
                                        'Bahnhof','Bredband2','Com Hem'))


ggplot(data=speed_byop, aes(x=Year, y=Dwl, fill=Operator)) +
  geom_bar(stat='identity',width=2.8, position = 'dodge')+
  geom_text(data=speed_byop,aes(x=Year,y=Dwl,label=sprintf("%0.1f", round(Dwl, digits = 1))),
            hjust=1.2,size=6,position = position_dodge(width = 2.8),color='white')+
  coord_flip()+
  theme_classic()+ 
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)))+
  #xlab("Year")+
  ylab('Donwload speed (Mbit/s)')+
  scale_x_continuous(breaks=c(2011,2016),
                     labels=c('2011','2016'))+
  scale_y_continuous(breaks=seq(0,90,20))+
  scale_fill_brewer(palette='Paired',name="",
                    breaks=c('Com Hem','Bredband2','Bahnhof','Telenor','others', 'Telia'))+
  theme(axis.line.x = element_blank(),axis.line.y = element_blank())+
  theme(panel.grid.major = element_line(colour="grey80", linetype="dashed",size=0.2),
        panel.grid.major.y = element_line(colour="white", linetype="dashed",size=0))+
  theme(legend.text=element_text(size=14),legend.position=c(0.92,0.4),
        plot.margin = unit(c(1,3,1,1), "lines"),
        legend.key.height=unit(3,"line"),legend.key.size=unit(1,"cm"),
        legend.background = element_rect(fill="transparent"))+
  #theme(axis.title.y = element_text(size = rel(1.8),angle=0,vjust=0.5))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.text.x = element_text(size = rel(2)))+      
  theme(axis.text.y = element_text(size = rel(2)),
        axis.ticks=element_blank(),axis.title.y=element_blank())      

ggsave("comparison_dwlspeeds_byop.pdf",width=898/72,height=610/72)

speed_byop$Operator <- factor(speed_byop$Operator,
                              levels=c('Com Hem','Telia','Telenor',
                                       'others','Bahnhof','Bredband2'))

ggplot(data=speed_byop, aes(x=Year, y=Upl, fill=Operator)) +
  geom_bar(stat='identity',width=2.8, position = 'dodge')+
  geom_text(data=speed_byop[7:12,],aes(x=Year,y=Upl,label=sprintf("%0.1f", round(Upl, digits = 1))),
            hjust=1.2,size=6,position = position_dodge(width = 2.8),color='white')+
  geom_text(data=speed_byop[1:6,],aes(x=Year,y=Upl,label=sprintf("%0.1f", round(Upl, digits = 1))),
            hjust=-0.2,size=6,position = position_dodge(width = 2.8),color='black')+
  coord_flip()+
  theme_classic()+ 
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)))+
  #xlab("Year")+
  ylab('Upload speed (Mbit/s)')+
  scale_x_continuous(breaks=c(2011,2016),
                     labels=c('2011','2016'))+
  scale_y_continuous(breaks=seq(0,90,20),limits = c(0,90))+
  scale_fill_brewer(palette='Paired',name="",
                    breaks=c('Bredband2','Bahnhof','others', 'Telenor','Telia','Com Hem'))+
  theme(axis.line.x = element_blank(),axis.line.y = element_blank())+
  theme(panel.grid.major = element_line(colour="grey80", linetype="dashed",size=0.2),
        panel.grid.major.y = element_line(colour="white", linetype="dashed",size=0))+
  theme(legend.text=element_text(size=14),legend.position=c(0.74,0.6),
        plot.margin = unit(c(1,3,1,1), "lines"),
        legend.key.height=unit(3,"line"),legend.key.size=unit(1,"cm"),
        legend.background = element_rect(fill="transparent"))+
  #theme(axis.title.y = element_text(size = rel(1.8),angle=0,vjust=0.5))+
  theme(axis.title.x = element_text(size = rel(2)))+
  theme(axis.text.x = element_text(size = rel(2)))+      
  theme(axis.text.y = element_text(size = rel(2)),
        axis.ticks=element_blank(),axis.title.y=element_blank())      

ggsave("comparison_uplspeeds_byop.pdf",width=898/72,height=610/72)


## OUTPUT MAP WITH COUNT

library(maptools)
library(data.table)
library(rgdal)
library(rgeos)
library(sp)

#Load the shapefile
shp <- readOGR(path.expand("~/BGSE_Classes/thesis/maps/svk/riks/an_riks.shp"),"an_riks",stringsAsFactors = F)

#plot it
plot(shp)

#get data from shapefile (dbf)
dbf <- shp@data
str(dbf)

#edit dbf
dbf <- data.table(shp@data)
dbf[,'COUNT'] <- 0

setwd("~/BGSE_Classes/thesis/results/")
dwl_region <- read.csv("dwl_byregion.csv", stringsAsFactors = FALSE, header = T)
dwl_region <- dwl_region[,-1]
year=2016
df_year <- dwl_region[dwl_region$Year==year,1:3]
regions <- df_year$Regioncode
df_year$Count <- 100 * df_year$Count/sum(df_year$Count)
df_year$Cat <-cut(df_year$Speed, c(30,40,50,60,70))

density <- data.frame('Regioncode'=c(10,20,9,21,13,23,6,8,7,25,12,1,4,3,17,24,22,19,14,18,5),
                      'Count'=c(99.73*75453,98.56*137716,98.02*26924,98.56*140668,98.39*137878,
                                99.95*65346,98.53*158894,99.45*116614,97.84*90333,97.72*125455,
                                99.73*598028,99.88*994683,97.58*131174,98.87*161165,96.53*137387,
                                95.37*128746,97.4*120713,99.17*127443,99.63*764205,99.82*139525,
                                98.61*209276)/100)

density$Count <- 100 * density$Count/sum(density$Count)
density$Cat <-cut(density$Count, c(0,5,10,15,20,25,30))

for(i in regions){
  dbf[dbf$LANSKOD==i,'COUNT'] <- round(df_year[df_year$Regioncode==i,'Speed'],1)
  dbf[dbf$LANSKOD==i,'CAT'] <- df_year[df_year$Regioncode==i,'Cat']
  
#dbf[dbf$LANSKOD==i,'COUNT'] <- round(density[density$Regioncode==i,'Count'],1)
#dbf[dbf$LANSKOD==i,'CAT'] <- density[density$Regioncode==i,'Cat']
}

#write back
shp@data <- as.data.frame(dbf)
writeOGR(shp,path.expand("~/BGSE_Classes/thesis/results/Broadband_speeds/maps/dwl_speed.shp"),"count_2016",driver="ESRI Shapefile")

# UPLOAD speeds maps
upl_region <- read.csv("upl_byregion.csv", stringsAsFactors = FALSE, header = T)
upl_region <- upl_region[,-1]
year=2016
df_year <- upl_region[upl_region$Year==year,1:3]
regions <- df_year$Regioncode
df_year$Count <- 100 * df_year$Count/sum(df_year$Count)
df_year$Cat <-cut(df_year$Speed, c(20,30,40,50))

density <- data.frame('Regioncode'=c(10,20,9,21,13,23,6,8,7,25,12,1,4,3,17,24,22,19,14,18,5),
                      'Count'=c(99.73*75453,98.56*137716,98.02*26924,98.56*140668,98.39*137878,
                                99.95*65346,98.53*158894,99.45*116614,97.84*90333,97.72*125455,
                                99.73*598028,99.88*994683,97.58*131174,98.87*161165,96.53*137387,
                                95.37*128746,97.4*120713,99.17*127443,99.63*764205,99.82*139525,
                                98.61*209276)/100)

density$Count <- 100 * density$Count/sum(density$Count)
density$Cat <-cut(density$Count, c(0,5,10,15,20,25,30))

for(i in regions){
  dbf[dbf$LANSKOD==i,'COUNT'] <- round(df_year[df_year$Regioncode==i,'Speed'],1)
  dbf[dbf$LANSKOD==i,'CAT'] <- df_year[df_year$Regioncode==i,'Cat']
  
  #dbf[dbf$LANSKOD==i,'COUNT'] <- round(density[density$Regioncode==i,'Count'],1)
  #dbf[dbf$LANSKOD==i,'CAT'] <- density[density$Regioncode==i,'Cat']
}

#write back
shp@data <- as.data.frame(dbf)
writeOGR(shp,path.expand("~/BGSE_Classes/thesis/results/Broadband_speeds/maps/upl_speed.shp"),"count_2016",driver="ESRI Shapefile")

