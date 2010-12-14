# File-Name:       ip_map.R           
# Date:            2010-12-03                                
# Author:          Drew Conway
# Email:           drew.conway@nyu.edu                                      
# Purpose:         Generate map of IP addresses from blog post http://www.drewconway.com/zia/?p=2537
# Data Used:       Raw log files from drewconway.com
# Packages Used:   infochimps, ggplot2
# Output File:    
# Data Output:     
# Machine:         Drew Conway's MacBook Pro

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

library(infochimps)
library(ggplot2)
library(maps)

# Need to load and clean the data
log.data<-read.delim("you.log.data.txt", sep="-", header=FALSE, as.is=TRUE)

# Note, your log data may be formatted differently and the following data processing may not work properly

log.data<-data.frame(list("IP"=log.data$V1, "Date.Time"=log.data$V3, "Log"=log.data$V4), stringsAsFactors=FALSE)
log.data$IP<-gsub(" ","", log.data$IP)

# First, get the dates in useable format
log.data$Date.Time<-gsub("[\\[ ]","",log.data$Date.Time)
log.data$Date.Time<-strptime(log.data$Date.Time, format="%d/%b/%Y:%H:%M:%S ")

# Filter out only those logs accessing the right blog post
log.data<-log.data[grep("(\\?p\\=|index\\.php)",log.data$Log),]

# Create infochimps session
api.key<-"api.key<-"your.infochimps.api.key""
ic<-infochimps(api.key)

# Get lattitude and longitude data for all of the IPs
ips<-unique(log.data$IP)

get.latlong<-function(ip) {
    geo.data<-ip.geo(ip,ic)
    return(c(ip, geo.data$lat,geo.data$longitude))
}

# Create data frame to merge into log data
geo.data<-lapply(ips, get.latlong)  # this can take awhile depending on how many IPs you have
geo.df<-as.data.frame(do.call("rbind", geo.data),stringsAsFactors=FALSE)
names(geo.df)<-c("IP","Latitude","Longitude")
log.geo<-merge(log.data,geo.df,by="IP")
log.geo$Latitude<-as.numeric(log.geo$Latitude)
log.geo$Longitude<-as.numeric(log.geo$Longitude)

# Create counts, and sort chronologically
log.count<-ddply(log.geo,.(IP, Date.Time, Latitude, Longitude), summarise, Count=length(Log))
log.count<-log.count[with(log.count, order(Date.Time)),]

# Ready to visualize
world.map<-data.frame(map(plot=FALSE)[c("x","y")])

plot.num<-1
for(d in strftime(log.count$Date.Time)) {
    log.sub<-log.count[which(strftime(log.count$Date.Time)==d),]
    geo.plot<-ggplot(world.map, aes(x=x,y=y))+geom_path(aes(colour="grey"))
    geo.plot<-geo.plot+geom_point(data=log.sub, aes(x=Longitude, y=Latitude, color="red", alpha=0.75, size=Count))+
        annotate("text",x=-125,y=-5,label=strftime(d, format="%H:%M:%S"))+theme_bw()+
        scale_colour_manual(values=c("grey"="grey","red"="red"),legend=FALSE)+scale_alpha(legend=FALSE)+
        scale_size(legend=FALSE)+coord_map(projection="lagrange",ylim=c(-40,70),xlim=c(-145,155))+
        opts(panel.grid.major=theme_blank(),axis.ticks=theme_blank(),axis.text.x=theme_blank(),axis.text.y=theme_blank())+
        xlab("")+ylab("")
    ggsave(plot=geo.plot,filename=paste("images/maps/",plot.num,".png",sep=""),width=6,height=4)
    plot.num<-plot.num+1
}

# Run this at the command-line to join the files as a movie
# ffmpeg -f image2 -r 10 -i images/maps/%d.png -b 600k blogpost.mp4
