#This script is to do QA on CRMS hydro data
#input: path to hydro file in .csv format

#output: .pdf file of diagnostic charts and tables for each site represented
#         in input file

#load some useful functions
library(xtable)
library(svglite)

#enter path to the tools folder used by this script
tool_path<-"Tools/"
preamb<-paste(readLines(paste0(tool_path,"html_preamble.txt")),collapse = "\n")
sonde_specs<-read.csv(paste0(tool_path,"SONDEspecs.csv"),check.names = F)
grps<-read.csv(paste0(tool_path,"groupspecs.csv"),check.names = F)


#enter path to input file between the quotes below (this points to a file)
in_path<-"Data/ME July.csv"

#enter path to desired location for output files (this points to a folder)
out_path<-"Output/"



#load the data 
dat<-read.csv(in_path,check.names = F)


#####create group plots####
station_short_names<-unique(sapply(strsplit(dat[,"Station ID"],"-"),"[",1))
unique(dat[,"Station ID"])
grp_list<-grps[grps$CRMS_SITE%in%station_short_names,"Group Name"]
names(grp_list)<-grps[grps$CRMS_SITE%in%station_short_names,"CRMS_SITE"]
sonde_list<-unique(dat[,"Station ID"])
names(sonde_list)<-station_short_names
grp_iter<-unique(grp_list)
clrs<-c("#003f5c","#bc5090","#ff6361","#58508d","#ffa600")
grp_plts_water<-list()
grp_plts_sal<-list()

for(i in grp_iter){
  s2p<-sonde_list[names(grp_list[which(grp_list==i)])]
  dat_2_plt<-dat[which(dat[,"Station ID"]%in%s2p),c(1,2,3,12,18)]
  dates<-format(as.Date(dat_2_plt[,"Date (mm/dd/yyyy)"],format="%m/%d/%Y"),"%m/%d/%Y")
  target_rows<-grep("Requested",dat_2_plt$`Data Source`)
  if(!all(is.na(dat_2_plt$`Adjusted Water Elevation to Datum (ft)`[target_rows]))){
    s <- svgstring()
  ylims<-range(dat_2_plt$`Adjusted Water Elevation to Datum (ft)`[target_rows],na.rm = T)
  who<-which(dat_2_plt$`Station ID`==s2p[1])[which(dat_2_plt$`Station ID`==s2p[1])%in%target_rows]
  marshelv<-sonde_specs$marshelv[which(sonde_specs$`Station ID`==s2p[1])]
  par(mar=c(10.1,4.1,4.1,2.1),xpd=F)
  plot(1:length(who),dat_2_plt$`Adjusted Water Elevation to Datum (ft)`[who],
       type = "l",col=clrs[1],ylim = ylims,ylab = "Adjusted Water Elevation to Datum (ft)",bty='n',
       xaxt='n',main=i,xlab='')
  abline(h=marshelv,lty=2,col=clrs[1])
  if(length(s2p)>1){
  for(j in 2:length(s2p)){
    who<-which(dat_2_plt$`Station ID`==s2p[j])[which(dat_2_plt$`Station ID`==s2p[1])%in%target_rows]
    marshelv<-sonde_specs$marshelv[which(sonde_specs$`Station ID`==s2p[j])]
    lines(1:length(who),dat_2_plt$`Adjusted Water Elevation to Datum (ft)`[who],col=clrs[j])
    abline(h=marshelv,lty=2,col=clrs[j])
  }}
    axis(side = 1,at = seq(1,length(who),25),labels =dates[seq(1,length(who),25)],las=2,cex.axis=.75)
    par(xpd=T)
legend("bottom",inset =c(0,-.3) ,legend = unlist(sapply(s2p,function(x)paste0(x,c("Water","Marsh")))),
       col=rep(clrs[1:length(s2p)],each=length(s2p)),bty='n',lty=rep(c(1:2),length(s2p)),cex=.75,ncol = length(s2p),lwd=1.5)
                                                                                                  
     grp_plts_water[[i]]<-htmltools::HTML(s())
     dev.off()  
   }
 
  #salinity group plots
  if(!all(is.na(dat_2_plt$`Adjusted Salinity (ppt)`[target_rows]))){
    s <- svgstring()
    ylims<-range(dat_2_plt$`Adjusted Salinity (ppt)`[target_rows],na.rm = T)
    who<-which(dat_2_plt$`Station ID`==s2p[1])[which(dat_2_plt$`Station ID`==s2p[1])%in%target_rows]
    #marshelv<-sonde_specs$marshelv[which(sonde_specs$`Station ID`==s2p[1])]
    par(mar=c(10.1,4.1,4.1,2.1),xpd=F)
    plot(1:length(who),dat_2_plt$`Adjusted Salinity (ppt)`[who],
         type = "l",col=clrs[1],ylim = ylims,ylab = "Adjusted Salinity (ppt)",bty='n',
         xaxt='n',main=i,xlab='')
    #abline(h=marshelv,lty=2,col=clrs[1])
    if(length(s2p)>1){
      for(j in 2:length(s2p)){
        who<-which(dat_2_plt$`Station ID`==s2p[j])[which(dat_2_plt$`Station ID`==s2p[1])%in%target_rows]
        #marshelv<-sonde_specs$marshelv[which(sonde_specs$`Station ID`==s2p[j])]
        lines(1:length(who),dat_2_plt$`Adjusted Salinity (ppt)`[who],col=clrs[j])
        #abline(h=marshelv,lty=2,col=clrs[j])
      }}
    axis(side = 1,at = seq(1,length(who),25),labels =dates[seq(1,length(who),25)],las=2,cex.axis=.75)
    par(xpd=T)
    legend("bottom",inset =c(0,-.25) ,legend = s2p,
           col=clrs[1:length(s2p)],bty='n',lty=1,cex=.75,lwd=1.5,horiz = T)
    grp_plts_sal[[i]]<-htmltools::HTML(s())
    dev.off()
  }
  
  
}

#### process QA for each station ####
df<-split.data.frame(dat,f=as.factor(dat[,"Station ID"]))

#set target columns
cols<-c("Raw Salinity (ppt)","Adjusted Salinity (ppt)","Raw Water Level (ft)", "Adjusted Water Level (ft)","Adjusted Water Elevation to Datum (ft)", "Adjusted Water Elevation to Marsh (ft)",
        "Raw Specific Conductance (uS/cm)","Adjusted Specific Conductance (uS/cm)","Raw Battery (V)","Adjusted Battery (V)")


#process data by site
for(j in 1:length(df)){
temp_dat<-df[[j]]
target_rows<-grep("Requested",temp_dat[,1])

stat_id<-temp_dat[1,"Station ID"]
len<-dim(temp_dat)[1]
#get dates
dates<-format(as.Date(temp_dat[,"Date (mm/dd/yyyy)"],format="%m/%d/%Y"),"%m/%d/%Y")

#get start and stop date-time for subset
min_date<-min(dates[target_rows])
max_date<-max(dates[target_rows])

time_at_min_date<-temp_dat[min(which(dates==min_date)),"Time (hh:mm:ss)"]
time_at_max_date<-temp_dat[max(which(dates==max_date)),"Time (hh:mm:ss)"]

#create table 1
tab_1<-xtable(data.frame(station_name=temp_dat[1,"Station ID"],N=dim(temp_dat[target_rows,])[1],start_date=min_date,
                  start_time=time_at_min_date,end_date=max_date,end_time=time_at_max_date),caption = " Date Ranges for Data Collected:<br> (N = the total number of readings)")


#create table 2
tab_2<-xtable(cbind(` N `=apply(temp_dat[target_rows,cols],2,function(x)as.integer(sum(!is.na(x)))),
                    ` Mean `=apply(temp_dat[target_rows,cols],2,mean,na.rm=TRUE),
                    ` SD `=apply(temp_dat[target_rows,cols],2,sd,na.rm=TRUE),
      ` Min `=apply(temp_dat[target_rows,cols],2,min,na.rm=TRUE),
      ` Max `=apply(temp_dat[target_rows,cols],2,max,na.rm=TRUE),
      ` Miss `=apply(temp_dat[target_rows,cols],2,function(x)as.integer(sum(is.na(x))))),
      caption = "Summary Statistics")

#find time intervals for subset
times<-as.POSIXct(paste(temp_dat[target_rows,"Date (mm/dd/yyyy)"],temp_dat[target_rows,"Time (hh:mm:ss)"],
                        temp_dat[target_rows,"Time Zone"],sep=" "),format=" %m/%d/%Y %H:%M:%S")
na_times<-which(is.na(times))
if(length(na_times)>0)times<-times[-na_times]
time_intervals<-as.numeric(diff(times))

#create table 3
intv<-table(time_intervals)
tab_3<-xtable(data.frame(interval=as.numeric(names(intv)),counts=as.numeric(intv)),caption = "Interval Frequencies")

#create table 4
who<-which(time_intervals!=1)
if(length(who)>0){
  bad_intvs<-temp_dat[target_rows[who],c("Station ID","Date (mm/dd/yyyy)","Time (hh:mm:ss)")]
  tab_4<-xtable(data.frame(Obs=rownames(bad_intvs),Station=bad_intvs[,"Station ID"],Date=bad_intvs[,"Date (mm/dd/yyyy)"],
                  Time=bad_intvs[,"Time (hh:mm:ss)"],Interval=time_intervals[who]),caption="Aberrant Intervals")
} else {
  tab_4<-xtable(data.frame(Obs=NA,Station=NA,Date=NA,
                    Time=NA,Interval=NA),caption="Aberrant Intervals")
}

# check of shifts between raw and adjusted specific conductance and water levels
#specific conductance shift

tab_5<-xtable(data.frame(date=c(temp_dat[min(which(dates==min_date)),"Date (mm/dd/yyyy)"],temp_dat[len,"Date (mm/dd/yyyy)"]),
           `Raw Specific Conductance (uS/cm)`=c(temp_dat[min(which(dates==min_date)),cols[7]],temp_dat[len,cols[7]]),
           `Adjusted Specific Conductance (uS/cm)`=c(temp_dat[min(which(dates==min_date)),cols[8]],temp_dat[len,cols[8]]),
           diff.sp.con=c(temp_dat[min(which(dates==min_date)),cols[8]]-temp_dat[min(which(dates==min_date)),cols[7]],temp_dat[len,cols[8]]-temp_dat[len,cols[7]]),
           pct.diff.sp.con=c((temp_dat[min(which(dates==min_date)),cols[7]]-temp_dat[min(which(dates==min_date)),cols[7]])/(temp_dat[min(which(dates==min_date)),cols[8]]+1e-6),
                             (temp_dat[len,cols[8]]-temp_dat[len,cols[7]])/(temp_dat[len,cols[8]]+1e-6)),check.names = F),caption = "Specific Conductance Shifts")

#water level shift
tab_6<-xtable(data.frame(date=c(temp_dat[min(which(dates==min_date)),"Date (mm/dd/yyyy)"],temp_dat[len,"Date (mm/dd/yyyy)"]),
          `Raw Water Level (ft)`=c(temp_dat[min(which(dates==min_date)),cols[3]],temp_dat[len,cols[3]]),
          `Adjusted Water Level (ft)`=c(temp_dat[min(which(dates==min_date)),cols[4]],temp_dat[len,cols[4]]),
           diff.wl=c(temp_dat[min(which(dates==min_date)),cols[4]]-temp_dat[min(which(dates==min_date)),cols[3]],temp_dat[len,cols[4]]-temp_dat[len,cols[3]]),
           pct.diff.wl=c((temp_dat[min(which(dates==min_date)),cols[4]]-temp_dat[min(which(dates==min_date)),cols[3]])/(temp_dat[min(which(dates==min_date)),cols[4]]+1e-3),
                             (temp_dat[len,cols[4]]-temp_dat[len,cols[3]])/(temp_dat[len,cols[4]]+1e-3)),check.names = F),caption = "Water Level Shifts")

#find values <> 3sd of mean for Adjusted Salinity and Adjusted Water Level
flag<-c("<3sd",">3sd")
mean_sal<-mean(temp_dat[target_rows,cols[2]],na.rm = T)
sd_sal<-sd(temp_dat[target_rows,cols[2]],na.rm = T)
who_sal<-which(temp_dat[,cols[2]]>mean_sal+3*sd_sal|temp_dat[,cols[2]]<mean_sal-3*sd_sal|is.na(temp_dat[,cols[2]]))
who_sal<-who_sal[who_sal%in%target_rows]

#create plots of salinity
#raw
s <- svgstring()
ylims<-range(c(temp_dat[,cols[1]],temp_dat[,cols[1]]),na.rm = T)*c(.90,1.10)
plot(1:len,temp_dat[,cols[1]],type = "l",xaxt='n',ylab="Raw Salinity (ppt)",ylim = ylims,main=stat_id,bty='l',xlab="")
abline(v=min(which(dates==min_date)),lty=3,lwd=2)
axis(side = 1,at = seq(1,len,25),labels =dates[seq(1,len,25)],las=2,cex.axis=.75)
plt_sal_raw<-htmltools::HTML(s())
dev.off()

#adjusted
s <- svgstring()
ylims<-c(min(mean_sal-2.5*sd_sal,.90*min(temp_dat[,cols[2]],na.rm = T),na.rm = T),max(mean_sal+2.5*sd_sal,1.10*max(temp_dat[,cols[2]],na.rm = T),na.rm = T))
plot(1:len,temp_dat[,cols[2]],type = "l",xaxt='n',ylab="Adjusted Salinity (ppt)",ylim = ylims,main=stat_id,bty='l',xlab="")
#abline(h=mean_sal,col="blue",lwd=2)
abline(h=mean_sal+2*sd_sal,col="blue",lwd=2,lty=2)
abline(h=mean_sal-2*sd_sal,col="blue",lwd=2,lty=2)
abline(v=min(which(dates==min_date)),lty=3,lwd=2)
axis(side = 1,at = seq(1,len,25),labels =dates[seq(1,len,25)],las=2,cex.axis=.75)
#legend("topright",legend = c("raw","adjusted"),lty=1,col=c("darkgrey","black"),bty='n')
plt_sal_adj<-htmltools::HTML(s())
dev.off()

#raw and adjusted
s <- svgstring()
ylims<-c(min(mean_sal-2.5*sd_sal,.90*min(temp_dat[,cols[2]],na.rm = T),na.rm = T),max(mean_sal+2.5*sd_sal,1.10*max(temp_dat[,cols[2]],na.rm = T),na.rm = T))
plot(1:len,temp_dat[,cols[2]],type = "l",xaxt='n',ylab="Salinity (ppt)",ylim = ylims,main=stat_id,bty='l',xlab="")
lines(1:len,temp_dat[,cols[1]],col="darkgrey")
#abline(h=mean_sal,col="blue",lwd=2)
abline(h=mean_sal+2*sd_sal,col="blue",lwd=2,lty=2)
abline(h=mean_sal-2*sd_sal,col="blue",lwd=2,lty=2)
abline(v=min(which(dates==min_date)),lty=3,lwd=2)
axis(side = 1,at = seq(1,len,25),labels =dates[seq(1,len,25)],las=2,cex.axis=.75)
legend("topright",legend = c("raw","adjusted"),lty=1,col=c("darkgrey","black"),bty='n')
plt_sal_both<-htmltools::HTML(s())
dev.off()

if(length(who_sal)>0){
  bad_sals<-temp_dat[who_sal,c("Station ID","Date (mm/dd/yyyy)","Time (hh:mm:ss)","Adjusted Salinity (ppt)")]
  flg_ind<-as.integer(mean_sal-bad_sals[,4]<0)+1
  flg<-flag[flg_ind]
  flg[is.na(flg)]<-"missing"
  tab_7<-xtable(data.frame(Obs=rownames(bad_sals),Station=bad_sals[,"Station ID"],Date=bad_sals[,"Date (mm/dd/yyyy)"],
                    Time=bad_sals[,"Time (hh:mm:ss)"],`Adjusted Salinity (ppt)`=bad_sals[,"Adjusted Salinity (ppt)"],Flag=flg,check.names = F),
                caption = "Extreme Salinities")
} else {
  tab_7<-xtable(data.frame(Obs=NA,Station=NA,Date=NA,
                           Time=NA,`Adjusted Salinity (ppt)`=NA,Flag=NA,check.names = F),caption="Extreme Salinities")
}

#Adjusted Water Level
flag<-c("<3sd",">3sd")
mean_wl<-mean(temp_dat[target_rows,cols[4]],na.rm = T)
sd_wl<-sd(temp_dat[target_rows,cols[4]],na.rm = T)
who_wl<-which(temp_dat[,cols[4]]>mean_wl+3*sd_wl|temp_dat[,cols[4]]<mean_wl-3*sd_wl|is.na(temp_dat[,cols[4]]))
who_wl<-who_wl[who_wl%in%target_rows]

# create plot of water level
#raw
s <- svgstring()
ylims<-range(c(temp_dat[,cols[3]]),na.rm = T)*c(.90,1.10)
plot(1:len,temp_dat[,cols[3]],type = "l",xaxt='n',ylim = ylims,ylab="Raw Water Level (ft)",main=stat_id,bty='n',xlab="")
abline(v=min(which(dates==min_date)),lty=3,lwd=2)
axis(side = 1,at = seq(1,len,25),labels =dates[seq(1,len,25)],las=2,cex.axis=.75)
plt_lvl_raw<-htmltools::HTML(s())
dev.off()
#adjusted to datum
s <- svgstring()
mean_wl_d<-mean(temp_dat[target_rows,cols[5]],na.rm = T)
sd_wl_d<-sd(temp_dat[target_rows,cols[5]],na.rm = T)
ylims<-c(min(mean_wl_d-2.5*sd_wl_d,.90*min(temp_dat[,cols[5]],na.rm = T),na.rm = T),max(mean_wl_d+2.5*sd_wl_d,1.10*max(temp_dat[,cols[5]],na.rm = T),na.rm = T))
plot(1:len,temp_dat[,cols[5]],type = "l",xaxt='n',ylim = ylims,ylab="Water Level Adjusted to Datum (ft)",main=stat_id,bty='l',xlab="")
marshelev<-sonde_specs$marshelv[which(sonde_specs$`Station ID`==stat_id)]
abline(h=marshelev,lty=2,lwd=2)
#abline(h=mean_wl_d,col="blue",lwd=2)
abline(h=mean_wl_d+2*sd_wl_d,col="blue",lwd=2,lty=2)
abline(h=mean_wl_d-2*sd_wl_d,col="blue",lwd=2,lty=2)
abline(v=min(which(dates==min_date)),lty=3,lwd=2)
axis(side = 1,at = seq(1,len,25),labels =dates[seq(1,len,25)],las=2,cex.axis=.75)
legend("topright",legend = c("adj to datum","marsh elev"),lty=c(1,2),col=c("black","black"),bty='n')
plt_lvl_datum<-htmltools::HTML(s())
dev.off()

s <- svgstring()
ylims<-c(min(mean_wl_d-2.5*sd_wl_d,.90*min(c(temp_dat[,cols[5]],temp_dat[,cols[3]]),na.rm = T),na.rm = T),max(mean_wl_d+2.5*sd_wl_d,1.10*max(c(temp_dat[,cols[5]],temp_dat[,cols[3]]),na.rm = T),na.rm = T))
plot(1:len,temp_dat[,cols[5]],type = "l",xaxt='n',ylim = ylims,ylab="Water Level (ft)",main=stat_id,bty='l',xlab="")
lines(1:len,temp_dat[,cols[3]],col="darkgrey")
abline(h=marshelev,lty=2,lwd=2)
abline(h=mean_wl_d+2*sd_wl_d,col="blue",lwd=2,lty=2)
abline(h=mean_wl_d-2*sd_wl_d,col="blue",lwd=2,lty=2)
abline(v=min(which(dates==min_date)),lty=3,lwd=2)
axis(side = 1,at = seq(1,len,25),labels =dates[seq(1,len,25)],las=2,cex.axis=.75)
legend("topright",legend = c("raw","adj to datum","marsh elev"),lty=c(1,1,2),col=c("darkgrey","black","black"),bty='n')
plt_lvl_both<-htmltools::HTML(s())
dev.off()

#all three
s <- svgstring()
ylims<-c(min(mean_wl_d-2.5*sd_wl_d,.90*min(c(temp_dat[,cols[5]],temp_dat[,cols[3]]),na.rm = T),na.rm = T),max(mean_wl_d+2.5*sd_wl_d,1.10*max(c(temp_dat[,cols[5]],temp_dat[,cols[3]]),na.rm = T),na.rm = T))
plot(1:len,temp_dat[,cols[5]],type = "l",xaxt='n',ylim = ylims,ylab="Water Level (ft)",main=stat_id,bty='l',xlab="")
lines(1:len,temp_dat[,cols[4]],col="red")
lines(1:len,temp_dat[,cols[3]],col="darkgrey")
marshelev<-sonde_specs$marshelv[which(sonde_specs$`Station ID`==stat_id)]
abline(h=marshelev,lty=2,lwd=2)
axis(side = 1,at = seq(1,len,25),labels =dates[seq(1,len,25)],las=2,cex.axis=.75)
legend("topright",legend = c("raw","adj","adj to datum","marsh elev"),lty=c(1,1,1,2),col=c("darkgrey","red","black","black"),bty='n')
plt_lvl_all<-htmltools::HTML(s())
dev.off()

if(length(who_wl)>0){
  bad_wls<-temp_dat[who_wl,c("Station ID","Date (mm/dd/yyyy)","Time (hh:mm:ss)","Adjusted Water Level (ft)")]
  flg_ind<-as.integer(mean_wl-bad_wls[,4]<0)+1
  flg<-flag[flg_ind]
  flg[is.na(flg)]<-"missing"
  tab_8<-xtable(data.frame(Obs=rownames(bad_wls),Station=bad_wls[,"Station ID"],Date=bad_wls[,"Date (mm/dd/yyyy)"],
                           Time=bad_wls[,"Time (hh:mm:ss)"],`Adjusted Water Level (ft)`=bad_wls[,"Adjusted Water Level (ft)"],Flag=flg,check.names = F),
                caption = "Extreme Water Levels")
} else {
  tab_8<-xtable(data.frame(Obs=NA,Station=NA,Date=NA,
                           Time=NA,`Adjusted Water Level (ft)`=NA,Flag=NA,check.names = F),caption="Extreme Water Levels")
}

#compare water level adjustment to sonde spec
wl_adj<-data.frame(`Station ID`=temp_dat[,"Station ID"],diff=temp_dat[,cols[5]]-temp_dat[,cols[4]],check.names = F)
wl_sonde<-merge(wl_adj,sonde_specs[,1:2],by="Station ID")
who_sonde<-which(abs(wl_sonde$diff-wl_sonde$sensorelv)>1e-4)
who_sonde<-who_sonde[who_sonde%in%target_rows]

if(length(who_sonde)>0){
  bad_sonde<-temp_dat[who_sonde,c("Station ID","Date (mm/dd/yyyy)","Time (hh:mm:ss)","Adjusted Water Elevation to Datum (ft)","Adjusted Water Level (ft)")]
  tab_9<-xtable(data.frame(Obs=rownames(bad_sonde),Station=bad_sonde[,"Station ID"],Date=bad_sonde[,"Date (mm/dd/yyyy)"],
                           Time=bad_sonde[,"Time (hh:mm:ss)"],`Observed Adjustment (ft)`=wl_sonde[who_sonde,2],`Sonde Spec (ft)`=wl_sonde[who_sonde,3],Flag="Incorrect Sensor Elevation",check.names = F),
                caption = "Aberrant Water Adjustment:\n observed adjustment different than spec")
} else {
  tab_9<-xtable(data.frame(Obs=NA,Station=NA,Date=NA,
                           Time=NA,`Observed Adjustment (ft)`=NA,`Sonde Spec (ft)`=NA,Flag=NA,check.names = F),
                caption="Aberrant Water Adjustment:\n observed adjustment different than spec")
}

#any zero values specific conductance
who_zero_cond<-which(temp_dat[,cols[7]]<=0)
who_zero_cond<-who_zero_cond[who_zero_cond%in%target_rows]

if(length(who_zero_cond)>0){
  bad_cond<-temp_dat[who_zero_cond,c("Station ID","Date (mm/dd/yyyy)","Time (hh:mm:ss)","Adjusted Specific Conductance (uS/cm)")]
  tab_10<-xtable(data.frame(Obs=rownames(bad_cond),Station=bad_cond[,"Station ID"],Date=bad_cond[,"Date (mm/dd/yyyy)"],
                           Time=bad_cond[,"Time (hh:mm:ss)"],`Adjusted Specific Conductance (uS/cm)`=bad_cond[,"Adjusted Specific Conductance (uS/cm)"],Flag="value <=0",check.names = F),
                caption = "Aberrant Specific Conductivity")
} else {
  tab_10<-xtable(data.frame(Obs=NA,Station=NA,Date=NA,
                           Time=NA,`Adjusted Specific Conductance (uS/cm)`=NA,Flag=NA,check.names = F),
                caption="Aberrant Specific Conductivity")
}


#any zero values for battery
who_zero_batt<-which(temp_dat[,cols[9]]<=0)
who_zero_batt<-who_zero_batt[who_zero_batt%in%target_rows]

if(length(who_zero_batt)>0){
  bad_batt<-temp_dat[who_zero_batt,c("Station ID","Date (mm/dd/yyyy)","Time (hh:mm:ss)","Adjusted Battery (V)")]
  tab_11<-xtable(data.frame(Obs=rownames(bad_batt),Station=bad_batt[,"Station ID"],Date=bad_batt[,"Date (mm/dd/yyyy)"],
                            Time=bad_batt[,"Time (hh:mm:ss)"],`Adjusted Battery (V)`=bad_batt[,"Adjusted Battery (V)"],Flag="value <=0",check.names = F),
                 caption = "Aberrant Battery Readings")
} else {
  tab_11<-xtable(data.frame(Obs=NA,Station=NA,Date=NA,
                            Time=NA,`Adjusted Battery (V)`=NA,Flag=NA,check.names = F),
                 caption="Aberrant Battery Readings")
}

zero_sal_wl<-which(temp_dat[,cols[2]]==0|temp_dat[,cols[5]]==0)
zero_sal_wl<-zero_sal_wl[zero_sal_wl%in%target_rows]


if(length(zero_sal_wl)>0){
  bad_sal_wl<-temp_dat[zero_sal_wl,c("Station ID","Date (mm/dd/yyyy)","Time (hh:mm:ss)",cols[2],cols[5])]
 flag=c("zero salinity","zero water level")
 flg_ind<-flag[1+as.numeric(bad_sal_wl[,3]==0)]
  tab_12<-xtable(data.frame(Obs=rownames(bad_sal_wl),Station=bad_sal_wl[,"Station ID"],Date=bad_sal_wl[,"Date (mm/dd/yyyy)"],
                            Time=bad_sal_wl[,"Time (hh:mm:ss)"],Flag=flg_ind,check.names = F),
                 caption = "Exactly Zero for Salinity or Water Level to Datum")
} else {
  tab_12<-xtable(data.frame(Obs=NA,Station=NA,Date=NA,
                            Time=NA,Flag=NA,check.names = F),
                 caption="Exactly Zero for Salinity or Water Level to Datum")
}

#plot 3
s <- svgstring()
marshelev<-sonde_specs[which(sonde_specs$`Station ID`==stat_id),3]
par(mar=c(5.1, 4.1, 4.1 ,4.1))
ylims<-c(min(marshelev,min(temp_dat[,cols[5]],na.rm = T)),max(temp_dat[,cols[5]],na.rm = T))
plot(1:len,temp_dat[,cols[5]],type = "l",xaxt='n',ylab=cols[5],main=stat_id,bty='l',ylim = ylims,xlab="",col="black")
abline(h=marshelev,lty=2,col="black")
par(new=T)
ylims<-c(min(temp_dat[,cols[2]],na.rm = T),max(temp_dat[,cols[2]],na.rm = T))
plot(1:len,temp_dat[,cols[2]],type = "l",xaxt='n',ylab="",main=stat_id,bty='n',xlab="",yaxt="n",col="#58508d")
p2<-par()$yaxp
axis(side = 4,at=seq(p2[1],p2[2],length.out=5),labels =seq(p2[1],p2[2],length.out=5))
axis(side = 1,at = seq(1,len,25),labels =dates[seq(1,len,25)],las=2,cex.axis=.75)
abline(v=min(which(dates==min_date)),lty=3,lwd=2)
mtext(cols[2],side = 4,padj=4)
legend("topright",legend = c("adj to datum","sal","marsh elev"),lty=c(1,1,2),col = c("black","#58508d","black"),bty='n')
plt_sal_lvl<-htmltools::HTML(s())
dev.off()

#plot 4
#s <- svgstring()
#marshelev<-sonde_specs[which(sonde_specs$`Station ID`==stat_id),3]
#plot(1:len,temp_dat[,cols[5]],type = "l",xaxt='n',ylab=cols[5],main=stat_id,bty='n',xlab="",col="blue")
#abline(h=marshelev,lty=2,col="blue")
#axis(side = 1,at = seq(1,len,25),labels =dates[seq(1,len,25)],las=2,cex.axis=.75)
#abline(v=min(which(dates==min_date)),lty=3,lwd=2)
#legend("topright",c("water elev", "marsh elev"),col = "blue",lty = c(1,2),lwd=2,bty='n')
#plt_4<-htmltools::HTML(s())
#dev.off()

#create the html output for each station
sink(paste0(out_path,stat_id,".html"))
cat(preamb)
cat(paste0('<h1>',stat_id,' QC (', min_date,' -- ',max_date,')</h1>'),sep = "\n")
print(tab_1,type = "html",caption.placement = "top",include.rownames=FALSE,html.table.attributes='')
cat("<br>")
print(tab_2,type = "html",caption.placement = "top",include.rownames=TRUE,html.table.attributes='')
cat("<br>")
print(tab_3,type = "html",caption.placement = "top",include.rownames=FALSE,html.table.attributes='')
cat("<br>")
print(tab_4,type = "html",caption.placement = "top",include.rownames=FALSE,html.table.attributes='')
cat("<br>")
print(tab_5,type = "html",caption.placement = "top",include.rownames=FALSE,html.table.attributes='')
cat("<br>")
print(tab_6,type = "html",caption.placement = "top",include.rownames=FALSE,html.table.attributes='')
cat("<br>")
print(tab_7,type = "html",caption.placement = "top",include.rownames=FALSE,html.table.attributes='')
cat("<br>")
print(tab_8,type = "html",caption.placement = "top",include.rownames=FALSE,html.table.attributes='')
cat("<br>")
print(tab_9,type = "html",caption.placement = "top",include.rownames=FALSE,html.table.attributes='')
cat("<br>")
print(tab_10,type = "html",caption.placement = "top",include.rownames=FALSE,html.table.attributes='')
cat("<br>")
print(tab_11,type = "html",caption.placement = "top",include.rownames=FALSE,html.table.attributes='')
cat("<br>")
print(tab_12,type = "html",caption.placement = "top",include.rownames=FALSE,html.table.attributes='')
cat("<br>")
cat("<h2>Raw Salinity</h2>",sep="\n")
cat("Vertical Line Represents Separation of Data Sets",sep = "\n")
cat(plt_sal_raw)
cat("<br>")
cat("<h2>Adjusted Salinity with +/- 2 stdev of mean</h2>",sep="\n")
cat("Vertical Line Represents Separation of Data Sets",sep = "\n")
cat(plt_sal_adj)
cat("<br>")

cat("<h2>Raw and Adjusted Salinity with +/- 2 stdev of mean of Adjusted Salinity</h2>",sep="\n")
cat("Vertical Line Represents Separation of Data Sets",sep = "\n")
cat(plt_sal_both)

cat("<br>")
cat("<h2>Raw Water Level</h2>",sep="\n")
cat("Vertical Line Represents Separation of Data Sets",sep = "\n")
cat(plt_lvl_raw)

cat("<br>")
cat("<h2>Adjusted Water Level to Datum with +/-2 stdev of mean(ft)</h2>",sep="\n")
cat("Vertical Line Represents Separation of Data Sets",sep = "\n")
cat(plt_lvl_datum)

cat("<br>")
cat("<h2>Raw Water Level and Adjusted Water Level to Datum (ft)</h2>",sep="\n")
cat("Vertical Line Represents Separation of Data Sets",sep = "\n")
cat(plt_lvl_both)

cat("<br>")
cat("<h2>Raw Water Level, Adjusted Water Level, Adjusted Water Level to Datum, and Marsh Elevation</h2>",sep="\n")
cat("Vertical Line Represents Separation of Data Sets",sep = "\n")
cat(plt_lvl_all)

cat("<br>")
cat("<h2>Adjusted Water Level to Datum (ft), Adjusted Salinity (ppt), and Marsh Elevation</h2>",sep="\n")
cat("Vertical Line Represents Separation of Data Sets",sep = "\n")
cat(plt_sal_lvl)
cat('</body>
    </html>')
sink()
}

#create the group graph document
#parse in_file to get names for out_file
nm_parse_1<-tail(strsplit(in_path,"/")[[1]],1)
nm_parse_2<-strsplit(nm_parse_1,"\\.")[[1]][1]
sink(paste0(out_path,"Group graphs ",nm_parse_2,".html"))
cat(preamb)
cat(paste0('<h1>',stat_id,' QC (', min_date,' -- ',max_date,')</h1>'),sep = "\n")
cat("<h2>Group Graphs Water Elevation</h2>",sep="\n")
  for(k in 1:length(grp_plts_water)){
    cat("<br>")
    cat(grp_plts_water[[k]])
  }
cat("<h2>Group Graphs Salinity</h2>",sep="\n")
for(k in 1:length(grp_plts_sal)){
  cat("<br>")
  cat(grp_plts_sal[[k]])
}
cat('</body>
    </html>')
sink()
  
