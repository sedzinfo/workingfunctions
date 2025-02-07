##########################################################################################
# ICE
##########################################################################################
options(noaakey="wmUcxgwckVYpOHZHzeGugclTMpdRyWQW")
library(rnoaa)
library(ggplot2)
library(plyr)
library(rgdal)
ys<-seq(1990,2023,1)
urls<-lapply(ys,function(x) {
  data.frame(id=x,sea_ice(year=x,month="Jan",pole="S"))
})
out<-do.call(rbind,urls)
ggplot(out,aes(long,lat,group=group))+
  geom_polygon(fill="black")+
  theme_ice()+
  facet_wrap(~id,ncol=10)+
  coord_equal()
##########################################################################################
# 
##########################################################################################
# https://github.com/jbkunst/jbkunst.github.io/blob/master/_posts/2016-06-23-case-study-animation-and-others-vizs.md
library(highcharter)
library(workingfunctions)
df_nasa<-read.csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",header=TRUE,stringsAsFactors=FALSE,na.strings="***",skip=1)
dfm<-df_nasa<-df_nasa[,c("Year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")]
dfma<-data.frame(Year=df_nasa$Year,
                 lower=apply(dfm[,2:length(dfm)],1,min,na.rm=TRUE),
                 upper=apply(dfm[,2:length(dfm)],1,max,na.rm=TRUE),
                 decade=paste0(substr(df_nasa$Year,1,3),0))
dfr<-reshape::melt(df_nasa,id.vars="Year",variable_name="month")
df_nasa<-merge(dfr,dfma,sort=FALSE)
df_nasa$month<-as.character(df_nasa$month)
df_nasa$year_mon<-paste0(df_nasa$Year,"-",stringr::str_pad(sapply(df_nasa$month,function(x) grep(paste("(?i)",x,sep=""),month.abb)),2, pad = "0"),"-01")
names(df_nasa)<-tolower(names(df_nasa))
df_nasa<-df_nasa[,c("year_mon","value","lower","upper","year","decade","month")]
df_nasa$decade<-as.numeric(df_nasa$decade)
df<-df_nasa[order(df_nasa$year_mon),]
# df<-read.csv("https://raw.githubusercontent.com/hrbrmstr/hadcrut/master/data/temps.csv")
##########################################################################################
# 
##########################################################################################
df<-dplyr::mutate(df,date=lubridate::ymd(year_mon),
                  tmpstmp=datetime_to_timestamp(date),
                  year=lubridate::year(date),
                  month=lubridate::month(date,label=TRUE),
                  unite=colorize(value,viridis::viridis(10,option="B")),
                  unite=hex_to_rgba(unite,0.65))
dfcolyrs<-df %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(value=median(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(color_y=colorize(value,viridis::viridis(10,option="B")),color_y=hex_to_rgba(color_y,0.65)) %>%
  dplyr::select(-value)
df<-dplyr::left_join(df,dfcolyrs,by="year")
lsseries<-df %>%
  dplyr::group_by(year) %>%
  dplyr::do(data=.$value,color=dplyr::first(.$color_y)) %>%
  dplyr::mutate(name=year) %>%
  list_parse()
hc1<-highchart() %>%
  hc_chart(polar=TRUE) %>%
  hc_plotOptions(series=list(marker=list(enabled=FALSE),animation=TRUE,pointIntervalUnit="month")) %>%
  hc_legend(enabled=FALSE) %>%
  hc_xAxis(type="datetime",labels=list(format="{value:%B}")) %>%
  hc_tooltip(headerFormat="{point.key}",xDateFormat="%B",pointFormat="{series.name}: {point.y}") %>%
  hc_add_series_list(lsseries)
hc1
##########################################################################################
# 
##########################################################################################
lsseries2<-df %>%
  dplyr::group_by(year) %>%
  dplyr::do(data=.$value,color="transparent",enableMouseTracking=FALSE,color2=dplyr::first(.$color_y)) %>%
  dplyr::mutate(name=year) %>%
  list_parse()
hc11<-highchart() %>%
  hc_chart(polar=TRUE) %>%
  hc_plotOptions(series=list(marker=list(enabled=FALSE),animation=TRUE,pointIntervalUnit="month")) %>%
  hc_legend(enabled=FALSE) %>%
  hc_title(text="Animated Spiral") %>%
  hc_xAxis(type="datetime",min=0,max=365*24*36e5,labels=list(format="{value:%B}")) %>%
  hc_tooltip(headerFormat="{point.key}",xDateFormat="%B",pointFormat=" {series.name}: {point.y}") %>%
  hc_add_series_list(lsseries2) %>%
  hc_chart(events=list(load=JS("function() {
  console.log('ready');
  var duration=16*1000;
  var delta=duration/this.series.length;
  var delay=500;
  this.series.map(function(e){
    setTimeout(function() {e.update({color:e.options.color2,enableMouseTracking:true});e.chart.setTitle({text:e.name})
    },delay)
    delay=delay + delta;
  });
}")))
hc11
##########################################################################################
# 
##########################################################################################
hc2<-hc1%>%
  hc_chart(polar=FALSE,type="spline") %>%
  hc_xAxis(max=(365-1)*24*36e5) %>%
  hc_yAxis(tickPositions=c(-1.5,0,1.5))
hc2
##########################################################################################
# 
##########################################################################################
m<-df%>%
  dplyr::select(year,month,value) %>%
  tidyr::spread(year,value) %>%
  dplyr::select(-month) %>%
  as.matrix()
rownames(m)<-month.abb
m<-remove_nc(m,value=-1)
hc3<-hchart(m) %>%
  hc_colorAxis(stops=color_stops(10,viridis::viridis(10,option="B")),min=-1,max=1) %>%
  hc_yAxis(title=list(text=NULL),tickPositions=FALSE,labels=list(format="{value}",useHTML=TRUE))
hc3
##########################################################################################
# 
##########################################################################################
dsts<-df%>%
  dplyr::mutate(name=paste(decade,month)) %>%
  dplyr::select(x=tmpstmp,y=value,name)
hc4<-highchart() %>%
  hc_xAxis(type="datetime") %>%
  hc_yAxis(tickPositions=c(-1.5,0,1.5,2)) %>%
  hc_add_series(dsts,name="Global Temperature",type="line",color=hex_to_rgba(viridis::viridis(10,option="B")[7]),lineWidth=1,states=list(hover=list(lineWidth=1)),shadow=FALSE) 
hc4
##########################################################################################
# 
##########################################################################################
dscr<-df %>%
  dplyr::mutate(name=paste(decade,month)) %>%
  dplyr::select(x=tmpstmp,low=lower,high=upper,name,color=color_y)
hc5<-highchart() %>%
  hc_yAxis(tickPositions=c(-2,0,1.5,2)) %>%
  hc_xAxis(type="datetime") %>%
  hc_add_series(dscr,name="Global Temperature",type="columnrange")
hc5
##########################################################################################
# 
##########################################################################################
options(noaakey="wmUcxgwckVYpOHZHzeGugclTMpdRyWQW")
st<-rnoaa::isd_stations(refresh=FALSE)
st$LAT<-st$lat/1000
st$LON<-st$lon/1000
st$BEGIN<-as.numeric(substr(st$begin,1,4))
st$END<-as.numeric(substr(st$end,1,4))
st<-st[st$icao %in% "LGAV",]

mi.list1<-st[st$ctry=="GR"&(st$BEGIN<=2019&st$END>=2019&!is.na(st$BEGIN)),]
mi.list2<-st[st$ctry=="UK"&(st$BEGIN<=2019&st$END>=2019&!is.na(st$BEGIN)),]
mi.list3<-st[st$ctry=="UP"&(st$BEGIN<=2019&st$END>=2019&!is.na(st$BEGIN)),]
mi.list4<-st[st$ctry=="AU"&(st$BEGIN<=2019&st$END>=2019&!is.na(st$BEGIN)),]
mi.list5<-rbind(mi.list1,mi.list2)
mi.list6<-rbind(mi.list3,mi.list4)
mi.list<-st

outputs<-as.data.frame(matrix(NA,dim(mi.list)[1],2))
names(outputs)<-c("FILE","STATUS")
for (y in 2000:2022) {
  y.mi.list<-mi.list[mi.list$BEGIN <= y & mi.list$END >=y,]
  for (s in 1:dim(y.mi.list)[1]) {
    outputs[s,1]<-paste(sprintf("%06s",y.mi.list[s,1]),"-",sprintf("%05s",y.mi.list[s,2]),"-",y,".gz",sep="")
    wget<-paste("wget -P ~/Desktop/raw/ ftp://ftp.ncdc.noaa.gov/pub/data/noaa/",y,"/",outputs[s,1],sep="")
    outputs[s,2]<-try(system(wget,intern=FALSE,ignore.stderr=TRUE))
  }
}
system("gunzip -r ~/Desktop/raw",intern=FALSE,ignore.stderr=TRUE)
files<-list.files("~/Desktop/raw")

column.widths<-c(4,6,5,4,2,2,2,2,1,6,7,5,5,5,4,3,1,1,4,1,5,1,1,1,6,1,1,1,5,1,5,1,5,1)
stations<-data.frame(matrix(NA,length(files),6))
names(stations)<-c("USAFID","WBAN","YR","LAT","LONG","ELEV")

for (i in 1:length(files)) {
  data<-read.fwf(paste("~/Desktop/raw/",files[i],sep=""),column.widths)
  data<-data[,c(2:8,10:11,13,16,19,29,31,33)]
  names(data)<-c("USAFID","WBAN","YR","M","D","HR","MIN","LAT","LONG","ELEV","WIND.DIR","WIND.SPD","TEMP","DEW.POINT","ATM.PRES")
  data$LAT<-data$LAT/1000
  data$LONG<-data$LONG/1000
  data$WIND.SPD<-data$WIND.SPD/10
  data$TEMP<-data$TEMP/10
  data$DEW.POINT<-data$DEW.POINT/10
  data$ATM.PRES<-data$ATM.PRES/10
  write.csv(data,file=paste("~/Desktop/raw/",files[i],".csv",sep=""),row.names=FALSE)
  print(data)
  stations[i,1:3]<-data[1,1:3]
  stations[i,4:6]<-data[1,8:10]
}

stations<-stations[complete.cases(stations),]
mp<-ggmap::get_map(location=c(lon=stations$LONG[1],lat=stations$LAT[1]),maptype="terrain-background",scale=1)
ggmap::ggmap(mp)+geom_point(data=stations,aes(x=LONG[1],y=LAT[1],fill="red",alpha=0.8),size=.1,shape=1)+guides(fill=FALSE,alpha=FALSE,size=FALSE)

files<-grep(".csv",list.files("~/Desktop/raw/"),value=TRUE)
df_ev<-data.frame()
for (i in files) {
  df_ev<-plyr::rbind.fill(df_ev,read.csv(file=paste("~/Desktop/raw/",i,sep="")))
}
df_ev<-remove_nc(df_ev)
df_ev[df_ev=="999"]<-df_ev[df_ev=="999.9"]<-NA
df_ev[df_ev=="99999"]<-df_ev[df_ev=="9999.9"]<-NA
df_ev$date<-paste0(df_ev$YR,"-",
                   stringr::str_pad(df_ev$M,2,side="left","0"),"-",
                   stringr::str_pad(df_ev$D,2,side="left","0"))
df_ev[,c("USAFID","WBAN","YR","M","D","HR","MIN","LAT","LONG","ELEV","ATM.PRES","WIND.DIR")]<-NULL
df_max<-plyr::ddply(df_ev,"date",plyr::numcolwise(max,na.rm=TRUE))
df_min<-plyr::ddply(df_ev,"date",plyr::numcolwise(min,na.rm=TRUE))
rs_max<-reshape2::melt(df_max)
rs_min<-reshape2::melt(df_min)

rs_max$variable<-paste0(rs_max$variable,"_max")
rs_min$variable<-paste0(rs_min$variable,"_min")
da<-rbind(rs_max,rs_min)
n<-length(unique(da$variable))
line.colors<-rainbow(n,s=1,v=1,start=0,end=max(1,n-1)/n,alpha=.5)

library(plotly)
plotly::plot_ly(y=~round(as.numeric(da$value,2)),
                x=~as.Date(da$date),
                color=~da$variable,
                colors=line.colors,
                text=da$date,
                type='scatter',
                mode='lines') %>%
  layout(autosize=TRUE,
         margin=list(l=50,r=50,b=200,t=50,pad=4),
         title="EL. VENIZELOS AIRPORT SOURCE: NOAA",
         xaxis=list(title="TIME"),
         yaxis=list(title=""))

tsmax<-ts(df_max$TEMP,frequency=365,start=c(2004,1))
fit<-decompose(tsmax)
autoplot(fit) +
  ggtitle("X11 decomposition of electrical equipment index")


