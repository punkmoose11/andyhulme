library("XML")
library("readxl")
setwd("C:/Users/andyh/Documents/OMAHASERIES/Code")

ncaa <- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/College Baseball Games.xlsx",sheet = "Games")
ncaa <- as.data.frame(ncaa) 

ncaa$a <- ifelse(ncaa$Runs1>ncaa$Runs2,ncaa$Team1,ncaa$Team2)
ncaa$b <- ifelse(ncaa$Runs1>ncaa$Runs2,ncaa$Team2,ncaa$Team1)
ncaa$c <- ifelse(ncaa$Runs1>ncaa$Runs2,ncaa$Runs1,ncaa$Runs2)
ncaa$d <- ifelse(ncaa$Runs1>ncaa$Runs2,ncaa$Runs2,ncaa$Runs1)
ncaa$z <- 1
ncaa$flip <- ifelse(ncaa$Runs1>ncaa$Runs2,0,1)

ncaa <- ncaa[order(ncaa$Year, ncaa$a, ncaa$b, ncaa$c, ncaa$d), ]
for (i in 1:nrow(ncaa)){
if (i==1) {}
if (i> 1) {ncaa$z[i] <- ifelse(
		ncaa$a[i]==ncaa$a[i-1] & ncaa$b[i]==ncaa$b[i-1] & ncaa$c[i]==ncaa$c[i-1] & 
		ncaa$d[i]==ncaa$d[i-1] & ncaa$Year[i]==ncaa$Year[i-1],
		ncaa$z[i-1]+1,1)
}}

boyd<- read_excel("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/boyd.xlsx",sheet = "scores")
boyd<- as.data.frame(boyd) 

boyd<- boyd[c("Year","Date","Team1","Team2","Runs1","Runs2","Home")]
names(boyd) <- c("Year","Dateb","a","b","c","d","Homeb")
boyd$z <- 1

boyd <- boyd[order(boyd$Year, boyd$a, boyd$b, boyd$c, boyd$d), ]
for (i in 1:nrow(boyd)){
if (i==1) {}
if (i> 1) {boyd$z[i] <- ifelse(
		boyd$a[i]==boyd$a[i-1] & boyd$b[i]==boyd$b[i-1] & boyd$c[i]==boyd$c[i-1] & 
		boyd$d[i]==boyd$d[i-1] & boyd$Year[i]==boyd$Year[i-1],
		boyd$z[i-1]+1,1)
}}

ncaa <- subset(ncaa, Year>=1997)
boyd <- subset(boyd, Year>=1997)

ncaa$x<-1
boyd$y<-1

recon <- merge(ncaa, boyd, by=c("Year","a","b","c","d","z"),all=T)

recon <- subset(recon, c!=d)
recon <- subset(recon, Year!=2020)
recon <- subset(recon, ! (Year==1998 & (is.na(Conf1)==F|is.na(Conf2)==F) ))
recon <- subset(recon, ! (Year==2009 & (a=="Hawaii Hilo"|b=="Hawaii Hilo") ))
recon <- subset(recon, ! (Year==2008 & (a=="NC Central"|b=="NC Central") ))
recon <- subset(recon, ! (Year==2004 & (a=="Utah Valley"|b=="Utah Valley") ))
recon <- subset(recon, ! (Year==2004 & (a=="Northern Colo."|b=="Northern Colo.") ))
recon <- subset(recon, ! (Year==2000 & (a=="TAMUCC"|b=="TAMUCC") ))
recon <- subset(recon, ! (Year==1999 & (a=="Oakland"|b=="Oakland") ))
recon <- subset(recon, ! (Year==1999 & (a=="High Point"|b=="High Point") ))
recon <- subset(recon, ! (Year==1999 & (a=="Elon"|b=="Elon") ))
recon <- subset(recon, ! (Year==1999 & (a=="Belmont"|b=="Belmont") ))
recon <- subset(recon, ! (Year==1999 & (a=="Alabama A&M"|b=="Alabama A&M") ))
recon <- subset(recon, ! (Year==1997 & (a=="Norfolk St."|b=="Norfolk St.") ))
recon <- subset(recon, ! (Year==1997 & (a=="IUPUI"|b=="IUPUI") ))

subset(recon , is.na(y))[c("Year","a","b","c","d","x","y","z","Date","Dateb")]
subset(recon , is.na(x))[c("Year","a","b","c","d","x","y","z","Date","Dateb")]

#####

ncaa2 <- merge(ncaa, boyd[c("Year","a","b","c","d","z","Dateb","Homeb")], by=c("Year","a","b","c","d","z"),all.x=T)
ncaa2$Homeb <- ifelse(ncaa2$flip==1 & (ncaa2$Homeb==1|ncaa2$Homeb==2),ncaa2$Homeb%%2+1,ncaa2$Homeb)
ncaa2$Date <- ifelse(is.na(ncaa2$Date),paste(ncaa2$Dateb),paste(ncaa2$Date))

ncaa2 <- ncaa2[c("Year","Date","DH","Ord1","Ord2","Team1","Runs1","Team2","Runs2","Inn","Home","Loc",
			"Conf1","Conf2","Preconf","Override","Conf","Dateb","Homeb")]

nrow(ncaa)
nrow(ncaa2)
write.csv(ncaa2,file="C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/ncaa2.csv",na='')

head(ncaa2)
