setwd("C:/Users/andyh/Documents/OMAHASERIES/Code")
library("XML")
library(httr)

#for (years in 1997:2022) {
## download list of teams with numberic codes
#teams <- paste("http://stats.ncaa.org/team/inst_team_list?academic_year=",years,"&conf_id=-1&division=1&sport_code=MBA",sep="")
#teams1<- htmlParse(teams)
#teams2<- xpathApply(teams1, "//div[@class='css-panes']/div/table/tr/td/table/tr/td/a")
#teams3<- xpathApply(teams1, "//div[@class='css-panes']/div/table/tr/td/table/tr/td/a/@href")#

#names <- sapply(teams2, function(x) xmlValue(x))
#codes <- gsub("(/team/)(\\d+)(/\\d+)", "\\2", teams3)
#map <- cbind(names,codes)

#filed <- paste("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/collbase/base",years,".csv",sep="")
#write.csv(map, file = filed, quote=FALSE, row.names = FALSE)
#######################################################################################################
#}

#######################################################################################################

scrapenew <- function(yyyy,ncaa) {

super <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/collbase/superindex.xlsx",sheet = "superindex")
super <- as.data.frame(super) 
names(super)[names(super) == paste("codes",yyyy,sep="")] <- "index"
bb <- subset(super, index != "XXXXX")[c("codes","names","myname","index")]

bb$check <- grepl("zzz",bb$index)
bb <- subset(bb, check==F)
for (i in c(1:nrow(bb))) {

# read team pages
u1<- paste("http://stats.ncaa.org/team/",bb$codes[i],"/",ncaa,sep="")

#for (j in 1:10) {
#r1<- GET(u1)
#  if (r1$status_code!=200){print(paste(j,"Failure:",bb$names[i])) }
#  else {print(paste(j,"Success:",bb$names[i])); break;}
#}

h1<- htmlParse(u1)
print(paste(i,bb$names[i]))

# game level
games<- xpathApply(h1, "//table[1]/tr[1]/td[1]/fieldset/table[1]/tbody/tr")
team   <- rep(paste(bb$names[i]), times = length(games))
date   <- vector(,length(games))
opp   <- vector(,length(games))
score <- vector(,length(games))

for (j in seq(1, length(games), 2)) { 
w<- xpathApply(h1, paste("//table[1]/tr[1]/td[1]/fieldset/table[1]/tbody/tr[",j,"]/td[1]",sep=""))
x<- xpathApply(h1, paste("//table[1]/tr[1]/td[1]/fieldset/table[1]/tbody/tr[",j,"]/td[2]",sep=""))
y<- xpathApply(h1, paste("//table[1]/tr[1]/td[1]/fieldset/table[1]/tbody/tr[",j,"]/td[3]",sep=""))

date[j] <- gsub("^\\s+|\\s+$", "",gsub("\t","",gsub("\n","",xmlValue(w[[1]]))))
opp[j]  <- gsub("^\\s+|\\s+$", "",gsub("\t","",gsub("\n","",xmlValue(x[[1]]))))
score[j]<- gsub("^\\s+|\\s+$", "",gsub("\t","",gsub("\n","",xmlValue(y[[1]]))))
}

bytm <- cbind(team,date,opp,score)
if (i>1) {sched <- rbind(sched,bytm)} 
else if (i==1) {sched <- bytm}
}

write.csv(sched, file = paste("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/collbase/bbsched",yyyy,".csv",sep=""), 
quote=TRUE, row.names = FALSE)
#######################################################################################################
}

scrapenew(2021,15580);
scrapenew(2020,15204);
scrapenew(2019,14781);

#######################################################################################################

library("readxl")

scrape <- function(yyyy,ncaa) {

super <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/collbase/superindex.xlsx",sheet = "superindex")
super <- as.data.frame(super) 
names(super)[names(super) == paste("codes",yyyy,sep="")] <- "index"
bb <- subset(super, index != "XXXXX")[c("codes","names","myname","index")]

bb$check <- grepl("zzz",bb$index)
bb <- subset(bb, check==F)

for (i in c(1:nrow(bb))) {

# read team pages
u1<- paste("http://stats.ncaa.org/team/",bb$codes[i],"/",ncaa,sep="")

if (yyyy==2009 & bb$codes[i]==81) {u1<-"http://stats.ncaa.org/teams/319712"} # Bryant 
if (yyyy==2009 & bb$codes[i]==94) {u1<-"http://stats.ncaa.org/teams/319722"} # CS Bakersfld 
if (yyyy==2009 & bb$codes[i]==287) {u1<-"http://stats.ncaa.org/teams/319875"} # Hou Bapt 
if (yyyy==2009 & bb$codes[i]==660) {u1<-"http://stats.ncaa.org/teams/320179"} # SIUE 
if (yyyy==2009 & bb$codes[i]==30024) {u1<-"http://stats.ncaa.org/teams/320516"} # Utah Val 

if (yyyy==2008 & bb$codes[i]==649) {u1<-"http://stats.ncaa.org/teams/235819"} # S Dak St 
if (yyyy==2008 & bb$codes[i]==30024) {u1<-"http://stats.ncaa.org/teams/236165"} # Utah Val 

if (yyyy==2007 & bb$codes[i]==108) {u1<-"http://stats.ncaa.org/teams/234445"} # UC Davis 
if (yyyy==2007 & bb$codes[i]==363) {u1<-"http://stats.ncaa.org/teams/234642"} # Longwood 
if (yyyy==2007 & bb$codes[i]==493) {u1<-"http://stats.ncaa.org/teams/234748"} # NDSU 
if (yyyy==2007 & bb$codes[i]==502) {u1<-"http://stats.ncaa.org/teams/234754"} # No Colo 
if (yyyy==2007 & bb$codes[i]==649) {u1<-"http://stats.ncaa.org/teams/234881"} # SDSU 
if (yyyy==2007 & bb$codes[i]==30024) {u1<-"http://stats.ncaa.org/teams/235226"} # Utah Val 

if (yyyy==2006 & bb$codes[i]==108) {u1<-"http://stats.ncaa.org/teams/196559"} # UC Davis 
if (yyyy==2006 & bb$codes[i]==363) {u1<-"http://stats.ncaa.org/teams/196755"} # Longwood 
if (yyyy==2006 & bb$codes[i]==493) {u1<-"http://stats.ncaa.org/teams/196860"} # NDSU 
if (yyyy==2006 & bb$codes[i]==502) {u1<-"http://stats.ncaa.org/teams/196866"} # No Colo 
if (yyyy==2006 & bb$codes[i]==649) {u1<-"http://stats.ncaa.org/teams/196994"} # SDSU 
if (yyyy==2006 & bb$codes[i]==30024) {u1<-"http://stats.ncaa.org/teams/197335"} # Utah Val 

if (yyyy==2005 & bb$codes[i]==108) {u1<-"http://stats.ncaa.org/teams/348589"} # UC Davis 
if (yyyy==2005 & bb$codes[i]==363) {u1<-"http://stats.ncaa.org/teams/348784"} # Longwood 
if (yyyy==2005 & bb$codes[i]==493) {u1<-"http://stats.ncaa.org/teams/348889"} # NDSU 
if (yyyy==2005 & bb$codes[i]==502) {u1<-"http://stats.ncaa.org/teams/348895"} # No Colo 
if (yyyy==2005 & bb$codes[i]==649) {u1<-"http://stats.ncaa.org/teams/349022"} # SDSU 
if (yyyy==2005 & bb$codes[i]==30024) {u1<-"http://stats.ncaa.org/teams/349359"} # Utah Val 

if (yyyy==2003 & bb$codes[i]==28593) {u1<-"http://stats.ncaa.org/teams/319593"} # Birm So 
if (yyyy==2003 & bb$codes[i]==28600) {u1<-"http://stats.ncaa.org/teams/319595"} # Lipscomb 

if (yyyy==2002 & bb$codes[i]==28593) {u1<-"http://stats.ncaa.org/teams/165176"} # Birm So 
if (yyyy==2002 & bb$codes[i]==28600) {u1<-"http://stats.ncaa.org/teams/165178"} # Lipscomb 
if (yyyy==2002 & bb$codes[i]==26172) {u1<-"http://stats.ncaa.org/teams/165173"} # AM CC 

if (yyyy==2001 & bb$codes[i]==26172) {u1<-"http://stats.ncaa.org/teams/390373"} # AM CC 

#for (j in 1:10) {
#r1<- GET(u1)
#  if (r1$status_code!=200){print(paste(j,"Failure:",bb$names[i])) }
#  else {print(paste(j,"Success:",bb$names[i])); break;}
#}

h1<- htmlParse(u1)
print(paste(i,bb$names[i]))

# game level
games<- xpathApply(h1, "//table[1]/tr[1]/td[1]/table[1]/tr")
team   <- rep(paste(bb$names[i]), times = length(games))
date   <- vector(,length(games))
opp   <- vector(,length(games))
score <- vector(,length(games))

for (j in seq(3, length(games), 1)) { 
w<- xpathApply(h1, paste("//table[1]/tr[1]/td[1]/table[1]/tr[",j,"]/td[1]",sep=""))
x<- xpathApply(h1, paste("//table[1]/tr[1]/td[1]/table[1]/tr[",j,"]/td[2]",sep=""))
y<- xpathApply(h1, paste("//table[1]/tr[1]/td[1]/table[1]/tr[",j,"]/td[3]",sep=""))

date[j] <- gsub("^\\s+|\\s+$", "",gsub("\t","",gsub("\n","",xmlValue(w[[1]]))))
opp[j]  <- gsub("^\\s+|\\s+$", "",gsub("\t","",gsub("\n","",xmlValue(x[[1]]))))
score[j]<- gsub("^\\s+|\\s+$", "",gsub("\t","",gsub("\n","",xmlValue(y[[1]]))))
}

bytm <- cbind(team,date,opp,score)
if (i>1) {sched <- rbind(sched,bytm)} 
else if (i==1) {sched <- bytm}
}

write.csv(sched, file = paste("C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/collbase/bbsched",yyyy,".csv",sep=""), 
quote=TRUE, row.names = FALSE)
#######################################################################################################
}

#scrape(2019,14781);
scrape(2018,12973);
scrape(2017,12560);
scrape(2016,12360);
scrape(2015,12080);
scrape(2014,11620);
scrape(2013,11320);
scrape(2012,10942);
scrape(2011,10561);
scrape(2010,10240);
scrape(2009,13461);
scrape(2008,13133);
scrape(2007,13132);
scrape(2006,12972);
scrape(2005,13595);
scrape(2004,13295);
scrape(2003,13460);
scrape(2002,12841);
scrape(2001,13766);
scrape(2000,12840);
scrape(1999,13594);
scrape(1998,13131);
scrape(1997,12839);

#######################################################################################################

for (i in c(1997:2021)) {
x<-"C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/collbase/bbsched"
bb <- read.csv(paste(x,i,".csv",sep=""))
bb$year <- i

if (i==1997) {bigsched.0 <- bb}
else {bigsched.0 <- rbind(bigsched.0,bb)}
}

for (i in c(1997:2021)) {
super <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/collbase/superindex.xlsx",sheet = "superindex")
super <- as.data.frame(super) 
names(super)[names(super) == paste("codes",i,sep="")] <- "conf"
bb <- subset(super, conf != "XXXXX" & grepl("zzz",conf)==F)[c("codes","names","myname","conf")]
bb$year <- i

if (i==1997) {index <- bb}
else {index <- rbind(index,bb)}
}
  
index$myname2 <- index$myname
index$conf2 <- index$conf
index$codes2 <- index$codes

###

bigsched <- subset(bigsched.0, date!=FALSE | opp!=FALSE)
nrow(bigsched)
bigsched <- subset(bigsched, year<=2001 | (year == substr(date,7,10)))
nrow(bigsched)

bigsched$index <- as.numeric(rownames(bigsched))

bigsched$loc <- ifelse(substring(bigsched$opp,1,1)=="@","A",
			ifelse(grepl("@",substring(bigsched$opp,4)),"N","H"))

bigsched$team2 <- ifelse(substring(bigsched$opp,1,1)=="@",substring(bigsched$opp,2,200),paste(bigsched$opp))
bigsched$team2 <- gsub("^@\\s+", "", bigsched$team2)
bigsched$loc2<- ifelse(grepl("@",bigsched$opp),gsub("(.*)(@\\s?)(.*)", "\\3", bigsched$opp),"")
bigsched$team2 <- gsub("\\s?@.*$", "", bigsched$team2 )
bigsched$team2 <- gsub("\\s?2021.*$", "", bigsched$team2 )
bigsched$team2 <- gsub("\\s?DI.*$", "", bigsched$team2 )
bigsched$team2 <- gsub("\\s?#\\d+\\s", "", bigsched$team2 )
bigsched$team2 <- gsub("Horizon League Baseball Championship", "", bigsched$team2 )
bigsched$team2 <- gsub("CAA Baseball Championship", "", bigsched$team2 )
bigsched$team2 <- gsub("Atlantic 10 Baseball Championship", "", bigsched$team2 )
bigsched$team2 <- gsub("Sun Belt Baseball Championship", "", bigsched$team2 )
bigsched$team2 <- gsub("Mountain West Baseball Championship", "", bigsched$team2 )
bigsched$team2 <- gsub("Mountain West Championship", "", bigsched$team2 )
bigsched$team2 <- gsub("America East Baseball Championship", "", bigsched$team2 )
bigsched$team2 <- gsub("ASUN Baseball Championship", "", bigsched$team2 )
bigsched$team2 <- gsub("MVC Baseball Championship", "", bigsched$team2 )
bigsched$team2 <- gsub("Summit League Championship", "", bigsched$team2 )
bigsched$team2 <- gsub("Patriot League Championship", "", bigsched$team2 )
bigsched$team2 <- gsub("Ivy League Playoff Series", "", bigsched$team2 )

bigsched$team2 <- trimws(bigsched$team2)
bigsched$team2 <- ifelse(bigsched$team2=="Albany (NY)","UAlbany",paste(bigsched$team2))
bigsched$team2 <- ifelse(bigsched$team2=="Northern Ill.","NIU",paste(bigsched$team2))

bigsched$score <- ifelse(substring(bigsched$score,1,1)=="W"|substring(bigsched$score,1,1)=="T"|substring(bigsched$score,1,1)=="L",
				substring(bigsched$score,3,20),paste(bigsched$score))

bigsched$rf <- gsub("(\\d+)(\\s?-\\s?)(\\d+)(.*)", "\\1", bigsched$score)
bigsched$ra <- gsub("(\\d+)(\\s?-\\s?)(\\d+)(.*)", "\\3", bigsched$score)
bigsched$inn<- gsub("(\\d+)(\\s?-\\s?)(\\d+)(.*)", "\\4", bigsched$score)

bigsched$team2 <- ifelse(bigsched$team=="St. Bonaventure" & bigsched$team2=="St. Bonaventure","Fordham",paste(bigsched$team2))

nrow(bigsched)
bigsched <- merge(bigsched, index[c("year","names","myname","conf","codes")], by.x=c("year","team"),by.y=c("year","names"),all.x=T)
nrow(bigsched)
bigsched <- merge(bigsched, index[c("year","names","myname2","conf2","codes2")], by.x=c("year","team2"),by.y=c("year","names"),all.x=T)
nrow(bigsched)

table(subset(bigsched, is.na(myname2)==T)$team2)
nrow(subset(bigsched, is.na(myname2)==T))

###

bigsched$rf <- as.numeric(bigsched$rf)
bigsched$ra <- as.numeric(bigsched$ra)
bigsched <- subset(bigsched, is.na(rf)==F)
nrow(bigsched)

bigsched <- bigsched[order(bigsched$year, bigsched$team, paste(bigsched$date), bigsched$index),]
bigsched$ord <- 0

for (i in 1:nrow(bigsched)) {
if (i==1) {bigsched$ord[i] <- 1}
else if (bigsched$team[i]!=bigsched$team[i-1]) {bigsched$ord[i] <- 1}
else {bigsched$ord[i] <- bigsched$ord[i-1]+1}
}

bigsched$rf <- ifelse(bigsched$myname=="Miss Valley" & bigsched$myname2=="Jackson St." & bigsched$rf==12 & bigsched$ra==4,4,bigsched$rf)
bigsched$ra <- ifelse(bigsched$myname=="Miss Valley" & bigsched$myname2=="Jackson St." & bigsched$rf== 4 & bigsched$ra==4,12,bigsched$ra)

bigsched$rf <- ifelse(bigsched$myname=="Arkansas St." & bigsched$myname2=="UAB" & bigsched$rf==5 & bigsched$ra==0,0,bigsched$rf)
bigsched$ra <- ifelse(bigsched$myname=="Arkansas St." & bigsched$myname2=="UAB" & bigsched$rf==0 & bigsched$ra==0,5,bigsched$ra)

bigsched$rf <- ifelse(bigsched$myname=="Arkansas St." & bigsched$myname2=="UAB" & bigsched$rf==5 & bigsched$ra==0,0,bigsched$rf)
bigsched$ra <- ifelse(bigsched$myname=="Arkansas St." & bigsched$myname2=="UAB" & bigsched$rf==0 & bigsched$ra==0,5,bigsched$ra)

bigsched$rf <- ifelse(bigsched$year==2001 & bigsched$myname=="Manhattan" & bigsched$myname2=="Marist" & bigsched$rf==0 & bigsched$ra==9,998,bigsched$rf)
bigsched$ra <- ifelse(bigsched$year==2001 & bigsched$myname=="Manhattan" & bigsched$myname2=="Marist" & bigsched$rf==998 & bigsched$ra==9,998,bigsched$ra)

bigsched$rf <- ifelse(bigsched$year==2001 & bigsched$myname=="Marist" & bigsched$myname2=="Manhattan" & bigsched$rf==0 & bigsched$ra==9,998,bigsched$rf)
bigsched$ra <- ifelse(bigsched$year==2001 & bigsched$myname=="Marist" & bigsched$myname2=="Manhattan" & bigsched$rf==998 & bigsched$ra==9,998,bigsched$ra)

bigsched$rf <- ifelse(bigsched$year==2003 & bigsched$myname=="Indiana St." & bigsched$myname2=="Illinois St." & bigsched$rf==0 & bigsched$ra==9,998,bigsched$rf)
bigsched$ra <- ifelse(bigsched$year==2003 & bigsched$myname=="Indiana St." & bigsched$myname2=="Illinois St." & bigsched$rf==998 & bigsched$ra==9,998,bigsched$ra)

bigsched$rf <- ifelse(bigsched$year==2003 & bigsched$myname=="Illinois St." & bigsched$myname2=="Indiana St." & bigsched$rf==0 & bigsched$ra==9,998,bigsched$rf)
bigsched$ra <- ifelse(bigsched$year==2003 & bigsched$myname=="Illinois St." & bigsched$myname2=="Indiana St." & bigsched$rf==998 & bigsched$ra==9,998,bigsched$ra)

bigsched$rf <- ifelse(bigsched$date=="05/17/2013" & bigsched$myname=="St. Bonaventure",9,bigsched$rf)
bigsched$ra <- ifelse(bigsched$date=="05/17/2013" & bigsched$myname=="St. Bonaventure",1,bigsched$ra)

xx <- subset(bigsched, (rf > ra & codes2 != "") | (rf == ra & codes < codes2 & codes2 != ""))
yy <- subset(bigsched, (rf < ra & codes2 != "") | (rf == ra & codes > codes2 & codes2 != ""))
zz <- subset(bigsched, is.na(codes2))
nrow(xx); nrow(yy); nrow(zz) 


xx <- xx[c("year","date","myname","myname2","rf","ra","inn","loc","loc2","team","opp","score","index","conf","ord")]

yy <- yy[c("year","date","myname","myname2","rf","ra","inn","loc","loc2","team","opp","score","index","conf","ord")]
names(yy) <- c("year","date","myname2","myname","ra","rf","inn","loc","loc2","team","opp","score","index","conf2","ord")
yy <- yy[c("year","date","myname","myname2","rf","ra","inn","loc","loc2","team","opp","score","index","conf2","ord")]

zz <- zz[c("year","date","myname","myname2","rf","ra","inn","loc","loc2","team","opp","score","index","team2","conf","ord")]

xx <- xx[order(xx$year, xx$date, xx$myname, xx$myname2, xx$rf, xx$ra, xx$inn),]
xx$ord2 <- 1
for (i in 1:nrow(xx)) {
if (i==1) {}
else if (xx$myname[i]==xx$myname[i-1] & xx$myname2[i]==xx$myname2[i-1] & xx$rf[i]==xx$rf[i-1]
 & xx$ra[i]==xx$ra[i-1] & xx$inn[i]==xx$inn[i-1]) {xx$ord2[i] <- xx$ord2[i-1]+1}
}

yy <- yy[order(yy$year, yy$date, yy$myname, yy$myname2, yy$rf, yy$ra, yy$inn),]
yy$ord2 <- 1
for (i in 1:nrow(yy)) {
if (i==1) {}
else if (yy$myname[i]==yy$myname[i-1] & yy$myname2[i]==yy$myname2[i-1] & yy$rf[i]==yy$rf[i-1]
 & yy$ra[i]==yy$ra[i-1] & yy$inn[i]==yy$inn[i-1]) {yy$ord2[i] <- yy$ord2[i-1]+1}
}

yy <- subset(yy, !(myname=="Alcorn" & myname2=="Miss Valley" & rf==7 & ra==6 & ord2==2))

comb <- merge(xx,yy,by=c("year","date","myname","myname2","rf","ra","inn","ord2"),all.x=T,all.y=T)
nrow(xx)
nrow(yy)
nrow(comb)
subset(comb, is.na(loc.y)==T)
subset(comb, is.na(loc.x)==T)

comb <- rbind(comb,zz)

comb$home <- ifelse(comb$loc.x=="H" & comb$loc.y=="A",1,
			ifelse(comb$loc.x=="A" & comb$loc.y=="H",2,
			ifelse(comb$loc.x=="N" & comb$loc.y=="N",3,
			ifelse(comb$year<=2011,0,99))))
table(comb$home)

comb$location <- ifelse(comb$home==3,paste(comb$loc2.x),"-")

zz$myname2 <- zz$team2
zz$home <- ifelse(zz$loc=="H",1,
			ifelse(zz$loc=="A",2,
			ifelse(zz$loc=="N",3,
			ifelse(zz$year<=2011,0,99))))
zz$location <- ifelse(zz$home==3,paste(zz$loc2),"-")
zz$conf2 <- ""
zz$ord2 <- 0


####################

final   <- comb[c("year","date","myname","ord.x","conf","rf","myname2","ord.y","conf2","ra","inn","home","location")]
names(final) <- c("year_","date_","name1_" ,"ord1_","conf1_","runs1_","name2_","ord2_","conf2_","runs2_","inn_","home_","loc_")

zzz <- zz[c("year","date","myname","ord","conf","rf","myname2","ord2","conf2","ra","inn","home","location")]
names(zzz) <- c("year_","date_","name1_" ,"ord1_","conf1_","runs1_","name2_","ord2_","conf2_","runs2_","inn_","home_","loc_")

final <- rbind(final,zzz)

final$year <- final$year_
final$date <- final$date_
final$ord1 <- final$ord1_
final$ord2 <- final$ord2_
final$conf <- ifelse(final$conf1_ == final$conf2_,"C","")
final$team1 <- final$name1_
final$runs1 <- final$runs1_
final$team2 <- final$name2_
final$runs2 <- final$runs2_
final$inn <- final$inn_
final$home <- final$home_
final$loc <- final$loc_

final <- final[order(final$year, final$date, final$team1, final$ord1),]

write.csv(final, file = "C:/Users/andyh/OneDrive/Documents/SPORTSDATA/Raw/collbase/final.csv", quote=TRUE, row.names = FALSE) 


