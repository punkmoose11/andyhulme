
############# import data
seasons <- read.csv("../Data/cfseasons.csv")
cfgames <- read.csv("../Data/cfgames.csv")
bowls   <- read.csv("../Data/cfbowls.csv")
polls   <- read.csv("../Data/cfpolls.csv")
weekly  <- read.csv("../Data/cfweekly.csv")
champs  <- read.csv("../Data/cfchamps.csv")
elo     <- read.csv("../Data/cfelortg.csv")
lowerdiv<- read.csv("../Data/cfplayoffs.csv")

teamlink <- function(x){teamx = tolower(gsub("&","_",gsub(".","",gsub("'","",gsub(" ", "",x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE))}
record   <- function(W,L,T){record = ifelse(W+L+T>0,paste(W,"-",L,ifelse(T>0,paste("-",T,sep=""),""),sep=""),"")}

###########################################

seasons$group<- paste(seasons$conf,seasons$div,sep="/")
seasons$key1 <- paste(seasons$year,seasons$conf,seasons$div,sep="/")

seasons$GP   <- seasons$W+seasons$L+ifelse(is.na(seasons$T),0,seasons$T)
seasons$PCT  <- (seasons$W+ifelse(is.na(seasons$T),0,seasons$T)/2) / seasons$GP
seasons$RANK <- ave(-seasons$PCT-seasons$W/10000, seasons$year, FUN = function(x) rank(x, ties.method = "random") )

seasons$GPC  <- seasons$WC+seasons$LC+ifelse(is.na(seasons$TC),0,seasons$TC)
seasons$PCTC <- ifelse(seasons$GPC==0,0,(seasons$WC+ifelse(is.na(seasons$TC),0,seasons$TC)/2) / seasons$GPC)
seasons$RANKC<- ave(-seasons$PCTC-seasons$WC/10000, seasons$key1, FUN = function(x) rank(x, ties.method = "min") )

seasons$RANKCC<- ifelse(seasons$RANKC==1,"1st",
			ifelse(seasons$RANKC==2,"2nd",
			ifelse(seasons$RANKC==3,"3rd",paste(seasons$RANKC,"th",sep=""))))

seasons$SORT<- 1
seasons$SORT<- ifelse(seasons$conf=="Indep",2,seasons$SORT)
seasons$SORT<- ifelse(seasons$conf %in% c("A10","AQ7","Am West","ASUN","Big Sky","Big South","BSCOVC","CAA","FCSIndep",
					"GWC","MAAC","MEAC","MVFC","NEC","OVC","Patriot","Pioneer","SWAC","UAC","Yankee"),3,seasons$SORT)
seasons$SORT<- ifelse(seasons$year>=1995 & seasons$conf %in% c("Ivy","Southern","Southland"),3,seasons$SORT)
seasons$SORT<- ifelse(seasons$year>=2019 & seasons$conf %in% c("WAC"),3,seasons$SORT)
seasons$SORT<- ifelse(seasons$conf=="FCSIndep",4,seasons$SORT)
seasons$SORT<- ifelse(seasons$conf=="Minor",5,seasons$SORT)
seasons$SORT<- ifelse(seasons$conf=="Partial",99,seasons$SORT)

ap <- subset(polls,poll=="AP"); ap$AP <- ap$rank;
cp <- subset(polls,poll=="CP"); cp$CP <- cp$rank;

ap.x <- merge(seasons,ap[c("year","team","AP")],by=c("year","team"),all.y=T)
ap.x <- subset(ap.x, is.na(conf) & year<2020)[c("year","team","AP")]
ap.x

cp.x <- merge(seasons,cp[c("year","team","CP")],by=c("year","team"),all.y=T)
cp.x <- subset(cp.x, is.na(conf) & year<2020)[c("year","team","CP")]
cp.x

#bowls.x <- merge(seasons,bgm[c("year","team","bowl")],by=c("year","team"),all.y=TRUE)
#bowls.x <- subset(bowls.x, is.na(conf) & year<2020)[c("year","team","bowl")]
#bowls.x

elo <- subset(elo,rating!=-9999)

seasons <- merge(seasons,ap[c("year","team","AP")],by=c("year","team"),all.x=T)
seasons <- merge(seasons,cp[c("year","team","CP")],by=c("year","team"),all.x=T)
seasons <- merge(seasons,elo[c("year","team","elo","rating")],by=c("year","team"),all.x=T)

cfgames <- merge(cfgames, seasons[c("year","team","W","L","T","AP","CP")], by.x=c("season","team1"), by.y=c("year","team"), all.y=T)
cfgames$monday <- paste(substring(cfgames$date,6,7),substring(cfgames$date,9,10),sep=".")
cfgames$result <- ifelse(cfgames$points1>cfgames$points2,"W",ifelse(cfgames$points1<cfgames$points2,"L","T"))
cfgames$home   <- ifelse(cfgames$loc==1,"H",ifelse(cfgames$loc==2,"A",ifelse(cfgames$loc==3,"N","")))

#### find first monday in September ####
aug1 <- as.data.frame(1869:2050)
names(aug1) <- c("year")
aug1$aug  <- as.Date(paste(aug1$year,'09','01',sep="/"),  format='%Y/%m/%d')
aug1$dow  <- as.numeric(format(aug1$aug-1, '%w'))
aug1$sun  <- aug1$aug + (7-ifelse(aug1$dow==0,7,aug1$dow))

cfgames$daten <- as.Date(cfgames$date, format='%Yx%mx%d')
cfgames$dow  <- as.numeric(format(cfgames$daten, '%w'))
cfgames <- merge(cfgames, aug1[c("year","sun")], by.x="season", by.y="year", all.x=T)
cfgames$week <- ceiling(as.numeric(cfgames$daten - (cfgames$sun+1))/7)+1

### merge in AP polls ###
weekly <- weekly[order(weekly$season, weekly$date),]
weekly$daten1  <- as.Date(weekly$date,  format='%Yx%mx%d')
polls <- unique(weekly[c("season","daten1")])
polls$daten2 <- polls$daten1
for (i in 1:nrow(polls)) {polls$daten2[i] <- polls$daten1[min(i+1,nrow(polls))]}
weekly <- merge(weekly, polls, by=c("season","daten1"))

z <- merge(cfgames,weekly[c("season","rank","school","daten1","daten2")],by.x=c("season","team1"),by.y=c("season","school"),all.x=T)
z$aprank1 <- z$rank
z1 <- subset(z, daten1 <= daten & daten < daten2)[c("season","team1","team2","date","points1","points2","aprank1")]
cfgames <- merge(cfgames, z1, by=c("season","team1","team2","date","points1","points2"),all.x=T)

z2 <- merge(cfgames,weekly[c("season","rank","school","daten1","daten2")],by.x=c("season","team2"),by.y=c("season","school"),all.x=T)
z2$aprank2 <- z2$rank
z3 <- subset(z2, daten1 <= daten & daten < daten2)[c("season","team1","team2","date","points1","points2","aprank2")]
cfgames <- merge(cfgames, z3, by=c("season","team1","team2","date","points1","points2"),all.x=T)
#### end ap polls ####

oppo <- as.data.frame.matrix(table(paste(cfgames$team1,cfgames$team2,sep="!"), cfgames$result))
oppo$key <- rownames(oppo)
keys <- data.frame(do.call('rbind', strsplit(as.character(oppo$key), '!', fixed=TRUE)))
oppo <- cbind(oppo,keys)
oppo$P <- oppo$W + oppo$L + oppo$T
oppo <- oppo[c("X1","X2","P","W","L","T")]
rownames(oppo) <- 1:nrow(oppo)

oppo <- oppo[order(oppo$X1, -oppo$P, -oppo$W, oppo$L, oppo$X2 ),]

#############################################

bowls$champ <- ifelse(grepl("CFP Champ",bowls$bowl)|grepl("BCS Champ",bowls$bowl)
				| ((bowls$year==1998|bowls$year==2002) & bowls$bowlid=="Fiesta")
				| ((bowls$year==1999|bowls$year==2003) & bowls$bowlid=="Sugar")
				| ((bowls$year==2000|bowls$year==2004) & bowls$bowlid=="Orange")
				| ((bowls$year==2001|bowls$year==2005) & bowls$bowlid=="Rose"),"Y","N")

bowls$group <- ifelse(grepl("CFP Champ",bowls$bowl) | grepl("Semifinal",bowls$ot) | grepl("QF",bowls$ot) | grepl("CFP First Round",bowls$bowl),"College Football Playoff",
			ifelse(bowls$year>=2014 & (bowls$bowlid=="Orange"|bowls$bowlid=="Rose"|bowls$bowlid=="Sugar"|bowls$bowlid=="Fiesta"|bowls$bowlid=="Cotton"|bowls$bowlid=="Peach"),"New Years Six",
			ifelse(bowls$champ=="Y","BCS Championship",
			ifelse(1998<=bowls$year & bowls$year<=2013 & (bowls$bowlid=="Orange"|bowls$bowlid=="Rose"|bowls$bowlid=="Sugar"|bowls$bowlid=="Fiesta"),"BCS Bowl Games",
			ifelse(1995<=bowls$year & bowls$year<=1997 & (bowls$bowlid=="Orange"|bowls$bowlid=="Rose"|bowls$bowlid=="Sugar"|bowls$bowlid=="Fiesta"),"Major Bowl Games",
			ifelse(1981<=bowls$year & bowls$year<=1994 & (bowls$bowlid=="Orange"|bowls$bowlid=="Rose"|bowls$bowlid=="Sugar"|bowls$bowlid=="Fiesta"|bowls$bowlid=="Cotton"),"Major Bowl Games",
			ifelse(bowls$year<=1980 & (bowls$bowlid=="Orange"|bowls$bowlid=="Rose"|bowls$bowlid=="Sugar"|bowls$bowlid=="Cotton"),"Major Bowl Games",
			ifelse(grepl(" Champ",bowls$bowl) & grepl("Champ Sports",bowls$bowl)==F,"Conference Championship Games",
			"Bowl Games"))))))))

bowls$name1 <- ifelse(bowls$group=="Conference Championship Games",gsub("(\\w+)(\\s)(\\w+)","\\1", bowls$bowl),
			ifelse(grepl(" Champ",bowls$bowl),paste(bowls$bowl,"ionship",sep=""),
			ifelse(bowls$bowl=="CFP First Round","First Round",paste(bowls$bowl,"Bowl"))))

bowls$name1 <- gsub("B12","Big 12",bowls$name1)
bowls$name1 <- gsub("B10","Big 10",bowls$name1)
bowls$name1 <- gsub("P10","Pac 10",bowls$name1)
bowls$name1 <- gsub("P12","Pac 12",bowls$name1)
bowls$name1 <- gsub("AAC","American",bowls$name1)

bowls$sort1 <- ifelse(bowls$group=="Conference Championship Games",1,
			ifelse(bowls$group=="Bowl Games",2,
			ifelse(bowls$group=="Major Bowl Games",3,
			ifelse(bowls$group=="BCS Bowl Games",4,
			ifelse(bowls$group=="New Years Six",5,
			ifelse(bowls$name1=="First Round",6.1,
			ifelse(grepl("QF",bowls$ot),6.2,
			ifelse(grepl("Semi",bowls$ot),6.3,
			ifelse(bowls$name1=="CFP Championship",8,
			ifelse(grepl(" Championship",bowls$group),6,7))))))))))
			

bowls.1 <- bowls
bowls.1$team <- bowls.1$team1
bowls.1$result <- paste(ifelse(bowls.1$score1==bowls.1$score2,"tied","won"),bowls.1$bowl)
bowls.1 <- bowls.1[c("year","team","result","sort1")]

bowls.2 <- bowls
bowls.2$team <- bowls.2$team2
bowls.2$result <- paste(ifelse(bowls.2$score1==bowls.2$score2,"tied","lost"),bowls.2$bowl)
bowls.2 <- bowls.2[c("year","team","result","sort1")]

games <- rbind(bowls.1,bowls.2) 

bgm <- subset(games, sort1!=1 & sort1!=8 & sort1!=6.1 & sort1!=6.2 & sort1!=6.3) 
ccg <- subset(games, sort1==1)
cf1 <- subset(games, sort1==6.1)
cf2 <- subset(games, sort1==6.2)
cf3 <- subset(games, sort1==6.3)
cfp <- subset(games, sort1==8)

bgm$bowl <- bgm$result
ccg$ccg  <- ifelse(grepl("won",ccg$result),"W","L")
cf1$cf1  <- cf1$result
cf2$cf2  <- cf2$result
cf3$cf3  <- cf3$result
cfp$cfp  <- paste("; ",cfp$result,sep="")

playoffs <- subset(games, sort1 %in% c(6.1,6.2,6.3,8))
playoffs$winloss  <- ifelse(grepl("won",playoffs$result),"W","L")
cfpwl <- as.data.frame.matrix(table(paste(playoffs$year,playoffs$team,sep="!"), playoffs$winloss))
cfpwl$key <- rownames(cfpwl)
keys <- data.frame(do.call('rbind', strsplit(as.character(cfpwl$key), '!', fixed=TRUE)))
cfpwl<- cbind(cfpwl,keys)
cfpwl$cfptext <- paste("<b>Playoff:</b> ",cfpwl$W,"-",cfpwl$L,sep="")

####

lower <- subset(lowerdiv, division!="")
lower.1 <- lower
lower.1$team <- lower.1$team1
lower.1$result <- "W"
lower.1 <- lower.1[c("season","team","result","division")]

lower.2 <- lower
lower.2$team <- lower.2$team2
lower.2$result <- "L"
lower.2 <- lower.2[c("season","team","result","division")]
lowers <- rbind(lower.1,lower.2)

lowwl <- as.data.frame.matrix(table(paste(lowers$season,lowers$team,lowers$division,sep="!"), lowers$result))
lowwl$key <- rownames(lowwl)
keys <- data.frame(do.call('rbind', strsplit(as.character(lowwl$key), '!', fixed=TRUE)))
lowwl<- cbind(lowwl,keys)
lowwl$lowtext <- paste("<b>",lowwl$X3,":</b> ",lowwl$W,"-",lowwl$L,sep="")

#####

bowls.x <- merge(seasons,bgm[c("year","team","bowl")],by=c("year","team"),all.y=TRUE)
bowls.x <- subset(bowls.x, is.na(conf) & year<2020)[c("year","team","bowl")]
bowls.x

seasons <- merge(seasons,bgm[c("year","team","bowl")],by=c("year","team"),all.x=TRUE)
seasons <- merge(seasons,ccg[c("year","team","ccg")] ,by=c("year","team"),all.x=TRUE)
seasons <- merge(seasons,cf1[c("year","team","cf1")] ,by=c("year","team"),all.x=TRUE)
seasons <- merge(seasons,cf2[c("year","team","cf2")] ,by=c("year","team"),all.x=TRUE)
seasons <- merge(seasons,cf3[c("year","team","cf3")] ,by=c("year","team"),all.x=TRUE)
seasons <- merge(seasons,cfp[c("year","team","cfp")] ,by=c("year","team"),all.x=TRUE)
seasons <- merge(seasons,cfpwl[c("X1","X2","cfptext")] ,by.x=c("year","team"),by.y=c("X1","X2"),all.x=TRUE)
seasons <- merge(seasons,lowwl[c("X1","X2","lowtext")] ,by.x=c("year","team"),by.y=c("X1","X2"),all.x=TRUE)

seasons$bowltext <- trimws(ifelse(is.na(seasons$cfptext)==F,paste(seasons$cfptext),
					ifelse(is.na(seasons$lowtext)==F,paste(seasons$lowtext),
        				paste(ifelse(is.na(seasons$bowl)==F,seasons$bowl,""),
					ifelse(is.na(seasons$cf1)==F,seasons$cf1,""),
					ifelse(is.na(seasons$cf2)==F,seasons$cf2,""),
					ifelse(is.na(seasons$cf3)==F,seasons$cf3,""),
					ifelse(is.na(seasons$cfp)==F,seasons$cfp,""),sep=" "))))

champs <- merge(champs,seasons[c("year","team","WC","LC","TC")],by.x=c("season","winner"),by.y=c("year","team"),all.x=T)
champs$record <- ifelse(is.na(champs$WC),"",paste("(",champs$WC,"-",champs$LC,ifelse(champs$TC>0,paste("-",champs$TC,sep=""),""),")",sep=""))
champs.1 <- subset(champs, splits==1)
champs.2 <- subset(champs, splits==2); names(champs.2)[names(champs.2) == "winner"] <- "winner2"; names(champs.2)[names(champs.2) == "record"] <- "record2";
champs.3 <- subset(champs, splits==3); names(champs.3)[names(champs.3) == "winner"] <- "winner3"; names(champs.3)[names(champs.3) == "record"] <- "record3";
champs.4 <- subset(champs, splits==4); names(champs.4)[names(champs.4) == "winner"] <- "winner4"; names(champs.4)[names(champs.4) == "record"] <- "record4";
champs.5 <- subset(champs, splits==5); names(champs.5)[names(champs.5) == "winner"] <- "winner5"; names(champs.5)[names(champs.5) == "record"] <- "record5";

champs.x <- merge(champs.1,champs.2[c("season","conference","winner2","record2")],by=c("season","conference"),all.x=T)
champs.x <- merge(champs.x,champs.3[c("season","conference","winner3","record3")],by=c("season","conference"),all.x=T)
champs.x <- merge(champs.x,champs.4[c("season","conference","winner4","record4")],by=c("season","conference"),all.x=T)
champs.x <- merge(champs.x,champs.5[c("season","conference","winner5","record5")],by=c("season","conference"),all.x=T)


##########################################
############# generate header / footer ###
##########################################

header <- vector()
header[1] <- '<html><head><title>College Football</title>'
header[2] <- '<link rel="stylesheet" type="text/css" href="styles.css"></head>' 
header[3] <- '<body><div id="red"><h1>College Football</h1></div>'
header[4] <- '<div id="head"><table class="head"><tr class="head">
<td class="head" style="text-align:left; width:75%;"><!--Insert Link--> &nbsp;</td>
<td class="head" style="text-align:right; width:25%;">
<a href="byyear.html">Years</a> - <a href="byteam.html">Teams</a> - <a href="index.html">Home</a>
</td></tr></table></div>'

footer <- vector()
footer[1] <- '<div id="foot"> <a class="foot" href="../index.html">AndyHulme.Net</a></div></body></html>'

##########################################
############# yearly pages ###############
##########################################

u.yr <- unique(seasons$year)

for (i in 1:length(u.yr))
{ 
print(paste(i,u.yr[i]))

# create summary for given year
YR <- u.yr[i]; YR0 <- YR-1; YR2 <- YR+1

SEAS <- paste(YR );
SEAS0<- paste(YR0);
SEAS2<- paste(YR2);

headerx <- gsub("ABC123","CFB",header)
summ.yr <- subset(seasons, year == u.yr[i])

#summ.yr$conf <- ifelse(summ.yr$conf=="MVC"|summ.yr$conf=="Southern"|summ.yr$conf=="Southland","Indep",paste(summ.yr$conf))

summ.yr <- summ.yr[order(summ.yr$SORT, summ.yr$conf, summ.yr$div, summ.yr$RANKC, summ.yr$RANK),]
summ.yr$class1 <- 0

summ.yr$row <- paste("<tr class='d",summ.yr$class1,"'><td>",paste(
				ifelse(is.na(summ.yr$AP )==FALSE,paste("#",summ.yr$AP,sep=""),""),
				ifelse(is.na(summ.yr$CP )==FALSE,paste("#",summ.yr$CP,sep=""),""),
				paste("<div style='text-align:left'><a href='",teamlink(summ.yr$team),"_g.html#",summ.yr$year,"'>",summ.yr$team,"</a></div>",sep=""),
				ifelse(summ.yr$conf!="Indep" & summ.yr$conf!="Minor",summ.yr$WC,""),
				ifelse(summ.yr$conf!="Indep" & summ.yr$conf!="Minor",summ.yr$LC,""),
				ifelse(summ.yr$conf!="Indep" & summ.yr$conf!="Minor" & summ.yr$TC>0,summ.yr$TC,""),
				ifelse(is.na(summ.yr$ccg )==FALSE,summ.yr$ccg,""),
				summ.yr$W,
				summ.yr$L,
				ifelse(summ.yr$T >0,summ.yr$T,""),
				summ.yr$bowltext,
			sep="</td><td>"),"</td></tr>",sep="")

## TOP 25 ##
top25 <- subset(summ.yr, year == u.yr[i] & (AP<=25 | CP<= 25 | elo<=30))
top25 <- top25 [order(top25$AP, top25$CP, top25$elo),]

top25$class1 <- 0

top25$row <- paste("<tr class='d",top25$class1,"'><td>",paste(
				ifelse(is.na(top25$AP )==FALSE,paste("#",top25$AP,sep=""),""),
				ifelse(is.na(top25$CP )==FALSE,paste("#",top25$CP,sep=""),""),
				ifelse(is.na(top25$elo)==FALSE,paste("",top25$elo,sep=""),""),
				paste("<div style='text-align:left'><a href='",teamlink(top25$team),"_g.html#",top25$year,"'>",top25$team,"</a></div>",sep=""),
				top25$W,
				top25$L,
				ifelse(top25$T >0,top25$T,""),
				top25$bowltext,
			sep="</td><td>"),"</td></tr>",sep="")

champs.x$class1 <- 0

champs.x$row <- paste("<tr class='d",top25$class1,"'><td>",paste(
				paste("<a href='",u.yr[i],"_c.html#",teamlink(champs.x$conference),"'>",champs.x$conference,"</a>",sep=""),
				ifelse(is.na(champs.x$wscore)==F,paste("<a href='",teamlink(champs.x$winner),".html'>",champs.x$winner,"</a>"," ",champs.x$wscore,"-",champs.x$lscore," <a href='",teamlink(champs.x$loser),".html'>",champs.x$loser,"</a>"," ",sep=""),
					paste("<a href='",teamlink(champs.x$winner),".html'>",champs.x$winner,"</a>"," ",champs.x$record,
						ifelse(is.na(champs.x$winner2),"",paste(", ","<a href='",teamlink(champs.x$winner2),".html'>",champs.x$winner2,"</a>"," ",champs.x$record2,sep="")),
						ifelse(is.na(champs.x$winner3),"",paste(", ","<a href='",teamlink(champs.x$winner3),".html'>",champs.x$winner3,"</a>"," ",champs.x$record3,sep="")),
						ifelse(is.na(champs.x$winner4),"",paste(", ","<a href='",teamlink(champs.x$winner4),".html'>",champs.x$winner4,"</a>"," ",champs.x$record4,sep="")),
						ifelse(is.na(champs.x$winner5),"",paste(", ","<a href='",teamlink(champs.x$winner5),".html'>",champs.x$winner5,"</a>"," ",champs.x$record5,sep="")),sep="")),
			sep="</td><td>"),"</td></tr>",sep="")

## ELO ##
eloranks <- subset(summ.yr, year == u.yr[i] & is.na(elo)==F)
eloranks <- eloranks[order(eloranks$elo),]

eloranks$class1 <- (0+(1:nrow(eloranks))) %% 2

eloranks$row <- paste("<tr class='d",eloranks$class1,"'><td>",paste(
				ifelse(is.na(eloranks$elo)==FALSE,paste("",eloranks$elo,sep=""),""),
				paste("<div style='text-align:left'><a href='",teamlink(eloranks$team),"_g.html#",eloranks$year,"'>",eloranks$team,"</a></div>",sep=""),
				eloranks$rating,
				eloranks$W,
				eloranks$L,
				ifelse(eloranks$T >0,eloranks$T,""),
				ifelse(is.na(eloranks$AP )==FALSE,paste("#",eloranks$AP,sep=""),""),
				ifelse(is.na(eloranks$CP )==FALSE,paste("#",eloranks$CP,sep=""),""),
			sep="</td><td>"),"</td></tr>",sep="")

## Bowls and Playoffs ##
play.yr <- subset(bowls, year == u.yr[i])
play.yr <- play.yr[order(play.yr$sort, play.yr$group, play.yr$name1 ),]

if (nrow(play.yr)>0) {
for (p in 1:nrow(play.yr)) {
if (p==1) { play.yr$flag[p] <- 2}
if (p >1) { play.yr$flag[p] <- ifelse(play.yr$group[p] != play.yr$group[p-1]|play.yr$group[p] != play.yr$group[p-1]
							,ifelse(play.yr$group[p] != play.yr$group[p-1],2,1),0)}}

play.yr$row <- paste(ifelse(play.yr$flag==1,"<tr><th colspan='5'></th></tr>",""),"<tr><td>",paste(
				ifelse(play.yr$flag>=-1,paste("",play.yr$name,"",sep=""),""),
				ifelse(is.na(play.yr$team1)==FALSE,paste("<a href='",teamlink(play.yr$team1),".html'>",play.yr$team1,"</a>",sep=""),paste(play.yr$team1)),
				ifelse(play.yr$score2!="",paste(play.yr$score1,play.yr$score2,sep=" - "),paste(play.yr$score1)),
				ifelse(is.na(play.yr$team2)==FALSE,paste("<a href='",teamlink(play.yr$team2),".html'>",play.yr$team2,"</a>",sep=""),paste(play.yr$team2)),
				play.yr$ot,
			sep="</td><td>"),"</td></tr>") }

headerx <- gsub("<!--Insert Link-->",paste(
u.yr[i],' >> <a href="',u.yr[i],'.html">Summary</a>',
' - <a href="',u.yr[i],'_c.html">Standings</a>',
' - <a href="',u.yr[i],'_post.html">Postseason</a>',
' - <a href="',u.yr[i],'_elo.html">Elo Rating</a>',
ifelse(u.yr[i]>=1995,paste(' - <a href="',u.yr[i],'_fcs.html">FCS</a>',sep=""),""),
sep=""),headerx)

### Top 25
sink(paste("../CFB/",u.yr[i],".html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",SEAS,"</center></h2>"))
cat(paste("<center><a href='",YR0,".html'> << ",SEAS0,"</a> | ",
	            "<a href='",YR2,".html'>",SEAS2," >> </a></center><br>",sep=""))

cat("<table width='100%' align='center' style='border: 0;'>")
#cat("<tr style='border: 0;'><td rowspan='2' width='50%' style='border: 0;'>")
#cat("<center><h3>Top Teams</h3></center>")
cat("<br><table width='90%' align='center'>")

if (u.yr[i]<1996) { 
cat("<col width='8%'><col width='8%'><col width='8%'><col width='25%'><col width='7%'><col width='7%'><col width='7%'><col width='30%'>")
ththth <- "</th><th>W</th><th>L</th><th>T</th><th></th>"
}

if (u.yr[i]>=1996) { 
cat("<col width='8%'><col width='8%'><col width='8%'><col width='25%'><col width='7%'><col width='7%'><col width='7%'><col width='30%'>")
ththth <- "</th><th>W</th><th>L</th><th></th><th></th>"
}

cat("<tr><th colspan='8' class='hl'>Top Teams</th></tr>")
cat(paste("<tr>","<th>AP</th><th>CP</th><th><a href='",u.yr[i],"_elo.html'>Elo</a></th><th>",ththth,"</tr>",sep=""))
write.table(top25[1:min(nrow(top25),30),"row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
cat("</td>")

majors <- subset(play.yr, sort1>2) 

## conference standings
if (0 == 0) {
#cat("<td width='50%' style='border: 0;'>")
#cat("<center><h3>Conference Winners</h3></center>")
cat("<br><table width='90%' align='center'>")
cat("<tr><th colspan='2' class='hl'>Conference Winners</th></tr>")
cat(paste("<tr>","<th>Conference</th><th>Winner(s)/Championship Game</th>","</tr>"))
write.table(subset(champs.x,season==u.yr[i])["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr class='d2'>","<td colspan='2'>","<a href='",u.yr[i],"_c.html'>All Conference Standings</a></td></tr>",sep=""))
cat("</table><br>")
#cat("</td></tr>")
}

## postseaason games
#cat("<tr style='border: 0;'><td width='50%' style='border: 0;'>")
if (nrow(majors)>0) {
#cat("<center><h3>Major Bowls</h3></center>")
cat("<br><table width='90%' align='center'>")
cat("<tr><th colspan='5' class='hl'>Major Bowls</th></tr>")
cat(paste("<tr>","<th>Bowl</th><th> </th><th>Score</th><th> </th><th> </th>","</tr>"))
write.table(majors["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr class='d2'>","<td colspan='5'>","<a href='",u.yr[i],"_post.html'> All Bowl Games</a></td></tr>",sep=""))
cat("</table><br>")
}
#cat("</tr></td></table>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

### Elo Ranks
sink(paste("../CFB/",u.yr[i],"_elo.html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",SEAS,"</center></h2>"))
cat(paste("<center><a href='",YR0,"_elo.html'> << ",SEAS0,"</a> | ",
	            "<a href='",YR2,"_elo.html'>",SEAS2," >> </a></center><br>",sep=""))

cat("<center><h3>Elo Ranking</h3></center><br><table width='80%' align='center'>")

if (u.yr[i]<1996) { 
cat("<col width='10%'><col width='24%'><col width='10%'><col width='8%'><col width='8%'><col width='8%'><col width='10%'><col width='10%'>")
ththth <- "</th><th>W</th><th>L</th><th>T</th>"
}

if (u.yr[i]>=1996) { 
cat("<col width='10%'><col width='24%'><col width='10%'><col width='8%'><col width='8%'><col width='8%'><col width='10%'><col width='10%'>")
ththth <- "</th><th>W</th><th>L</th><th></th>"
}

cat(paste("<tr>","<th>Elo</th><th><th>Rating</th>",ththth,"<th>AP</th><th>CP</th></tr>"))
write.table(eloranks["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

### Major Conference Standings
sink(paste("../CFB/",u.yr[i],"_c.html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",SEAS,"</center></h2>"))
cat(paste("<center><a href='",YR0,"_c.html'> << ",SEAS0,"</a> | ",
	            "<a href='",YR2,"_c.html'>",SEAS2," >> </a></center><br>",sep=""))

cat("<center><h3>Conference Standings</h3></center><br><table width='80%' align='center'>")

if (u.yr[i]<1996) { 
cat("<col width='6%'><col width='6%'><col width='20%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='26%'>")
ththth <- "</th><th>CW</th><th>CL</th><th>CT</th><th>CCG</th><th>W</th><th>L</th><th>T</th><th></th>"
}

if (u.yr[i]>=1996) { 
cat("<col width='6%'><col width='6%'><col width='22%'><col width='7%'><col width='7%'><col width='1%'><col width='6%'><col width='7%'><col width='7%'><col width='1%'><col width='30%'>")
ththth <- "</th><th>CW</th><th>CL</th><th></th><th>CCG</th><th>W</th><th>L</th><th></th><th></th>"
}

u.lg  <- unique(subset(summ.yr,SORT==1|SORT==2|SORT==5)$conf)
for (j in 1:length(u.lg)) {
cat(paste("<tr><th colspan='11' class='hl'>","<a name='",teamlink(u.lg[j]),"'></a>",u.lg[j],"</th></tr>",sep=""))
u.div <- subset(summ.yr,u.lg[j]==summ.yr$conf); u.div <- unique(u.div$div);

for (k in 1:length(u.div)) {
cat(paste("<tr>","<th>AP</th><th>CP</th><th>",u.div[k],ththth,"</tr>"))
write.table(subset(summ.yr,u.lg[j]==summ.yr$conf & u.div[k]==summ.yr$div)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
cat(paste("<tr><td colspan='11'>&nbsp;</td></tr>"))
}

cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

### Postseason Games ###
sink(paste("../CFB/",u.yr[i],"_post.html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",SEAS,"</center></h2>"))
cat(paste("<center><a href='",YR0,"_p.html'> << ",SEAS0,"</a> | ",
	            "<a href='",YR2,"_p.html'>",SEAS2," >> </a></center><br>",sep=""))

## postseaason games
if (nrow(play.yr)>0) {
cat("<center><a name='bowls'></a><h3>Postseason Games</h3></center>")
cat("<br><table width='80%' align='center'>")

u.rd  <- unique(play.yr$group)

for (m in 1:length(u.rd)) {
cat(paste("<tr>","<th colspan='5' class='hl'>",u.rd[m],"</th>","</tr>"))
write.table(subset(play.yr,u.rd[m]==play.yr$group)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr>","<td colspan='5'>&nbsp;</td>","</tr>"))
}
cat("</table><br>")
}

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

###### FCS #####

if (u.yr[i]>=1995) {
sink(paste("../CFB/",u.yr[i],"_fcs.html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",SEAS,"</center></h2>"))
cat(paste("<center><a href='",YR0,"_fcs.html'> << ",SEAS0,"</a> | ",
	            "<a href='",YR2,"_fcs.html'>",SEAS2," >> </a></center><br>",sep=""))

### Minor
if (nrow(subset(summ.yr,SORT==3|SORT==4))>0) {
cat("<center><h3>Football Championship Subdivision</h3></center><br><table width='80%' align='center'>")

if (u.yr[i]<1996) { 
cat("<col width='6%'><col width='6%'><col width='20%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='26%'>")
ththth <- "</th><th>CW</th><th>CL</th><th>CT</th><th>CCG</th><th>W</th><th>L</th><th>T</th><th></th>"
}

if (u.yr[i]>=1996) { 
cat("<col width='6%'><col width='6%'><col width='22%'><col width='7%'><col width='7%'><col width='1%'><col width='6%'><col width='7%'><col width='7%'><col width='1%'><col width='30%'>")
ththth <- "</th><th>CW</th><th>CL</th><th></th><th>CCG</th><th>W</th><th>L</th><th></th><th></th>"
}

u.lg  <- unique(subset(summ.yr,SORT==3|SORT==4)$conf)
for (j in 1:length(u.lg)) {
cat(paste("<tr><th colspan='11' class='hl'>",u.lg[j],"</th></tr>"))
u.div <- subset(summ.yr,u.lg[j]==summ.yr$conf); u.div <- unique(u.div$div);

for (k in 1:length(u.div)) {
cat(paste("<tr>","<th> </th><th> </th><th>",u.div[k],ththth,"</tr>"))
write.table(subset(summ.yr,u.lg[j]==summ.yr$conf & u.div[k]==summ.yr$div)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
cat(paste("<tr><td colspan='11'>&nbsp;</td></tr>"))
}

cat("</table><br>")
}

## FCS postseaason games
if (nrow(play.yr)>9999) {
cat("<center><h3>Postseason Games</h3></center>")
cat("<br><table width='80%' align='center'>")

u.rd  <- unique(play.yr$group)

for (m in 1:length(u.rd)) {
cat(paste("<tr>","<th colspan='5' class='hl'>",u.rd[m],"</th>","</tr>"))
write.table(subset(play.yr,u.rd[m]==play.yr$group)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
cat("</table><br>")
}

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}}

##########################################
############# team pages #################
##########################################

u.tm <- unique(seasons$team)

for (i in 1:length(u.tm))
{
print(paste(i,u.tm[i]))

# create summary for given year
headerx <- gsub("ABC123","CFB",header)
summ.tm <- subset(seasons, team == u.tm[i])
#summ.tm$RANKCC <- ifelse(summ.tm$conf=="MVC"|summ.tm$conf=="Southern"|summ.tm$conf=="Southland","",paste(summ.tm$RANKCC))
summ.tm <- summ.tm[order(-summ.tm$year),]

summ.tm$class2 <- (0+(1:nrow(summ.tm))) %% 2

summ.tm$row <- paste("<tr class='d",summ.tm$class2,"'><td>",paste(
				paste("<a href='",teamlink(summ.tm$team),"_g.html#",summ.tm$year,"'>",summ.tm$year,"</a>",sep=""),
				ifelse(is.na(summ.tm$AP )==FALSE,paste("#",summ.tm$AP,sep=""),""),
				ifelse(is.na(summ.tm$CP )==FALSE,paste("#",summ.tm$CP,sep=""),""),
				paste(summ.tm$conf,summ.tm$div,sep=" "),
				ifelse(summ.tm$conf!="Indep" & summ.tm$conf!="Minor",paste(summ.tm$RANKCC),""),
				record(summ.tm$WC,summ.tm$LC,summ.tm$TC),
				ifelse(is.na(summ.tm$ccg )==FALSE,summ.tm$ccg,""),
				record(summ.tm$W ,summ.tm$L ,summ.tm$T ),
				summ.tm$bowltext,
				ifelse(is.na(summ.tm$elo)==FALSE,summ.tm$elo,"--"),
			sep="</td><td>"),"</td></tr>",sep="")

# list bowls by year
bowl.tm <- subset(bowls, team1 == paste(u.tm[i]) | team2 == paste(u.tm[i]))
bowl.tm <- bowl.tm[order(-bowl.tm$year, bowl.tm$sort1),]

if (nrow(bowl.tm)>0) {
for (n in 1:nrow(bowl.tm)) {
if (n==1) { bowl.tm$flag[n] <- 2}
if (n >1) { bowl.tm$flag[n] <- ifelse(bowl.tm$year[n] != bowl.tm$year[n-1],1,0)}}

bowl.tm$class2 <- (0+(1:nrow(bowl.tm))) %% 2

bowl.tm$row <- paste(ifelse(bowl.tm$flag==1,"<tr><th colspan='6'></th></tr>",""),"<tr class='d",bowl.tm$class2,"'><td>",paste(
				ifelse(bowl.tm$flag>=1,paste("<a href='",bowl.tm$year,".html'><b>",bowl.tm$year,"</b></a>",sep=""),""),
				paste(bowl.tm$name1),
				paste("<a href='",teamlink(bowl.tm$team1),".html'>",bowl.tm$team1,"</a></div>",sep=""),
				paste(bowl.tm$score1,bowl.tm$score2,sep=" - "),
				paste("<a href='",teamlink(bowl.tm$team2),".html'>",bowl.tm$team2,"</a></div>",sep=""),
				bowl.tm$ot,
			sep="</td><td>"),"</td></tr>",sep="") }

# list games by year
play.tm <- subset(cfgames, team1 == paste(u.tm[i]))
play.tm <- play.tm[order(-play.tm$season, play.tm$date, play.tm$team2),]

if (nrow(play.tm)>0) {
for (n in 1:nrow(play.tm)) {
if (n==1) { play.tm$flag[n] <- 2}
if (n >1) { play.tm$flag[n] <- ifelse(play.tm$season[n] != play.tm$season[n-1],1,0)}}

play.tm$row <- paste(ifelse(play.tm$flag>=1,

				paste("<tr><th>Date</th><th>AP</th><th></th><th align='left'>Opponent</th><th></th><th>PF</th><th></th><th>PA</th><th colspan='2'></th><th>EloPts</th><th>EloRtg</th><th>EloRank</th></tr>",sep="")
				,""),

			"<tr><td>",
			paste(
				paste(play.tm$monday),
				ifelse(is.na(play.tm$aprank1),"",paste("#",play.tm$aprank1,sep="")),
				ifelse(play.tm$home=="A","at",ifelse(play.tm$home=="N","vs",ifelse(play.tm$home=="H","","?"))),

			ifelse(play.tm$pts==-9999|play.tm$pts==9999,
				paste("<div align='left'>",play.tm$team2,"</div>",sep=""),
				paste("<div align='left'>",ifelse(is.na(play.tm$aprank2),"",paste("#",play.tm$aprank2," ",sep="")),
					"<a href='",teamlink(play.tm$team2),"_g.html#",play.tm$season,"'>",play.tm$team2,"</a>",
					ifelse(play.tm$inrank2==-9999,"",paste(" <h5>[",play.tm$inrank2,"]</h5>",sep="")),"</div>",sep="")),

				ifelse(play.tm$points1>play.tm$points2,"W",ifelse(play.tm$points1<play.tm$points2,"L","T")),
				paste(play.tm$points1),
				paste("&#8211;"),
				paste(play.tm$points2),
				paste(play.tm$ot),
				ifelse(play.tm$text!="",paste("<div align='left'><h5>",play.tm$text,"</h5></div>",sep=""),paste("<div align='left'><h5>",play.tm$locn,"</h5></div>",sep="")),
				ifelse(play.tm$pts>0 & play.tm$pts!=-9999,paste("+",play.tm$pts,sep=""),ifelse(play.tm$pts==-9999|play.tm$pts==9999,".",play.tm$pts)),
				ifelse(play.tm$rtg1==-9999,"NR",play.tm$rtg1),
				ifelse(play.tm$rank1==-9999,"--",play.tm$rank1),
			sep="</td><td>"),"</td></tr>",sep="")
 }

#list games by opponent
opp.tm <- subset(oppo, X1 == paste(u.tm[i]))
opp.tm <- opp.tm[order(-opp.tm$P, -opp.tm$W, -opp.tm$T, opp.tm$X2),]

opp.tm$class2 <- (0+(1:nrow(opp.tm))) %% 2

opp.tm$row <- paste("<tr class='d",opp.tm$class2,"'><td>",paste(
				paste("<a href='#",teamlink(opp.tm$X2),"'>",opp.tm$X2,"</a></div>",sep=""),
				opp.tm$P,
				opp.tm$W,
				opp.tm$L,
				opp.tm$T,
				sprintf("%.3f",(opp.tm$W+opp.tm$T/2)/opp.tm$P),
			sep="</td><td>"),"</td></tr>",sep="")

oppg.tm <- subset(cfgames, team1 == paste(u.tm[i]))
opp.tm$Rec <- paste(" (",opp.tm$W,"-",opp.tm$L,ifelse(opp.tm$T>=1,paste("-",opp.tm$T,")",sep=""),")"),sep="")
oppg.tm <- merge(oppg.tm, opp.tm[c("X2","Rec")], by.x="team2", by.y="X2", all.x=T)
oppg.tm <- oppg.tm[order(oppg.tm$team2, oppg.tm$season, oppg.tm$date),]

for (n in 1:nrow(oppg.tm)) {
if (n==1) { oppg.tm$flag[n] <- 2}
if (n >1) { oppg.tm$flag[n] <- ifelse(oppg.tm$team2[n] != oppg.tm$team2[n-1],1,0)}}

oppg.tm$row <- paste(ifelse(oppg.tm$flag>=1,"<tr><th>Year</th><th>Date</th><th>Result</th><th>Score</th><th> </th><th>Location</th><th> </th></tr>",""),"<tr><td>",paste(
				paste(oppg.tm$season),
				paste(oppg.tm$monday),
				ifelse(oppg.tm$points1>oppg.tm$points2,"W",ifelse(oppg.tm$points1<oppg.tm$points2,"L","T")),
				paste(oppg.tm$points1,"&#8211;",oppg.tm$points2),
				paste(oppg.tm$ot),
				paste(oppg.tm$home),
				paste("<div style='text-align: left; margin-left: 10%;'>",ifelse(oppg.tm$text!="",paste(oppg.tm$text),paste(oppg.tm$locn)),"</div>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

### SUMMARY PAGE ###
sink(paste("../CFB/",teamlink(u.tm[i]),".html",sep=""))
headerx <- gsub("<!--Insert Link-->",paste(
u.tm[i],' >> <a href="',teamlink(u.tm[i]),'.html">Summary</a>',
' - <a href="',teamlink(u.tm[i]),'_g.html">Games</a>',
' - <a href="',teamlink(u.tm[i]),'_o.html">Opponents</a>',
sep=""),headerx)

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",summ.tm$team[1],"</center></h2>"))

#cat("<center><h3>Seasons</h3></center>")
#cat(paste("<center>Summary &nbsp; &nbsp; ",sep=""))
#cat(paste("<a href='",teamlink(u.tm[i]),"_g.html'>Games</a> &nbsp; &nbsp; ",sep=""))
#cat(paste("<a href='",teamlink(u.tm[i]),"_o.html'>Opponents</a><br>",sep=""))

cat("<br><table width='80%' align='center'>")
#cat("<col width='7%'><col width='22%'><col width='13%'><col width='5%'><col width='5%'><col width='5%'><col width='5%'><col width='5%'><col width='7%'><col width='7%'><col width='7%'><col width='7%'>")
cat(paste("<tr>","<th>Year</th><th>AP</th><th>CP</th><th>Conference</th><th>Rank</th><th>Conf</th><th>CCG</th><th>Overall</th><th></th><th>EloRank</th></tr>",sep=""))
write.table(summ.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

if (nrow(bowl.tm)>0) {
cat("<center><h3>Postseason Games</h3></center>")
cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Year</th><th colspan='10'></th>","</tr>"))
write.table(bowl.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
}

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

### OPPONENTS PAGE ###
sink(paste("../CFB/",teamlink(u.tm[i]),"_o.html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",summ.tm$team[1],"</center></h2>"))

#cat("<center><h3>Seasons</h3></center>")
#cat(paste("<center><a href='",teamlink(u.tm[i]),".html'>Summary</a> &nbsp; &nbsp; ",sep=""))
#cat(paste("<a href='",teamlink(u.tm[i]),"_g.html'>Games</a> &nbsp; &nbsp; ",sep=""))
#cat(paste("Opponents<br>",sep=""))

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Opponent</th><th>Played</th><th>Wins</th><th>Losses</th><th>Ties</th><th>Pct</th>","</tr>"))
write.table(opp.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

#cat("<center><h3>Games</h3></center>")
#cat("<br><table width='80%' align='center'>")
#cat(paste("<tr>","<th>Opponent</th><th>Year</th><th>Date</th><th>Result</th><th>Score</th><th>Location</th><th> </th>","</tr>"))
#write.table(oppg.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
#cat("</table><br>")

for (pp in 1:length(unique(oppg.tm$team2))) {

WW <- unique(subset(opp.tm,X2==unique(oppg.tm$team2)[pp])$W)
LL <- unique(subset(opp.tm,X2==unique(oppg.tm$team2)[pp])$L)
TT <- unique(subset(opp.tm,X2==unique(oppg.tm$team2)[pp])$T)

	cat(paste("<a name='",teamlink(unique(oppg.tm$team2)[pp]),"'></a>",sep=""))
	cat(paste("<br><h6><div style='text-align: left; margin-left: 10%;'>",
		"<a href='",teamlink(unique(oppg.tm$team2)[pp]),".html'>",unique(oppg.tm$team2)[pp],"</a>",
		" (",WW,"-",LL,ifelse(TT>0,paste("-",TT,sep=""),""),")</div></h6>",sep=""))
	cat("<table width='80%' align='center'>")
	cat("<col width='10%'><col width='10%'><col width='10%'><col width='15%'><col width='5%'><col width='10%'><col width='40%'>")
	write.table(subset(oppg.tm,team2==unique(oppg.tm$team2)[pp])["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
	cat("</table><br><hr>")
	}
cat("<br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

### GAMES PAGE ###
sink(paste("../CFB/",teamlink(u.tm[i]),"_g.html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",summ.tm$team[1],"</center></h2>"))

#cat("<center><h3>Seasons</h3></center>")
#cat(paste("<center><a href='",teamlink(u.tm[i]),".html'>Summary</a> &nbsp; &nbsp; ",sep=""))
#cat(paste("Games &nbsp; &nbsp; ",sep=""))
#cat(paste("<a href='",teamlink(u.tm[i]),"_o.html'>Opponents</a><br>",sep=""))

if (nrow(play.tm)>0) {

for (yr in 1:length(unique(play.tm$season))) {

WW <- unique(subset(play.tm,season==unique(play.tm$season)[yr])$W)
LL <- unique(subset(play.tm,season==unique(play.tm$season)[yr])$L)
TT <- unique(subset(play.tm,season==unique(play.tm$season)[yr])$T)
AP <- unique(subset(play.tm,season==unique(play.tm$season)[yr])$AP)
CP <- unique(subset(play.tm,season==unique(play.tm$season)[yr])$CP)

	cat(paste("<a name='",unique(play.tm$season)[yr],"'></a>",sep=""))
	cat(paste("<br><h6><div style='text-align: left; margin-left: 10%;'>",
		"<a href='",unique(play.tm$season)[yr],".html'>",unique(play.tm$season)[yr],"</a>",
		" (",WW,"-",LL,ifelse(TT>0,paste("-",TT,sep=""),""),")",
		ifelse(is.na(AP)==F,paste(" AP #",AP,sep=""),""),ifelse(is.na(CP)==F,paste(" CP #",CP,sep=""),""),"</div></h6>",sep=""))
	cat("<table width='80%' align='center'>")
	cat("<col width='5%'><col width='5%'><col width='5%'><col width='20%'><col width='5%'><col width='3%'><col width='3%'><col width='3%'><col width='4%'>
		<col width='17%'><col width='10%'><col width='10%'><col width='10%'>")
	write.table(subset(play.tm,season==unique(play.tm$season)[yr])["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
	cat("</table><br><hr>")
	}
cat("<br>")
}

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# by year page ###############
##########################################

byyear <- as.data.frame(u.yr)
names(byyear)[1] <- "year"

fin <- subset(bowls, group=="BCS Championship" | bowl=="CFP Champ")
ap1 <- subset(seasons, AP==1); ap1$ap1 <- ap1$team; 
ap1$ap1rec <- record(ap1$W,ap1$L,ap1$T); ap1$ap1bowl <- ap1$bowl 

cp1 <- subset(seasons, CP==1); cp1$cp1 <- cp1$team; 
cp1$cp1rec <- record(cp1$W,cp1$L,cp1$T); cp1$cp1bowl <- cp1$bowl

byyear <- merge(byyear,fin[c("year","team1","score1","team2","score2")],by=c("year"),all.x=TRUE)
byyear <- merge(byyear,ap1[c("year","ap1","ap1rec","ap1bowl")],by=c("year"),all.x=TRUE)
byyear <- merge(byyear,cp1[c("year","cp1","cp1rec","cp1bowl")],by=c("year"),all.x=TRUE)

byyear$cochamps <- ifelse(is.na(byyear$ap1)==F & is.na(byyear$cp1)==F & byyear$ap1!=byyear$cp1,"Y","N")

seasonsteams <- subset(seasons, is.na(rating)==F)
teams.yr <- as.data.frame.matrix(table(seasonsteams$year,rep("no.teams",length(seasonsteams$year)) ))
teams.yr$year <- rownames(teams.yr)
byyear <- merge(byyear,teams.yr,by="year",all.x=TRUE)

byyear <- byyear[order(-byyear$year),]
byyear$class <- (1+(1:nrow(byyear))) %% 2
byyear$row <- ""

for (yr in 1:nrow(byyear)) {

if (byyear$year[yr]>=1998) {
byyear$row[yr] <- paste("<tr class='d",byyear$class[yr],"'><td>",paste(
	paste("<a href='",byyear$year[yr],".html'>",byyear$year[yr] ,"</a>",sep=""),
	paste("<a href='",byyear$year[yr],"_c.html'>","Standings","</a>",sep=""),
	paste("<a href='",byyear$year[yr],"_c.html#bowls'>","Postseason","</a>",sep=""),
	paste("<a href='",byyear$year[yr],"_elo.html'>","Elo Rating","</a>",sep=""),
	ifelse(byyear$year[yr]>=1995,paste("<a href='",byyear$year[yr],"_elo.html'>","FCS","</a>",sep=""),""),
	byyear$no.teams[yr],
	paste("<a href='",teamlink(byyear$team1[yr]),".html'>", byyear$team1[yr],"</a>",sep=""),
	paste(byyear$score1[yr],byyear$score2[yr],sep="-"),
	paste("<a href='",teamlink(byyear$team2[yr]),".html'>", byyear$team2[yr],"</a>",sep=""),
		sep="</td><td>"),"</td></tr>",sep="") }

if (byyear$year[yr]<1998 & byyear$cochamps[yr]=="N") {
byyear$row[yr] <- paste("<tr class='d",byyear$class[yr],"'><td>",paste(
	paste("<a href='",byyear$year[yr],".html'>",byyear$year[yr] ,"</a>",sep=""),
	paste("<a href='",byyear$year[yr],"_c.html'>","Standings","</a>",sep=""),
	paste("<a href='",byyear$year[yr],"_c.html#bowls'>","Postseason","</a>",sep=""),
	paste("<a href='",byyear$year[yr],"_elo.html'>","Elo Rating","</a>",sep=""),
	ifelse(byyear$year[yr]>=1995,paste("<a href='",byyear$year[yr],"_elo.html'>","FCS","</a>",sep=""),""),
	byyear$no.teams[yr],
	paste("<a href='",teamlink(byyear$ap1[yr]),".html'>", byyear$ap1[yr],"</a>",sep=""),
	paste(byyear$ap1rec[yr]),
	paste(byyear$ap1bowl[yr]),
		sep="</td><td>"),"</td></tr>",sep="") }

if (byyear$year[yr]<1998 & byyear$cochamps[yr]=="Y") {
byyear$row[yr] <- paste("<tr class='d",byyear$class[yr],"'><td>",paste(
	paste("<a href='",byyear$year[yr],".html'>",byyear$year[yr] ,"</a>",sep=""),
	paste("<a href='",byyear$year[yr],"_c.html'>","Standings","</a>",sep=""),
	paste("<a href='",byyear$year[yr],"_c.html#bowls'>","Postseason","</a>",sep=""),
	paste("<a href='",byyear$year[yr],"_elo.html'>","Elo Rating","</a>",sep=""),
	ifelse(byyear$year[yr]>=1995,paste("<a href='",byyear$year[yr],"_elo.html'>","FCS","</a>",sep=""),""),
	byyear$no.teams[yr],
	paste("<a href='",teamlink(byyear$ap1[yr]),".html'>", byyear$ap1[yr],"</a><br>",
			"<a href='",teamlink(byyear$cp1[yr]),".html'>", byyear$cp1[yr],"</a>",sep=""),
	paste(byyear$ap1rec[yr], "<br>",byyear$cp1rec[yr] ,sep=""),
	paste(byyear$ap1bowl[yr],"<br>",byyear$cp1bowl[yr],sep=""),
		sep="</td><td>"),"</td></tr>",sep="") }
}

sink(paste("../CFB/byyear.html",sep=""))

headerx <- gsub("ABC123","CFB",header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>College Football</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Season</th><th> </th><th> </th><th> </th><th> </th><th>Teams</th><th>Champion</th><th></th><th></th></tr>",sep=""))
write.table(byyear["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()


##########################################
############# by team page ###############
##########################################

seasons$count <- 1
byteam <- as.data.frame(u.tm)
names(byteam)[1] <- "team"

seas <- aggregate(seasons$count, list(seasons$team), sum); names(seas)[names(seas)=="x"] <- "Years"
wins <- aggregate(seasons$W    , list(seasons$team), sum); names(wins)[names(wins)=="x"] <- "W";   summ <- merge(seas,wins,by="Group.1");
loss <- aggregate(seasons$L    , list(seasons$team), sum); names(loss)[names(loss)=="x"] <- "L";   summ <- merge(summ,loss,by="Group.1");
ties <- aggregate(seasons$T    , list(seasons$team), sum, na.rm = T); names(ties)[names(ties)=="x"] <- "T";   summ <- merge(summ,ties,by="Group.1");
fyr  <- aggregate(seasons$year , list(seasons$team), min); names(fyr )[names(fyr )=="x"] <- "FYr"; summ <- merge(summ,fyr ,by="Group.1");
lyr  <- aggregate(seasons$year , list(seasons$team), max); names(lyr )[names(lyr )=="x"] <- "LYr"; summ <- merge(summ,lyr ,by="Group.1");

byteam <- merge(byteam,summ,by.x="team",by.y="Group.1")
byteam <- merge(byteam,subset(seasons, year==2024)[c("team","conf","SORT")],by="team",all.x=T)
byteam$SORT <- ifelse(is.na(byteam$SORT),99,byteam$SORT)
byteam$conf <- ifelse(byteam$SORT==99,"Minor/Defunct",byteam$conf)
byteam <- byteam[order(byteam$SORT, byteam$conf, byteam$team),]

byteam$class <- (1+(1:nrow(byteam))) %% 2
byteam$row <- paste("<tr class='d",byteam$class,"'><td>",paste(
				paste(byteam$team),
				paste("<a href='",teamlink(byteam$team),".html'>Summary</a>",sep=""),
				paste("<a href='",teamlink(byteam$team),"_g.html'>Seasons</a>",sep=""),
				paste("<a href='",teamlink(byteam$team),"_o.html'>Opponents</a>",sep=""),
				byteam$Years,
				byteam$W,
				byteam$L,
				byteam$T,
				byteam$FYr,
				byteam$LYr,
			sep="</td><td>"),"</td></tr>",sep="")


sink(paste("../CFB/byteam.html",sep=""))

headerx <- gsub("ABC123","CFB",header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>College Football</center></h2>"))

u.conf <- unique(subset(byteam,SORT==1|SORT==2)$conf)
cat("<br><table width='80%' align='center'>")
cat("<col width='19%'><col width='10%'><col width='10%'><col width='9%'><col width='9%'><col width='9%'><col width='9%'><col width='9%'><col width='8%'><col width='8%'>")
for (i in 1:length(u.conf)) {
cat(paste("<tr><th colspan='10' class='hl'>",u.conf[i],"</th></tr>"))
cat(paste("<tr>","<th colspan='4'>Team</th><th>Years</th><th>W</th><th>L</th><th>T</th><th>First</th><th>Last</th></tr>",sep=""))
write.table(subset(byteam, conf==u.conf[i])["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
cat("</table><br>")

u.conf <- unique(subset(byteam,SORT==3|SORT==4)$conf)
cat("<hr><br><h3><center>FCS</center></h3><br><table width='80%' align='center'>")
cat("<col width='19%'><col width='10%'><col width='10%'><col width='9%'><col width='9%'><col width='9%'><col width='9%'><col width='9%'><col width='8%'><col width='8%'>")
for (i in 1:length(u.conf)) {
cat(paste("<tr><th colspan='10' class='hl'>",u.conf[i],"</th></tr>"))
cat(paste("<tr>","<th colspan='4'>Team</th><th>Years</th><th>W</th><th>L</th><th>T</th><th>First</th><th>Last</th></tr>",sep=""))
write.table(subset(byteam, conf==u.conf[i])["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
cat("</table><br>")

u.conf <- unique(subset(byteam,SORT>4)$conf)
cat("<hr><br><h3><center>Minor or Defunct</center></h3><br><table width='80%' align='center'>")
cat("<col width='19%'><col width='10%'><col width='10%'><col width='9%'><col width='9%'><col width='9%'><col width='9%'><col width='9%'><col width='8%'><col width='8%'>")
for (i in 1:length(u.conf)) {
cat(paste("<tr><th colspan='10' class='hl'>",u.conf[i],"</th></tr>"))
cat(paste("<tr>","<th colspan='4'>Team</th><th>Years</th><th>W</th><th>L</th><th>T</th><th>First</th><th>Last</th></tr>",sep=""))
write.table(subset(byteam, conf==u.conf[i])["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()


##########################################
############# index page #################
##########################################

byyear2 <- byyear 
byyear2 <- byyear2[order(-byyear2$year),]
byyear2 <- byyear2[1:10,]

byteam2 <- byteam
byteam2 <- byteam2[order(-byteam2$W),]
byteam2 <- byteam2[1:10,]

byyear2$row <- paste("<tr><td>",paste(
	paste("<a href='",byyear2$year,".html'>",byyear2$year ,"</a>",sep=""),
	paste("<a href='",teamlink(byyear2$team1),".html'>", byyear2$team1,"</a>",sep=""),
	paste(byyear2$score1,byyear2$score2,sep="-"),
	paste("<a href='",teamlink(byyear2$team2),".html'>", byyear2$team2,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

byteam2$row <- paste("<tr><td>",paste(
				paste("<a href='",teamlink(byteam2$team),".html'>",byteam2$team,"</a>",sep=""),
				byteam2$W,
				byteam2$L,
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CFB/index.html",sep=""))

headerx <- gsub("ABC123","CFB",header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th colspan='4' class='hl'><div style='font-size:1.5em;'>Results by Year</div></th>","</tr>"))
cat(paste("<tr>","<th>Year</th><th>Champion</th><th>Score</th><th>Runner-up</th>","</tr>"))
write.table(byyear2["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr>","<td colspan='4'><a href='byyear.html'>All Years</a></td>","</tr>"))
cat("</table><br>")

cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th colspan='3' class='hl'><div style='font-size:1.5em;'>Results by Team</div></th>","</tr>"))
cat(paste("<tr>","<th>Team</th><th>W</th><th>L</th>","</tr>"))
write.table(byteam2["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr>","<td colspan='3'><a href='byteam.html'>All Teams</a></td>","</tr>"))
cat("</table><br>")

cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th class='hl'><div style='font-size:1.5em;'>Event Results</div></th>","</tr>"))

	cat(paste("<tr>","<td><a href='cfp.html'>College Football Playoff</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='bowls.html'>All Bowls</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='rose.html'>Rose Bowl</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='orange.html'>Orange Bowl</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='ccg.html'>Conference Championship Games</a></td>","</tr>"))

cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()


##########################################
############# event pages ################
##########################################

rose	<- subset(bowls, bowlid == "Rose")
orange<- subset(bowls, bowlid == "Orange")
ccg	<- subset(bowls, group  == "Conference Championship Games")
all	<- subset(bowls, group  != "Conference Championship Games")

##################################################################################################
ccg <- ccg[order(ccg$name1,-ccg$year),]
ccg$class <- (1+(1:nrow(ccg))) %% 2

if (nrow(ccg)>0) {
for (p in 1:nrow(ccg)) {
if (p==1) { ccg$flag[p] <- 2}
if (p >1) { ccg$flag[p] <- ifelse(ccg$name1[p] != ccg$name1[p-1]|ccg$name1[p] != ccg$name1[p-1]
							,ifelse(ccg$name1[p] != ccg$name1[p-1],2,1),0)}}}

ccg$row <- paste(ifelse(ccg$flag==2,"<tr><th colspan='6'></th></tr>",""),"<tr><td>",paste(
			ifelse(ccg$flag==2,paste("<b>",ccg$name1,"</b>",sep=""),""),
			paste("<a href='",ccg$year,".html'>",ccg$year ,"</a>",sep=""),
			paste("<a href='",teamlink(ccg$team1),".html'>", ccg$team1,"</a>",sep=""),
			paste(ccg$score1,ccg$score2,sep=" - "),
			paste("<a href='",teamlink(ccg$team2),".html'>", ccg$team2,"</a>",sep=""),
			paste(ccg$ot),
		sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CFB/ccg.html",sep=""))
headerx <- gsub("ABC123","CFB",header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>Conference Championship Games</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat("<tr><th>Conference</th><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th></tr>")
write.table(ccg["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

##################################################################################################
cfp <- subset(all, group=="College Football Playoff")
cfp <- cfp[order(-cfp$year,cfp$sort1),]
cfp$class <- (1+(1:nrow(cfp))) %% 2

if (nrow(cfp)>0) {
for (p in 1:nrow(cfp)) {
if (p==1) { cfp$flag[p] <- 2}
if (p >1) { cfp$flag[p] <- ifelse(cfp$year[p] != cfp$year[p-1]|cfp$year[p] != cfp$year[p-1]
							,ifelse(cfp$year[p] != cfp$year[p-1],2,1),0)}}}

cfp$row <- paste(ifelse(cfp$flag==2,"<tr><th colspan='6'></th></tr>",""),"<tr><td>",paste(
			ifelse(cfp$flag==2,paste("<b><a href='",cfp$year,".html'>",cfp$year ,"</a></b>",sep=""),""),
			paste(cfp$name1),
			paste("<a href='",teamlink(cfp$team1),".html'>", cfp$team1,"</a>",sep=""),
			paste(cfp$score1,cfp$score2,sep=" - "),
			paste("<a href='",teamlink(cfp$team2),".html'>", cfp$team2,"</a>",sep=""),
			paste(cfp$ot),
		sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CFB/cfp.html",sep=""))
headerx <- gsub("ABC123","CFB",header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>College Football Playoff</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat("<tr><th>Conference</th><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th></tr>")
write.table(cfp["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

##################################################################################################
all <- all[order(all$bowlid,-all$year),]
all$class <- (1+(1:nrow(all))) %% 2

if (nrow(all)>0) {
for (p in 1:nrow(all)) {
if (p==1) { all$flag[p] <- 2}
if (p >1) { all$flag[p] <- ifelse(all$bowlid[p] != all$bowlid[p-1]|all$bowlid[p] != all$bowlid[p-1]
							,ifelse(all$bowlid[p] != all$bowlid[p-1],2,1),0)}}}

all$row <- paste(ifelse(all$flag==2,"<tr><th colspan='6'></th></tr>",""),"<tr><td>",paste(
			ifelse(all$flag==2,paste("<b>",all$bowlid,"</b>",sep=""),""),
			paste("<a href='",all$year,".html'>",all$year ,"</a>",sep=""),
			paste("<a href='",teamlink(all$team1),".html'>", all$team1,"</a>",sep=""),
			paste(all$score1,all$score2,sep=" - "),
			paste("<a href='",teamlink(all$team2),".html'>", all$team2,"</a>",sep=""),
			paste(all$ot),
		sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CFB/bowls.html",sep=""))
headerx <- gsub("ABC123","CFB",header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>Bowl Games</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat("<tr><th>Bowl</th><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th></tr>")
write.table(all["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

##################################################################################################
singlepage <- function(gamesx,sportx,htmlx,titlex,thx) {

gamesx <- gamesx[order(-gamesx$year),]
gamesx$class <- (1+(1:nrow(gamesx))) %% 2

gamesx$row <- paste("<tr class='d",gamesx$class,"'><td>",paste(
			paste("<a href='",gamesx$year,".html'>",gamesx$year ,"</a>",sep=""),
			paste("<a href='",teamlink(gamesx$team1),".html'>", gamesx$team1,"</a>",sep=""),
			paste(gamesx$score1,gamesx$score2,sep=" - "),
			paste("<a href='",teamlink(gamesx$team2),".html'>", gamesx$team2,"</a>",sep=""),
			paste(gamesx$ot),
		sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../",sportx,"/",htmlx,sep=""))
headerx <- gsub("ABC123",sportx,header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",titlex,"</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat(thx)
write.table(gamesx["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}
##################################################################################################

singlepage(rose,"CFB","rose.html","Rose Bowl","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th></tr>")
singlepage(orange,"CFB","orange.html","Orange Bowl","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th></tr>")
#singlepage(ccg,"CFB","ccg.html","Conference Championship Games","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th></tr>")


