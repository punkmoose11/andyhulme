
############# import data
seasons <- read.csv("../Data/cfseasons.csv")
bowls   <- read.csv("../Data/cfbowls.csv")
polls   <- read.csv("../Data/cfpolls.csv")

teamlink <- function(x){teamx = tolower(gsub("&","",gsub(".","",gsub("'","",gsub(" ", "",x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE))}
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

seasons <- merge(seasons,ap[c("year","team","AP")],by=c("year","team"),all.x=T)
seasons <- merge(seasons,cp[c("year","team","CP")],by=c("year","team"),all.x=T)

#############################################

bowls$champ <- ifelse(grepl("CFP Champ",bowls$bowl)|grepl("BCS Champ",bowls$bowl)
				| ((bowls$year==1998|bowls$year==2002) & bowls$bowlid=="Fiesta")
				| ((bowls$year==1999|bowls$year==2003) & bowls$bowlid=="Sugar")
				| ((bowls$year==2000|bowls$year==2004) & bowls$bowlid=="Orange")
				| ((bowls$year==2001|bowls$year==2005) & bowls$bowlid=="Rose"),"Y","N")

bowls$group <- ifelse(grepl("CFP Champ",bowls$bowl) | grepl("Semifinal",bowls$ot),"College Football Playoff",
			ifelse(bowls$year>=2014 & (bowls$bowlid=="Orange"|bowls$bowlid=="Rose"|bowls$bowlid=="Sugar"|bowls$bowlid=="Fiesta"|bowls$bowlid=="Cotton"|bowls$bowlid=="Peach"),"New Years Six",
			ifelse(bowls$champ=="Y","BCS Championship",
			ifelse(1998<=bowls$year & bowls$year<=2013 & (bowls$bowlid=="Orange"|bowls$bowlid=="Rose"|bowls$bowlid=="Sugar"|bowls$bowlid=="Fiesta"),"BCS Bowl Games",
			ifelse(1995<=bowls$year & bowls$year<=1997 & (bowls$bowlid=="Orange"|bowls$bowlid=="Rose"|bowls$bowlid=="Sugar"|bowls$bowlid=="Fiesta"),"Major Bowl Games",
			ifelse(1981<=bowls$year & bowls$year<=1994 & (bowls$bowlid=="Orange"|bowls$bowlid=="Rose"|bowls$bowlid=="Sugar"|bowls$bowlid=="Fiesta"|bowls$bowlid=="Cotton"),"Major Bowl Games",
			ifelse(bowls$year<=1980 & (bowls$bowlid=="Orange"|bowls$bowlid=="Rose"|bowls$bowlid=="Sugar"|bowls$bowlid=="Cotton"),"Major Bowl Games",
			ifelse(grepl(" Champ",bowls$bowl) & grepl("Champ Sports",bowls$bowl)==F,"Conference Championship Games",
			"Bowl Games"))))))))

bowls$name1 <- ifelse(bowls$group=="Conference Championship Games",gsub("(\\w+)(\\s)(\\w+)","\\1", bowls$bowl),
			ifelse(grepl(" Champ",bowls$bowl),paste(bowls$bowl,"ionship",sep=""),paste(bowls$bowl,"Bowl")))

bowls$name1 <- gsub("B12","Big 12",bowls$name1)
bowls$name1 <- gsub("B10","Big 10",bowls$name1)
bowls$name1 <- gsub("P12","Pac 12",bowls$name1)
bowls$name1 <- gsub("P12","Pac 12",bowls$name1)
bowls$name1 <- gsub("AAC","American",bowls$name1)

bowls$sort1 <- ifelse(bowls$group=="Conference Championship Games",1,
			ifelse(bowls$group=="Bowl Games",2,
			ifelse(bowls$group=="Major Bowl Games",3,
			ifelse(bowls$group=="BCS Bowl Games",4,
			ifelse(bowls$group=="New Years Six",5,
			ifelse(bowls$name1=="CFP Championship",8,
			ifelse(grepl(" Championship",bowls$group),6,7)))))))
			

bowls.1 <- bowls
bowls.1$team <- bowls.1$team1
bowls.1$result <- paste(ifelse(bowls.1$score1==bowls.1$score2,"tied","won"),bowls.1$bowl)
bowls.1 <- bowls.1[c("year","team","result")]

bowls.2 <- bowls
bowls.2$team <- bowls.2$team2
bowls.2$result <- paste(ifelse(bowls.2$score1==bowls.2$score2,"tied","lost"),bowls.2$bowl)
bowls.2 <- bowls.2[c("year","team","result")]

games <- rbind(bowls.1,bowls.2) 

bgm <- subset(games, bowls$sort1!=1 & bowls$sort1!=8)
ccg <- subset(games, bowls$sort1==1)
cfp <- subset(games, bowls$sort1==8)

bgm$bowl <- bgm$result
ccg$ccg  <- ifelse(grepl("won",ccg$result),"W","L")
cfp$cfp  <- paste("; ",cfp$result,sep="")

#####

bowls.x <- merge(seasons,bgm[c("year","team","bowl")],by=c("year","team"),all.y=TRUE)
bowls.x <- subset(bowls.x, is.na(conf) & year<2020)[c("year","team","bowl")]
bowls.x

seasons <- merge(seasons,bgm[c("year","team","bowl")],by=c("year","team"),all.x=TRUE)
seasons <- merge(seasons,ccg[c("year","team","ccg")] ,by=c("year","team"),all.x=TRUE)
seasons <- merge(seasons,cfp[c("year","team","cfp")] ,by=c("year","team"),all.x=TRUE)

##########################################
############# generate header / footer ###
##########################################

header <- vector()
header[1] <- '<html><head><title>OmahaSeries.com</title>'
header[2] <- '<link rel="stylesheet" type="text/css" href="styles.css"></head>' 
header[3] <- '<body><div id="red"><h1><a class="red" href="http://omahaseries.com">OmahaSeries.com</a></h1></div><div id="yellow"></div>'
header[4] <- '<div id="blue"><h4><a class="blue" href="index.html">ABC123</a> -- <a class="blue" href="byyear.html">YEARS</a> -- <a class="blue" href="byteam.html">TEAMS</a></h4></div>'

footer <- vector()
footer[1] <- '<div id="foot"> </div></body></html>'

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

summ.yr <- summ.yr[order(summ.yr$conf, summ.yr$div, summ.yr$RANKC, summ.yr$RANK),]
summ.yr$class1 <- 0

summ.yr$row <- paste("<tr class='d",summ.yr$class1,"'><td>",paste(
				ifelse(is.na(summ.yr$AP )==FALSE,paste("#",summ.yr$AP,sep=""),""),
				ifelse(is.na(summ.yr$CP )==FALSE,paste("#",summ.yr$CP,sep=""),""),
				paste("<div style='text-align:left'><a href='",teamlink(summ.yr$team),".html'>",summ.yr$team,"</a></div>",sep=""),
				ifelse(summ.yr$conf!="Indep" & summ.yr$conf!="Minor",summ.yr$WC,""),
				ifelse(summ.yr$conf!="Indep" & summ.yr$conf!="Minor",summ.yr$LC,""),
				ifelse(summ.yr$conf!="Indep" & summ.yr$conf!="Minor" & summ.yr$TC>0,summ.yr$TC,""),
				ifelse(is.na(summ.yr$ccg )==FALSE,summ.yr$ccg,""),
				summ.yr$W,
				summ.yr$L,
				ifelse(summ.yr$T >0,summ.yr$T,""),
				ifelse(is.na(summ.yr$bowl)==FALSE,paste(summ.yr$bowl,
					ifelse(is.na(summ.yr$cfp),"",summ.yr$cfp),sep=""),""),
			sep="</td><td>"),"</td></tr>",sep="")

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

sink(paste("../CFB/",u.yr[i],".html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",SEAS,"</center></h2>"))
cat(paste("<center><a href='",YR0,".html'> << ",SEAS0,"</a> | ",
	            "<a href='",YR2,".html'>",SEAS2," >> </a></center><br>",sep=""))

cat("<br><table width='80%' align='center'>")

if (u.yr[i]<1996) { 
cat("<col width='6%'><col width='6%'><col width='20%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='26%'>")
ththth <- "</th><th>CW</th><th>CL</th><th>CT</th><th>CCG</th><th>W</th><th>L</th><th>T</th><th></th>"
}

if (u.yr[i]>=1996) { 
cat("<col width='6%'><col width='6%'><col width='22%'><col width='7%'><col width='7%'><col width='1%'><col width='6%'><col width='7%'><col width='7%'><col width='1%'><col width='30%'>")
ththth <- "</th><th>CW</th><th>CL</th><th></th><th>CCG</th><th>W</th><th>L</th><th></th><th></th>"
}

u.lg  <- unique(summ.yr$conf)
for (j in 1:length(u.lg)) {
cat(paste("<tr><th colspan='11' class='hl'>",u.lg[j],"</th></tr>"))
u.div <- subset(summ.yr,u.lg[j]==summ.yr$conf); u.div <- unique(u.div$div);

for (k in 1:length(u.div)) {
cat(paste("<tr>","<th>AP</th><th>CP</th><th>",u.div[k],ththth,"</tr>"))
write.table(subset(summ.yr,u.lg[j]==summ.yr$conf & u.div[k]==summ.yr$div)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}}

cat("</table><br>")

if (nrow(play.yr)>0) {
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
}

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

summ.tm$class2 <- 0

summ.tm$row <- paste("<tr class='d",summ.tm$class2,"'><td>",paste(
				paste("<a href='",summ.tm$year,".html'>",summ.tm$year,"</a>",sep=""),
				ifelse(is.na(summ.tm$AP )==FALSE,paste("#",summ.tm$AP,sep=""),""),
				ifelse(is.na(summ.tm$CP )==FALSE,paste("#",summ.tm$CP,sep=""),""),
				paste(summ.tm$conf,summ.tm$div,sep=" "),
				ifelse(summ.tm$conf!="Indep" & summ.tm$conf!="Minor",paste(summ.tm$RANKCC),""),
				record(summ.tm$WC,summ.tm$LC,summ.tm$TC),
				ifelse(is.na(summ.tm$ccg )==FALSE,summ.tm$ccg,""),
				record(summ.tm$W ,summ.tm$L ,summ.tm$T ),
				ifelse(is.na(summ.tm$bowl)==FALSE,paste(summ.tm$bowl,
					ifelse(is.na(summ.tm$cfp),"",summ.tm$cfp),sep=""),""),
			sep="</td><td>"),"</td></tr>",sep="")

play.tm <- subset(bowls, team1 == paste(u.tm[i]) | team2 == paste(u.tm[i]))
play.tm <- play.tm[order(-play.tm$year, play.tm$sort1),]

if (nrow(play.tm)>0) {
for (n in 1:nrow(play.tm)) {
if (n==1) { play.tm$flag[n] <- 2}
if (n >1) { play.tm$flag[n] <- ifelse(play.tm$year[n] != play.tm$year[n-1],1,0)}}

play.tm$row <- paste(ifelse(play.tm$flag==1,"<tr><th colspan='6'></th></tr>",""),"<tr><td>",paste(
				ifelse(play.tm$flag>=1,paste("<a href='",play.tm$year,".html'><b>",play.tm$year,"</b></a>",sep=""),""),
				paste(play.tm$name1),
				paste("<a href='",teamlink(play.tm$team1),".html'>",play.tm$team1,"</a></div>",sep=""),
				paste(play.tm$score1,play.tm$score2,sep=" - "),
				paste("<a href='",teamlink(play.tm$team2),".html'>",play.tm$team2,"</a></div>",sep=""),
				play.tm$ot,
			sep="</td><td>"),"</td></tr>",sep="") }

sink(paste("../CFB/",teamlink(u.tm[i]),".html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",summ.tm$team[1],"</center></h2>"))

cat("<br><table width='80%' align='center'>")
#cat("<col width='7%'><col width='22%'><col width='13%'><col width='5%'><col width='5%'><col width='5%'><col width='5%'><col width='5%'><col width='7%'><col width='7%'><col width='7%'><col width='7%'>")
cat(paste("<tr>","<th>Year</th><th>AP</th><th>CP</th><th>Conference</th><th>Rank</th><th>Conf</th><th>CCG</th><th>Overall</th><th></th></tr>",sep=""))
write.table(summ.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

if (nrow(play.tm)>0) {
cat("<center><h3>Postseason Games</h3></center>")
cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Year</th><th colspan='5'></th>","</tr>"))
write.table(play.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
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

teams.yr <- as.data.frame.matrix(table(seasons$year,rep("no.teams",length(seasons$year)) ))
teams.yr$year <- rownames(teams.yr)
byyear <- merge(byyear,teams.yr,by="year",all.x=TRUE)

byyear <- byyear[order(-byyear$year),]
byyear$class <- (1+(1:nrow(byyear))) %% 2
byyear$row <- ""

for (yr in 1:nrow(byyear)) {

if (byyear$year[yr]>=1998) {
byyear$row[yr] <- paste("<tr class='d",byyear$class[yr],"'><td>",paste(
	paste("<a href='",byyear$year[yr],".html'>",byyear$year[yr] ,"</a>",sep=""),
	byyear$no.teams[yr],
	paste("<a href='",teamlink(byyear$team1[yr]),".html'>", byyear$team1[yr],"</a>",sep=""),
	paste(byyear$score1[yr],byyear$score2[yr],sep="-"),
	paste("<a href='",teamlink(byyear$team2[yr]),".html'>", byyear$team2[yr],"</a>",sep=""),
		sep="</td><td>"),"</td></tr>",sep="") }

if (byyear$year[yr]<1998 & byyear$cochamps[yr]=="N") {
byyear$row[yr] <- paste("<tr class='d",byyear$class[yr],"'><td>",paste(
	paste("<a href='",byyear$year[yr],".html'>",byyear$year[yr] ,"</a>",sep=""),
	byyear$no.teams[yr],
	paste("<a href='",teamlink(byyear$ap1[yr]),".html'>", byyear$ap1[yr],"</a>",sep=""),
	paste(byyear$ap1rec[yr]),
	paste(byyear$ap1bowl[yr]),
		sep="</td><td>"),"</td></tr>",sep="") }

if (byyear$year[yr]<1998 & byyear$cochamps[yr]=="Y") {
byyear$row[yr] <- paste("<tr class='d",byyear$class[yr],"'><td>",paste(
	paste("<a href='",byyear$year[yr],".html'>",byyear$year[yr] ,"</a>",sep=""),
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
cat(paste("<tr>","<th>Season</th><th>Teams</th><th>Champion</th><th></th><th></th></tr>",sep=""))
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
byteam <- byteam[order(-byteam$LYr, byteam$team),]

byteam$class <- (1+(1:nrow(byteam))) %% 2
byteam$row <- paste("<tr class='d",byteam$class,"'><td>",paste(
				paste("<a href='",teamlink(byteam$team),".html'>",byteam$team,"</a>",sep=""),
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

cat("<br><table width='80%' align='center'>")
cat("<col width='34%'><col width='11%'><col width='11%'><col width='11%'><col width='11%'><col width='11%'><col width='11%'>")
cat(paste("<tr>","<th>Team</th><th>Years</th><th>W</th><th>L</th><th>T</th><th>First</th><th>Last</th></tr>",sep=""))
write.table(byteam["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
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


