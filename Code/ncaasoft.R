
############# import data
playoff <- read.csv("../Data/ncaagames.csv")
playoff <- subset(playoff, sport=="SOF")

playoff.1 <- playoff[c("sport","year","name1","name2","round","game","ot","team1","score1","team2","score2")]
playoff.1$score <- paste(playoff.1$score1,playoff.1$score2,sep="-")
playoff.1$result <- ifelse(playoff.1$score1>playoff.1$score2,"W",ifelse(playoff.1$score1<playoff.1$score2,"L","T"))
playoff.1$numb <- 1

playoff.2 <- playoff[c("sport","year","name1","name2","round","game","ot")]
playoff.2$team1 <- playoff$team2
playoff.2$team2 <- playoff$team1
playoff.2$score1 <- playoff$score2
playoff.2$score2 <- playoff$score1
playoff.2$score <- paste(playoff.2$score1,playoff.2$score2,sep="-")
playoff.2$result <- ifelse(playoff.2$score1>playoff.2$score2,"W",ifelse(playoff.2$score1<playoff.2$score2,"L","T"))
playoff.2$numb <- 2

playoff <- rbind(playoff.1,playoff.2)

playoff$score1 <- gsub(".123","PK",playoff$score1)
playoff$score2 <- gsub(".123","PK",playoff$score2)
playoff$score <- gsub(".123","PK",playoff$score)

###########################################

teamlink <- function(x){teamx = tolower(gsub("&","",gsub(".","",gsub("'","",gsub(" ", "",x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE))}

seedsort <- function(seed) {
	if (is.na(seed))  {seedn<-0}
	else if (seed==1) {seedn<-1}
	else if (seed==16){seedn<-2}
	else if (seed==8) {seedn<-3}
	else if (seed==9) {seedn<-4}
	else if (seed==5) {seedn<-5}
	else if (seed==12){seedn<-6}
	else if (seed==4) {seedn<-7}
	else if (seed==13){seedn<-8}
	else if (seed==6) {seedn<-9}
	else if (seed==11){seedn<-10}
	else if (seed==3) {seedn<-11}
	else if (seed==14){seedn<-12}
	else if (seed==7) {seedn<-13}
	else if (seed==10){seedn<-14}
	else if (seed==2) {seedn<-15}
	else if (seed==15){seedn<-16}
}

##########################################

############# build seasons from playoff
teams1 <- unique(playoff[c("sport","year","team1")]); names(teams1) <- c("sport","year","team");
teams2 <- unique(playoff[c("sport","year","team2")]); names(teams2) <- c("sport","year","team");
teams3 <- rbind(teams1, teams2)
seasons<- unique(teams3[c("sport","year","team")])
rownames(seasons) <- seq(length=nrow(seasons))

seasons<- seasons[order(seasons$team, seasons$year),]
seasons$key <- paste(seasons$sport,seasons$year,seasons$team,sep="/")

###########################################
#process softball rounds

rounds <- playoff[c("sport","year","round","team1","score","result")]

rounds$key <- paste(rounds$sport,rounds$year,rounds$team1,sep="/")
rounds$key2<- paste(rounds$sport,rounds$team1,sep="/")
summ <- as.data.frame.matrix(table(rounds$key,paste(rounds$result,rounds$round,sep="")))
summ$key <- rownames(summ)
summ$W <- summ$W33 + summ$W22 + summ$W11 + summ$W1
summ$L <- summ$L33 + summ$L22 + summ$L11 + summ$L1

seasons <- merge(seasons,summ,by="key",all.x=TRUE)

#rounds$final <- ifelse(rounds$round==1 ,rounds$score,"")
#rounds$third <- ifelse(rounds$round==3 ,rounds$result,"")
#rounds$semif <- ifelse(rounds$round==4 ,rounds$score,"")
#rounds$elite <- ifelse(rounds$round==8 ,rounds$score,"")
#rounds$sweet <- ifelse(rounds$round==16,rounds$score,"")
#rounds$rd.32 <- ifelse(rounds$round==32,rounds$score,"")
#rounds$rd.64 <- ifelse(rounds$round==64,rounds$score,"")

#####

#seasons <- merge(seasons,subset(rounds,final!="")[c("key","final","result")],by=c("key"),all.x=TRUE)
#seasons <- merge(seasons,subset(rounds,third!="")[c("key","third")],by=c("key"),all.x=TRUE)
#seasons <- merge(seasons,subset(rounds,semif!="")[c("key","semif")],by=c("key"),all.x=TRUE)
#seasons <- merge(seasons,subset(rounds,elite!="")[c("key","elite")],by=c("key"),all.x=TRUE)
#seasons <- merge(seasons,subset(rounds,sweet!="")[c("key","sweet")],by=c("key"),all.x=TRUE)
#seasons <- merge(seasons,subset(rounds,rd.32!="")[c("key","rd.32")],by=c("key"),all.x=TRUE)
#seasons <- merge(seasons,subset(rounds,rd.64!="")[c("key","rd.64")],by=c("key"),all.x=TRUE)

################

seasons$finish <- ifelse(seasons$W1>seasons$L1,"<b>CHAMPION</b>",
			ifelse(seasons$W1<seasons$L1 & seasons$L1>0,"Runner-up",
			ifelse(seasons$W11+seasons$L11,"CWS",
			ifelse(seasons$W22+seasons$L22,"Super",
			ifelse(seasons$W33+seasons$L33," ","")))))

seasons$key1 <- paste(seasons$sport,seasons$year,sep="/")
seasons$key2 <- paste(seasons$sport,seasons$team,sep="/")
playoff$key1 <- paste(playoff$sport,playoff$year,sep="/")

playoff$key01<- paste(playoff$sport,playoff$year,playoff$team1,sep="/")
playoff$key02<- paste(playoff$sport,playoff$year,playoff$team2,sep="/")

u.1 <- unique(playoff[c("key01","name1","name2")]); names(u.1)[names(u.1) == "key01"] <- "key";
u.2 <- unique(playoff[c("key02","name1","name2")]); names(u.2)[names(u.2) == "key02"] <- "key";
u.3 <- rbind(u.1,u.2)
reg <- unique(subset(u.3, grepl("SR",name2)==F & name1!="WCWS" & name1!="Championship"))

seasons <- merge(seasons,reg,by="key",all.x=T)

seasons$sort <- 1000 * seasons$W1 + 100 * seasons$L1 + 10*seasons$W11 + seasons$L11
seasons$rank <- ave(-seasons$sort, seasons$year, FUN = function(x) rank(x, ties.method = "first") )


##########################################
############# generate header / footer ###
##########################################

header <- vector()
header[1] <- '<html><head><title>AndyHulme.Net</title>'
header[2] <- '<link rel="stylesheet" type="text/css" href="styles.css"></head>' 
header[3] <- '<body><div id="red"></div><div id="yellow"></div>'
header[4] <- '<div id="blue"><h4><a class="blue" href="index.html">ABC123</a> -- <a class="blue" href="byyear.html">YEARS</a> -- <a class="blue" href="byteam.html">TEAMS</a></h4></div>'

footer <- vector()
footer[1] <- '<div id="foot"> <a class="foot" href="../index.html">AndyHulme.Net</a></div></body></html>'

##########################################
############# yearly pages ###############
##########################################

u.yr <- unique(seasons$key1)

for (i in 1:length(u.yr))
{ 
print(paste(i,u.yr[i]))

SPORT<- substring(u.yr[i],1,3)
YEAR <- substring(u.yr[i],5,8)

# create summary for given year
YR <- as.numeric(YEAR); 
if (i==1) {YR0<-"0000"; YR2<-substring(u.yr[i+1],5,8);
}  else if (i==length(u.yr)) {YR0<-substring(u.yr[i-1],5,8); YR2<-"0000";
}  else if (substring(u.yr[i],1,3)!=substring(u.yr[i-1],1,3)) {YR0<-"0000"; YR2<-substring(u.yr[i+1],5,8);
}  else if (substring(u.yr[i],1,3)!=substring(u.yr[i+1],1,3)) {YR0<-substring(u.yr[i-1],5,8); YR2<-"0000";
}  else {YR0<-substring(u.yr[i-1],5,8); YR2<-substring(u.yr[i+1],5,8);}

print(paste(YR,YR0,YR2))

headerx <- gsub("ABC123",SPORT,header)

seas.yr <- subset(seasons, year == YEAR)
seas.yr <- seas.yr[order(seas.yr$name1, seas.yr$name2, seas.yr$L33, -seas.yr$W33),]

cws <- subset(seas.yr, seas.yr$W11 + seas.yr$L11 > 0)
cws <- cws[order(-cws$W1, -cws$L1, -cws$W11-cws$W1, cws$L11+cws$L1),]

cws$row <- paste("<tr><td>",paste(
				paste("<a href='",teamlink(cws$team),".html'>",cws$team,"</a>",sep=""),
				paste(cws$W11, cws$L11,sep=" - "),
				ifelse(cws$W1+cws$L1>0,paste(cws$W1 , cws$L1 ,sep=" - "),""),
				paste(cws$W11+cws$W1, cws$L11+cws$L1,sep=" - "),
			sep="</td><td>"),"</td></tr>")

for (p in 1:nrow(seas.yr)) {
if (p==1) { seas.yr$flag[p] <- 2}
if (p >1) { seas.yr$flag[p] <- ifelse(seas.yr$name2[p] != seas.yr$name2[p-1]|seas.yr$name1[p] != seas.yr$name1[p-1]
							,ifelse(seas.yr$name1[p] != seas.yr$name1[p-1],2,1),0)}}

seas.yr$row <- paste("<tr><td>",paste(
				ifelse(seas.yr$flag>=1,paste("<b>",ifelse(seas.yr$name2!="",paste(seas.yr$name2),paste(seas.yr$name1)),"</b>"),""),
				paste("<a href='",teamlink(seas.yr$team),".html'>",seas.yr$team,"</a>",sep=""),
				paste(seas.yr$W33, seas.yr$L33,sep=" - "),
				ifelse(seas.yr$W22+seas.yr$L22>0,paste(seas.yr$W22, seas.yr$L22 ,sep=" - "),""),
			sep="</td><td>"),"</td></tr>")

play.yr <- subset(playoff, year == YEAR & sport == SPORT & (result=="W"|result=="T" & numb==1))
#play.yr$group <- ifelse(play.yr$name1=="Frozen Four"|play.yr$name1=="Final Four",1,0)
#play.yr$group <- ifelse(play.yr$sport=="MSO",-play.yr$round,play.yr$group)
#play.yr$group <- ifelse(play.yr$sport=="WSO",-play.yr$round,play.yr$group)
#play.yr$group <- ifelse(play.yr$sport=="LAX",-play.yr$round,play.yr$group)

play.yr <- play.yr[order(-play.yr$round, play.yr$name1, play.yr$name2),]

for (p in 1:nrow(play.yr)) {
if (p==1) { play.yr$flag[p] <- 2}
if (p >1) { play.yr$flag[p] <- ifelse(play.yr$name2[p] != play.yr$name2[p-1]|play.yr$name1[p] != play.yr$name1[p-1]
							,ifelse(play.yr$name1[p] != play.yr$name1[p-1],2,1),0)}}

play.yr$row <- paste(ifelse(play.yr$flag==99,"<tr><th colspan='5'></th></tr>",""),"<tr><td>",paste(
				ifelse(play.yr$flag>=1 & paste(play.yr$name1) != paste(play.yr$name2) & play.yr$name2!="",
						paste("<b>",play.yr$name2,"</b>",sep=""),
					ifelse(play.yr$name2=="",paste("Game",play.yr$game),"")),
				paste("<a href='",teamlink(play.yr$team1),".html'>",play.yr$team1,"</a>",sep=""),
				paste(play.yr$score1,play.yr$score2,sep=" - "),
				paste("<a href='",teamlink(play.yr$team2),".html'>",play.yr$team2,"</a>",sep=""),				
				play.yr$ot,
			sep="</td><td>"),"</td></tr>")

sink(paste("../",u.yr[i],".html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",YR,"</center></h2>"))
cat(paste("<center>",
	ifelse(YR0!="0000",paste("<a href='",YR0,".html'> << ",YR0,"</a>",sep=""),"")," | ",
	ifelse(YR2!="0000",paste("<a href='",YR2,".html'>",YR2," >> </a>",sep=""),""),
	"</center><br>",sep=""))

#cat("<!--Insert Bracket--!>")
#cat(paste("<center><h3><a href='bracket",paste(YR),".html'>Tournament Bracket</a><h3></center>",sep=""))

cat("<center><h3>College World Series</h3></center>")

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Team</th><th>Bracket</th><th>Final</th><th>Overall</th>","</tr>"))
write.table(cws$row, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

cat("<br><table width='80%' align='center'>")

u.rd  <- unique(subset(play.yr,name1=="WCWS"|name1=="Championship")$name1)

for (m in 1:length(u.rd)) {
cat(paste("<tr>","<th colspan='5' class='hl'>",u.rd[m],"</th>","</tr>"))
write.table(subset(play.yr,u.rd[m]==play.yr$name1)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
cat("</table><br>")


u.reg  <- unique(subset(play.yr,name1!="WCWS" & name1!="Championship")$name1)

for (k in 1:length(u.reg)) {
cat(paste("<hr><center><h3>",paste(u.reg[k]),"</h3></center>"))

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th> </th><th>Team</th><th>Regional</th><th>Super</th>","</tr>"))
write.table(subset(seas.yr,seas.yr$name1==u.reg[k])$row, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

cat("<br><table width='80%' align='center'>")

cat(paste("<tr><th></th><th>Winner</th><th>Score</th><th>Loser</th><th>Inn</th></tr>"))
write.table(subset(play.yr,play.yr$name1==u.reg[k])["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)

cat("</table><br>")
}

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# team pages #################
##########################################

u.tm <- unique(seasons$key2)

for (i in 1:length(u.tm))
{
print(paste(i,u.tm[i]))

SPORT<- substring(u.tm[i],1,3)
TEAM <- substring(u.tm[i],5,99)

# create summary for given year
headerx <- gsub("ABC123",SPORT,header)
summ.tm <- subset(seasons, seasons$team == TEAM & seasons$sport == SPORT)
summ.tm <- summ.tm[order(-summ.tm$year),]

summ.tm$class <- 0

summ.tm$row <- paste("<tr class='d",summ.tm$class,"'><td>",paste(
				paste("<a href='",summ.tm$year,".html'>",summ.tm$year,"</a>",sep=""),
				paste(summ.tm$W,"-",summ.tm$L,ifelse(summ.tm$T>=1,paste("-",summ.tm$T,sep=""),""),sep=""),
				paste(summ.tm$finish),
				ifelse(summ.tm$W33+summ.tm$L33>0,paste(summ.tm$W33,summ.tm$L33,sep="-"),""),
				ifelse(summ.tm$W22+summ.tm$L22>0,paste(summ.tm$W22,summ.tm$L22,sep="-"),""),
				ifelse(summ.tm$W11+summ.tm$L11>0,paste(summ.tm$W11,summ.tm$L11,sep="-"),""),
				ifelse(summ.tm$W1 +summ.tm$L1 >0,paste(summ.tm$W1 ,summ.tm$L1 ,sep="-"),""),
			sep="</td><td>"),"</td></tr>",sep="")

thth <- "<th>Year</th><th>W-L</th><th>Finish</th><th>Regional</th><th>Super</th><th>CWS</th><th>Final</th>"

play.tm <- subset(playoff, (team1 == TEAM) & sport == SPORT)
play.tm <- play.tm[order(-play.tm$year, -play.tm$round, play.tm$game),]

for (n in 1:nrow(play.tm)) {
if (n==1) { play.tm$flag[n] <- 2}
if (n >1) { play.tm$flag[n] <- ifelse(play.tm$year[n] != play.tm$year[n-1],1,0)}

if (n==1) { play.tm$flag2[n] <- 1}
if (n >1) { play.tm$flag2[n] <- ifelse(play.tm$name1[n] != play.tm$name1[n-1] |
							play.tm$name2[n] != play.tm$name2[n-1],1,0)}
}

play.tm$name2 <- ifelse(grepl("Quarter",play.tm$name1),"Quarterfinals",paste(play.tm$name2))
play.tm$name2 <- ifelse(grepl("First Round",play.tm$name1),"First Round",paste(play.tm$name2))

play.tm$row <- paste(ifelse(play.tm$flag==1,"<tr><th colspan='6'></th></tr>",""),"<tr><td>",paste(
				ifelse(play.tm$flag>=1,paste("<a href='",play.tm$year,".html'><b>",play.tm$year,"</b></a>",sep=""),""),
				ifelse(play.tm$flag2==1,
					paste("<b>",ifelse(play.tm$name2!="",paste(play.tm$name2),paste(play.tm$name1)),"</b>"),""),
				paste("<a href='",teamlink(play.tm$team2),".html'>",play.tm$team2,"</a>",sep=""),
				paste(play.tm$result),
				paste(play.tm$score),
				play.tm$ot,
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../",SPORT,"/",teamlink(TEAM),".html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",summ.tm$team[1],"</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat("<col width='8%'><col width='8%'><col width='20%'><col width='16%'><col width='16%'><col width='16%'><col width='16%'>")
cat(paste("<tr>",thth,"</tr>",sep=""))
write.table(summ.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

if (nrow(play.tm)>0) {
cat("<center><h3>Games</h3></center>")
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

u.sport <- unique(seasons$sport)

byyear <- as.data.frame(u.yr)
byyear$key1 <- byyear$u.yr
byyear$year <- substring(byyear$u.yr,5,8)
byyear$sport <- substring(byyear$u.yr,1,3)

first  <- subset(seasons, rank==1)[c("year","team","W1","L1")]; names(first)[names(first) == "team"] <- "first";
second <- subset(seasons, rank==2)[c("year","team")]; names(second)[names(second) == "team"] <- "second";
third  <- subset(seasons, rank==3)[c("year","team")]; names(third)[names(third) == "team"] <- "third";
fourth <- subset(seasons, rank==4)[c("year","team")]; names(fourth)[names(fourth) == "team"] <- "fourth";

final <- subset(playoff, name1=="Championship" & year<=2004 & result=="W")[c("year","team1","score1","team2","score2")]

byyear <- merge(byyear,final,by="year",all.x=TRUE)
byyear <- merge(byyear,first,by="year",all.x=TRUE)
byyear <- merge(byyear,second,by="year",all.x=TRUE)
byyear <- merge(byyear,third,by="year",all.x=TRUE)
byyear <- merge(byyear,fourth,by="year",all.x=TRUE)

teams.yr <- as.data.frame.matrix(table(seasons$year,rep("no.teams",length(seasons$year)) ))
teams.yr$year <- rownames(teams.yr)
byyear <- merge(byyear,teams.yr,by="year",all.x=TRUE)

#semis <- subset(playoff, round==4 & result=="W")[c("key1","team2","game")]
#semis <- semis[order(semis$key1, semis$game),]
#semis$team3 <- semis$team2; 
#semis$team4 <- semis$team2;
#semis.1 <- semis[seq(1, nrow(semis), 2),]
#semis.1 <- semis.1[c("key1","team3")]
#semis.2 <- semis[seq(2, nrow(semis), 2),]
#semis.2 <- semis.2[c("key1","team4")]

#byyear <- merge(byyear,semis.1,by="key1",all.x=TRUE)
#byyear <- merge(byyear,semis.2,by="key1",all.x=TRUE)
byyear$score <- ifelse(byyear$year>=2005,paste(byyear$W1,byyear$L1,sep="-"),paste(byyear$score1,byyear$score2,sep="-"))

#byyear$team1 <- ifelse(byyear$score=="0-0"|byyear$score=="1-1"|byyear$score=="2-2",
#			paste(byyear$team1,"<br>",byyear$team2,sep=""),paste(byyear$team1)) 
#byyear$team2 <- ifelse(byyear$score=="0-0"|byyear$score=="1-1"|byyear$score=="2-2","",paste(byyear$team2)) 

for (i in 1:length(u.sport))
{

byyearx <- subset(byyear, sport==u.sport[i])
byyearx <- byyearx[order(-as.numeric(byyearx$year)),]
byyearx$class <- (1+(1:nrow(byyearx))) %% 2

byyearx$row <- paste("<tr class='d",byyearx$class,"'><td>",paste(
	paste("<a href='",byyearx$year,".html'>",byyearx$year ,"</a>",sep=""),
	byyearx$no.teams,
	paste("<a href='",teamlink(byyearx$first),".html'>", byyearx$first,"</a>",sep=""),
	ifelse(is.na(byyearx$score),"",byyearx$score),
	paste("<a href='",teamlink(byyearx$second),".html'>", byyearx$second,"</a>",sep=""),
	paste("<a href='",teamlink(byyearx$third),".html'>", byyearx$third,"</a>",sep=""),
	paste("<a href='",teamlink(byyearx$fourth),".html'>", byyearx$fourth,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../",u.sport[i],"/byyear.html",sep=""))

headerx <- gsub("ABC123",SPORT,header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>NCAA Tournament</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Season</th><th>Teams</th><th>Champion</th><th></th><th>Runner-Up</th><th colspan='2'>Final Four</th></tr>",sep=""))
write.table(byyearx["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# by team page ###############
##########################################

u.sport <- unique(seasons$sport)

seasons$count <- 1
byteam <- as.data.frame(u.tm)
byteam$key2 <- byteam$u.tm
byteam$team <- substring(byteam$u.tm,5,99)
byteam$sport <- substring(byteam$u.tm,1,3)

seas <- aggregate(seasons$count, list(seasons$key2), sum); names(seas)[names(seas)=="x"] <- "Years"

byteam <- merge(byteam,seas,by.x="key2",by.y="Group.1")

rounds$resultf <- paste(rounds$result,rounds$round,sep="")
summ <- as.data.frame.matrix(table(rounds$key2,rounds$result))
summ$key2 <- rownames(summ)
summf <- as.data.frame.matrix(table(rounds$key2,rounds$resultf))
summf$key2 <- rownames(summ)

byteam <- merge(byteam,summ,by="key2",all.x=TRUE)
byteam <- merge(byteam,summf,by="key2",all.x=TRUE)
byteam$Pct <- byteam$W/(byteam$W+byteam$L)

firstapp <- aggregate(rounds$year , list(rounds$key2), min); names(firstapp)[names(firstapp)=="x"] <- "firstapp";
last.app <- aggregate(rounds$year , list(rounds$key2), max); names(last.app)[names(last.app)=="x"] <- "last.app";

byteam <- merge(byteam,firstapp,by.x="key2",by.y="Group.1",all.x=TRUE)
byteam <- merge(byteam,last.app,by.x="key2",by.y="Group.1",all.x=TRUE)

byyear$count <- 1
byyear$key_1 <- paste(byyear$sport,byyear$first,sep="/")
byyear$key_2 <- paste(byyear$sport,byyear$second,sep="/")

champ <- aggregate(byyear$count, list(byyear$key_1), sum); names(champ)[names(champ)=="x"] <- "Champs"
secnd <- aggregate(byyear$count, list(byyear$key_2), sum); names(secnd)[names(secnd)=="x"] <- "Seconds"
byteam <- merge(byteam,champ,by.x="key2",by.y="Group.1",all.x=T)
byteam <- merge(byteam,secnd,by.x="key2",by.y="Group.1",all.x=T)

#split <- subset(playoff, result=="T" & round==1)
#split$count <- 1
#split$key_1 <- paste(split$sport,split$team1,sep="/")
#split <- aggregate(split$count, list(split$key_1), sum); names(split)[names(split)=="x"] <- "Splits"
#byteam <- merge(byteam,split,by.x="key2",by.y="Group.1",all=T)

#byteam$Champs <- ifelse(is.na(byteam$Splits)==T,byteam$Champs,
#					ifelse(is.na(byteam$Champs)==T,byteam$Splits,byteam$Champs+byteam$Splits))

for (i in 1:length(u.sport))
{

byteamx <- subset(byteam, sport==u.sport[i])
byteamx$class <- (1+(1:nrow(byteamx))) %% 2
byteamx$row <- paste("<tr class='d",byteamx$class,"'><td>",paste(
				paste("<a href='",teamlink(byteamx$team),".html'>",byteamx$team,"</a>",sep=""),
				byteamx$Years,
				paste(byteamx$W,"-",byteamx$L,ifelse(byteamx$T>=1,paste("-",byteamx$T,sep=""),""),sep=""),
				ifelse(is.na(byteamx$Champs)==T,0,byteamx$Champs),
				ifelse(is.na(byteamx$Seconds)==T,0,byteamx$Seconds),
				byteamx$firstapp,
				byteamx$last.app,
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../",u.sport[i],"/byteam.html",sep=""))

headerx <- gsub("ABC123",u.sport[i],header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>NCAA Softball Tournament</center></h2>"))

cat("<br><table width='80%' align='center'>")
#cat("<col width='20%'><col width='6%'><col width='7%'><col width='7%'><col width='4%'><col width='4%'><col width='4%'><col width='8%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='4%'><col width='4%'><col width='4%'><col width='4%'>")
cat("<tr><th></th><th>Years</th><th>Record</th><th>1ST</th><th>2ND</th><th>Debut</th><th>Last</th></tr>")
write.table(byteamx["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

###########################
#sortable NCAAT records ###
###########################

byteam$team <- paste(byteam$team)

for (i in 1:length(u.sport))
{

byteamy <- subset(byteam, sport==u.sport[i])

for (ll in 1:8) {
sink(paste("../",u.sport[i],"/byteam",ll,".html",sep=""))

if (ll==1) { byteamy <- byteamy[order(byteamy$team), ] }
if (ll==2) { byteamy <- byteamy[order(-byteamy$Years, -byteamy$Pct, -byteamy$W+byteamy$L, -byteamy$W, byteamy$team), ] }
if (ll==3) { byteamy <- byteamy[order(-byteamy$W-byteamy$L, -byteamy$Pct, -byteamy$W+byteamy$L, -byteamy$W, byteamy$team), ] }
if (ll==4) { byteamy <- byteamy[order(-byteamy$W, -byteamy$Pct, -byteamy$W+byteamy$L, -byteamy$W, byteamy$team), ] }
if (ll==5) { byteamy <- byteamy[order(-byteamy$Pct, -byteamy$W+byteamy$L, -byteamy$W, byteamy$team), ] }
if (ll==6) { byteamy <- byteamy[order(-byteamy$Champs, -byteamy$Years, -byteamy$Pct, -byteamy$W+byteamy$L, -byteamy$W, byteamy$team), ] }
if (ll==7) { byteamy <- byteamy[order(-byteamy$Seconds, -byteamy$Years, -byteamy$Pct, -byteamy$W+byteamy$L, -byteamy$W, byteamy$team), ] }
if (ll==8) { byteamy <- byteamy[order(byteamy$firstapp, byteamy$last.app), ] }

byteamy$class2 <- (1+(1:nrow(byteamy))) %% 2
byteamy$row3   <- paste("<tr class='d",byteamy$class2,"'><td>",paste(
				paste("<a href='",teamlink(byteamy$team),".html'>",byteamy$team,"</a>",sep=""),
				byteamy$Years,
				byteamy$W + byteamy$L,
				byteamy$W,
				byteamy$L,
				sprintf("%.3f", round(byteamy$Pct,3) ),
				ifelse(is.na(byteamy$Champs)==T,"",byteamy$Champs),
				ifelse(is.na(byteamy$Seconds)==T,"",byteamy$Seconds),
				paste("<a href='",byteamy$firstapp,".html'>",byteamy$firstapp,"</a>",sep=""),
				paste("<a href='",byteamy$last.app,".html'>",byteamy$last.app,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

headerx <- gsub("ABC123",u.sport[i],header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>NCAA Tournament Records</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr>", "<th",ifelse(ll==1," class='hl3'",""),"><a href='byteam1.html'>Team</a></th>",
			"<th",ifelse(ll==2," class='hl3'",""),"><a href='byteam2.html'>Apps.</a></th>",
			"<th",ifelse(ll==3," class='hl3'",""),"><a href='byteam3.html'>GP</a></th>",
			"<th",ifelse(ll==4," class='hl3'",""),"><a href='byteam4.html'>W</a></th><th>L</th>",
			"<th",ifelse(ll==5," class='hl3'",""),"><a href='byteam5.html'>PCT</a></th>",
			"<th",ifelse(ll==6," class='hl3'",""),"><a href='byteam6.html'>1ST</a></th>",
			"<th",ifelse(ll==7," class='hl3'",""),"><a href='byteam7.html'>2ND</a></th>",
			"<th",ifelse(ll==8," class='hl3'",""),"><a href='byteam8.html'>Debut</a></th><th>Last</th>","</tr>",sep=""))
write.table(byteamy["row3"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}}

##########################################
############# index page #################
##########################################

for (i in 1:length(u.sport))
{

byyear2 <- subset(byyear, sport==u.sport[i])
byyear2 <- byyear2[order(-1 * as.numeric(byyear2$year)),]
byyear2 <- byyear2[1:10,]

byteam2 <- subset(byteam, sport==u.sport[i])
byteam2 <- byteam2[order(-byteam2$Years, -byteam2$Champs, -byteam2$W, byteam2$L),]
byteam2 <- byteam2[1:10,]

byyear2$row <- paste("<tr><td>",paste(
	paste("<a href='",byyear2$year,".html'>",byyear2$year ,"</a>",sep=""),
	paste("<a href='",teamlink(byyear2$first),".html'>", byyear2$first,"</a>",sep=""),
	byyear2$score,
	paste("<a href='",teamlink(byyear2$second),".html'>", byyear2$second,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

byteam2$row <- paste("<tr><td>",paste(
				paste("<a href='",teamlink(byteam2$team),".html'>",byteam2$team,"</a>",sep=""),
				byteam2$Years,
				ifelse(is.na(byteam2$Champs)==T,"",byteam2$Champs),
				paste(byteam2$W,byteam2$L,sep="-"),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../",u.sport[i],"/index.html",sep=""))

headerx <- gsub("ABC123",u.sport[i],header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th colspan='4' class='hl'><div style='font-size:1.5em;'>Results by Year</div></th>","</tr>"))
cat(paste("<tr>","<th>Year</th><th>Champion</th><th></th><th>Runner-up</th>","</tr>"))
write.table(byyear2["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr>","<td colspan='4'><a href='byyear.html'>All Years</a></td>","</tr>"))
cat("</table><br>")

cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th colspan='4' class='hl'><div style='font-size:1.5em;'>Results by Team</div></th>","</tr>"))
cat(paste("<tr>","<th>Team</th><th>App.</th><th>Champs</th><th>W-L</th>","</tr>"))
write.table(byteam2["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr>","<td colspan='4'><a href='byteam.html'>All Teams</a></td>","</tr>"))
cat("</table><br>")

cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th class='hl'><div style='font-size:1.5em;'>Event Results</div></th>","</tr>"))
	cat(paste("<tr>","<td><a href='champgm.html'>Championship Games</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='byteam1.html'>Sortable Tournament Records</a></td>","</tr>"))
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# event pages ################
##########################################

champs	<- subset(playoff, name1 == "Championship" & result=="W")

##################################################################################################
singlepage <- function(gamesx,sportx,htmlx,titlex,thx) {

gamesx <- gamesx[order(-gamesx$year, -gamesx$round, gamesx$name1),]

for (n in 1:nrow(gamesx)) {
if (n==1) { gamesx$flag[n] <- 2}
if (n >1) { gamesx$flag[n] <- ifelse(gamesx$year[n] != gamesx$year[n-1],1,0)}}
gamesx$class <- (1+(1:nrow(gamesx))) %% 2

gamesx$row <- paste(ifelse(gamesx$flag==1,"<tr><th colspan='6'></th></tr>",""),"<tr><td>",paste(
				ifelse(gamesx$flag>=1,paste("<a href='",gamesx$year,".html'><b>",gamesx$year,"</b></a>",sep=""),""),
			paste(ifelse(is.na(gamesx$seed1),"",paste("<h5>",gamesx$seed1,"</h5> ",sep="")),
				"<a href='",teamlink(gamesx$team1),".html'>",gamesx$team1,"</a>",sep=""),
			paste(gamesx$score1,gamesx$score2,sep=" - "),
			paste(ifelse(is.na(gamesx$seed2),"",paste("<h5>",gamesx$seed2,"</h5> ",sep="")),
				"<a href='",teamlink(gamesx$team2),".html'>",gamesx$team2,"</a>",sep=""),
			paste(gamesx$ot),
			paste(gamesx$loc),
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

singlepage(champs,"SOF","champgm.html","Championship Games","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")


