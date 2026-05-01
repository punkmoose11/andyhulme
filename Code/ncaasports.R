
############# import data
playoff <- read.csv("../Data/ncaagames.csv")
playoff <- subset(playoff, sport=="HKY"|sport=="VOL"|sport=="WBB"|sport=="MSO"|sport=="WSO"|sport=="LAX")

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

teamlink <- function(x){teamx = tolower(gsub("&","_",gsub(".","",gsub("'","",gsub(" ", "",x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE))}

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
#process hockey series
series.1 <- subset(playoff, (round==10 | round==20) & name2=="Game 1")
series.2 <- subset(playoff, (round==10 | round==20) & name2=="Game 2")
series.3 <- subset(playoff, (round==10 | round==20) & name2=="Game 3")

series <- merge(series.1,series.2,by=c("sport","year","name1","team1","round"),all.x=T)
series <- merge(series  ,series.3,by=c("sport","year","name1","team1","round"),all.x=T)

series$scorec <- paste(series$score.x,series$score.y,ifelse(is.na(series$score)==F,series$score,""))
series$resultc<- paste(series$result.x,series$result.y,ifelse(is.na(series$result)==F,series$result,""),sep="")
series$wl     <- ifelse(series$resultc=="WW","[2-0]",
			ifelse(series$resultc=="WLW"|series$resultc=="LWW","[2-1]",
			ifelse(series$resultc=="WLL"|series$resultc=="LWL","[1-2]",
			ifelse(series$resultc=="LL","[0-2]",""))))
series$agg    <- paste("<i>",as.numeric(series$score1.x)+as.numeric(series$score1.y),"-",
					as.numeric(series$score2.x)+as.numeric(series$score2.y),"</i>",sep="")

series$rr <- ifelse(series$year<=1988,paste(series$agg),paste(series$wl))
series <- series[c("sport","year","name1","team1","round","rr")]
series$key <- paste(series$sport,series$year,series$team1,sep="/")
series$ser8  <- ifelse(series$round==10,series$rr,"")
series$ser16 <- ifelse(series$round==20,series$rr,"")
###########################################

rounds <- playoff[c("sport","year","round","team1","score","result")]

rounds$key <- paste(rounds$sport,rounds$year,rounds$team1,sep="/")
rounds$key2<- paste(rounds$sport,rounds$team1,sep="/")
summ <- as.data.frame.matrix(table(rounds$key,rounds$result))
summ$key <- rownames(summ)

seasons <- merge(seasons,summ,by="key",all.x=TRUE)

rounds$final <- ifelse(rounds$round==1 ,rounds$score,"")
rounds$third <- ifelse(rounds$round==3 ,rounds$result,"")
rounds$semif <- ifelse(rounds$round==4 ,rounds$score,"")
rounds$elite <- ifelse(rounds$round==8 ,rounds$score,"")
rounds$sweet <- ifelse(rounds$round==16,rounds$score,"")
rounds$rd.32 <- ifelse(rounds$round==32,rounds$score,"")
rounds$rd.64 <- ifelse(rounds$round==64,rounds$score,"")
rounds$pig   <- ifelse(rounds$round==128,rounds$score,"")

#####

seasons <- merge(seasons,subset(rounds,final!="")[c("key","final","result")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,third!="")[c("key","third")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,semif!="")[c("key","semif")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,elite!="")[c("key","elite")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,sweet!="")[c("key","sweet")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,rd.32!="")[c("key","rd.32")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,rd.64!="")[c("key","rd.64")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,pig  !="")[c("key","pig"  )],by=c("key"),all.x=TRUE)

# for hockey ###
seasons <- merge(seasons,subset(series,ser8 !="")[c("key","ser8" )],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(series,ser16!="")[c("key","ser16")],by=c("key"),all.x=TRUE)
seasons$elite <- ifelse(is.na(seasons$ser8 )==F,seasons$ser8 ,seasons$elite)
seasons$sweet <- ifelse(is.na(seasons$ser16)==F,seasons$ser16,seasons$sweet)
################

seasons$finish <- ifelse(is.na(seasons$final)==F & (seasons$result=="W"|seasons$result=="T"),"<b>CHAMPION</b>",
			ifelse(is.na(seasons$final)==F & seasons$result=="L","Runner-up",
			ifelse(is.na(seasons$third)==F & seasons$third=="W","Third Place",
			ifelse(is.na(seasons$third)==F & seasons$third=="L","Fourth Place",
			ifelse(is.na(seasons$semif)==F,"Final Four",
			ifelse(is.na(seasons$elite)==F,"Last 8",
			ifelse(is.na(seasons$sweet)==F,"Last 16",
			ifelse(is.na(seasons$rd.32)==F,"Last 32",
			ifelse(is.na(seasons$rd.64)==F,"Last 64",
			ifelse(is.na(seasons$pig  )==F,"Play-in",""))))))))))

seasons$key1 <- paste(seasons$sport,seasons$year,sep="/")
seasons$key2 <- paste(seasons$sport,seasons$team,sep="/")
playoff$key1 <- paste(playoff$sport,playoff$year,sep="/")

##########################################
############# generate header / footer ###
##########################################

header <- vector()
header[1] <- '<html><head><title>AndyHulme.Net</title>'
header[2] <- '<link rel="stylesheet" type="text/css" href="styles.css"></head>' 
#header[3] <- '<body><div id="red"><h1><a class="red" href="http://omahaseries.com">OmahaSeries.com</a></h1></div><div id="yellow"></div>'
#header[4] <- '<div id="blue"><h4><a class="blue" href="index.html">ABC123</a> -- <a class="blue" href="byyear.html">YEARS</a> -- <a class="blue" href="byteam.html">TEAMS</a></h4></div>'
header[3] <- '<body><div id="head"><table class="head"><tr class="head">
<td class="head" style="text-align:left; width:75%;"><!--Insert Link--> &nbsp;</td>
<td class="head" style="text-align:right; width:25%;"><!--Insert Right--> &nbsp;</td></tr></table></div>'

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

if (SPORT=="HKY") {sporttext<-"Hockey"
} else if (SPORT=="LAX") {sporttext<-"LaCrosse"; 
} else if (SPORT=="MSO") {sporttext<-"Soccer Men"
} else if (SPORT=="WSO") {sporttext<-"Soccer Women"
} else if (SPORT=="WBB") {sporttext<-"Basketball Women"
} else if (SPORT=="VOL") {sporttext<-"Volleyball"
}

# create summary for given year
YR <- as.numeric(YEAR); 
if (i==1) {YR0<-"0000"; YR2<-substring(u.yr[i+1],5,8);
}  else if (i==length(u.yr)) {YR0<-substring(u.yr[i-1],5,8); YR2<-"0000";
}  else if (substring(u.yr[i],1,3)!=substring(u.yr[i-1],1,3)) {YR0<-"0000"; YR2<-substring(u.yr[i+1],5,8);
}  else if (substring(u.yr[i],1,3)!=substring(u.yr[i+1],1,3)) {YR0<-substring(u.yr[i-1],5,8); YR2<-"0000";
}  else {YR0<-substring(u.yr[i-1],5,8); YR2<-substring(u.yr[i+1],5,8);}

print(paste(YR,YR0,YR2))

play.yr <- subset(playoff, year == YEAR & sport == SPORT & (result=="W"|result=="T" & numb==1))
play.yr$group <- ifelse(play.yr$name1=="Frozen Four"|play.yr$name1=="Final Four",1,0)
play.yr$group <- ifelse(play.yr$sport=="MSO",-play.yr$round,play.yr$group)
play.yr$group <- ifelse(play.yr$sport=="WSO",-play.yr$round,play.yr$group)
play.yr$group <- ifelse(play.yr$sport=="LAX",-play.yr$round,play.yr$group)

play.yr <- play.yr[order(play.yr$group, play.yr$name1, -play.yr$round, play.yr$name2, play.yr$game),]

for (p in 1:nrow(play.yr)) {
if (p==1) { play.yr$flag[p] <- 2}
if (p >1) { play.yr$flag[p] <- ifelse(play.yr$name2[p] != play.yr$name2[p-1]|play.yr$name1[p] != play.yr$name1[p-1]
							,ifelse(play.yr$name1[p] != play.yr$name1[p-1],2,1),0)}}

play.yr$row <- paste(ifelse(play.yr$flag==1,"<tr><th colspan='5'></th></tr>",""),"<tr><td>",paste(
				ifelse(play.yr$flag>=1 & paste(play.yr$name1) != paste(play.yr$name2),paste("<b>",play.yr$name2,"</b>",sep=""),""),
				paste("<a href='",teamlink(play.yr$team1),".html'>",play.yr$team1,"</a>",sep=""),
				paste(play.yr$score1,play.yr$score2,sep=" - "),
				paste("<a href='",teamlink(play.yr$team2),".html'>",play.yr$team2,"</a>",sep=""),				
				play.yr$ot,
			sep="</td><td>"),"</td></tr>")

sink(paste("../",u.yr[i],".html",sep=""))

headerx <- gsub("ABC123",SPORT,header)
headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>",sporttext,"</a> > ",YR," (","<a href='bracket",YR,".html'>Bracket</a>",")",sep=""),headerx)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",YR,"</center></h2>"))
cat(paste("<center>",
	ifelse(YR0!="0000",paste("<a href='",YR0,".html'> << ",YR0,"</a>",sep=""),"")," | ",
	ifelse(YR2!="0000",paste("<a href='",YR2,".html'>",YR2," >> </a>",sep=""),""),
	"</center><br>",sep=""))

cat("<!--Insert Bracket--!>")
cat(paste("<center><h3><a href='bracket",paste(YR),".html'>Tournament Bracket</a><h3></center>",sep=""))

#cat("<center><h3>Games</h3></center>")
cat("<br><table width='80%' align='center'>")

u.rd  <- unique(play.yr$name1)

for (m in 1:length(u.rd)) {
cat(paste("<tr>","<th colspan='5' class='hl'>",u.rd[m],"</th>","</tr>"))
write.table(subset(play.yr,u.rd[m]==play.yr$name1)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
cat("</table><br>")

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

final <- subset(playoff, round==1 & (result=="W"|(sport=="MSO" & result=="T" & (team1=="Michigan St."|team1=="Virginia")))
		)[c("key1","team1","score1","team2","score2")]
byyear <- merge(byyear,final,by="key1",all.x=TRUE)

teams.yr <- as.data.frame.matrix(table(seasons$key1,rep("no.teams",length(seasons$key1)) ))
teams.yr$key1 <- rownames(teams.yr)
byyear <- merge(byyear,teams.yr,by="key1",all.x=TRUE)

semis <- subset(playoff, round==4 & result=="W")[c("key1","team2","game")]
semis <- semis[order(semis$key1, semis$game),]
semis$team3 <- semis$team2; 
semis$team4 <- semis$team2;
semis.1 <- semis[seq(1, nrow(semis), 2),]
semis.1 <- semis.1[c("key1","team3")]
semis.2 <- semis[seq(2, nrow(semis), 2),]
semis.2 <- semis.2[c("key1","team4")]

byyear <- merge(byyear,semis.1,by="key1",all.x=TRUE)
byyear <- merge(byyear,semis.2,by="key1",all.x=TRUE)
byyear$score <- paste(byyear$score1,byyear$score2,sep="-")

byyear$team1 <- ifelse(byyear$score=="0-0"|byyear$score=="1-1"|byyear$score=="2-2",
			paste(byyear$team1,"<br>",byyear$team2,sep=""),paste(byyear$team1)) 
byyear$team2 <- ifelse(byyear$score=="0-0"|byyear$score=="1-1"|byyear$score=="2-2","",paste(byyear$team2)) 

for (i in 1:length(u.sport))
{

byyearx <- subset(byyear, sport==u.sport[i])
byyearx <- byyearx[order(-as.numeric(byyearx$year)),]
byyearx$class <- (1+(1:nrow(byyearx))) %% 2

if (u.sport[i]=="HKY") {sporttext<-"Hockey"; champtext<-"NCAA Hockey Tournament";
} else if (u.sport[i]=="LAX") {sporttext<-"LaCrosse"; champtext<-"NCAA LaCrosse Tournament";
} else if (u.sport[i]=="MSO") {sporttext<-"Soccer Men"; champtext<-"NCAA Men's Soccer Tournament";
} else if (u.sport[i]=="WSO") {sporttext<-"Soccer Women"; ; champtext<-"NCAA Women's Soccer Tournament";
} else if (u.sport[i]=="WBB") {sporttext<-"Basketball Women"; champtext<-"NCAA Women's Basketball Tournament";
} else if (u.sport[i]=="VOL") {sporttext<-"Volleyball"; champtext<-"NCAA Volleyball Tournament";
}

byyearx$row <- paste("<tr class='d",byyearx$class,"'><td>",paste(
	paste("<a href='bracket",byyearx$year,".html'>",byyearx$year ,"</a>",sep=""),
	byyearx$no.teams,
	ifelse(byyearx$team2=="",paste(byyearx$team1),
		paste("<a href='",teamlink(byyearx$team1),".html'>", byyearx$team1,"</a>",sep="")),
	ifelse(is.na(byyearx$score),"",byyearx$score),
	paste("<a href='",teamlink(byyearx$team2),".html'>", byyearx$team2,"</a>",sep=""),
	paste("<a href='",teamlink(byyearx$team3),".html'>", byyearx$team3,"</a>",sep=""),
	paste("<a href='",teamlink(byyearx$team4),".html'>", byyearx$team4,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../",u.sport[i],"/byyear.html",sep=""))

headerx <- gsub("ABC123",SPORT,header)
headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>",sporttext,"</a> > ","Years",sep=""),headerx)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",champtext,"</center></h2>"))

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
u.tm <- unique(seasons$key2)

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
byyear$key_1 <- paste(byyear$sport,byyear$team1,sep="/")
byyear$key_2 <- paste(byyear$sport,byyear$team2,sep="/")

champ <- aggregate(byyear$count, list(byyear$key_1), sum); names(champ)[names(champ)=="x"] <- "Champs"
secnd <- aggregate(byyear$count, list(byyear$key_2), sum); names(secnd)[names(secnd)=="x"] <- "Seconds"
byteam <- merge(byteam,champ,by.x="key2",by.y="Group.1",all.x=T)
byteam <- merge(byteam,secnd,by.x="key2",by.y="Group.1",all.x=T)

split <- subset(playoff, result=="T" & round==1)
split$count <- 1
split$key_1 <- paste(split$sport,split$team1,sep="/")
split <- aggregate(split$count, list(split$key_1), sum); names(split)[names(split)=="x"] <- "Splits"
byteam <- merge(byteam,split,by.x="key2",by.y="Group.1",all=T)

byteam$Champs <- ifelse(is.na(byteam$Splits)==T,byteam$Champs,
					ifelse(is.na(byteam$Champs)==T,byteam$Splits,byteam$Champs+byteam$Splits))

for (i in 1:length(u.sport))
{

if (u.sport[i]=="HKY") {sporttext<-"Hockey"; champtext<-"NCAA Hockey Tournament";
} else if (u.sport[i]=="LAX") {sporttext<-"LaCrosse"; champtext<-"NCAA LaCrosse Tournament";
} else if (u.sport[i]=="MSO") {sporttext<-"Soccer Men"; champtext<-"NCAA Men's Soccer Tournament";
} else if (u.sport[i]=="WSO") {sporttext<-"Soccer Women"; ; champtext<-"NCAA Women's Soccer Tournament";
} else if (u.sport[i]=="WBB") {sporttext<-"Basketball Women"; champtext<-"NCAA Women's Basketball Tournament";
} else if (u.sport[i]=="VOL") {sporttext<-"Volleyball"; champtext<-"NCAA Volleyball Tournament";
}

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

byteamx$row2 <- paste("<tr class='d4'><td>",paste(
				"Overall",
				ifelse(byteamx$W+byteamx$L+byteamx$T>0,paste(byteamx$W,"-",byteamx$L, ifelse(byteamx$T>0,paste("-",byteamx$T,sep=""),"") ,sep=""),""),
				paste(byteamx$Years,"Apps."),
				ifelse(byteamx$W64 +byteamx$L64 >0,paste(byteamx$W64 ,byteamx$L64 ,sep="-"),""),
				ifelse(byteamx$W32 +byteamx$L32 >0,paste(byteamx$W32 ,byteamx$L32 ,sep="-"),""),
				ifelse(byteamx$W16+byteamx$L16+byteamx$W20+byteamx$L20+byteamx$T20>0,paste(byteamx$W16+byteamx$W20,"-",byteamx$L16+byteamx$L20, ifelse(byteamx$T20>0,paste("-",byteamx$T20,sep=""),"") ,sep=""),""),
				ifelse(byteamx$W8 +byteamx$L8 +byteamx$W10+byteamx$L10+byteamx$T10>0,paste(byteamx$W8 +byteamx$W10,"-",byteamx$L8 +byteamx$L10, ifelse(byteamx$T10>0,paste("-",byteamx$T10,sep=""),"") ,sep=""),""),
				ifelse(byteamx$W4  +byteamx$L4  >0,paste(byteamx$W4  ,byteamx$L4  ,sep="-"),""),
				ifelse(byteamx$W1  +byteamx$L1  >0,paste(byteamx$W1  ,"-" ,byteamx$L1  ,ifelse(byteamx$T1>0,paste("-",byteamx$T1,sep=""),"") ,sep=""),""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../",u.sport[i],"/byteam.html",sep=""))

headerx <- gsub("ABC123",u.sport[i],header)
headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>",sporttext,"</a> > ","Teams",sep=""),headerx)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",champtext,"</center></h2>"))

cat("<br><table width='80%' align='center'>")
#cat("<col width='20%'><col width='6%'><col width='7%'><col width='7%'><col width='4%'><col width='4%'><col width='4%'><col width='8%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='4%'><col width='4%'><col width='4%'><col width='4%'>")
cat("<tr><th></th><th>Years</th><th>Record</th><th>1ST</th><th>2ND</th><th>Debut</th><th>Last</th></tr>")
write.table(byteamx["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

assign(paste("byteamx",i,sep="_"), byteamx)
}

byteamx <- rbind(byteamx_1, byteamx_2, byteamx_3, byteamx_4, byteamx_5, byteamx_6)

##########################################
############# team pages #################
##########################################

u.tm <- unique(seasons$key2)

for (i in 1:length(u.tm))
{
print(paste(i,u.tm[i]))

SPORT<- substring(u.tm[i],1,3)
TEAM <- substring(u.tm[i],5,99)

if (SPORT=="HKY") {sporttext<-"Hockey"
} else if (SPORT=="LAX") {sporttext<-"LaCrosse"; 
} else if (SPORT=="MSO") {sporttext<-"Soccer Men"
} else if (SPORT=="WSO") {sporttext<-"Soccer Women"
} else if (SPORT=="WBB") {sporttext<-"Basketball Women"
} else if (SPORT=="VOL") {sporttext<-"Volleyball"
}

# create summary for given year
headerx <- gsub("ABC123",SPORT,header)
headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>",sporttext,"</a> > ",TEAM,sep=""),headerx)
headerx <- gsub("<!--Insert Right-->",paste("<a href='byteam1.html'>Teams</a> ",sep=""),headerx)

summ.tm <- subset(seasons, seasons$team == TEAM & seasons$sport == SPORT)
summ.tm <- summ.tm[order(-summ.tm$year),]

summ.tm$class <- 0

summ.tm$row <- paste("<tr class='d",summ.tm$class,"'><td>",paste(
				paste("<a href='bracket",summ.tm$year,".html'>",summ.tm$year,"</a>",sep=""),
				paste(summ.tm$W,"-",summ.tm$L,ifelse(summ.tm$T>=1,paste("-",summ.tm$T,sep=""),""),sep=""),
				paste(summ.tm$finish),
				ifelse(is.na(summ.tm$rd.64)==FALSE,summ.tm$rd.64,""),
				ifelse(is.na(summ.tm$rd.32)==FALSE,summ.tm$rd.32,""),
				ifelse(is.na(summ.tm$sweet)==FALSE,summ.tm$sweet,""),
				ifelse(is.na(summ.tm$elite)==FALSE,summ.tm$elite,""),
				ifelse(is.na(summ.tm$semif)==FALSE,summ.tm$semif,""),
				ifelse(is.na(summ.tm$final)==FALSE,summ.tm$final,""),
			sep="</td><td>"),"</td></tr>",sep="")

thth <- "<th>Year</th><th>W-L</th><th>Finish</th><th>R64</th><th>R32</th><th>S16</th><th>E8</th><th>F4</th><th>NC</th>"

play.tm <- subset(playoff, (team1 == TEAM) & sport == SPORT)
play.tm <- play.tm[order(-play.tm$year, -play.tm$round),]

for (n in 1:nrow(play.tm)) {
if (n==1) { play.tm$flag[n] <- 2}
if (n >1) { play.tm$flag[n] <- ifelse(play.tm$year[n] != play.tm$year[n-1],1,0)}}

play.tm$name2 <- ifelse(grepl("Quarter",play.tm$name1),"Quarterfinals",paste(play.tm$name2))
play.tm$name2 <- ifelse(grepl("First Round",play.tm$name1),"First Round",paste(play.tm$name2))

play.tm$row <- paste(ifelse(play.tm$flag==1,"<tr><th colspan='6'></th></tr>",""),"<tr><td>",paste(
				ifelse(play.tm$flag>=1,paste("<a href='bracket",play.tm$year,".html'><b>",play.tm$year,"</b></a>",sep=""),""),
				paste(play.tm$name2),
				paste("<a href='",teamlink(play.tm$team2),".html'>",play.tm$team2,"</a>",sep=""),
				paste(play.tm$result),
				paste(play.tm$score),
				play.tm$ot,
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../",SPORT,"/",teamlink(TEAM),".html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",summ.tm$team[1],"</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat("<col width='8%'><col width='8%'><col width='18%'><col width='11%'><col width='11%'><col width='11%'><col width='11%'><col width='11%'><col width='11%'>")
cat(paste("<tr>",thth,"</tr>",sep=""))
write.table(subset(byteamx,team==TEAM & sport==SPORT)["row2"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
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

###########################
#sortable NCAAT records ###
###########################

byteam$team <- paste(byteam$team)

for (i in 1:length(u.sport))
{

byteamy <- subset(byteamx, sport==u.sport[i])

if (u.sport[i]=="HKY") {sporttext<-"Hockey"; champtext<-"NCAA Hockey Tournament";
} else if (u.sport[i]=="LAX") {sporttext<-"LaCrosse"; champtext<-"NCAA LaCrosse Tournament";
} else if (u.sport[i]=="MSO") {sporttext<-"Soccer Men"; champtext<-"NCAA Men's Soccer Tournament";
} else if (u.sport[i]=="WSO") {sporttext<-"Soccer Women"; ; champtext<-"NCAA Women's Soccer Tournament";
} else if (u.sport[i]=="WBB") {sporttext<-"Basketball Women"; champtext<-"NCAA Women's Basketball Tournament";
} else if (u.sport[i]=="VOL") {sporttext<-"Volleyball"; champtext<-"NCAA Volleyball Tournament";
}

for (ll in 1:8) {
sink(paste("../",u.sport[i],"/byteam",ll,".html",sep=""))

if (ll==1) { byteamy <- byteamy[order(byteamy$team), ] }
if (ll==2) { byteamy <- byteamy[order(-byteamy$Years, -byteamy$Pct, -byteamy$W+byteamy$L, -byteamy$W, byteamy$team), ] }
if (ll==3) { byteamy <- byteamy[order(-byteamy$W-byteamy$L-byteamy$T, -byteamy$Pct, -byteamy$W+byteamy$L, -byteamy$W, byteamy$team), ] }
if (ll==4) { byteamy <- byteamy[order(-byteamy$W, -byteamy$Pct, -byteamy$W+byteamy$L, -byteamy$W, byteamy$team), ] }
if (ll==5) { byteamy <- byteamy[order(-byteamy$Pct, -byteamy$W+byteamy$L, -byteamy$W, byteamy$team), ] }
if (ll==6) { byteamy <- byteamy[order(-byteamy$Champs, -byteamy$Years, -byteamy$Pct, -byteamy$W+byteamy$L, -byteamy$W, byteamy$team), ] }
if (ll==7) { byteamy <- byteamy[order(-byteamy$Seconds, -byteamy$Years, -byteamy$Pct, -byteamy$W+byteamy$L, -byteamy$W, byteamy$team), ] }
if (ll==8) { byteamy <- byteamy[order(byteamy$firstapp, byteamy$last.app), ] }

byteamy$class2 <- (1+(1:nrow(byteamy))) %% 2
byteamy$row3   <- paste("<tr class='d",byteamy$class2,"'><td>",paste(
				paste("<a href='",teamlink(byteamy$team),".html'>",byteamy$team,"</a>",sep=""),
				byteamy$Years,
				byteamy$W + byteamy$L + byteamy$T,
				byteamy$W,
				byteamy$L,
				sprintf("%.3f", round(byteamy$Pct,3) ),
				ifelse(is.na(byteamy$Champs)==T,"",byteamy$Champs),
				ifelse(is.na(byteamy$Seconds)==T,"",byteamy$Seconds),
				paste("<a href='bracket",byteamy$firstapp,".html'>",byteamy$firstapp,"</a>",sep=""),
				paste("<a href='bracket",byteamy$last.app,".html'>",byteamy$last.app,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

headerx <- gsub("ABC123",u.sport[i],header)
headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>",sporttext,"</a> > ","Teams",sep=""),headerx)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",champtext,"</center></h2>"))
cat("<br><table width='80%' align='center'>")
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

byteam2 <- subset(byteamx, sport==u.sport[i])
byteam2 <- byteam2[order(-byteam2$Years, -byteam2$Champs, -byteam2$W, byteam2$L),]
byteam2 <- byteam2[1:10,]

byyear2$row <- paste("<tr><td>",paste(
	paste("<a href='bracket",byyear2$year,".html'>",byyear2$year ,"</a>",sep=""),
	paste("<a href='",teamlink(byyear2$team1),".html'>", byyear2$team1,"</a>",sep=""),
	byyear2$score,
	paste("<a href='",teamlink(byyear2$team2),".html'>", byyear2$team2,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

byteam2$row <- paste("<tr><td>",paste(
				paste("<a href='",teamlink(byteam2$team),".html'>",byteam2$team,"</a>",sep=""),
				byteam2$Years,
				ifelse(is.na(byteam2$Champs)==T,"",byteam2$Champs),
				paste(byteam2$W,byteam2$L,sep="-"),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../",u.sport[i],"/index.html",sep=""))

if (u.sport[i]=="HKY") {sporttext<-"Hockey"; champtext<-"NCAA Hockey Tournament";
} else if (u.sport[i]=="LAX") {sporttext<-"LaCrosse"; champtext<-"NCAA LaCrosse Tournament";
} else if (u.sport[i]=="MSO") {sporttext<-"Soccer Men"; champtext<-"NCAA Men's Soccer Tournament";
} else if (u.sport[i]=="WSO") {sporttext<-"Soccer Women"; ; champtext<-"NCAA Women's Soccer Tournament";
} else if (u.sport[i]=="WBB") {sporttext<-"Basketball Women"; champtext<-"NCAA Women's Basketball Tournament";
} else if (u.sport[i]=="VOL") {sporttext<-"Volleyball"; champtext<-"NCAA Volleyball Tournament";
}

headerx <- gsub("ABC123",u.sport[i],header)
headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>",sporttext,"</a>",sep=""),headerx)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)

cat(paste("<br><h2><center>",champtext,"</center></h2>"))
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
cat(paste("<tr>","<td colspan='4'><a href='byteam1.html'>All Teams</a></td>","</tr>"))
cat("</table><br>")

cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th class='hl'><div style='font-size:1.5em;'>Event Results</div></th>","</tr>"))
	cat(paste("<tr>","<td><a href='champgm.html'>Championship Games</a></td>","</tr>"))
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# event pages ################
##########################################

champsmhky	<- subset(playoff, round==1 & sport=="HKY" & result=="W")

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

singlepage(champsmhky,"HKY","champgm.html","Championship Games","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")


