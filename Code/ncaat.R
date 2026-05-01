
############# import data
playoff <- read.csv("../Data/ncaat.csv")

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
playoff$group <- ifelse(playoff$name1=="Championship"|playoff$name1=="Final Four",9,
			ifelse(playoff$name1=="Third Place",8,0))
playoff$seedn<- sapply(playoff$seed1, seedsort)

############# build seasons from playoff
playoffx <- subset(playoff, group==0)
teams1 <- unique(playoffx[c("year","seed1","team1","name1")]); names(teams1) <- c("year","seed","team","regional");
teams2 <- unique(playoffx[c("year","seed2","team2","name1")]); names(teams2) <- c("year","seed","team","regional");
teams3 <- rbind(teams1, teams2)
seasons<- unique(teams3[c("year","seed","team","regional")])
rownames(seasons) <- seq(length=nrow(seasons))

seasons<- seasons[order(seasons$team, seasons$year),]
seasons$key <- paste(seasons$year,seasons$team,sep="/")
seasons$seedn<- sapply(seasons$seed, seedsort)

###########################################

rounds <- playoff[c("year","round","team1","score1","team2","score2")]

rounds.w <- rounds
rounds.w$team <- rounds$team1
rounds.w$score <- paste(rounds.w$score1,rounds.w$score2,sep="-")
rounds.w$result <- "W"
rounds.w <- rounds.w[c("year","round","team","result","score")]

rounds.l <- rounds
rounds.l$team <- rounds$team2
rounds.l$score <- paste(rounds.l$score2,rounds.l$score1,sep="-")
rounds.l$result <- "L"
rounds.l <- rounds.l[c("year","round","team","result","score")]

rounds <- rbind(rounds.w,rounds.l)

rounds$key <- paste(rounds$year,rounds$team,sep="/")
summ <- as.data.frame.matrix(table(rounds$key,rounds$result))
summ$key <- rownames(summ)

seasons <- merge(seasons,summ,by="key",all.x=TRUE)

rounds$final <- ifelse(rounds$round==2 ,rounds$score,"")
rounds$semif <- ifelse(rounds$round==4 ,rounds$score,"")
rounds$elite <- ifelse(rounds$round==8 ,rounds$score,"")
rounds$sweet <- ifelse(rounds$round==16,rounds$score,"")
rounds$rd.32 <- ifelse(rounds$round==32,rounds$score,"")
rounds$rd.64 <- ifelse(rounds$round==64,rounds$score,"")
rounds$prelm <- ifelse(rounds$round==128,rounds$score,"")

#####

seasons <- merge(seasons,subset(rounds,final!="")[c("key","final")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,semif!="")[c("key","semif")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,elite!="")[c("key","elite")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,sweet!="")[c("key","sweet")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,rd.32!="")[c("key","rd.32")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,rd.64!="")[c("key","rd.64")],by=c("key"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,prelm!="")[c("key","prelm")],by=c("key"),all.x=TRUE)

seasons$finish <- ifelse(seasons$L==0,1,
			ifelse(is.na(seasons$final)==F,2,
			ifelse(is.na(seasons$semif)==F,4,
			ifelse(is.na(seasons$elite)==F,8,
			ifelse(is.na(seasons$sweet)==F,16,
			ifelse(is.na(seasons$rd.32)==F,32,
			ifelse(is.na(seasons$rd.64)==F,64,99)))))))

##########################################
############# generate header / footer ###
##########################################

header <- vector()
header[1] <- '<html><head><title>OmahaSeries.com</title>'
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

u.yr <- unique(seasons$year)

for (i in 1:length(u.yr))
{ 
print(paste(i,u.yr[i]))

# create summary for given year
YR <- u.yr[i]; YR0 <- YR-1; YR2 <- YR+1

headerx <- gsub("ABC123","CBB",header)
summ.yr <- subset(seasons, year == u.yr[i])
summ.yr <- summ.yr[order(summ.yr$regional, summ.yr$seed, summ.yr$finish, summ.yr$L, -summ.yr$W, summ.yr$team),]

summ.yr$class <- ifelse(is.na(summ.yr$semif)==FALSE,3,0)

summ.yr$row <- paste("<tr class='d",summ.yr$class,"'><td>",paste(
				ifelse(is.na(summ.yr$seed)==F,paste(summ.yr$seed),""),
				paste("<a href='",teamlink(summ.yr$team),".html'>",summ.yr$team,"</a>",sep=""),
				paste(summ.yr$W,summ.yr$L,sep="-"),
			sep="</td><td>"),"</td></tr>",sep="")

play.yr <- subset(playoff, year == u.yr[i])
play.yr <- play.yr[order(play.yr$group, play.yr$name1, -play.yr$round, play.yr$seedn),]

for (p in 1:nrow(play.yr)) {
if (p==1) { play.yr$flag[p] <- 2}
if (p >1) { play.yr$flag[p] <- ifelse(play.yr$name2[p] != play.yr$name2[p-1]|play.yr$name1[p] != play.yr$name1[p-1]
							,ifelse(play.yr$name1[p] != play.yr$name1[p-1],2,1),0)}}

play.yr$row <- paste(ifelse(play.yr$flag==1,"<tr><th colspan='5'></th></tr>",""),"<tr><td>",paste(
				ifelse(play.yr$flag>=1,paste("<b>",play.yr$name2,"</b>",sep=""),""),
				paste(ifelse(is.na(play.yr$seed1),"",paste("<h5>",play.yr$seed1,"</h5> ",sep="")),
					"<a href='",teamlink(play.yr$team1),".html'>",play.yr$team1,"</a>",sep=""),
				paste(play.yr$score1,play.yr$score2,sep=" - "),
				paste(ifelse(is.na(play.yr$seed2),"",paste("<h5>",play.yr$seed2,"</h5> ",sep="")),
					"<a href='",teamlink(play.yr$team2),".html'>",play.yr$team2,"</a>",sep=""),				
				ifelse(play.yr$ot!="",paste(play.yr$ot),"&nbsp;&nbsp;"),
			sep="</td><td>"),"</td></tr>")

sink(paste("../CBB/",u.yr[i],".html",sep=""))

headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>Basketball Men</a> > ",u.yr[i]," (","<a href='bracket",YR,".html'>Bracket</a>",")",sep=""),headerx)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",YR,"</center></h2>"))
cat(paste("<center><a href='",YR0,".html'> << ",YR0,"</a> | ",
	            "<a href='",YR2,".html'>",YR2," >> </a></center><br>",sep=""))

cat("<br><table width='90%' align='center' style='border-color:white;'><tr>")

u.lg  <- unique(summ.yr$regional)
for (j in 1:length(u.lg)) {
cat("<td width='25%' valign='top' style='border-color:white;'>")
cat("<table width='95%' align='center'><col width='20%'><col width='60%'><col width='20%'>")
cat(paste("<tr><th colspan='3' class='hl'>",u.lg[j],"</th></tr>"))
write.table(subset(summ.yr,u.lg[j]==summ.yr$regional)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table></td>"))
}

cat("</tr></table><br>")
cat("<!--Insert Bracket--!>")

cat("<center><h3>Games</h3></center>")
cat(paste("<br><center><a href='bracket",paste(u.yr[i]),".html'>Tournament Bracket</a></center>",sep=""))
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
############# by team page ###############
##########################################

u.tm <- unique(seasons$team)

seasons$count <- 1
byteam <- as.data.frame(u.tm)
names(byteam)[1] <- "team"

seas <- aggregate(seasons$count, list(seasons$team), sum); names(seas)[names(seas)=="x"] <- "Years"

byteam <- merge(byteam,seas,by.x="team",by.y="Group.1")

rounds$resultf <- paste(rounds$result,rounds$round,sep="")
summ <- as.data.frame.matrix(table(rounds$team,rounds$result))
summ$team <- rownames(summ)
summf <- as.data.frame.matrix(table(rounds$team,rounds$resultf))
summf$team <- rownames(summ)

byteam <- merge(byteam,summ,by="team",all.x=TRUE)
byteam <- merge(byteam,summf,by="team",all.x=TRUE)
byteam$Pct <- byteam$W/(byteam$W+byteam$L)

firstapp <- aggregate(rounds$year , list(rounds$team), min); names(firstapp)[names(firstapp)=="x"] <- "firstapp";
last.app <- aggregate(rounds$year , list(rounds$team), max); names(last.app)[names(last.app)=="x"] <- "last.app";

byteam <- merge(byteam,firstapp,by.x="team",by.y="Group.1",all.x=TRUE)
byteam <- merge(byteam,last.app,by.x="team",by.y="Group.1",all.x=TRUE)

byteam$class <- (1+(1:nrow(byteam))) %% 2
byteam$row <- paste("<tr class='d",byteam$class,"'><td>",paste(
				paste("<a href='",teamlink(byteam$team),".html'>",byteam$team,"</a>",sep=""),
				byteam$Years,
				byteam$W,
				byteam$L,
				ifelse(byteam$W128+byteam$L128>0,paste(byteam$W128,byteam$L128,sep="-"),""),
				ifelse(byteam$W64 +byteam$L64 >0,paste(byteam$W64 ,byteam$L64 ,sep="-"),""),
				ifelse(byteam$W32 +byteam$L32 >0,paste(byteam$W32 ,byteam$L32 ,sep="-"),""),
				ifelse(byteam$W16 +byteam$L16 >0,paste(byteam$W16 ,byteam$L16 ,sep="-"),""),
				ifelse(byteam$W8  +byteam$L8  >0,paste(byteam$W8  ,byteam$L8  ,sep="-"),""),
				ifelse(byteam$W4  +byteam$L4  >0,paste(byteam$W4  ,byteam$L4  ,sep="-"),""),
				ifelse(byteam$W2  +byteam$L2  >0,paste(byteam$W2  ,byteam$L2  ,sep="-"),""),
				byteam$firstapp,
				byteam$last.app,
			sep="</td><td>"),"</td></tr>",sep="")

byteam$row2 <- paste("<tr class='d4'><td>",paste(
				"Overall",
				"&nbsp;",
				paste(byteam$Years,"Apps."),
				ifelse(byteam$W+byteam$L>0,paste(byteam$W,byteam$L,sep="-"),""),
				ifelse(byteam$W128+byteam$L128>0,paste(byteam$W128,byteam$L128,sep="-"),""),
				ifelse(byteam$W64 +byteam$L64 >0,paste(byteam$W64 ,byteam$L64 ,sep="-"),""),
				ifelse(byteam$W32 +byteam$L32 >0,paste(byteam$W32 ,byteam$L32 ,sep="-"),""),
				ifelse(byteam$W16 +byteam$L16 >0,paste(byteam$W16 ,byteam$L16 ,sep="-"),""),
				ifelse(byteam$W8  +byteam$L8  >0,paste(byteam$W8  ,byteam$L8  ,sep="-"),""),
				ifelse(byteam$W4  +byteam$L4  >0,paste(byteam$W4  ,byteam$L4  ,sep="-"),""),
				ifelse(byteam$W2  +byteam$L2  >0,paste(byteam$W2  ,byteam$L2  ,sep="-"),""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CBB/byteam.html",sep=""))

headerx <- gsub("ABC123","CBB",header)
headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>Basketball Men</a> >> ","Teams",sep=""),headerx)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>NCAA Men's Basketball Tournament</center></h2>"))

cat("<br><table width='80%' align='center'>")
#cat("<col width='20%'><col width='6%'><col width='7%'><col width='7%'><col width='4%'><col width='4%'><col width='4%'><col width='8%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='4%'><col width='4%'><col width='4%'><col width='4%'>")
cat("<tr><th></th><th>Years</th><th>Win</th><th>Loss</th><th>128</th><th>64</th><th>32</th><th>16</th><th>E8</th><th>F4</th><th>NC</th><th>First</th><th>Last</th></tr>")
write.table(byteam["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

##########################################
############# team pages #################
##########################################

u.tm <- unique(seasons$team)

for (i in 1:length(u.tm))
{
print(paste(i,u.tm[i]))

# create summary for given year
headerx <- gsub("ABC123","CBB",header)
headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>Basketball Men</a> >> ",u.tm[i],sep=""),headerx)
headerx <- gsub("<!--Insert Right-->",paste("<a href='byteam1.html'>Teams</a> ",sep=""),headerx)
summ.tm <- subset(seasons, seasons$team == u.tm[i])
summ.tm <- summ.tm[order(-summ.tm$year),]

summ.tm$class <- (1+(1:nrow(summ.tm))) %% 2

summ.tm$row <- paste("<tr class='d",summ.tm$class,"'><td>",paste(
				paste("<a href='bracket",summ.tm$year,".html'>",summ.tm$year,"</a>",sep=""),
				ifelse(is.na(summ.tm$seed)==F,paste(summ.tm$seed),""),
				paste(gsub(" Regional","",summ.tm$regional)),
				paste(summ.tm$W,summ.tm$L,sep="-"),
				ifelse(is.na(summ.tm$prelm)==FALSE,summ.tm$prelm,""),
				ifelse(is.na(summ.tm$rd.64)==FALSE,summ.tm$rd.64,""),
				ifelse(is.na(summ.tm$rd.32)==FALSE,summ.tm$rd.32,""),
				ifelse(is.na(summ.tm$sweet)==FALSE,summ.tm$sweet,""),
				ifelse(is.na(summ.tm$elite)==FALSE,summ.tm$elite,""),
				ifelse(is.na(summ.tm$semif)==FALSE,summ.tm$semif,""),
				ifelse(is.na(summ.tm$final)==FALSE,summ.tm$final,""),
			sep="</td><td>"),"</td></tr>",sep="")

thth <- "<th>Year</th><th>Seed</th><th>Regional</th><th>W-L</th><th>PIG</th><th>R64</th><th>R32</th><th>S16</th><th>E8</th><th>F4</th><th>NC</th>"

play.tm <- subset(playoff, team1 == paste(u.tm[i]) | team2 == paste(u.tm[i]))
play.tm$team2x <- ifelse(play.tm$team2 == paste(u.tm[i]),play.tm$team1,play.tm$team2)
play.tm$seed2x <- ifelse(play.tm$team2 == paste(u.tm[i]),play.tm$seed1,play.tm$seed2)
play.tm$score1x <- ifelse(play.tm$team2 == paste(u.tm[i]),play.tm$score2,play.tm$score1)
play.tm$score2x <- ifelse(play.tm$team2 == paste(u.tm[i]),play.tm$score1,play.tm$score2)
play.tm$result <- ifelse(play.tm$team2 == paste(u.tm[i]),"L","W")

play.tm <- play.tm[order(-play.tm$year, -play.tm$round),]

for (n in 1:nrow(play.tm)) {
if (n==1) { play.tm$flag[n] <- 2}
if (n >1) { play.tm$flag[n] <- ifelse(play.tm$year[n] != play.tm$year[n-1],1,0)}}

#play.tm$row <- paste(ifelse(play.tm$flag==1,"<tr><th colspan='6'></th></tr>",""),"<tr><td>",paste(
#				ifelse(play.tm$flag>=1,paste("<a href='bracket",play.tm$year,".html'><b>",play.tm$year,"</b></a>",sep=""),""),
#				gsub("Final Four","National",gsub(" Regional","",paste(play.tm$name1,play.tm$name2))),
#				paste(ifelse(is.na(play.tm$seed1),"",paste("<h5>",play.tm$seed1,"</h5> ",sep="")),
#					"<a href='",teamlink(play.tm$team1),".html'>",play.tm$team1,"</a>",sep=""),
#				paste(play.tm$score1,play.tm$score2,sep=" - "),
#				paste(ifelse(is.na(play.tm$seed2),"",paste("<h5>",play.tm$seed2,"</h5> ",sep="")),
#					"<a href='",teamlink(play.tm$team2),".html'>",play.tm$team2,"</a>",sep=""),
#				ifelse(play.tm$ot!="",paste(play.tm$ot),"&nbsp;&nbsp;"),
#			sep="</td><td>"),"</td></tr>",sep="")

play.tm$row <- paste(ifelse(play.tm$flag==1,"<tr><th colspan='6'></th></tr>",""),"<tr><td>",paste(
				ifelse(play.tm$flag>=1,paste("<a href='bracket",play.tm$year,".html'><b>",play.tm$year,"</b></a>",sep=""),""),
				gsub("Final Four","National",gsub(" Regional","",paste(play.tm$name1,play.tm$name2))),
				paste(ifelse(is.na(play.tm$seed2x),"",paste("<h5>",play.tm$seed2x,"</h5> ",sep="")),
					"<a href='",teamlink(play.tm$team2x),".html'>",play.tm$team2x,"</a>",sep=""),
				paste(play.tm$result),
				paste(play.tm$score1x,play.tm$score2x,sep=" - "),
				ifelse(play.tm$ot!="",paste(play.tm$ot),"&nbsp;&nbsp;"),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CBB/",teamlink(u.tm[i]),".html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",summ.tm$team[1],"</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat("<col width='8%'><col width='8%'><col width='18%'><col width='8%'><col width='8%'><col width='8%'><col width='8%'><col width='8%'><col width='8%'><col width='8%'><col width='8%'>")
cat(paste("<tr>",thth,"</tr>",sep=""))
write.table(subset(byteam,team==u.tm[i])["row2"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
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

byyear <- as.data.frame(u.yr)
names(byyear)[1] <- "year"

final <- subset(playoff, round==2)
byyear <- merge(byyear,final,by="year",all.x=TRUE)

teams.yr <- as.data.frame.matrix(table(seasons$year,rep("no.teams",length(seasons$year)) ))
teams.yr$year <- rownames(teams.yr)
byyear <- merge(byyear,teams.yr,by="year",all.x=TRUE)

semis <- subset(playoff, round==4)[c("year","team2")]
semis$team3 <- semis$team2; 
semis$team4 <- semis$team2;
semis.1 <- semis[seq(1, nrow(semis), 2),]
semis.1 <- semis.1[c("year","team3")]
semis.2 <- semis[seq(2, nrow(semis), 2),]
semis.2 <- semis.2[c("year","team4")]

byyear <- merge(byyear,semis.1,by="year",all.x=TRUE)
byyear <- merge(byyear,semis.2,by="year",all.x=TRUE)
byyear$score <- paste(byyear$score1,byyear$score2,sep=" - ")
byyear$class <- (1+(1:nrow(byyear))) %% 2

byyear$row <- paste("<tr class='d",byyear$class,"'><td>",paste(
	paste("<a href='bracket",byyear$year,".html'>",byyear$year ,"</a>",sep=""),
	byyear$no.teams,
	paste("<a href='",teamlink(byyear$team1),".html'>", byyear$team1,"</a>",sep=""),
	ifelse(is.na(byyear$score),"",byyear$score),
	paste("<a href='",teamlink(byyear$team2),".html'>", byyear$team2,"</a>",sep=""),
	paste("<a href='",teamlink(byyear$team3),".html'>", byyear$team3,"</a>",sep=""),
	paste("<a href='",teamlink(byyear$team4),".html'>", byyear$team4,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

byyear <- byyear[order(-byyear$year),]

sink(paste("../CBB/byyear.html",sep=""))

headerx <- gsub("ABC123","CBB",header)
headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>Basketball Men</a> >> ","Years",sep=""),headerx)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>NCAA Men's Basketball Tournament</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Season</th><th>Teams</th><th>Champion</th><th></th><th>Runner-Up</th><th colspan='2'>Final Four</th></tr>",sep=""))
write.table(byyear["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# sortable NCAAT records ###
############################

byteam$team <- paste(byteam$team)

for (ll in 1:11) {
sink(paste("../CBB/byteam",ll,".html",sep=""))

if (ll==1) { byteam <- byteam[order(byteam$team), ] }
if (ll==2) { byteam <- byteam[order(-byteam$Years, -byteam$Pct, -byteam$W+byteam$L, -byteam$W, byteam$team), ] }
if (ll==3) { byteam <- byteam[order(-byteam$W-byteam$L, -byteam$Pct, -byteam$W+byteam$L, -byteam$W, byteam$team), ] }
if (ll==4) { byteam <- byteam[order(-byteam$W, -byteam$Pct, -byteam$W+byteam$L, -byteam$W, byteam$team), ] }
if (ll==5) { byteam <- byteam[order(-byteam$Pct, -byteam$W+byteam$L, -byteam$W, byteam$team), ] }
if (ll==6) { byteam <- byteam[order(-byteam$W2, -byteam$L2, -byteam$Years, -byteam$Pct, -byteam$W+byteam$L, -byteam$W, byteam$team), ] }
if (ll==7) { byteam <- byteam[order(-byteam$L2, -byteam$Years, -byteam$Pct, -byteam$W+byteam$L, -byteam$W, byteam$team), ] }
if (ll==8) { byteam <- byteam[order(-byteam$W8, -byteam$Years, -byteam$Pct, -byteam$W+byteam$L, -byteam$W, byteam$team), ] }
if (ll==9) { byteam <- byteam[order(-byteam$W8-byteam$L8, -byteam$Years, -byteam$Pct, -byteam$W+byteam$L, -byteam$W, byteam$team), ] }
if (ll==10) { byteam <- byteam[order(-byteam$W16-byteam$L16, -byteam$Years, -byteam$Pct, -byteam$W+byteam$L, -byteam$W, byteam$team), ] }
if (ll==11) { byteam <- byteam[order(byteam$firstapp, byteam$last.app), ] }

byteam$class2 <- (1+(1:nrow(byteam))) %% 2
byteam$row3   <- paste("<tr class='d",byteam$class2,"'><td>",paste(
				paste("<a href='",teamlink(byteam$team),".html'>",byteam$team,"</a>",sep=""),
				byteam$Years,
				byteam$W + byteam$L,
				byteam$W,
				byteam$L,
				sprintf("%.3f", round(byteam$Pct,3) ),
				byteam$W2,
				byteam$L2,
				byteam$W8,
				byteam$W8+byteam$L8,
				byteam$W16+byteam$L16,
				paste("<a href='bracket",byteam$firstapp,".html'>",byteam$firstapp,"</a>",sep=""),
				paste("<a href='bracket",byteam$last.app,".html'>",byteam$last.app,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

headerx <- gsub("ABC123","CBB",header)
headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>Basketball Men</a> >> ","Teams",sep=""),headerx)

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>NCAA Men's Basketball Tournament</center></h2>"))
cat("<br><table width='80%' align='center'>")
cat(paste("<tr>", "<th",ifelse(ll==1," class='hl3'",""),"><a href='byteam1.html'>Team</a></th>",
			"<th",ifelse(ll==2," class='hl3'",""),"><a href='byteam2.html'>Apps.</a></th>",
			"<th",ifelse(ll==3," class='hl3'",""),"><a href='byteam3.html'>GP</a></th>",
			"<th",ifelse(ll==4," class='hl3'",""),"><a href='byteam4.html'>W</a></th><th>L</th>",
			"<th",ifelse(ll==5," class='hl3'",""),"><a href='byteam5.html'>PCT</a></th>",
			"<th",ifelse(ll==6," class='hl3'",""),"><a href='byteam6.html'>1ST</a></th>",
			"<th",ifelse(ll==7," class='hl3'",""),"><a href='byteam7.html'>2ND</a></th>",
			"<th",ifelse(ll==8," class='hl3'",""),"><a href='byteam8.html'>F4s</a></th>",
			"<th",ifelse(ll==9," class='hl3'",""),"><a href='byteam9.html'>E8s</a></th>",
			"<th",ifelse(ll==10," class='hl3'",""),"><a href='byteam10.html'>16s</a></th>",
			"<th",ifelse(ll==11," class='hl3'",""),"><a href='byteam11.html'>First</a></th><th>Last</th>","</tr>",sep=""))
write.table(byteam["row3"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# index page #################
##########################################

byyear2 <- byyear 
byyear2 <- byyear2[order(-byyear2$year),]
byyear2 <- byyear2[1:10,]

byteam2 <- byteam
byteam2 <- byteam2[order(-byteam2$Years, -byteam2$W2, -byteam2$W, byteam2$L),]
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
				byteam2$W2,
				paste(byteam2$W,byteam2$L,sep="-"),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CBB/index.html",sep=""))

headerx <- gsub("ABC123","CBB",header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)

cat(paste("<br><h2><center>NCAA Men's Basketball Tournament</center></h2>"))

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
	cat(paste("<tr>","<td><a href='semis.html'>National Semifinals</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='regfins.html'>Regional Finals</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='upsets.html'>Upsets, Seed Difference of 5 or more</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='seed.html'>Seeding Statistics</a></td>","</tr>"))
#	cat(paste("<tr>","<td><a href='byteam1.html'>Sortable Tournament Records</a></td>","</tr>"))
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

##########################################
############# event pages ################
##########################################

champs	<- subset(playoff, round==2)
semis		<- subset(playoff, round==4)
regfinals	<- subset(playoff, round==8 & year >= 1952)
upsets	<- subset(playoff, seed1-seed2>=5)

regfinals$loc <- gsub(" Regional","",regfinals$name1)
upsets$loc <- gsub("Final Four","National",gsub(" Regional","",paste(upsets$name1,upsets$name2)))

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
headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>Basketball Men</a> >> ",titlex,sep=""),headerx)
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

singlepage(champs,"CBB","champgm.html","Championship Games","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")
singlepage(semis,"CBB","semis.html","National Semifinals","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")
singlepage(regfinals,"CBB","regfins.html","Regional Finals","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")
singlepage(upsets,"CBB","upsets.html","Upsets","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")

#############################
######## seed report ########
#############################
 
seeds <- subset(playoff,year>=1979 & round<=64)

games.1 <- seeds[c("year","round","seed1","team1","score1","seed2","team2","score2","ot")]
games.1$result <- "W"

games.2 <- seeds[c("year","round","seed2","team2","score2","seed1","team1","score1","ot")]
colnames(games.2) <- c("year","round","seed1","team1","score1","seed2","team2","score2","ot")
games.2$result <- "L"

games <- rbind(games.1,games.2)
denom <- length(unique(games$year))

games85 <- subset(games,year>=1985)
denom85 <- length(unique(games85$year))

################################
# seeds by round;
table00 <- as.data.frame.matrix(table(games$seed1,games$result))
table01 <- as.data.frame.matrix(table(games$seed1,paste(games$result,games$round,sep="")))

table00$seed <- as.numeric(rownames(table00))
table00$X0   <- paste(table00$W,table00$L,sep="-")
table00$A0   <- round(table00$W /denom,digits=2)
table00 <- table00[c("seed","X0","A0")]

table01$seed <- as.numeric(rownames(table01))
table01$X64  <- paste(table01$W64,table01$L64,sep="-")
table01$X32  <- paste(table01$W32,table01$L32,sep="-")
table01$X16  <- paste(table01$W16,table01$L16,sep="-")
table01$X8   <- paste(table01$W8 ,table01$L8 ,sep="-")
table01$X4   <- paste(table01$W4 ,table01$L4 ,sep="-")
table01$X2   <- paste(table01$W2 ,table01$L2 ,sep="-")

table01$A64  <- round(table01$W64/denom,digits=2)
table01$A32  <- round(table01$W32/denom,digits=2)
table01$A16  <- round(table01$W16/denom,digits=2)
table01$A8   <- round(table01$W8 /denom,digits=2)
table01$A4   <- round(table01$W4 /denom,digits=2)
table01$A2   <- round(table01$W2 /denom,digits=2)

table01<- table01[c("seed","X64","X32","X16","X8","X4","X2","A64","A32","A16","A8","A4","A2")]

table01 <- merge(table00,table01,by="seed")

table01$class <- (1+(1:nrow(table01))) %% 2

table01$row1  <- paste("<tr class='d",table01$class,"'><td>",paste(
				table01$seed,
				table01$X0,
				table01$X64,
				table01$X32,
				table01$X16,
				table01$X8,
				table01$X4,
				table01$X2,
			sep="</td><td>"),"</td></tr>",sep="")

################################
# seeds by round;
table0 <- as.data.frame.matrix(table(games85$seed1,games85$result))
table1 <- as.data.frame.matrix(table(games85$seed1,paste(games85$result,games85$round,sep="")))

table0$seed <- as.numeric(rownames(table0))
table0$X0   <- paste(table0$W,table0$L,sep="-")
table0$A0   <- round(table0$W /denom85,digits=2)
table0 <- table0[c("seed","X0","A0")]

table1$seed <- as.numeric(rownames(table1))
table1$X64  <- paste(table1$W64,table1$L64,sep="-")
table1$X32  <- paste(table1$W32,table1$L32,sep="-")
table1$X16  <- paste(table1$W16,table1$L16,sep="-")
table1$X8   <- paste(table1$W8 ,table1$L8 ,sep="-")
table1$X4   <- paste(table1$W4 ,table1$L4 ,sep="-")
table1$X2   <- paste(table1$W2 ,table1$L2 ,sep="-")

table1$A64  <- round(table1$W64/denom,digits=2)
table1$A32  <- round(table1$W32/denom,digits=2)
table1$A16  <- round(table1$W16/denom,digits=2)
table1$A8   <- round(table1$W8 /denom,digits=2)
table1$A4   <- round(table1$W4 /denom,digits=2)
table1$A2   <- round(table1$W2 /denom,digits=2)

table1 <- table1[c("seed","X64","X32","X16","X8","X4","X2","A64","A32","A16","A8","A4","A2")]

table1 <- merge(table0,table1,by="seed")

table1$class <- (1+(1:nrow(table1))) %% 2

table1$row1  <- paste("<tr class='d",table1$class,"'><td>",paste(
				table1$seed,
				table1$X0,
				table1$X64,
				table1$X32,
				table1$X16,
				table1$X8,
				table1$X4,
				table1$X2,
				table1$A64,
				table1$A32,
				table1$A16,
				table1$A8,
				table1$A4,
				table1$A2,
			sep="</td><td>"),"</td></tr>",sep="")

################################
# seeds by year;
table2 <- as.data.frame.matrix(table(games$year,paste(games$result,games$seed1,sep="_")))
table2$A1  <- paste(table2$W_1,table2$L_1,sep="-")
table2$A2  <- paste(table2$W_2,table2$L_2,sep="-")
table2$A3  <- paste(table2$W_3,table2$L_3,sep="-")
table2$A4  <- paste(table2$W_4,table2$L_4,sep="-")
table2$A5  <- paste(table2$W_5,table2$L_5,sep="-")
table2$A6  <- paste(table2$W_6,table2$L_6,sep="-")
table2$A7  <- paste(table2$W_7,table2$L_7,sep="-")
table2$A8  <- paste(table2$W_8,table2$L_8,sep="-")
table2$A9  <- paste(table2$W_9,table2$L_9,sep="-")
table2$A10 <- paste(table2$W_10,table2$L_10,sep="-")
table2$A11 <- paste(table2$W_11,table2$L_11,sep="-")
table2$A12 <- paste(table2$W_12,table2$L_12,sep="-")
table2$A13 <- paste(table2$W_13,table2$L_13,sep="-")
table2$A14 <- paste(table2$W_14,table2$L_14,sep="-")
table2$A15 <- paste(table2$W_15,table2$L_15,sep="-")
table2$A16 <- paste(table2$W_16,table2$L_16,sep="-")
table2$year <- as.numeric(rownames(table2))

table2 <- table2[c("year","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16")]

table2 <- table2[order(-table2$year), ] 

table2$class <- (1+(1:nrow(table2))) %% 2

table2$row1  <- paste("<tr class='d",table2$class,"'><td>",paste(
				paste("<a href='",table2$year,".html'>",table2$year,"</a>",sep=""),
				table2$A1,
				table2$A2,
				table2$A3,
				table2$A4,
				table2$A5,
				table2$A6,
				table2$A7,
				table2$A8,
				table2$A9,
				table2$A10,
				table2$A11,
				table2$A12,
				table2$A13,
				table2$A14,
				table2$A15,
				table2$A16,
			sep="</td><td>"),"</td></tr>",sep="")

################################
# seeds vs seeds;
table3 <- as.data.frame.matrix(table(games85$seed1,paste(games85$result,games85$seed2,sep="_")))
table3$A1  <- paste(table3$W_1,table3$L_1,sep="-")
table3$A2  <- paste(table3$W_2,table3$L_2,sep="-")
table3$A3  <- paste(table3$W_3,table3$L_3,sep="-")
table3$A4  <- paste(table3$W_4,table3$L_4,sep="-")
table3$A5  <- paste(table3$W_5,table3$L_5,sep="-")
table3$A6  <- paste(table3$W_6,table3$L_6,sep="-")
table3$A7  <- paste(table3$W_7,table3$L_7,sep="-")
table3$A8  <- paste(table3$W_8,table3$L_8,sep="-")
table3$A9  <- paste(table3$W_9,table3$L_9,sep="-")
table3$A10 <- paste(table3$W_10,table3$L_10,sep="-")
table3$A11 <- paste(table3$W_11,table3$L_11,sep="-")
table3$A12 <- paste(table3$W_12,table3$L_12,sep="-")
table3$A13 <- paste(table3$W_13,table3$L_13,sep="-")
table3$A14 <- paste(table3$W_14,table3$L_14,sep="-")
table3$A15 <- paste(table3$W_15,table3$L_15,sep="-")
table3$A16 <- paste(table3$W_16,table3$L_16,sep="-")
table3$seed <- as.numeric(rownames(table3))

table3 <- table3[c("seed","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16")]

table3$class <- (1+(1:nrow(table3))) %% 2

table3$row1  <- paste("<tr class='d",table3$class,"'><td>",paste(
				table3$seed,
				table3$A1,
				table3$A2,
				table3$A3,
				table3$A4,
				table3$A5,
				table3$A6,
				table3$A7,
				table3$A8,
				table3$A9,
				table3$A10,
				table3$A11,
				table3$A12,
				table3$A13,
				table3$A14,
				table3$A15,
				table3$A16,
			sep="</td><td>"),"</td></tr>",sep="")

################################
# Round 32;
games32 <- subset(games, round==64)

table4 <- as.data.frame.matrix(table(games32$year,paste(games32$result,games32$seed1,sep="_")))
table4$A1  <- paste(table4$W_1,table4$L_1,sep="-")
table4$A2  <- paste(table4$W_2,table4$L_2,sep="-")
table4$A3  <- paste(table4$W_3,table4$L_3,sep="-")
table4$A4  <- paste(table4$W_4,table4$L_4,sep="-")
table4$A5  <- paste(table4$W_5,table4$L_5,sep="-")
table4$A6  <- paste(table4$W_6,table4$L_6,sep="-")
table4$A7  <- paste(table4$W_7,table4$L_7,sep="-")
table4$A8  <- paste(table4$W_8,table4$L_8,sep="-")
table4$X1   <- table4$L_1 + table4$L_2 + table4$L_3 + table4$L_4 + table4$L_5 + table4$L_6 + table4$L_7 + table4$L_8
table4$X2   <- table4$L_1 + table4$L_2 + table4$L_3 + table4$L_4 + table4$L_5 + table4$L_6
table4$X3   <- table4$L_1 + table4$L_2 + table4$L_3 + table4$L_4
table4$year <- as.numeric(rownames(table4))

table4 <- table4[c("year","A1","A2","A3","A4","A5","A6","A7","A8","X1","X2","X3")]

table4 <- table4[order(-table4$year), ] 

table4$class <- (1+(1:nrow(table4))) %% 2

table4$row1  <- paste("<tr class='d",table4$class,"'><td>",paste(
				paste("<a href='",table4$year,".html'>",table4$year,"</a>",sep=""),
				table4$A1,
				table4$A2,
				table4$A3,
				table4$A4,
				table4$A5,
				table4$A6,
				table4$A7,
				table4$A8,
				table4$X1,
				table4$X2,
				table4$X3,
			sep="</td><td>"),"</td></tr>",sep="")

################################
# Sweet 16;
sweet16 <- subset(games, round==32 & result=="W")
sweet16 <- sweet16[order(sweet16$year, sweet16$seed1),]

SS <- matrix(999,nrow=denom,ncol=16)
for (i in 1:denom) {
	for (j in 1:16) {
		SS[i,j] <- sweet16$seed1[16*i+j-16]
}}
SS<-as.data.frame(SS)
SS$y <- do.call(paste, c(SS[1:16], sep=" "))

sweet16_1 <- aggregate(sweet16$seed1, list(sweet16$year), sum)
sweet16_2 <- as.data.frame.matrix(table(sweet16$year,sweet16$seed1))
sweet16_2$top4 <- sweet16_2$"1" + sweet16_2$"2" + sweet16_2$"3" + sweet16_2$"4"
sweet16_3 <- cbind(sweet16_1,sweet16_2,SS)

sweet16_f <- sweet16_3[c("Group.1","x","top4","y")]

sweet16_f <- sweet16_f[order(-sweet16_f$Group.1), ] 

sweet16_f$class <- (1+(1:nrow(sweet16_f))) %% 2

sweet16_f$row1  <- paste("<tr class='d",sweet16_f$class,"'><td>",paste(
				paste("<a href='",sweet16_f$Group.1,".html'>",sweet16_f$Group.1,"</a>",sep=""),
				sweet16_f$top4,
				sweet16_f$x,
				gsub(" ","</td><td>",sweet16_f$y),
			sep="</td><td>"),"</td></tr>",sep="")

################################
# Elite 8;
elite8 <- subset(games, round==16 & result=="W")
elite8 <- elite8[order(elite8$year, elite8$seed1),]

EE <- matrix(999,nrow=denom,ncol=8)
for (i in 1:denom) {
	for (j in 1:8) {
		EE[i,j] <- elite8$seed1[8*i+j-8]
}}
EE<-as.data.frame(EE)
EE$y <- do.call(paste, c(EE[1:8], sep=" "))

elite8_1 <- aggregate(elite8$seed1, list(elite8$year), sum)
elite8_2 <- as.data.frame.matrix(table(elite8$year,elite8$seed1))
elite8_2$top2 <- elite8_2$"1" + elite8_2$"2"
elite8_3 <- cbind(elite8_1,elite8_2,EE)

elite8_f <- elite8_3[c("Group.1","x","top2","y")]

elite8_f <- elite8_f[order(-elite8_f$Group.1), ] 

elite8_f$class <- (1+(1:nrow(elite8_f))) %% 2

elite8_f$row1  <- paste("<tr class='d",elite8_f$class,"'><td>",paste(
				paste("<a href='",elite8_f$Group.1,".html'>",elite8_f$Group.1,"</a>",sep=""),
				elite8_f$top2,
				elite8_f$x,
				gsub(" ","</td><td>",elite8_f$y),
			sep="</td><td>"),"</td></tr>",sep="")

################################
# Final 4;
final4 <- subset(games, round==8 & result=="W")
final4 <- final4[order(final4$year, final4$seed1),]

FF <- matrix(999,nrow=denom,ncol=4)
for (i in 1:denom) {
	for (j in 1:4) {
		FF[i,j] <- final4$seed1[4*i+j-4]
}}
FF<-as.data.frame(FF)
FF$y <- do.call(paste, c(FF[1:4], sep=" "))

final4_1 <- aggregate(final4$seed1, list(final4$year), sum)
final4_2 <- as.data.frame.matrix(table(final4$year,final4$seed1))
final4_2$top1 <- final4_2$"1"
final4_3 <- cbind(final4_1,final4_2,FF)

final4_f <- final4_3[c("Group.1","x","top1","y")]

final4_f <- final4_f[order(-final4_f$Group.1), ] 

final4_f$class <- (1+(1:nrow(final4_f))) %% 2

final4_f$row1  <- paste("<tr class='d",final4_f$class,"'><td>",paste(
				paste("<a href='",final4_f$Group.1,".html'>",final4_f$Group.1,"</a>",sep=""),
				final4_f$top1,
				final4_f$x,
				gsub(" ","</td><td>",final4_f$y),
			sep="</td><td>"),"</td></tr>",sep="")

################################

sink(paste("../","CBB","/seed.html",sep=""))
headerx <- gsub("ABC123","CBB",header)
headerx <- gsub("<!--Insert Link-->",paste("<a href='index.html'>Basketball Men</a> >> ","Seeds",sep=""),headerx)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)

cat(paste("<br><h3><center>Round by Round (since 1979)</center></h3>"))
cat("<br><table width='80%' align='center'>")
cat("<tr><th> </th><th class='hl'>Overall</th><th colspan='6' class='hl'>By Round</th></tr>")
cat("<tr><th>Seed</th><th>W-L</th><th>R64</th><th>R32</th><th>S16</th><th>E8</th><th>F4</th><th>NC</th></tr>")
write.table(table01["row1"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

cat(paste("<br><h3><center>Round by Round (since 1985)</center></h3>"))
cat("<br><table width='80%' align='center'>")
cat("<tr><th> </th><th class='hl'>Overall</th><th colspan='6' class='hl'>By Round</th><th colspan='6' class='hl'>Wins per Year</th></tr>")
cat("<tr><th>Seed</th><th>W-L</th><th>R64</th><th>R32</th><th>S16</th><th>E8</th><th>F4</th><th>NC</th><th>R64</th><th>R32</th><th>S16</th><th>E8</th><th>F4</th><th>NC</th></tr>")
write.table(table1["row1"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

cat(paste("<br><h3><center>Record by Year</center></h3>"))
cat("<br><table width='80%' align='center'>")
cat("<tr><th>Year</th><th>#1</th><th>#2</th><th>#3</th><th>#4</th><th>#5</th><th>#6</th><th>#7</th><th>#8</th><th>#9</th><th>#10</th>
			<th>#11</th><th>#12</th><th>#13</th><th>#14</th><th>#15</th><th>#16</th></tr>")
write.table(table2["row1"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

cat(paste("<br><h3><center>Seed vs. Seed (since 1985)</center></h3>"))
cat("<br><table width='80%' align='center'>")
cat("<tr><th>Year</th><th>#1</th><th>#2</th><th>#3</th><th>#4</th><th>#5</th><th>#6</th><th>#7</th><th>#8</th><th>#9</th><th>#10</th>
			<th>#11</th><th>#12</th><th>#13</th><th>#14</th><th>#15</th><th>#16</th></tr>")
write.table(table3["row1"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

cat(paste("<br><h3><center>First Round</center></h3>"))
cat("<br><table width='60%' align='center'>")
cat("<tr><th>Year</th><th>1-16</th><th>2-15</th><th>3-14</th><th>4-13</th><th>5-12</th><th>6-11</th><th>7-10</th><th>8-9</th>
			<th>Upsets</th><th>Upsets<br>11+</th><th>Upsets<br>13+</th></tr>")
write.table(table4["row1"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

cat(paste("<br><h3><center>Sweet Sixteen</center></h3>"))
cat("<br><table width='60%' align='center'>")
cat("<tr><th>Year</th><th>Out of Top 16</th><th>Sum(Seeds)</th><th colspan='16'>Seeds</th></tr>")
write.table(sweet16_f["row1"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

cat(paste("<br><h3><center>Elite Eight</center></h3>"))
cat("<br><table width='60%' align='center'>")
cat("<tr><th>Year</th><th>Out of Top 8</th><th>Sum(Seeds)</th><th colspan='8'>Seeds</th></tr>")
write.table(elite8_f["row1"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

cat(paste("<br><h3><center>Final Four</center></h3>"))
cat("<br><table width='60%' align='center'>")
cat("<tr><th>Year</th><th>Out of Top 4</th><th>Sum(Seeds)</th><th colspan='8'>Seeds</th></tr>")
write.table(final4_f["row1"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

