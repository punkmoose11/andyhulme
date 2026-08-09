
############# import data
data <- read.csv("../Data/wbc.csv")
data <- data [c("YEAR","Round","Group","GameNo","TEAM1","RUNS1","TEAM2","RUNS2","INN")]
data <- subset(data, is.na(RUNS1)==F)

teamlink <- function(x){teamx = tolower(gsub("&","",gsub(".","",gsub("'","",gsub(" ", "",x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE))}
groupnm  <- function(x){groupx= paste("Group",gsub("frd","",gsub("Grp","",gsub("2nd","",x))))}

############# create df with one record for each team per game
games.1 <- data
games.1$team <- games.1$TEAM1
games.1$opponent <- games.1$TEAM2
games.1$gf <- games.1$RUNS1
games.1$ga <- games.1$RUNS2
games.1$result <- ifelse(games.1$gf>games.1$ga,"W",ifelse(games.1$gf<games.1$ga,"L",
			ifelse(games.1$gf==games.1$ga & grepl("P",games.1$OT),"PW","T")))
games.1$XXX <- "XXX"
games.1 <- games.1[c("YEAR","Round","Group","GameNo","team","opponent","gf","ga","result","INN","XXX")]

games.2 <- data
games.2$team <- games.2$TEAM2
games.2$opponent <- games.2$TEAM1
games.2$gf <- games.2$RUNS2
games.2$ga <- games.2$RUNS1
games.2$result <- ifelse(games.2$gf>games.2$ga,"W",ifelse(games.2$gf<games.2$ga,"L",
			ifelse(games.2$gf==games.2$ga & grepl("P",games.2$OT),"PL","T")))
games.2$XXX <- " "
games.2 <- games.2[c("YEAR","Round","Group","GameNo","team","opponent","gf","ga","result","INN","XXX")]

games <- rbind(games.1,games.2)
colnames(games)[colnames(games)=="YEAR"] <- "yr"
colnames(games)[colnames(games)=="Group"] <- "GROUP"

games$score <- paste(games$gf,ifelse(games$result=="PW","p",""),"-",
			games$ga,ifelse(games$result=="PL","p",""),ifelse(games$OT=="Replay","R",""),sep="")
games$score2<- paste(games$gf,"-",games$ga,sep="")
games$team2 <- rename(games$team)

############# create dataset with yearly summaries

summ <- unique(games[c("yr","team")])
summ$team2 <- rename(summ$team)

#overall
games$okey <- paste(games$yr, games$team, sep="_")
overall <- as.data.frame.matrix(table(games$okey,games$result))
overall$okey <- rownames(overall)

OGF <- aggregate(as.numeric(games$gf), list(games$okey), sum, na.rm=TRUE); names(OGF)[names(OGF)=="x"] <- "gf";
OGA <- aggregate(as.numeric(games$ga), list(games$okey), sum, na.rm=TRUE); names(OGA)[names(OGA)=="x"] <- "ga";
overall <- merge(overall ,OGF ,by.x="okey",by.y="Group.1",all.x=TRUE)
overall <- merge(overall ,OGA ,by.x="okey",by.y="Group.1",all.x=TRUE)

overall$yr   <- gsub("(\\d+)(_)([^_]+)", "\\1", overall$okey)
overall$team <- gsub("(\\d+)(_)([^_]+)", "\\3", overall$okey)

#excluding qualifying
games$pkey <- paste(games$yr, games$team, sep="_")
gamesxq <- subset(games,Round>0)
overallxq <- as.data.frame.matrix(table(gamesxq$pkey,paste(gamesxq$result,"X",sep="")))
overallxq$pkey <- rownames(overallxq)

PGF <- aggregate(as.numeric(gamesxq$gf), list(gamesxq$pkey), sum, na.rm=TRUE); names(PGF)[names(PGF)=="x"] <- "pgf";
PGA <- aggregate(as.numeric(gamesxq$ga), list(gamesxq$pkey), sum, na.rm=TRUE); names(PGA)[names(PGA)=="x"] <- "pga";
overallxq <- merge(overallxq,PGF ,by.x="pkey",by.y="Group.1",all.x=TRUE)
overallxq <- merge(overallxq,PGA ,by.x="pkey",by.y="Group.1",all.x=TRUE)

overallxq$yr   <- gsub("(\\d+)(_)([^_]+)", "\\1", overallxq$pkey)
overallxq$team <- gsub("(\\d+)(_)([^_]+)", "\\3", overallxq$pkey)

# create group stage summaries
games$key <- paste(games$yr, games$team, games$GROUP, games$Round, sep="_")
games.g <- subset(games  , Round<3)
games.gg<- subset(games.g, Round<3)

group <- as.data.frame.matrix(table(games.gg$key,games.gg$result))
group$key <- rownames(group)
group$yr   <- gsub("(\\d+)(_)([^_]+)(_)([^_]+)(_)([^_]+)", "\\1", group$key)
group$team <- gsub("(\\d+)(_)([^_]+)(_)([^_]+)(_)([^_]+)", "\\3", group$key)
group$grp  <- gsub("(\\d+)(_)([^_]+)(_)([^_]+)(_)([^_]+)", "\\5", group$key)
group$rd   <- gsub("(\\d+)(_)([^_]+)(_)([^_]+)(_)([^_]+)", "\\7", group$key)

group$Pts  <- group$W / (group$W + group$L)

GF <- aggregate(as.numeric(games.g$gf), list(games.g$key), sum, na.rm=TRUE); names(GF)[names(GF)=="x"] <- "GF";
GA <- aggregate(as.numeric(games.g$ga), list(games.g$key), sum, na.rm=TRUE); names(GA)[names(GA)=="x"] <- "GA";
group <- merge(group,GF ,by.x="key",by.y="Group.1",all.x=TRUE)
group <- merge(group,GA ,by.x="key",by.y="Group.1",all.x=TRUE)
group$sort <- 100*group$Pts + group$GF-group$GA + group$GF/100
group$short <- gsub("frd","",gsub("Grp","",gsub("2nd","",group$grp)))

group$grank <- ave(-group$sort, paste(group$yr,group$grp), FUN = function(x) rank(x, ties.method = "min") )

group$WTL <- paste(group$W,group$L,sep="-")
group$grankc <- ifelse(group$grank==1,"1ST",ifelse(group$grank==2,"2ND",ifelse(group$grank==3,"3RD","4TH")))

group0 <- subset(group,rd==0)[c("yr","team","grp","WTL","grankc")]
names(group0) <- c("yr","team","grp0","record0","rank0");

group1 <- subset(group,rd==1)[c("yr","team","grp","WTL","grankc")]
names(group1) <- c("yr","team","grp1","record1","rank1");

group2 <- subset(group,rd==2)[c("yr","team","grp","WTL","grankc")]
names(group2) <- c("yr","team","grp2","record2","rank2");

quart <- subset(games,GROUP=="Quarters" & result!="T")[c("yr","team","score","result")]
names(quart) <- c("yr","team","qua","qua.r");

semif <- subset(games,GROUP=="Semis" & result!="T")[c("yr","team","score","result")]
names(semif) <- c("yr","team","sem","sem.r");

final <- subset(games,GROUP=="Final" & result!="T")[c("yr","team","score","result")]
names(final) <- c("yr","team","fin","fin.r");

summ <- merge(summ,overall,by=c("yr","team"),all.x=T)
summ <- merge(summ,overallxq,by=c("yr","team"),all.x=T)
summ <- merge(summ,group0,by=c("yr","team"),all.x=T)
summ <- merge(summ,group1,by=c("yr","team"),all.x=T)
summ <- merge(summ,group2,by=c("yr","team"),all.x=T)
summ <- merge(summ,quart ,by=c("yr","team"),all.x=T)
summ <- merge(summ,semif ,by=c("yr","team"),all.x=T)
summ <- merge(summ,final ,by=c("yr","team"),all.x=T)

summ[is.na(summ)] <- ""
summ$WX <- as.numeric(ifelse(summ$WX=="","0",summ$WX))
summ$LX <- as.numeric(ifelse(summ$LX=="","0",summ$LX))
summ$pgf <- as.numeric(ifelse(summ$pgf=="","0",summ$pgf))
summ$pga <- as.numeric(ifelse(summ$pga=="","0",summ$pga))

summ$class <- ifelse(summ$fin.r=="W"|summ$fin.r=="PW",100000,
			ifelse(summ$fin.r=="L"|summ$fin.r=="PL",90000,
			ifelse(summ$sem.r=="L"|summ$sem.r=="PL",70000,
			ifelse(summ$qua.r=="L"|summ$qua.r=="PL",60000,
			ifelse(summ$grp2!="",40000,
			ifelse(summ$grp1!="",30000,20000))))))

summ$class <- summ$class + ifelse(summ$grp2=="Wfrd",-1000*as.numeric(substring(summ$rank2,1,1)),0)

summ$sort <- summ$class + 1000*(summ$WX - summ$LX) + 10*(summ$pgf-summ$pga) + summ$W - summ$L + (summ$gf-summ$ga)/100 + summ$gf/100000

summ$rank <- ave(-summ$sort, summ$yr, FUN = function(x) rank(x, ties.method = "min") )
summ <- summ[order(summ$yr, summ$rank),]

summ$place <- ifelse(summ$rank==1,"1ST",
			ifelse(summ$rank==2,"2ND",
			ifelse(summ$rank==3|summ$rank==4,"SF",
			ifelse(summ$qua.r=="L"|summ$qua.r=="PL","QF","999"))))

summ$okey <- NULL; summ$class <- NULL; summ$sort <- NULL;

summ$REC <- paste(summ$W,summ$L,sep="-")
summ$COL1 <- paste(summ$record0)
summ$COL2 <- paste(summ$record1)
summ$COL3 <- paste(summ$record2)
summ$COL4 <- paste(summ$qua)
summ$COL5 <- paste(summ$sem)
summ$COL6 <- paste(summ$fin)

##########################################
############# generate header / footer ###
##########################################

header <- vector()
header[1] <- '<html><head><title>OmahaSeries.com</title>'
header[2] <- '<link rel="stylesheet" type="text/css" href="styles.css"></head>' 
header[3] <- '<body><div id="red"></div><div id="yellow"></div>'
header[4] <- '<div id="blue"><h4><a class="blue" href="index.html">WBC</a> -- <a class="blue" href="byyear.html">YEARS</a> -- <a class="blue" href="byteam.html">TEAMS</a></h4></div>'

footer <- vector()
footer[1] <- '<div id="foot"> <a class="foot" href="../index.html">AndyHulme.Net</a></div></body></html>'

##########################################
############# yearly pages ###############
##########################################

u.yr <- unique(summ$yr)

for (i in 1:length(u.yr))
{

# summary data set for given year
summ.yr <- subset(summ, yr == u.yr[i])

summ.yr$class <- (1+(1:nrow(summ.yr))) %% 2

summ.yr$row <- paste(ifelse(summ.yr$rank==999,"<tr><th colspan='8'></th></tr>",""),"<tr class='d",summ.yr$class,"'><td>",sep="",paste(
				paste(summ.yr$rank),
				paste("<a href='",teamlink(rename(summ.yr$team)),".html'>",summ.yr$team,"</a>",sep=""),
				paste(summ.yr$REC),
				paste(summ.yr$COL1),
				paste(summ.yr$COL2),
				paste(summ.yr$COL3),
				paste(summ.yr$COL4),
				paste(summ.yr$COL5),
				paste(summ.yr$COL6),
			sep="</td><td>"),"</td></tr>")

# group standings data set for given year
group.yr <- subset(group, yr == u.yr[i])
group.yr <- group.yr[order(group.yr$rd, group.yr$short, group.yr$grank),]

if (nrow(group.yr)>0) {
group.yr$class <- 0

group.yr$row <- paste("<tr><td>",paste(
				paste("<a href='",teamlink(rename(group.yr$team)),".html'>",group.yr$team,"</a>",sep=""),
				paste(group.yr$W),
				paste(group.yr$L),
				paste(group.yr$GF),
				paste(group.yr$GA),
				sprintf("%.3f", round(group.yr$Pts,3) ),
			sep="</td><td>"),"</td></tr>")
}

# group games data set for given year
games.yr <- subset(games.g, yr == u.yr[i] & result=="W")
games.yr <- games.yr[order(games.yr$GROUP, games.yr$GameNo),]

if (nrow(games.yr)>0) {
games.yr$class <- 0

games.yr$row <- paste("<tr><td>",paste(
				paste("<a href='",teamlink(rename(games.yr$team)),".html'>",games.yr$team,"</a>",sep=""),
				paste(games.yr$score2),
				paste("<a href='",teamlink(rename(games.yr$opponent)),".html'>",games.yr$opponent,"</a>",sep=""),
				ifelse(is.na(games.yr$INN),"",paste(games.yr$INN)),
			sep="</td><td>"),"</td></tr>")
}

### knockout games data set for given year
ko.yr <- subset(games, yr == u.yr[i] & Round==3 & result=="W")

if (nrow(ko.yr)>0) {
ko.yr <- ko.yr[order(ko.yr$GameNo),]

ko.yr$row <- paste("<tr><td>",paste(
				paste("<a href='",teamlink(rename(ko.yr$team)),".html'>",ko.yr$team,"</a>",sep=""),
				paste(ko.yr$score2),
				paste("<a href='",teamlink(rename(ko.yr$opponent)),".html'>",ko.yr$opponent,"</a>",sep=""),
				ifelse(is.na(ko.yr$INN),"",paste(ko.yr$INN)),
			sep="</td><td>"),"</td></tr>")
}

u.reg <- unique(group.yr$grp)

##### HTML #####
sink(paste("../WBC/",u.yr[i],".html",sep=""))

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",u.yr[i],"</center></h2>"))
cat(paste("<center><a href='",u.yr[i-1],".html'> << ",u.yr[i-1],"</a> | ",
	            "<a href='",u.yr[i+1],".html'>",u.yr[i+1]," >> </a></center><br>",sep=""))

cat("<br><table width='80%' align='center'>")
cat("<col width='11%'><col width='20%'><col width='11%'>")
cat(paste("<tr>","<th class='xx2'><font color='white'>Rank</font></th><th class='xx2'> </th><th class='xx2'><font color='white'>Record</font></th><th colspan='3' class='hl2'>Group Stages</th><th colspan='3' class='hl2'>Knockout Rounds</th>","</tr>"))
write.table(summ.yr["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

if (nrow(group.yr)>0) {
for (j in 1:length(u.reg)) 
{
groupr.yr <- subset(group.yr, grp == u.reg[j])
cat(paste("<hr><br><h3><center>",ifelse(u.reg[j]=="Wfrd","Final Group",groupnm(u.reg[j])),"</center></h3>",sep=""))
cat("<table width='60%' align='center'>")
cat(paste("<tr>","<th> </th><th>W</th><th>L</th><th>F</th><th>A</th><th>Pts</th>","</tr>"))
write.table(groupr.yr["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))

gamesr.yr <- subset(games.yr, GROUP == u.reg[j])
cat("<table width='60%' align='center'>")
cat(paste("<tr>","<th> </th><th>Score</th><th> </th><th>Inn</th>","</tr>"))
write.table(gamesr.yr["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
}}

if (nrow(ko.yr)>0) {
cat("<hr><br><center><h3>Knockout</h3></center>")
cat("<br><table width='80%' align='center'>")
cat("<col width='35%'><col width='20%'><col width='35%'><col width='10%'>")
u.rd  <- unique(ko.yr$GROUP)

for (m in 1:length(u.rd)) {
cat(paste("<tr>","<th colspan='4' class='hl'>",u.rd[m],"</th>","</tr>"))
write.table(subset(ko.yr,u.rd[m]==ko.yr$GROUP)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
cat("</table><br>")
}

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# team pages #################
##########################################

u.tm <- unique(summ$team2)

for (i in 1:length(u.tm))
{
print(paste(i,u.tm[i]))

# create summary by year
summ.tm <- subset(summ, team2 == u.tm[i])
summ.tm <- summ.tm[order(-summ.tm$yr),]

summ.tm$class <- (1+(1:nrow(summ.tm))) %% 2

summ.tm$row <- paste("<tr class='d",summ.tm$class,"'><td>",paste(
				paste("<a href='",summ.tm$yr,".html'>",summ.tm$yr,"</a>",sep=""),
				paste("<div style='text-align:left'>",summ.tm$team,"</div>",sep=""),
				summ.tm$rank,
				summ.tm$COL1,
				summ.tm$COL2,
				summ.tm$COL3,
				summ.tm$COL4,
				summ.tm$COL5,
				summ.tm$COL6,
			sep="</td><td>"),"</td></tr>",sep="")

# games by year
games.tm <- subset(games, team2 == u.tm[i])

games.tm <- games.tm[order(-games.tm$yr, games.tm$Round, games.tm$GameNo),]

for (k in 1:nrow(games.tm)) {
if (k==1) { games.tm$flag[k] <- 2 }
if (k >1) { games.tm$flag[k] <- ifelse(games.tm$yr[k] != games.tm$yr[k-1],1,0)}
if (k==1) { games.tm$flag2[k] <- 1 }
if (k >1) { games.tm$flag2[k] <- ifelse(games.tm$yr[k] != games.tm$yr[k-1]|games.tm$GROUP[k] != games.tm$GROUP[k-1],1,0)}
}  

games.tm$row <- paste(ifelse(games.tm$flag==1,"<tr><th colspan='6'></th></tr>",""),"<tr class='d0'><td>",paste(
				ifelse(games.tm$flag >=1,paste("<b><a href='",games.tm$yr,".html'>",games.tm$yr,"</a></b>",sep=""),""),
				ifelse(games.tm$flag2>=1,paste(games.tm$GROUP,sep=""),""),				
				paste("<a href='",teamlink(rename(games.tm$opponent)),".html'>",games.tm$opponent,"</a>",sep=""),
				paste(games.tm$result),
				paste(games.tm$score2),
				paste(games.tm$OT),
			sep="</td><td>"),"</td></tr>",sep="")

##### HTML #####
sink(paste("../WBC/",teamlink(u.tm[i]),".html",sep=""))

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",summ.tm$team[1],"</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat("<col width='11%'><col width='20%'><col width='11%'><col width='11%'><col width='11%'><col width='9%'><col width='9%'><col width='9%'><col width='9%'>")
cat(paste("<tr>","<th colspan='3'></th><th colspan='3' class='hl2'>Group Stages</th><th colspan='3' class='hl2'>Knockout Rounds</th></tr>",sep=""))
cat(paste("<tr>","<th>Year</th><th>Team</th><th>Rank</th><th>Qual</th><th>1st</th><th>2nd</th><th>QF</th><th>SF</th><th>F</th></tr>",sep=""))
write.table(summ.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

cat("<center><h3>Games</h3></center>")
cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Year</th><th>Stage</th><th>Opponent</th><th>Result</th><th>Score</th><th> </th>","</tr>"))
write.table(games.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# by year page ###############
##########################################

byyear <- as.data.frame(u.yr)
names(byyear)[1] <- "yr"

byyear <- merge(byyear,subset(summ,rank==1)[c("yr","team")],by=c("yr"),all.x=T); colnames(byyear)[2] <- "first";
byyear <- merge(byyear,subset(summ,rank==2)[c("yr","team")],by=c("yr"),all.x=T); colnames(byyear)[3] <- "second";
byyear <- merge(byyear,subset(summ,rank==3)[c("yr","team")],by=c("yr"),all.x=T); colnames(byyear)[4] <- "third";
byyear <- merge(byyear,subset(summ,rank==4)[c("yr","team")],by=c("yr"),all.x=T); colnames(byyear)[5] <- "fourth";
byyear <- merge(byyear,subset(games,GROUP=="Final" & result=="W")[c("yr","score2","INN")],by=c("yr"),all.x=T)

teams.yr <- as.data.frame.matrix(table(summ$yr,rep("no.teams",length(summ$yr)) ))
teams.yr$yr <- rownames(teams.yr)
byyear <- merge(byyear,teams.yr,by=c("yr"),all.x=T)

byyear <- byyear[order(-byyear$yr),]

byyear$class <- (1+(1:nrow(byyear))) %% 2

byyear$row <- paste("<tr class='d",byyear$class,"'><td>",paste(
	paste("<a href='",byyear$yr,".html'>",byyear$yr ,"</a>",sep=""),
	byyear$no.teams,
	paste("<a href='",teamlink(rename(byyear$first)),".html'>", byyear$first,"</a>",sep=""),
	ifelse(is.na(byyear$score2),"",paste(byyear$score2)),
	paste("<a href='",teamlink(rename(byyear$second)),".html'>", byyear$second,"</a>",sep=""),
	paste("<a href='",teamlink(rename(byyear$third )),".html'>", byyear$third ,"</a>",sep=""),
	paste("<a href='",teamlink(rename(byyear$fourth)),".html'>", byyear$fourth,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../WBC/byyear.html",sep=""))

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>World Baseball Classic</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Season</th><th>Teams</th><th>Champion</th><th></th><th>Runner-Up</th><th>Third</th><th>Fourth</th></tr>",sep=""))
write.table(byyear["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

##########################################
############# by team page ###############
##########################################

summ$count <- 1
byteam <- as.data.frame(u.tm)
names(byteam)[1] <- "team2"

seas <- aggregate(summ$count, list(summ$team2), sum); names(seas)[names(seas)=="x"] <- "Years"
wins <- aggregate(summ$W    , list(summ$team2), sum); names(wins)[names(wins)=="x"] <- "W";   agg <- merge(seas,wins,by="Group.1");
loss <- aggregate(summ$L    , list(summ$team2), sum); names(loss)[names(loss)=="x"] <- "L";   agg <- merge(agg ,loss,by="Group.1");
gfor <- aggregate(summ$gf   , list(summ$team2), sum); names(gfor)[names(gfor)=="x"] <- "GF";  agg <- merge(agg ,gfor,by="Group.1");
gagt <- aggregate(summ$ga   , list(summ$team2), sum); names(gagt)[names(gagt)=="x"] <- "GA";  agg <- merge(agg ,gagt,by="Group.1");
fyr  <- aggregate(summ$yr   , list(summ$team2), min); names(fyr )[names(fyr )=="x"] <- "FYr"; agg <- merge(agg ,fyr ,by="Group.1");
lyr  <- aggregate(summ$yr   , list(summ$team2), max); names(lyr )[names(lyr )=="x"] <- "LYr"; agg <- merge(agg ,lyr ,by="Group.1");

byteam <- merge(byteam,agg,by.x="team2",by.y="Group.1")

po.tm <- subset(games,Round==3)
po.tm$resultf <- paste(gsub("P","",po.tm$result),po.tm$Round,sep="")
potab <- as.data.frame.matrix(table(po.tm$team2,po.tm$resultf))
potab$team2 <- rownames(potab)

places <- as.data.frame.matrix(table(summ$team2,summ$place))
places$team2 <- rownames(places)

byteam <- merge(byteam,potab ,by="team2",all.x=TRUE)
byteam <- merge(byteam,places,by="team2",all.x=TRUE)

byteam[is.na(byteam)] <- 0

byteam$class <- (1+(1:nrow(byteam))) %% 2
byteam$row <- paste("<tr class='d",byteam$class,"'><td>",paste(
				paste("<a href='",teamlink(byteam$team2),".html'>",byteam$team2,"</a>",sep=""),
				byteam$Years,
				byteam$"1ST",
				byteam$"2ND",
				byteam$W,
				byteam$L,
				paste(byteam$GF,byteam$GA,sep="-"),
				byteam$FYr,
				byteam$LYr,
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../WBC/byteam.html",sep=""))

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>World Baseball Classic</center></h2>"))

cat("<br><table width='80%' align='center'>")
#cat("<col width='20%'><col width='6%'><col width='7%'><col width='7%'><col width='4%'><col width='4%'><col width='4%'><col width='8%'><col width='6%'><col width='6%'><col width='6%'><col width='6%'><col width='4%'><col width='4%'><col width='4%'><col width='4%'>")
cat("<tr><th></th><th>Years</th><th>Champs</th><th>2nds</th><th>W</th><th>L</th><th>RF-RA</th><th>First</th><th>Last</th></tr>")
write.table(byteam["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

##########################################
############# index page #################
##########################################

byyear2 <- byyear[order(-byyear$yr),]
#byyear2 <- byyear2[1:10,]

byteam2 <- byteam[order(-byteam$"1ST", -byteam$"2ND", -byteam$W),]
byteam2 <- byteam2[1:10,]

byyear2$row <- paste("<tr><td>",paste(
	paste("<a href='",byyear2$yr,".html'>",byyear2$yr ,"</a>",sep=""),
	paste("<a href='",teamlink(rename(byyear2$first)),".html'>", byyear2$first,"</a>",sep=""),
	paste(byyear2$score),
	paste("<a href='",teamlink(rename(byyear2$second)),".html'>", byyear2$second,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

byteam2$row <- paste("<tr><td>",paste(
				paste("<a href='",teamlink(byteam2$team),".html'>",byteam2$team,"</a>",sep=""),
				byteam2$"1ST",
				byteam2$"2ND",
				paste(byteam2$W,byteam2$L,sep="-"),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../WBC/index.html",sep=""))

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th colspan='4' class='hl'><div style='font-size:1.5em;'>Results by Year</div></th>","</tr>"))
cat(paste("<tr>","<th>Year</th><th>Champion</th><th></th><th>Runner-up</th>","</tr>"))
write.table(byyear2["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr>","<td colspan='4'><a href='byyear.html'>All Years</a></td>","</tr>"))
cat("</table><br>")

cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th colspan='4' class='hl'><div style='font-size:1.5em;'>Results by Team</div></th>","</tr>"))
cat(paste("<tr>","<th>Team</th><th>Cups</th><th>2nds</th><th>W-T-L</th>","</tr>"))
write.table(byteam2["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr>","<td colspan='4'><a href='byteam.html'>All Teams</a></td>","</tr>"))
cat("</table><br>")

cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th class='hl'><div style='font-size:1.5em;'>Event Results</div></th>","</tr>"))
	cat(paste("<tr>","<td><a href='finals.html'>World Baseball Classic Finals</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='byteam1.html'>Sortable Records</a></td>","</tr>"))
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

##########################################
############# event pages ################
##########################################

finals <- subset(games, Group=="Final" & Result=="W")

finals <- finals[order(-finals$yr),]
finals$class <- (1+(1:nrow(finals))) %% 2

finals$row <- paste("<tr class='d",finals$class,"'><td>",paste(
				paste("<a href='",finals$yr,".html'>",finals$yr,"</a>",sep=""),
				paste("<a href='",teamlink(rename(finals$team)),".html'>",finals$team,"</a>",sep=""),
				paste(finals$score2),
				paste("<a href='",teamlink(rename(finals$opponent)),".html'>",finals$opponent,"</a>",sep=""),
				paste(finals$OT),
		sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../WBC/finals.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>World Baseball Classic Finals</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat("<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th> </th></tr>")
write.table(finals["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# sortable NCAAT records ###
############################

byteam$team <- paste(byteam$team2)

byteam$PPG <- byteam$W /(byteam$W + byteam$L)
byteam$GP <- byteam$W + byteam$L

for (ll in 1:10) {
sink(paste("../WBC/byteam",ll,".html",sep=""))

if (ll==1) { byteam <- byteam[order(byteam$team), ] }
if (ll==2) { byteam <- byteam[order(-byteam$Years, -byteam$GP, -byteam$PPG, byteam$team), ] }
if (ll==3) { byteam <- byteam[order(-byteam$"1ST", -byteam$"2ND", -byteam$GP, -byteam$PPG, byteam$team), ] }
if (ll==4) { byteam <- byteam[order(-byteam$"2ND", -byteam$"1ST", -byteam$GP, -byteam$PPG, byteam$team), ] }
if (ll==5) { byteam <- byteam[order(-byteam$GP, -byteam$PPG, byteam$team), ] }
if (ll==6) { byteam <- byteam[order(-byteam$W, -byteam$PPG, byteam$team), ] }
if (ll==7) { byteam <- byteam[order(-byteam$L, -byteam$PPG, byteam$team), ] }
if (ll==8) { byteam <- byteam[order(-byteam$PPG, -byteam$W, byteam$team), ] }
if (ll==9) { byteam <- byteam[order(byteam$FYr, byteam$LYr), ] }

byteam$class2 <- (1+(1:nrow(byteam))) %% 2
byteam$row3   <- paste("<tr class='d",byteam$class2,"'><td>",paste(
				paste("<a href='",teamlink(byteam$team),".html'>",byteam$team,"</a>",sep=""),
				byteam$Years,
				byteam$"1ST",
				byteam$"2ND",
				byteam$GP,
				byteam$W,
				byteam$L,
				sprintf("%.3f", round(byteam$PPG,3) ),
				paste(byteam$GF,byteam$GA,sep="-"),
				paste("<a href='",byteam$FYr,".html'>",byteam$FYr,"</a>",sep=""),
				paste("<a href='",byteam$LYr,".html'>",byteam$LYr,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>World Baseball Classic Records</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr>", "<th",ifelse(ll==1," class='hl3'",""),"><a href='byteam1.html'>Team</a></th>",
			"<th",ifelse(ll==2," class='hl3'",""),"><a href='byteam2.html'>Apps.</a></th>",
			"<th",ifelse(ll==3," class='hl3'",""),"><a href='byteam3.html'>Champs</a></th>",
			"<th",ifelse(ll==4," class='hl3'",""),"><a href='byteam4.html'>2nds</a></th>",
			"<th",ifelse(ll==5," class='hl3'",""),"><a href='byteam5.html'>GP</a></th>",
			"<th",ifelse(ll==6," class='hl3'",""),"><a href='byteam6.html'>W</a></th>",
			"<th",ifelse(ll==7," class='hl3'",""),"><a href='byteam7.html'>L</a></th>",
			"<th",ifelse(ll==8," class='hl3'",""),"><a href='byteam8.html'>Pct</a></th><th>RF-RA</th>",
			"<th",ifelse(ll==9," class='hl3'",""),"><a href='byteam9.html'>First</a></th><th>Last</th>","</tr>",sep=""))
write.table(byteam["row3"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}




