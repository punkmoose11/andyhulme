
############# import data
data <- read.csv("../Data/cwshtml.csv")
info <- read.csv("../Data/cwsteams.csv")
info <- info [c("team","yr","nickname","conference","record","seed","natseed","host","suphost")]

data$cwsno <- ifelse(data$regional == "CWS" ,data$cwsno + data$yr*10000 + 9000, data$cwsno)
data$roundno <- as.numeric(ifelse((grepl("Champi",data$round)|grepl("Final",data$round))&data$round!="Finals",99,
			ifelse(grepl("Round",data$round)&data$round!="Round Rock",substring(data$round,7,7),
			ifelse(grepl("Bracket",data$round),substring(data$round,9,9),999))))
data$date <- as.Date(data$date, format="%Y-%m-%d")

teamlink <- function(x){teamx = tolower(gsub("&","",gsub(".","",gsub("'","",gsub(" ", "",x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE))}

############# create df with one record for each team per game
games.1 <- subset(data, wsc != 0)
games.1$team <- games.1$winner
games.1$opponent <- games.1$loser
games.1$rf <- games.1$wsc
games.1$ra <- games.1$lsc
games.1$result  <- "W"
games.1 <- games.1[c("yr","cwsno","date","regional","round","roundno","gameno","team","opponent",
				"rf","ra","result","inn","notes")]

games.2 <- subset(data, wsc != 0)
games.2$team <- games.2$loser
games.2$opponent <- games.2$winner
games.2$rf <- games.2$lsc
games.2$ra <- games.2$wsc
games.2$result  <- "L"
games.2 <- games.2[c("yr","cwsno","date","regional","round","roundno","gameno","team","opponent",
				"rf","ra","result","inn","notes")]

games.3 <- subset(data, loser == "(automatic qualifier)")
games.3$team <- games.3$winner
games.3$opponent <- games.3$loser
games.3$rf <- " "
games.3$ra <- " "
games.3$result  <- "A"
games.3 <- games.3[c("yr","cwsno","date","regional","round","roundno","gameno","team","opponent",
				"rf","ra","result","inn","notes")]

games.4 <- subset(data, wsc == 0)

if (nrow(games.4)>0) {
games.4$team <- games.4$winner
games.4$opponent <- games.4$loser
games.4$rf <- " "
games.4$ra <- " "
games.4$result  <- "P"
games.4 <- games.4[c("yr","cwsno","date","regional","round","roundno","gameno","team","opponent",
				"rf","ra","result","inn","notes")]
}

games.5 <- subset(data, wsc == 0)

if (nrow(games.5)>0) {
games.5$team <- games.5$loser
games.5$opponent <- games.5$winner
games.5$rf <- " "
games.5$ra <- " "
games.5$result  <- "Q"
games.5 <- games.5[c("yr","cwsno","date","regional","round","roundno","gameno","team","opponent",
				"rf","ra","result","inn","notes")]
}

if (nrow(games.4)>0 & nrow(games.5)>0) {games <- rbind(games.1,games.2,games.3,games.4,games.5)
} else {games <- rbind(games.1,games.2,games.3)}

games$result.f <- ifelse(grepl("SR",games$round),paste(games$result,"S",sep=""),
			 ifelse(grepl("Finals",games$round),paste(games$result,"D",sep=""),
                   ifelse(games$regional == "CWS" & grepl("Final",games$gameno),paste(games$result,"F",sep=""),
			 ifelse(games$regional == "CWS",paste(games$result,"C",sep=""),paste(games$result,"R",sep="")))))

############# create dataset with yearly summaries
games$key <- paste(games$yr, games$team, sep="_")
summ <- as.data.frame.matrix(table(games$key,games$result.f))
summ$key <- rownames(summ)
summ$yr <- as.numeric(substr(summ$key,1,4))
summ$team <- substr(summ$key,6,24)
summ$W  <- summ$WR + summ$WS + summ$WC + summ$WF + summ$WD
summ$L  <- summ$LR + summ$LS + summ$LC + summ$LF + summ$LD
summ$WT <- summ$WC + summ$WF
summ$LT <- summ$LC + summ$LF

summ$sort <- 1000 * summ$WF + 100 * summ$LF + 10*summ$WT + summ$LT
summ$crank <- ave(-summ$sort, summ$yr, FUN = function(x) rank(x, ties.method = "min") )
summ$crank <- ifelse(summ$WT+summ$LT>0,summ$crank,999) #ifelse(summ$PC+summ$QC,9,999))
summ$crankc <- ifelse(summ$crank==1,"1ST",
			ifelse(summ$crank==2,"2ND",
			ifelse(summ$crank==3,"3RD",
			ifelse(summ$crank< 9,paste(summ$crank,"TH",sep=""),""))))

summ <- merge(summ,info,by=c("team","yr"))
summ <- summ[order(summ$yr, summ$crank, summ$team), ]

regs <- unique(games[c("yr","team","regional","round")])
regs <- subset(regs, regional != "CWS" & grepl("SR",round) == FALSE & grepl("Finals",round) == FALSE)
summ <- merge(summ,regs,by=c("team","yr"))

regsh <- unique(summ[c("yr","regional","round","natseed")])
regsh <- subset(regsh, yr>= 1999 & is.na(natseed)==FALSE & natseed<=8)
regsh$sortn <- 1
summ <- merge(summ,regsh[c("yr","regional","round","sortn")],by=c("yr","regional","round"),all.x=TRUE)
summ$sortn <- ifelse(is.na(summ$sortn),99,summ$sortn)

############# generate summary by school
teams <- as.data.frame.matrix(table(games$team,games$result.f))
teams$team <- rownames(teams)
teams$W  <- teams$WR + teams$WS + teams$WC + teams$WF + teams$WD
teams$L  <- teams$LR + teams$LS + teams$LC + teams$LF + teams$LD
teams$WT <- teams$WC + teams$WF
teams$LT <- teams$LC + teams$LF
teams$WX <- teams$WR + teams$WS + teams$WD
teams$LX <- teams$LR + teams$LS + teams$LD

nicknames <- unique(summ[c("team","nickname")])
teams <- merge(teams,nicknames,by="team")

apps <- as.data.frame.matrix(table(summ$team,summ$crank))
apps$team <- rownames(apps)
teams <- merge(teams,apps,by="team")
teams$capps <- teams$"1" + teams$"2" + teams$"3" + teams$"4" + teams$"5" + teams$"7"
teams$rapps <- teams$"1" + teams$"2" + teams$"3" + teams$"4" + teams$"5" + teams$"7" + teams$"99"

summc <- subset(summ, summ$WT+summ$LT>0)
fyearc <- aggregate(summc$yr, list(summc$team), min)
names(fyearc)[names(fyearc)=="x"] <- "fyearc"
teams <- merge(teams,fyearc,by.x="team",by.y="Group.1",all.x=TRUE)

lyearc <- aggregate(summc$yr, list(summc$team), max)
names(lyearc)[names(lyearc)=="x"] <- "lyearc"
teams <- merge(teams,lyearc,by.x="team",by.y="Group.1",all.x=TRUE)

fyear <- aggregate(summ$yr, list(summ$team), min)
names(fyear)[names(fyear)=="x"] <- "fyear"
teams <- merge(teams,fyear,by.x="team",by.y="Group.1")

lyear <- aggregate(summ$yr, list(summ$team), max)
names(lyear)[names(lyear)=="x"] <- "lyear"
teams <- merge(teams,lyear,by.x="team",by.y="Group.1")

############# generate summary page for years
byyear1 <- subset(summ, crank == 1)
byyear1$champion <- byyear1$team
byyear1 <- byyear1[c("yr","champion","WF","LF","WT","LT","W","L")]

byyear2 <- subset(summ, crank == 2)
byyear2$runnerup <- byyear2$team
byyear2 <- byyear2[c("yr","runnerup")]

finals <- subset(games, gameno == "Final" & result.f == "WF")
finals <- finals[c("yr","rf","ra","inn")]

byyear <- merge(byyear1,byyear2,by="yr")
byyear <- merge(byyear,finals,by="yr",all.x=TRUE)

teams.yr <- as.data.frame.matrix(table(summ$yr,rep("no.teams",length(summ$yr)) ))
teams.yr$yr <- rownames(teams.yr)

byyear <- merge(byyear,teams.yr,by="yr")

##########################################
############# generate header / footer ###
##########################################

header <- vector()
header[1] <- '<html><head><title>College World Series</title>'
header[2] <- '<link rel="stylesheet" type="text/css" href="styles.css"></head>' 
header[3] <- '<body><div id="red"><h1><a class="red" href="index.html">College World Series</a></h1></div><div id="yellow"></div>'
header[4] <- '<div id="blue"><h4><a class="blue" href="byyear.html">YEARS</a> -- <a class="blue" href="byteam.html">TEAMS</a> -- <a class="blue" href="stats.html">STATS</a></h4></div>'

footer <- vector()
footer[1] <- '<div id="foot"> <a class="foot" href="../index.html">AndyHulme.Net</a></div></body></html>'

############# generate by year page
byyear$class <- (1+(1:nrow(byyear))) %% 2
byyear$row <- paste("<tr class='d",byyear$class,"'><td>",paste(
				paste("<a href='",byyear$yr,".html'>",byyear$yr,"</a>",sep=""),
				paste(byyear$no.teams),
				paste("<a href='",teamlink(byyear$champion),".html'>",byyear$champion,"</a>",sep=""),
				paste(byyear$WT,"-",byyear$LT," (",byyear$W,"-",byyear$L,")",sep=""),
				paste("<b>",ifelse(byyear$yr >= 2003,paste(byyear$WF,byyear$LF,sep="-"),paste(byyear$rf,"-",byyear$ra," ",ifelse(is.na(byyear$inn),"",paste("(",byyear$inn,")",sep="")),sep="")),"</b>"),
				paste("<a href='",teamlink(byyear$runnerup),".html'>",byyear$runnerup,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/byyear.html",sep=""))

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>NCAA Tournament Results by Year</center></h2><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th><br>Year</th><th><br>Teams</th><th><br>Champion</th><th>Record<br>CWS (NCAAT)</th>",
	"<th>Final<br>Score</th><th><br>Runner-up</th>","</tr>"))
write.table(byyear["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############# generate by team page
teams$class <- (1+(1:nrow(teams))) %% 2
teams$row <- paste("<tr class='d",teams$class,"'><td>",paste(
				paste("<a href='",teamlink(teams$team),".html'>",teams$team,"</a>",sep=""),
				ifelse(is.na(teams$capps),"&nbsp;",teams$capps),
				ifelse(teams$WT+teams$LT>0,paste(teams$WT,teams$LT,sep="-"),"&nbsp;"),
				ifelse(teams$"1" != 0,teams$"1","&nbsp;"),
				ifelse(teams$"2" != 0,teams$"2","&nbsp;"),
				ifelse(teams$WT+teams$LT==0,"&nbsp;",paste("<a href='",teams$fyearc,".html'>",teams$fyearc,"</a>",sep="")),
				ifelse(teams$WT+teams$LT==0,"&nbsp;",paste("<a href='",teams$lyearc,".html'>",teams$lyearc,"</a>",sep="")),
				ifelse(is.na(teams$rapps),"&nbsp;",teams$rapps),
				paste(teams$W,teams$L,sep="-"),
				paste("<a href='",teams$fyear,".html'>",teams$fyear,"</a>",sep=""),
				paste("<a href='",teams$lyear,".html'>",teams$lyear,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/byteam.html",sep=""))

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>NCAA Tournament Results by Team</center></h2><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th> </th><th colspan='6' class='hl'>College World Series</th><th colspan='4' class='hl'>Entire NCAA Tournament</th>","</tr>"))
cat(paste("<tr>","<th>Team</th><th>App</th>",
	"<th>W-L</th><th>Champ</th><th>R-up</th><th>Debut</th><th>Last</th>",
	"<th>App.</th><th>W-L</th><th>Debut</th><th>Last</th>","</tr>"))

write.table(teams["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############# generate homepage
byyear$row2 <- paste("<tr><td>",paste(
				paste("<a href='",byyear$yr,".html'>",byyear$yr,"</a>",sep=""),
				byyear$no.teams,
				paste("<a href='",teamlink(byyear$champion),".html'>",byyear$champion,"</a>",sep=""),
				paste("<b>",ifelse(byyear$yr >= 2003,paste(byyear$WF,byyear$LF,sep="-"),paste(byyear$rf,"-",byyear$ra," ",ifelse(is.na(byyear$inn),"",paste("(",byyear$inn,")",sep="")),sep="")),"</b>"),
				paste("<a href='",teamlink(byyear$runnerup),".html'>",byyear$runnerup,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>")

teams$row2 <- paste("<tr><td>",paste(
				paste("<a href='",teamlink(teams$team),".html'>",teams$team,"</a>",sep=""),
				ifelse(is.na(teams$capps),"&nbsp;",teams$capps),
				ifelse(teams$WT+teams$LT>0,paste(teams$WT,teams$LT,sep="-"),"&nbsp;"),
				ifelse(is.na(teams$rapps),"&nbsp;",teams$rapps),
				paste(teams$W,teams$L,sep="-"),
			sep="</td><td>"),"</td></tr>")

byyearx <- byyear[order(-byyear$yr),]
byyearx <- byyearx[1:10,]

teamsx <- teams[order(-teams$capps, -teams$WT),]
teamsx <- teamsx[1:10,]

sink(paste("../CWS/index.html",sep=""))

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th colspan='5' class='hl'><div style='font-size:1.5em;'>Results by Year</div></th>","</tr>"))
cat(paste("<tr>","<th>Year</th><th>Teams</th><th>Champion</th>","<th> </th><th>Runner-up</th>","</tr>"))
write.table(byyearx["row2"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr>","<td colspan='5'><a href='byyear.html'>All Years</a></td>","</tr>"))
cat("</table><br>")

cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th colspan='5' class='hl'><div style='font-size:1.5em;'>Results by Team</div></th>","</tr>"))
cat(paste("<tr>","<th>Team</th><th>CWS App</th>","<th>CWS W-L</th><th>Total App</th><th>Total W-L</th>","</tr>"))
write.table(teamsx["row2"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr>","<td colspan='5'><a href='byteam.html'>All Teams</a></td>","</tr>"))
cat("</table><br>")

cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th class='hl'><div style='font-size:1.5em;'>Statistics</div></th>","</tr>"))
cat(paste("<tr>","<td><a href='byteam1.html'>Sortable College World Series Records</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='byteamr1.html'>Sortable NCAA Tournament Records</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='supers.html'>Super Regional Winners and Results (since 1999)</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='regnls2.html'>Regional Winners and Finals (since 1999)</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='regnls1.html'>Regional Winners and Finals (1975-1998)</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='dists.html'>District Winners (1951-1974)</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='stats.html'>More Stats</a></td>","</tr>"))
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

#######################################
############# generate yearly pages ###
#######################################

u.yr <- unique(regs[order(regs$yr),]$yr)
if (! "PC" %in% names(summ)) {summ$PC<-0} 
if (! "QC" %in% names(summ)) {summ$QC<-0} 

for (i in 1:length(u.yr))
{

# create summary for given year
summ.yr <- subset(summ, yr == u.yr[i] & WT+LT+PC+QC>0)

if (nrow(summ.yr)>0) {
summ.yr <- summ.yr[order(summ.yr$crank, summ.yr$team),]

summ.yr$row <- paste("<tr><td>",paste(
				summ.yr$crankc,
				paste(ifelse(is.na(summ.yr$natseed),"",paste("<h5>",summ.yr$natseed,"</h5> ",sep="")),"<a href='",teamlink(summ.yr$team),".html'>",summ.yr$team,"</a>",sep=""),
				paste(summ.yr$WC,summ.yr$LC,sep="-"),
				ifelse(summ.yr$WF+summ.yr$LF != 0,paste(summ.yr$WF,summ.yr$LF,sep="-"),"&nbsp;"),
				paste(summ.yr$WT,summ.yr$LT,sep="-"),
			sep="</td><td>"),"</td></tr>")
}

summr.yr <- subset(summ, yr == u.yr[i])
summr.yr <- summr.yr[order(summr.yr$regional, summr.yr$sortn, summr.yr$round, -summr.yr$WR, summr.yr$LR, summr.yr$seed),]

for (k in 1:nrow(summr.yr)) {
if (k==1) { summr.yr$flag[k] <- 2 }
if (k >1) { summr.yr$flag[k] <- ifelse(summr.yr$regional[k] != summr.yr$regional[k-1] | summr.yr$round[k] != summr.yr$round[k-1],1,0)}

if (k==1) { summr.yr$flag2[k] <- 0 }
if (k >1) { summr.yr$flag2[k] <- ifelse(summr.yr$regional[k] == summr.yr$regional[k-1] & summr.yr$round[k] != summr.yr$round[k-1],1,0)}
} 

summr.yr$row <- paste(ifelse(summr.yr$flag2==1,"<tr><th colspan='7'></th></tr>",""),"<tr><td>",paste(
				ifelse(summr.yr$flag>=1,ifelse(summr.yr$round != "",paste("<b>",summr.yr$round,"</b>",sep=""),paste("<b>",summr.yr$regional,"</b>",sep="")),""),
				paste(ifelse(is.na(summr.yr$seed),"",paste("<h5>",summr.yr$seed,"</h5> ",sep="")),
					"<a href='",teamlink(summr.yr$team),".html'>",summr.yr$team,"</a>",
					ifelse(summr.yr$yr < 1999 | is.na(summr.yr$natseed),"",paste(" <h5>(",summr.yr$natseed,")</h5> ",sep="")),
					ifelse(summr.yr$yr < 1999 & summr.yr$host=="H"," <h5>(H)</h5> ",""),sep=""),
				ifelse(summr.yr$AR != 0,"(A)",paste(summr.yr$WR,summr.yr$LR,sep="-")),
				ifelse(summr.yr$WS+summr.yr$LS+summr.yr$WD+summr.yr$LD != 0,paste(summr.yr$WS+summr.yr$WD,summr.yr$LS+summr.yr$LD,sep="-"),"<!--SUPER--!>"),
				ifelse(summr.yr$WT+summr.yr$LT != 0,paste("<a href='#CWS'>",summr.yr$WT,"-",summr.yr$LT,"</a>",sep=""),"&nbsp;"),
				paste(summr.yr$W,summr.yr$L,sep="-"),
			sep="</td><td>"),"</td></tr>",sep="")

# remove super column
for (m in 1:nrow(summr.yr)) {
if (1949 < summr.yr$yr[m] & summr.yr$yr[m] < 1999) { summr.yr$row[m] <- gsub("<td><!--SUPER--!></td>", "", summr.yr$row[m]) }}

# create list of games
games.yr <- subset(games, yr == u.yr[i] & (result=="W"|result == "P")  & regional=="CWS")
games.yr <- games.yr[order(games.yr$roundno, games.yr$cwsno, games.yr$date, games.yr$cwsno),]

if (nrow(games.yr)>0) {
for (k in 1:nrow(games.yr)) {
if (k==1) { games.yr$flag[k] <- 2 }
if (k >1) { games.yr$flag[k] <- ifelse(games.yr$round[k] != games.yr$round[k-1],1,0) }

if (k==1) { games.yr$flag2[k] <- 1 }
if (k >1) { games.yr$flag2[k] <- ifelse(games.yr$date[k] != games.yr$date[k-1] | games.yr$round[k] != games.yr$round[k-1],1,0) }
} 

games.yr$row <- paste(ifelse(games.yr$flag==1,"<tr><th colspan='8'></th></tr>",""),"<tr><td>",paste(
				ifelse(games.yr$flag >=1,paste("<b>",games.yr$round,"</b>",sep=""),""),
				ifelse(games.yr$flag2>=1,gsub(" 0"," ",format(as.Date(games.yr$date,format = "%m/%d/%y"),"%b %d")),""),
				games.yr$gameno,
				paste("<a href='",teamlink(games.yr$team),".html'>",games.yr$team,"</a>",sep=""),
				ifelse(is.na(games.yr$rf)&is.na(games.yr$ra),"&nbsp;",paste(games.yr$rf,games.yr$ra,sep=" - ")),
				paste("<a href='",teamlink(games.yr$opponent),".html'>",games.yr$opponent,"</a>",sep=""),
				ifelse(is.na(games.yr$inn),"&nbsp;",games.yr$inn),
				games.yr$notes,
			sep="</td><td>"),"</td></tr>")
}

gamesr.yr <- subset(games, yr == u.yr[i] & (result == "W"|result == "A"|result == "P") & regional!="CWS")
gamesr.yr <- gamesr.yr[order(gamesr.yr$cwsno),]

for (k in 1:nrow(gamesr.yr)) {
if (k==1) { gamesr.yr$flag[k] <- 2 }
if (k >1) { gamesr.yr$flag[k] <- ifelse(gamesr.yr$regional[k] != gamesr.yr$regional[k-1] | gamesr.yr$round[k] != gamesr.yr$round[k-1],1,0) }

if (k==1) { gamesr.yr$flag2[k] <- 0 }
if (k >1) { gamesr.yr$flag2[k] <- ifelse(gamesr.yr$regional[k] == gamesr.yr$regional[k-1] & gamesr.yr$round[k] != gamesr.yr$round[k-1],1,0) }
} 

gamesr.yr$row <- paste(ifelse(gamesr.yr$flag2==1,"<tr><th colspan='6'></th></tr>",""),"<tr><td>",paste(
				ifelse(gamesr.yr$flag>=1,ifelse(gamesr.yr$round != "",paste("<b>",gamesr.yr$round,"</b>",sep=""),paste("<b>",gamesr.yr$regional,"</b>",sep="")),""),
				gamesr.yr$gameno,
				paste("<a href='",teamlink(gamesr.yr$team),".html'>",gamesr.yr$team,"</a>",sep=""),
				ifelse(gamesr.yr$rf==""&gamesr.yr$ra=="","&nbsp;",paste(gamesr.yr$rf,gamesr.yr$ra,sep=" - ")),
				ifelse(gamesr.yr$result=="A",paste(gamesr.yr$opponent),paste("<a href='",teamlink(gamesr.yr$opponent),".html'>",gamesr.yr$opponent,"</a>",sep="")),
				ifelse(is.na(gamesr.yr$inn),"&nbsp;",gamesr.yr$inn),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/",u.yr[i],".html",sep=""))

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",u.yr[i],"</center></h2>"))

if (u.yr[i]!=1947 & u.yr[i]!=max(u.yr)) {
cat(paste("<center><a href='",u.yr[i-1],".html'> << ",u.yr[i-1],"</a> | ",
	            "<a href='",u.yr[i+1],".html'>",u.yr[i+1]," >> </a></center><br>",sep=""))
}
if (u.yr[i]==1947) {
cat(paste("<center><a href='",u.yr[i+1],".html'>",u.yr[i+1]," >> </a></center><br>",sep=""))
}
if (u.yr[i]==max(u.yr)) {
cat(paste("<center><a href='",u.yr[i-1],".html'> << ",u.yr[i-1],"</a> | ",u.yr[i]+1," >> </center><br>",sep=""))
}

if (nrow(games.yr)>0) {
cat(paste("<br><h3><center><a name='CWS'></a>College World Series</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th>Rank</th><th>Team</th><th>Bracket</th><th>Final</th><th>Overall</th>","</tr>"))
write.table(summ.yr["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
cat("<!--Insert Bracket--!>")

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Round</th><th>Date</th><th>Game</th><th>Winner</th><th>Score</th><th>Loser</th><th>Inn</th><th> </th>","</tr>"))
write.table(games.yr["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
}

u.reg <- unique(summr.yr$regional)

for (j in 1:length(u.reg)) 
{
summr.yr.r <- subset(summr.yr, regional == u.reg[j])
cat(paste("<hr><br><h3><center><a name='",teamlink(u.reg[j]),"'></a>",u.reg[j],"</center></h3>",sep=""))
cat("<table width='80%' align='center'>")
if (u.yr[i]<=1949) {cat(paste("<tr>","<th>Regional</th><th>Team</th><th>District</th><th>Regional</th><th>CWS</th><th>Total</th>","</tr>"))
} else if (u.yr[i]<=1974) {cat(paste("<tr>","<th>Regional</th><th>Team</th><th>District</th><th>CWS</th><th>Total</th>","</tr>"))
} else if (u.yr[i]<=1998) {cat(paste("<tr>","<th>Regional</th><th>Team</th><th>Regional</th><th>CWS</th><th>Total</th>","</tr>"))
} else if (u.yr[i]<=9999) {cat(paste("<tr>","<th>Regional</th><th>Team</th><th>Regional</th><th>Super</th><th>CWS</th><th>Total</th>","</tr>"))}
write.table(summr.yr.r["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))

gamesr.yr.r <- subset(gamesr.yr, regional == u.reg[j])
cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th>Regional</th><th>Game</th><th>Winner</th><th>Score</th><th>Loser</th><th>Inn</th>","</tr>"))
write.table(gamesr.yr.r["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
}

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

########################################
############# generate team pages ######
########################################

u.tm <- unique(games$team)
summ <- summ[order(summ$team, summ$yr), ]
games <- games[order(games$cwsno), ]

for (i in 1:length(u.tm))
{

# create totals for given team
teams.tm <- subset(teams, team == u.tm[i])

teams.tm$row <- paste(  ifelse(teams.tm$WT+teams.tm$LT==0,"",paste("<tr><td>CWS Appearences:</td><td>",teams.tm$capps,"</td></tr>")),
				ifelse(teams.tm$WT+teams.tm$LT==0,"",paste("<tr><td>CWS Record:</td><td>",paste(teams.tm$WT,teams.tm$LT,sep="-"),"</td></tr>")),
				ifelse(teams.tm$"1"+teams.tm$"2"==0,"",paste("<tr><td>Champions:</td><td>",teams.tm$"1","</td></tr>")),
				ifelse(teams.tm$"1"+teams.tm$"2"==0,"",paste("<tr><td>Runners-up:</td><td>",teams.tm$"2","</td></tr>")),
				"<tr><td>Tournament Appearences:</td><td>",teams.tm$rapps,"</td></tr>",
				"<tr><td>Tournament Record:</td><td>",paste(teams.tm$W,teams.tm$L,sep="-"),"</td></tr>")

# create summary for given team
summ.tm <- subset(summ, team == u.tm[i])
summ.tm$class <- (1+(1:nrow(summ.tm))) %% 2

summ.tm$row <- paste("<tr class='d",summ.tm$class,"'><td>",paste(
				paste("<a href='",summ.tm$yr,".html'>",summ.tm$yr,"</a>",sep=""),
				paste(summ.tm$W,summ.tm$L,sep="-"),
				paste("<a href='",summ.tm$yr,".html#",teamlink(summ.tm$regional),"'>",
					ifelse(summ.tm$round != "",paste(summ.tm$round),paste(summ.tm$regional)),"</a>",sep=""),
				ifelse(summ.tm$WR+summ.tm$LR != 0,paste(summ.tm$WR,summ.tm$LR,sep="-"),ifelse(summ.tm$AR>0,"(A)","&nbsp;")),
				ifelse(summ.tm$WS+summ.tm$LS+summ.tm$WD+summ.tm$LD != 0,paste(summ.tm$WS+summ.tm$WD,summ.tm$LS+summ.tm$LD,sep="-"),"&nbsp;"),
				ifelse(summ.tm$crank<9,paste("<a href='",summ.tm$yr,".html#CWS'>",summ.tm$crankc,"</a>",sep=""),"&nbsp;"),
				ifelse(summ.tm$WC+summ.tm$LC != 0,paste(summ.tm$WC,summ.tm$LC,sep="-"),"&nbsp;"),
				ifelse(summ.tm$WF+summ.tm$LF != 0,paste(summ.tm$WF,summ.tm$LF,sep="-"),"&nbsp;"),
			sep="</td><td>"),"</td></tr>",sep="")

# create list of games
games.tm <- subset(games, team == u.tm[i])
games.tm <- games.tm[order(games.tm$team, games.tm$cwsno),]
games.tm$name <- ifelse(games.tm$round!=""&games.tm$regional!="CWS",paste(games.tm$round),paste(games.tm$regional))

for (k in 1:nrow(games.tm)) {
if (k==1) { games.tm$classtm[k] <- 0 }
if (k >1) { games.tm$classtm[k] <- ifelse(games.tm$yr[k] != games.tm$yr[k-1],(1+games.tm$classtm[k-1]) %% 2,games.tm$classtm[k-1])}
if (k==1) { games.tm$flag[k] <- 2 }
if (k >1) { games.tm$flag[k] <- ifelse(games.tm$yr[k] != games.tm$yr[k-1],1,0)}
if (k==1) { games.tm$flag2[k] <- 1 }
if (k >1) { games.tm$flag2[k] <- ifelse(games.tm$yr[k] != games.tm$yr[k-1]|games.tm$name[k] != games.tm$name[k-1],1,0)}
}  

games.tm$classtm <- ifelse(games.tm$name=="CWS",2,games.tm$classtm)

games.tm$row <- paste(ifelse(games.tm$flag==1,"<tr><th colspan='7'></th></tr>",""),"<tr class='d",games.tm$classtm,"'><td>",paste(
				ifelse(games.tm$flag >=1,paste("<b><a href='",games.tm$yr,".html'>",games.tm$yr,"</a></b>",sep=""),""),
				ifelse(games.tm$flag2>=1,paste("<b><a href='",games.tm$yr,".html#",teamlink(games.tm$regional),"'>",
					games.tm$name,"</a></b>",sep=""),""),				
				games.tm$gameno,
				ifelse(games.tm$result=="A",paste(games.tm$opponent),paste("<a href='",teamlink(games.tm$opponent),".html'>",games.tm$opponent,"</a>",sep="")),
				ifelse(games.tm$result=="W","W&nbsp;&nbsp;&nbsp;",ifelse(games.tm$result=="L","&nbsp;&nbsp;&nbsp;L","")),
				ifelse(games.tm$rf==""&games.tm$ra=="","&nbsp;",paste(games.tm$rf,games.tm$ra,sep="-")),
				ifelse(is.na(games.tm$inn),"&nbsp;",games.tm$inn),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/",teamlink(u.tm[i]),".html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",paste(teams.tm$team,teams.tm$nickname,sep=" "),"</center></h2><br>"))

cat("<table width='50%' align='center'><col width='50%'><col width='50%'>")
write.table(teams.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

cat("<hr>")

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th> </th><th> </th><th colspan='3' class='hl'>Regionals</th><th colspan='3' class='hl'>College World Series</th>","</tr>"))
cat(paste("<tr>","<th>Year</th><th>Total</th><th>Regional</th><th>Regional</th><th>Super</th><th>Rank</th><th>Bracket</th><th>Final</th>","</tr>"))
write.table(summ.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

cat("<hr>")

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Year</th><th>Round</th><th>Game</th><th>Opponent</th><th>Result</th><th>Score</th><th>Inn</th>","</tr>"))
write.table(games.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

############################
#### generate stat pages ###
############################

############################
# sortable CWS records
############################

teamsc <- subset(teams, capps>0)

for (kk in 1:7) {
sink(paste("../CWS/byteam",kk,".html",sep=""))

if (kk==1) { teamsc <- teamsc[order(teamsc$team), ] }
if (kk==2) { teamsc <- teamsc[order(-teamsc$capp, -teamsc$WT / (teamsc$WT + teamsc$LT), -teamsc$WT+teamsc$LT, teamsc$team), ] }
if (kk==3) { teamsc <- teamsc[order(-teamsc$WT - teamsc$LT, -teamsc$WT / (teamsc$WT + teamsc$LT), -teamsc$WT+teamsc$LT, teamsc$team), ] }
if (kk==4) { teamsc <- teamsc[order(-teamsc$WT, -teamsc$WT / (teamsc$WT + teamsc$LT),  -teamsc$WT+teamsc$LT, teamsc$team), ] }
if (kk==5) { teamsc <- teamsc[order(-teamsc$WT / (teamsc$WT + teamsc$LT), -teamsc$WT+teamsc$LT, -teamsc$WT, teamsc$team), ] }
if (kk==6) { teamsc <- teamsc[order(-teamsc$"1", -teamsc$"2", -teamsc$WT / (teamsc$WT + teamsc$LT), -teamsc$WT+teamsc$LT, teamsc$team), ] }
if (kk==7) { teamsc <- teamsc[order(teamsc$fyearc, teamsc$lyearc), ] }

teamsc$class <- (1+(1:nrow(teamsc))) %% 2
teamsc$row3  <- paste("<tr class='d",teamsc$class,"'><td>",paste(
				paste("<a href='",teamlink(teamsc$team),".html'>",teamsc$team,"</a>",sep=""),
				teamsc$capps,
				teamsc$WT + teamsc$LT,
				teamsc$WT,
				teamsc$LT,
				sprintf("%.3f", round(teamsc$WT / (teamsc$WT + teamsc$LT),3) ),
				teamsc$"1",
				teamsc$"2",
				paste("<a href='",teamsc$fyear,".html'>",teamsc$fyearc,"</a>",sep=""),
				paste("<a href='",teamsc$lyear,".html'>",teamsc$lyearc,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>College World Series Records</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr>", "<th",ifelse(kk==1," class='hl'",""),"><a href='byteam1.html'>Team</a></th>",
			"<th",ifelse(kk==2," class='hl'",""),"><a href='byteam2.html'>Apps.</a></th>",
			"<th",ifelse(kk==3," class='hl'",""),"><a href='byteam3.html'>GP</a></th>",
			"<th",ifelse(kk==4," class='hl'",""),"><a href='byteam4.html'>W</a></th><th>L</th>",
			"<th",ifelse(kk==5," class='hl'",""),"><a href='byteam5.html'>PCT</a></th>",
			"<th",ifelse(kk==6," class='hl'",""),"><a href='byteam6.html'>1ST</a></th><th>2ND</th>",
			"<th",ifelse(kk==7," class='hl'",""),"><a href='byteam7.html'>First</a><th>Last</th>","</tr>",sep=""))
write.table(teamsc["row3"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

############################
# sortable NCAAT records
############################

for (ll in 1:7) {
sink(paste("../CWS/byteamr",ll,".html",sep=""))

if (ll==1) { teams <- teams[order(teams$team), ] }
if (ll==2) { teams <- teams[order(-teams$rapp, -teams$W / (teams$W + teams$L), -teams$W+teams$L, teams$team), ] }
if (ll==3) { teams <- teams[order(-teams$W - teams$L, -teams$W / (teams$W + teams$L), -teams$W+teams$L, teams$team), ] }
if (ll==4) { teams <- teams[order(-teams$W, -teams$W / (teams$W + teams$L), -teams$W+teams$L, teams$team), ] }
if (ll==5) { teams <- teams[order(-teams$W / (teams$W + teams$L), -teams$W+teams$L, -teams$W, teams$team), ] }
if (ll==6) { teams <- teams[order(-teams$capps/teams$rapps, -teams$capps, -teams$rapps, -teams$W / (teams$W + teams$L), -teams$W+teams$L, teams$team), ] }
if (ll==7) { teams <- teams[order(teams$fyear, teams$lyear), ] }

teams$class <- (1+(1:nrow(teams))) %% 2
teams$row3  <- paste("<tr class='d",teams$class,"'><td>",paste(
				paste("<a href='",teamlink(teams$team),".html'>",teams$team,"</a>",sep=""),
				teams$rapps,
				teams$W + teams$L,
				teams$W,
				teams$L,
				sprintf("%.3f", round(teams$W / (teams$W + teams$L),3) ),
				teams$capps,
				sprintf("%.1f", round(100 * teams$capps / teams$rapps,1) ),
				paste("<a href='",teams$fyear,".html'>",teams$fyear,"</a>",sep=""),
				paste("<a href='",teams$lyear,".html'>",teams$lyear,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>NCAA Tournament Records</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr>", "<th",ifelse(ll==1," class='hl'",""),"><a href='byteamr1.html'>Team</a></th>",
			"<th",ifelse(ll==2," class='hl'",""),"><a href='byteamr2.html'>Apps.</a></th>",
			"<th",ifelse(ll==3," class='hl'",""),"><a href='byteamr3.html'>GP</a></th>",
			"<th",ifelse(ll==4," class='hl'",""),"><a href='byteamr4.html'>W</a></th><th>L</th>",
			"<th",ifelse(ll==5," class='hl'",""),"><a href='byteamr5.html'>PCT</a></th><th>CWS</th>",
			"<th",ifelse(ll==6," class='hl'",""),"><a href='byteamr6.html'>QPct</a></th>",
			"<th",ifelse(ll==7," class='hl'",""),"><a href='byteamr7.html'>First</a></th><th>Last</th>","</tr>",sep=""))
write.table(teams["row3"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

############################
# super regional results
############################

supers <- subset(games, grepl("SR",games$round) & result == "W")
supers$W <- ifelse(grepl("1",supers$gameno),100,ifelse(grepl("2",supers$gameno),10,1))
supersw <- aggregate(supers$W, list(yr = supers$yr, regional = supers$regional, round = supers$round, 
							team = supers$team, opponent = supers$opponent), sum)
supersw <- subset(supersw, x == 110 | x == 11 | x == 101)
supersw$result1 <- ifelse(supersw$x==110,"2-0","2-1")
supersw$result2 <- ifelse(supersw$x==110,"WW",ifelse(supersw$x==11,"LWW","WLW"))

supersw <- merge(supersw,info[c("yr","team","suphost","natseed")],by=c("yr","team"),all.x=T)
supersw <- merge(supersw,info[c("yr","team","natseed")],by.x=c("yr","opponent"),by.y=c("yr","team"))
supersw <- supersw[order(supersw$yr, supersw$regional), ]

supersw$class <- (ceiling(1:nrow(supersw)/8)+1) %% 2

for (k in 1:nrow(supersw)) {
if (k==1) { supersw$flag[k] <- 2 }
if (k >1) { supersw$flag[k] <- ifelse(supersw$yr[k] != supersw$yr[k-1],1,0)}
}

supersw$row <- paste(ifelse(supersw$flag==1,"<tr><th colspan='6'></th></tr>",""),"<tr class='d",supersw$class,"'><td>",paste(
				ifelse(supersw$flag>=1,paste("<b><a href='",supersw$yr,".html'>",supersw$yr,"</a></b>",sep=""),""),
				paste("<a href='",supersw$yr,".html#",teamlink(supersw$regional),"'>",supersw$round,"</a>",sep=""),
				paste(ifelse(is.na(supersw$natseed.x),"",paste("<h5>",supersw$natseed.x,"</h5> ",sep="")),"<a href='",teamlink(supersw$team),".html'>",supersw$team,"</a>",sep=""),
				supersw$result1,
				supersw$result2,
				paste(ifelse(is.na(supersw$natseed.y),"",paste("<h5>",supersw$natseed.y,"</h5> ",sep="")),"<a href='",teamlink(supersw$opponent),".html'>",supersw$opponent,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/supers.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Super Regional Results</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr><th>Year</th><th>Regional</th><th>Winner</th><th>Result</th><th>Series</th><th>Loser</th></tr>"))
write.table(supersw["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# super regional records
############################

supersl <- supersw
supersl$team <- supersl$opponent
supersw$result <- paste("W",ifelse(supersw$suphost=="H","H","A"),sep="")
supersl$result <- paste("L",ifelse(supersw$suphost=="H","A","H"),sep="")

superst <- rbind(supersw[c("yr","team","result")],supersl[c("yr","team","result")])
supersum <- as.data.frame.matrix(table(superst$team,superst$result))
supersum$team <- rownames(supersum)
supersum$W <- supersum$WH + supersum$WA
supersum$L <- supersum$LH + supersum$LA
supersum <- merge(supersum,teams[c("team","WS","LS")],by="team",all.x=T)

supersum <- subset(supersum[order(-supersum$W-supersum$L, -supersum$W+supersum$L, -supersum$WS+supersum$LS, supersum$team), ], W + L > 0)

supersum$class <- (1+(1:nrow(supersum))) %% 2

supersum$row <- paste("<tr class='d",supersum$class,"'><td>",paste(
				paste("<a href='",teamlink(supersum$team),".html'>",supersum$team,"</a>",sep=""),
				supersum$W+supersum$L,
				supersum$W,
				supersum$L,
				sprintf("%.3f", round(supersum$W / (supersum$W + supersum$L),3) ),
				ifelse(supersum$WH+supersum$LH==0,"",paste(supersum$WH,supersum$LH,sep="-")),
				ifelse(supersum$WA+supersum$LA==0,"",paste(supersum$WA,supersum$LA,sep="-")),
				ifelse(supersum$WS+supersum$LS==0,"",paste(supersum$WS,supersum$LS,sep="-")),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/supersum.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Super Regional Records</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr><th>Team</th><th>Apps.</th><th>Wins</th><th>Losses</th><th>Pct</th><th>Home</th><th>Away</th><th>Game W-L</th></tr>"))
write.table(supersum["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# regional results
############################

regnls <- subset(summ, LR<2 & yr>=1975)
reggames <- subset(games, grepl("SR",round)==FALSE & regional!="CWS" & result=="W" & yr>=1975) 

for (k in 1:nrow(reggames)) {
if (k==nrow(reggames)) { reggames$finalfl[k] <- 1 }
if (k <nrow(reggames)) { reggames$finalfl[k] <- ifelse(reggames$regional[k] != reggames$regional[k+1] |
							reggames$round[k] != reggames$round[k+1]	,1,0)}
} 

regfinals <- subset(reggames, finalfl==1)
regnls <- merge(regnls,regfinals[c("yr","gameno","team","opponent","rf","ra","inn")],by=c("yr","team"))
regnls <- merge(regnls,summ[c("yr","team","seed")],by.x=c("yr","opponent"),by.y=c("yr","team"))
regnls <- regnls [order(regnls$yr, regnls$round, regnls$regional), ]


for (k in 1:nrow(regnls)) {
if (k==1) { regnls$class[k] <- 0 }
if (k >1) { regnls$class[k] <- ifelse(regnls$yr[k] != regnls$yr[k-1],(1+regnls$class[k-1]) %% 2,regnls$class[k-1])}
if (k==1) { regnls$flag[k] <- 2 }
if (k >1) { regnls$flag[k] <- ifelse(regnls$yr[k] != regnls$yr[k-1],1,0)}
}

regnls$row <- paste(ifelse(regnls$flag==1,"<tr><th colspan='6'></th></tr>",""),"<tr class='d",regnls$class,"'><td>",paste(
				ifelse(regnls$flag>=1,paste("<b><a href='",regnls$yr,".html'>",regnls$yr,"</a></b>",sep=""),""),
				paste("<a href='",regnls$yr,".html#",teamlink(regnls$regional),"'>",
					ifelse(regnls$round!="",paste(regnls$round),paste(regnls$regional)),"</a>",sep=""),
				paste(ifelse(is.na(regnls$seed.x),"",paste("<h5>",regnls$seed.x,"</h5> ",sep="")),"<a href='",teamlink(regnls$team),".html'>",regnls$team,"</a>",sep=""),
				paste(regnls$WR,regnls$LR,sep="-"),
				paste("<b>",regnls$rf,"-",regnls$ra,ifelse(is.na(regnls$inn),"",paste(" (",regnls$inn,")",sep="")),"</b>",sep=""),
				paste(ifelse(is.na(regnls$seed.y),"",paste("<h5>",regnls$seed.y,"</h5> ",sep="")),"<a href='",teamlink(regnls$opponent),".html'>",regnls$opponent,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/regnls1.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Regional Winners and Finals (1975-1998)</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr><th>Year</th><th>Regional</th><th>Winner</th><th>Record</th><th>Score</th><th>Runner-up</th></tr>"))
write.table(subset(regnls,yr<1999)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

sink(paste("../CWS/regnls2.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Regional Winners and Finals (since 1999)</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr><th>Year</th><th>Regional</th><th>Winner</th><th>Record</th><th>Score</th><th>Runner-up</th></tr>"))
write.table(subset(regnls,yr>=1999)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# regional winners stats
############################

regstat.1 <- as.data.frame.matrix(table(regnls$yr,regnls$seed.x))
regstat.1$yr <- rownames(regstat.1)
regstat.2 <- as.data.frame.matrix(table(regnls$yr,regnls$natseed))
regstat.2$ns <- regstat.2$"1"+regstat.2$"2"+regstat.2$"3"+regstat.2$"4"+regstat.2$"5"+regstat.2$"6"+regstat.2$"7"+regstat.2$"8"
regstat.3 <- as.data.frame.matrix(table(regnls$yr,regnls$host))
regstat.4 <- as.data.frame.matrix(table(regnls$yr,regnls$gameno))

supstat <- merge(supersw[c("yr","team","result1","result2")],
			summ[c("yr","team","seed","natseed","suphost")],by=c("yr","team"))
supstat.1 <- as.data.frame.matrix(table(supstat$yr,supstat$seed))
supstat.1$yr <- rownames(supstat.1)
supstat.2 <- as.data.frame.matrix(table(supstat$yr,supstat$natseed))
supstat.2$ns <- supstat.2$"1"+supstat.2$"2"+supstat.2$"3"+supstat.2$"4"+supstat.2$"5"+supstat.2$"6"+supstat.2$"7"+supstat.2$"8"
supstat.3 <- as.data.frame.matrix(table(supstat$yr,supstat$suphost))
supstat.4 <- as.data.frame.matrix(table(supstat$yr,supstat$result2))

regstatr <- cbind(regstat.1[c("yr","1","2","3","4","5")],regstat.2["ns"],regstat.3["H"],regstat.4["Game 7"])
regstats <-	cbind(supstat.1[c("yr","1","2","3","4")],supstat.2["ns"],supstat.3["H"],supstat.4[c("WW","WLW","LWW")])
regstats <- merge(subset(regstatr,yr>=1999),regstats,by="yr",all.x=T)

regstats.a <- subset(regstats, select = -yr )
regstats.m <- as.data.frame(t(sapply(regstats.a,mean))); regstats.m$yr <- "AVE";
regstats.p <- regstats.m; regstats.p$yr <- "PCT"; 

for (yy in 1:ncol(regstats.p)) {
if (yy<=8&yy!=6) { regstats.p[yy] <- 100*regstats.p[yy]/16 }
else if (yy<18)  { regstats.p[yy] <- 100*regstats.p[yy]/8 }

if (yy<18) { regstats.m[yy] <- round(regstats.m[yy],1); 
		 regstats.p[yy] <- round(regstats.p[yy],1);}
}

regstats <- rbind(regstats,regstats.m,regstats.p)

regstats$class <- (1+(1:nrow(regstats))) %% 2
regstats$class <- ifelse(regstats$yr=="AVE"|regstats$yr=="PCT",2,regstats$class)

regstats$row <- paste("<tr class='d",regstats$class,"'><td>",paste(
				ifelse(regstats$yr=="AVE"|regstats$yr=="PCT",paste("<b>",regstats$yr,"</b>",sep=""),
					paste("<a href='",regstats$yr,".html'>",regstats$yr,"</a>",sep="")),
				paste(regstats$H.x),
				paste(regstats$ns.x),
				paste(regstats$"1.x"),
				paste(regstats$"2.x"),
				paste(regstats$"3.x"),
				paste(regstats$"4.x"),
				paste(regstats$"Game 7"),
				"&nbsp;",
				paste(regstats$H.y),
				paste(regstats$ns.y),
				paste(regstats$"1.y"),
				paste(regstats$"2.y"),
				paste(regstats$"3.y"),
				paste(regstats$"4.y"),
				paste(regstats$WW),
				paste(regstats$WLW),
				paste(regstats$LWW),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/regstat.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Regional Statistics (since 1999)</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th>&nbsp;</th><th colspan='7' class='hl'>Regionals</th><th>&nbsp;</th><th colspan='9' class='hl'>Super Regionals</th>","</tr>"))
cat(paste("<tr><th>Year</th><th>Host</th><th>NS</th><th>#1</th><th>#2</th><th>#3</th><th>#4</th><th>Game 7s</th>",
		"<th></th><th>Host</th><th>NS</th><th>#1</th><th>#2</th><th>#3</th><th>#4</th>",
		"<th>WW</th><th>WLW</th><th>LWW</th></tr>"))
write.table(regstats["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# district results
############################

dist <- subset(summ, WC+LC+WD+LD>0 & 1946<yr & yr<1975)
dist$regional <- ifelse(dist$round!="",dist$round,dist$regional)
dist <- dist[order(dist$yr, dist$regional), ]
distw<- matrix(, nrow = length(unique(dist$yr)), ncol = 8)

for (k in 1:length(unique(dist$yr))) {
	for (kk in 1:8) {
		distw[k,kk] = dist$team[(k-1)*8+kk]
}}

dists <- subset(byyear, 1946<yr & yr<1975)["yr"]
dists <- cbind(dists,distw)
dists$class <- (1+(1:nrow(dists))) %% 2

dists$row <- paste("<tr class='d",dists$class,"'><td>",paste(
				paste("<a href='",dists$yr,".html'>",dists$yr,"</a>",sep=""),
				paste("<a href='",teamlink(dists$"1"),".html'>",dists$"1","</a>",sep=""),
				paste("<a href='",teamlink(dists$"2"),".html'>",dists$"2","</a>",sep=""),
				paste("<a href='",teamlink(dists$"3"),".html'>",dists$"3","</a>",sep=""),
				paste("<a href='",teamlink(dists$"4"),".html'>",dists$"4","</a>",sep=""),
				paste("<a href='",teamlink(dists$"5"),".html'>",dists$"5","</a>",sep=""),
				paste("<a href='",teamlink(dists$"6"),".html'>",dists$"6","</a>",sep=""),
				paste("<a href='",teamlink(dists$"7"),".html'>",dists$"7","</a>",sep=""),
				paste("<a href='",teamlink(dists$"8"),".html'>",dists$"8","</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/dists.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>District Winners (1947-1974)</center></h3><br>"))
cat("<table width='90%' align='center'>")
cat(paste("<tr><th>Year</th><th>District 1</th><th>District 2</th><th>District 3</th><th>District 4</th>",
				   "<th>District 5</th><th>District 6</th><th>District 7</th><th>District 8</th></tr>"))
write.table(dists["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# find common games
############################

games$key2 <- paste(games$team, games$opponent, sep="_")
matchups <- as.data.frame.matrix(table(games$key2,games$result.f))

matchups$key <- rownames(matchups)
keys <- data.frame(do.call('rbind', strsplit(as.character(matchups$key), '_', fixed=TRUE)))
matchups <- cbind(matchups,keys)

matchups$W  <- matchups$WR + matchups$WS + matchups$WC + matchups$WF
matchups$L  <- matchups$LR + matchups$LS + matchups$LC + matchups$LF
matchups$WT <- matchups$WC + matchups$WF
matchups$LT <- matchups$LC + matchups$LF

matchupsr <- subset(matchups, W + L >= 8 & (W>L | (W==L & as.character(X1)<as.character(X2)) ))
matchupsr <- matchupsr[order(-matchupsr$W-matchupsr$L, matchupsr$X1, matchupsr$X2), ]
matchupsr$class <- (1+(1:nrow(matchupsr))) %% 2

matchupsr$row <- paste("<tr class='d",matchupsr$class,"'><td>",paste(
				paste(matchupsr$W+matchupsr$L),
				paste("<a href='",teamlink(matchupsr$X1),".html'>",matchupsr$X1,"</a>",sep=""),
				paste(matchupsr$W,matchupsr$L,sep="-"),
				paste("<a href='",teamlink(matchupsr$X2),".html'>",matchupsr$X2,"</a>",sep=""),
				ifelse(matchupsr$WR+matchupsr$LR != 0,paste(matchupsr$WR,matchupsr$LR,sep="-"),""),
				ifelse(matchupsr$WS+matchupsr$LS != 0,paste(matchupsr$WS,matchupsr$LS,sep="-"),""),
				ifelse(matchupsr$WT+matchupsr$LT != 0,paste(matchupsr$WT,matchupsr$LT,sep="-"),""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/matchupsr.html",sep=""))

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Most Common Tournament Matchups</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th>Games</th><th>More Wins</th><th>Record</th><th>Opponent</th>",
	"<th>Regional</th><th>Super</th><th>CWS</th>","</tr>"))
write.table(matchupsr["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

matchupsc <- subset(matchups, WT+LT >= 4 & (WT>LT | (WT==LT & as.character(X1)<as.character(X2)) ))
matchupsc <- matchupsc [order(-matchupsc$WT-matchupsc$LT, matchupsc$X1, matchupsc$X2), ]
matchupsc$class <- (1+(1:nrow(matchupsc))) %% 2

matchupsc$row <- paste("<tr class='d",matchupsc$class,"'><td>",paste(
				paste(matchupsc$WT+matchupsc$LT),
				paste("<a href='",teamlink(matchupsc$X1),".html'>",matchupsc$X1,"</a>",sep=""),
				paste(matchupsc$WT,matchupsc$LT,sep="-"),
				paste("<a href='",teamlink(matchupsc$X2),".html'>",matchupsc$X2,"</a>",sep=""),
				ifelse(matchupsc$WC+matchupsc$LC != 0,paste(matchupsc$WC,matchupsc$LC,sep="-"),""),
				ifelse(matchupsc$WF+matchupsc$LF != 0,paste(matchupsc$WF,matchupsc$LF,sep="-"),""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/matchupsc.html",sep=""))

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Most Common CWS Matchups</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th>Games</th><th>More Wins</th><th>Record</th><th>Opponent</th>",
	"<th>Bracket</th><th>Final</th>","</tr>"))
write.table(matchupsc["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# round 1 records
############################

round1 <- subset(games,yr>=1999 & (gameno=="Game 1"|gameno=="Game 2") & result.f=="WR")
round1 <- merge(round1,summ[c("yr","team","seed")],by=c("yr","team"))
round1r <- as.data.frame.matrix(table(round1$yr,round1$seed))

summ.1 <- subset(summ, yr>=1999 & WR+LR>0)
results.1 <- as.data.frame.matrix(table(summ.1$yr,paste(summ.1$seed,summ.1$WR,summ.1$LR,sep="")))
results.1$yr <- as.numeric(rownames(results.1))

results.1$S1N1 <- results.1$"130" + results.1$"131" + results.1$"141"
results.1$S1N2 <- results.1$"122" + results.1$"132"
results.1$S1N3 <- results.1$"112" 
results.1$S1N4 <- results.1$"102" 
results.1$S2N1 <- results.1$"230" + results.1$"231" + results.1$"241"
results.1$S2N2 <- results.1$"222" + results.1$"232"
results.1$S2N3 <- results.1$"212" 
results.1$S2N4 <- results.1$"202" 
results.1$S3N1 <- results.1$"330" + results.1$"331" + results.1$"341"
results.1$S3N2 <- results.1$"322" + results.1$"332"
results.1$S3N3 <- results.1$"312" 
results.1$S3N4 <- results.1$"302" 
results.1$S4N1 <- results.1$"430" + results.1$"431" + results.1$"441"
results.1$S4N2 <- results.1$"422" + results.1$"432"
results.1$S4N3 <- results.1$"412" 
results.1$S4N4 <- results.1$"402" 

results.1 <- cbind(results.1,round1r)

results.1.m <- as.data.frame(t(sapply(results.1,mean)))
for (yy in 1:ncol(results.1.m)) {results.1.m[yy] <- round(results.1.m[yy],1);}
results.1.m$yr <- "AVE";

results.1 <- rbind(results.1,results.1.m)

results.1$class <- ifelse(results.1$yr=="AVE",2,(1+(1:nrow(results.1))) %% 2)
results.1$row <- paste("<tr class='d",results.1$class,"'><td>",paste(
				ifelse(results.1$yr=="AVE",results.1$yr,paste("<a href='",results.1$yr,".html'>",results.1$yr,"</a>",sep="")),
				paste(results.1$"1",results.1$"4",sep="-"),
				paste(results.1$"2",results.1$"3",sep="-"),
				paste("<b>",results.1$S1N1,"</b>"),paste(results.1$S1N2),paste(results.1$S1N3),paste(results.1$S1N4),
				paste("<b>",results.1$S2N1,"</b>"),paste(results.1$S2N2),paste(results.1$S2N3),paste(results.1$S2N4),
				paste("<b>",results.1$S3N1,"</b>"),paste(results.1$S3N2),paste(results.1$S3N3),paste(results.1$S3N4),
				paste("<b>",results.1$S4N1,"</b>"),paste(results.1$S4N2),paste(results.1$S4N3),paste(results.1$S4N4),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/regseed.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Regional Finishes By Seed</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th>&nbsp;</th><th colspan='2' class='hl'>First Round</th><th colspan='4' class='hl'>#1 Seed</th>",
		"<th colspan='4' class='hl'>#2 Seed</th><th colspan='4' class='hl'>#3 Seed</th><th colspan='4' class='hl'>#4 Seed</th>",
		"</tr>"))
cat(paste("<tr><th>Year</th><th>1-4</th><th>2-3</th><th>1st</th><th>2nd</th><th>3rd</th><th>4th</th>",
		"<th>1st</th><th>2nd</th><th>3rd</th><th>4th</th><th>1st</th><th>2nd</th><th>3rd</th><th>4th</th>",
		"<th>1st</th><th>2nd</th><th>3rd</th><th>4th</th>"))
write.table(results.1["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# regional round finishes since 1999
############################

results.2 <- as.data.frame.matrix(table(summ.1$team,paste(summ.1$WR,summ.1$LR,sep="")))
results.2$team <- rownames(results.2)

results.2$N1 <- results.2$"30" + results.2$"31" + results.2$"41"
results.2$N2 <- results.2$"22" + results.2$"32"
results.2$N3 <- results.2$"12" 
results.2$N4 <- results.2$"02" 

reg99 <- subset(games,regional != "CWS" & grepl("SR",round)==F & yr>=1999)[c("yr","team","result")]
sum99 <- as.data.frame.matrix(table(reg99$team,reg99$result))
sum99$team <- rownames(sum99)
results.2 <- merge(results.2,sum99,by="team")

results.2 <- results.2[order(-results.2$N1, -results.2$N2, -results.2$N3, -results.2$N4, -results.2$W+results.2$L), ]

results.2$class <- (1+(1:nrow(results.2))) %% 2
results.2$row <- paste("<tr class='d",results.2$class,"'><td>",paste(
				paste("<a href='",teamlink(results.2$team),".html'>",results.2$team,"</a>",sep=""),
				paste(results.2$N1+results.2$N2+results.2$N3+results.2$N4),
				paste(results.2$W,results.2$L,sep="-"),
				paste(results.2$N1),
				paste(results.2$N2),
				paste(results.2$N3),
				paste(results.2$N4),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/regteam.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Regional Finishes By Team (since 1999)</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr><th></th><th>App.</th><th>W-L</th><th>1st</th><th>2nd</th><th>3rd</th><th>4th</th>"))
write.table(results.2["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# regional round finishes 1975-98
############################

summ.3 <- subset(summ, 1975<=yr & yr<=1998)
results.3 <- as.data.frame.matrix(table(summ.3$team,paste(summ.3$WR,summ.3$LR,sep="")))
results.3$team <- rownames(results.3)

reg75w <- subset(regfinals, 1975<=yr & yr<=1998)[c("yr","team","opponent")]
reg75w$result <- "RW"
reg75l <- reg75w
reg75l$team <- reg75l$opponent
reg75l$result <- "RL"
reg75f <- rbind(reg75w,reg75l)
reg75t <- as.data.frame.matrix(table(reg75f$team,reg75f$result))
reg75t$team <- rownames(reg75t)
results.3 <- merge(results.3,reg75t,by="team")

reg75 <- subset(games,regional != "CWS" & 1975<=yr & yr<=1998)[c("yr","team","result")]
sum75 <- as.data.frame.matrix(table(reg75$team,reg75$result))
sum75$team <- rownames(sum75)
results.3 <- merge(results.3,sum75,by="team")

results.3$app <- results.3$"02"+results.3$"12"+results.3$"22"+results.3$"32"+results.3$"42"+results.3$RW
results.3$N3  <- results.3$app-results.3$RW-results.3$RL-results.3$"02"-results.3$"12"

results.3 <- results.3[order(-results.3$RW, -results.3$RL, -results.3$N3, -results.3$"12", -results.3$"02",-results.3$W+results.3$L), ]

results.3$class <- (1+(1:nrow(results.3))) %% 2
results.3$row <- paste("<tr class='d",results.3$class,"'><td>",paste(
				paste("<a href='",teamlink(results.3$team),".html'>",results.3$team,"</a>",sep=""),
				paste(results.3$app),
				paste(results.3$W,results.3$L,sep="-"),
				paste(results.3$RW),
				paste(results.3$RL),
				paste(results.3$N3),
				paste(results.3$"12"),
				paste(results.3$"02"),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/regteam2.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Regional Finishes By Team (1975-1998)</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr><th></th><th>App.</th><th>W-L</th><th>1st</th><th>2nd</th><th>3/6</th><th>1-2</th><th>0-2</th>"))
write.table(results.3["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# regional round stats
############################

fin75 <- subset(regnls, 1975<=yr & yr<=1998)
seed751<- as.data.frame.matrix(table(fin75$yr,fin75$seed.x))
seed752<- as.data.frame.matrix(table(fin75$yr,fin75$host))
seed751$yr <- rownames(seed751)
seed752$yr <- rownames(seed752)

seed75 <- merge(seed751,seed752,by="yr",all=T)
seed75 <- merge(seed75,byyear[c("yr","no.teams")],by="yr",all.x=T)
seed75$"6" <- 0

seed75$class <- (1+(1:nrow(seed75))) %% 2
seed75$row <- paste("<tr class='d",seed75$class,"'><td>",paste(
				paste("<a href='",seed75$yr,".html'>",seed75$yr,"</a>",sep=""),
				paste(seed75$no.teams),
				paste(seed75$H),
				ifelse(seed75$yr>=1987,paste(seed75$"1"),""),
				ifelse(seed75$yr> 1987,paste(seed75$"2"),""),
				ifelse(seed75$yr> 1987,paste(seed75$"3"),""),
				ifelse(seed75$yr> 1987,paste(seed75$"4"),""),
				ifelse(seed75$yr> 1987,paste(seed75$"5"),""),
				ifelse(seed75$yr> 1987,paste(seed75$"6"),""),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/regstat2.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Regional Statistics (1975-1998)</center></h3><br>"))
cat("<table width='80%' align='center'>")
cat(paste("<tr><th>Year</th><th>Teams</th><th>Host</th><th>#1</th><th>#2</th><th>#3</th><th>#4</th><th>#5</th><th>#6</th>"))
write.table(seed75["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# national seed results
############################

natseed <- subset(summ, is.na(natseed)==FALSE & yr>=1999)
natseed <- natseed[order(natseed$yr, natseed$natseed), ]
natseed$class <- ceiling((1:nrow(natseed))/8+1) %% 2
natseed$class <- ifelse(natseed$crank==1,2,natseed$class)

for (k in 1:nrow(natseed)) {
if (k==1) { natseed$flag[k] <- 2 }
if (k >1) { natseed$flag[k] <- ifelse(natseed$yr[k] != natseed$yr[k-1],1,0)}
}

natseed$row <- paste(ifelse(natseed$flag==1,"<tr><th colspan='10'></th></tr>",""),"<tr class='d",natseed$class,"'><td>",paste(
				ifelse(natseed$flag>=1,paste("<b><a href='",natseed$yr,".html'>",natseed$yr,"</a></b>",sep=""),""),
				natseed$natseed,
				paste("<a href='",teamlink(natseed$team),".html'>",natseed$team,"</a>",sep=""),
				paste(natseed$W,natseed$L,sep="-"),
				paste("<a href='",natseed$yr,".html#",teamlink(natseed$regional),"'>",
					ifelse(natseed$round != "",paste(natseed$round),paste(natseed$regional)),"</a>",sep=""),
				ifelse(natseed$WR+natseed$LR != 0,paste(natseed$WR,natseed$LR,sep="-"),ifelse(natseed$AR>0,"(A)","&nbsp;")),
				ifelse(natseed$WS+natseed$LS != 0,paste(natseed$WS,natseed$LS,sep="-"),"&nbsp;"),
				ifelse(natseed$crank<9,paste("<a href='",natseed$yr,".html#CWS'>",natseed$crankc,"</a>",sep=""),"&nbsp;"),
				ifelse(natseed$WC+natseed$LC != 0,paste(natseed$WC,natseed$LC,sep="-"),"&nbsp;"),
				ifelse(natseed$WF+natseed$LF != 0,paste(natseed$WF,natseed$LF,sep="-"),"&nbsp;"),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/natseed.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>National Seed Results by Year</center></h3><br>"))
cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th> </th><th> </th><th> </th><th> </th><th colspan='3' class='hl'>Regionals</th><th colspan='3' class='hl'>College World Series</th>","</tr>"))
cat(paste("<tr>","<th>Year</th><th>Seed</th><th>Team</th><th>Total</th><th>Regional</th><th>Regional</th><th>Super</th><th>Rank</th><th>Bracket</th><th>Final</th>","</tr>"))
write.table(natseed["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# championship games
############################

place.1 <- subset(summ, crank==1)[c("yr","team","WT","LT","natseed","WF","LF")]
place.2 <- subset(summ, crank==2)[c("yr","team","WT","LT","natseed")]
champgm <- merge(place.1,place.2,by="yr",suffixes=c("",".2"))

finals.1  <- subset(games, gameno=="Final"|gameno=="Final 1")[c("yr","team","rf","ra","inn")]
finals.2  <- subset(games, gameno=="Final 2")[c("yr","team","rf","ra","inn")]
finals.3  <- subset(games, gameno=="Final 3")[c("yr","team","rf","ra","inn")]

champgm <- merge(champgm,finals.1,by=c("yr","team"))
champgm <- merge(champgm,finals.2,by=c("yr","team"),suffixes=c("",".g2"),all.x=TRUE)
champgm <- merge(champgm,finals.3,by=c("yr","team"),suffixes=c("",".g3"),all.x=TRUE)

champgm$game.1 <- ifelse(is.na(champgm$rf),"&nbsp;",
				paste(champgm$rf,"-",champgm$ra,ifelse(is.na(champgm$inn),"",paste(" (",champgm$inn,")",sep="")),sep=""))
champgm$game.2 <- ifelse(is.na(champgm$rf.g2),"&nbsp;",
				paste(champgm$rf.g2,"-",champgm$ra.g2,ifelse(is.na(champgm$inn.g2),"",paste(" (",champgm$inn.g2,")",sep="")),sep=""))
champgm$game.3 <- ifelse(is.na(champgm$rf.g3),"&nbsp;",
				paste(champgm$rf.g3,"-",champgm$ra.g3,ifelse(is.na(champgm$inn.g3),"",paste(" (",champgm$inn.g3,")",sep="")),sep=""))

champgm$class <- (1+(1:nrow(champgm))) %% 2
champgm$row <- paste("<tr class='d",champgm$class,"'><td>",paste(
				paste("<a href='",champgm$yr,".html'>",champgm$yr,"</a>",sep=""),
				paste(ifelse(is.na(champgm$natseed),"",paste("<h5>",champgm$natseed,"</h5> ",sep="")),"<a href='",teamlink(champgm$team),".html'>",champgm$team,"</a>",sep=""),
				paste(champgm$WT,champgm$LT,sep="-"),
				paste(ifelse(is.na(champgm$natseed.2),"",paste("<h5>",champgm$natseed.2,"</h5> ",sep="")),"<a href='",teamlink(champgm$team.2),".html'>",champgm$team.2,"</a>",sep=""),
				paste(champgm$WT.2,champgm$LT.2,sep="-"),
				champgm$game.1,champgm$game.2,champgm$game.3,
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/champgm.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Championship Games/Series</center></h3><br>"))
cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Year</th><th>Champion</th><th>Record</th><th>Runner-up</th><th>Record</th><th>Game 1</th><th>Game 2</th><th>Game 3</th>","</tr>"))
write.table(champgm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# hosts
############################

regwin <- regnls[c("yr","team","LR")]
reghost<- subset(info,host=="H")[c("yr","team","host")]
reghost <- merge(reghost,regwin,by=c("yr","team"),all=T)
reghost$stat <- ifelse(is.na(reghost$host),"N",ifelse(is.na(reghost$LR),"L","W"))
reghost$stat2 <- paste(reghost$stat,ifelse(reghost$yr<1999,"75","99"),sep="")
regtab <- as.data.frame.matrix(table(reghost$team,reghost$stat2))
regtab$team <- rownames(regtab)

hosttab <- subset(regtab, L99+N99+W99>0)
hosttab<- hosttab[order(-hosttab$W99-hosttab$N99), ]
hosttab$class <- (1+(1:nrow(hosttab))) %% 2

hosttab$row <- paste("<tr class='d",hosttab$class,"'><td>",paste(
				paste("<a href='",teamlink(hosttab$team),".html'>",hosttab$team,"</a>",sep=""),
				paste(hosttab$W99+hosttab$N99),
				paste(hosttab$W99+hosttab$L99),
				paste(hosttab$W99),
				ifelse(hosttab$W99+hosttab$L99>0,paste(sprintf("%.1f", 100*round(hosttab$W99/(hosttab$W99+hosttab$L99),3))),""),
				paste(hosttab$N99),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/hosts1.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Regional Wins and Hosting Success (since 1999)</center></h3><br>"))
cat("<br><table width='80%' align='center'>")
cat(paste("<tr><th></th><th>Regionals<br>Won</th><th>Times<br>Hosted</th><th>Wins as<br>Host</th><th>Success<br>Pct</th><th>Wins as<br>Non-Host</th></tr>",sep=""))
write.table(hosttab["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

hosttab2<- subset(regtab, L75+N75+W75>0)
hosttab2<- hosttab2[order(-hosttab2$W75-hosttab2$N75), ]
hosttab2$class <- (1+(1:nrow(hosttab2))) %% 2

hosttab2$row <- paste("<tr class='d",hosttab2$class,"'><td>",paste(
				paste("<a href='",teamlink(hosttab2$team),".html'>",hosttab2$team,"</a>",sep=""),
				paste(hosttab2$W75+hosttab2$N75),
				paste(hosttab2$W75+hosttab2$L75),
				paste(hosttab2$W75),
				ifelse(hosttab2$W75+hosttab2$L75>0,paste(sprintf("%.1f", 100*round(hosttab2$W75/(hosttab2$W75+hosttab2$L75),3))),""),
				paste(hosttab2$N75),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/hosts2.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Regional Wins and Hosting Success (1975-1998)</center></h3><br>"))
cat("<br><table width='80%' align='center'>")
cat(paste("<tr><th></th><th>Regionals<br>Won</th><th>Times<br>Hosted</th><th>Wins as<br>Host</th><th>Success<br>Pct</th><th>Wins as<br>Non-Host</th></tr>",sep=""))
write.table(hosttab2["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# CWS Summary
############################

cwssum <- subset(teams,capps>0)[c("team","WT","LT","WC","LC","WF","LF","1","2","3","4","5","7")]

cwsfin <- subset(summ,crank==1|crank==2)[c("yr","team","crank")]
cwsfin$result <- ifelse(cwsfin$yr<1988,paste("A",cwsfin$crank,sep=""),
			ifelse(cwsfin$yr<2003,paste("B",cwsfin$crank,sep=""),paste("C",cwsfin$crank,sep="")))
fintab <- as.data.frame.matrix(table(cwsfin$team,cwsfin$result))
fintab$team <- rownames(fintab)
cwssum <- merge(cwssum,fintab,by="team",all.x=T)

cwssum <- cwssum[order(-cwssum$"1",-cwssum$"2",-cwssum$"3",-cwssum$"4",-cwssum$"5",-cwssum$"7"), ]
cwssum$class <- (1+(1:nrow(cwssum))) %% 2

cwssum$row <- paste("<tr class='d",cwssum$class,"'><td>",paste(
				paste("<a href='",teamlink(cwssum$team),".html'>",cwssum$team,"</a>",sep=""),
				ifelse(cwssum$WC+cwssum$LC==0,"0",paste(cwssum$WC,cwssum$LC,sep="-")),
				ifelse(cwssum$WF+cwssum$LF==0,"0",paste(cwssum$WF,cwssum$LF,sep="-")),
				ifelse(cwssum$WT+cwssum$LT==0,"0",paste(cwssum$WT,cwssum$LT,sep="-")),
				ifelse(cwssum$"1"==0,"0",paste(cwssum$"1")),
				ifelse(cwssum$"2"==0,"0",paste(cwssum$"2")),
				ifelse(cwssum$"3"==0,"0",paste(cwssum$"3")),
				ifelse(cwssum$"4"==0,"0",paste(cwssum$"4")),
				ifelse(cwssum$"5"==0,"0",paste(cwssum$"5")),
				ifelse(cwssum$"7"==0,"0",paste(cwssum$"7")),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/cwssum.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>CWS Records by Round and Finish</center></h3>"))
cat("<br><table width='80%' align='center'>")
cat(paste("<tr><th></th><th class='hl' colspan='3'>W-L By Round:</th><th class='hl' colspan='6'>Placings</th></tr>",sep=""))
cat(paste("<tr><th></th><th>Bracket</th><th>Final</th><th>Total</th><th>1ST</th><th>2ND</th><th>3RD</th><th>4TH</th><th>5TH</th><th>7TH</th></tr>",sep=""))
write.table(cwssum["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# CWS placings by year
############################

place <- subset(summ, WC+LC>0 & yr!=2023)[c("yr","team","crank")]
place <- place[order(place$yr, place$crank, place$team), ]
places<- matrix("", nrow = length(unique(place$yr)), ncol = 8)

placez <- unique(place$yr)

for (k in 1:length(unique(place$yr))) {
	place.yr <- subset(place, yr==placez[k])
	for (kk in 1:length(place.yr$team)) {
		places[k,kk] = place.yr$team[kk]
}}

places <- as.data.frame(cbind(placez,places))
places$class <- (1+(1:nrow(places))) %% 2
places$yr <- places$placez

places$row <- paste("<tr class='d",places$class,"'><td>",paste(
				paste("<a href='",places$yr,".html'>",places$yr,"</a>",sep=""),
				paste("<a href='",teamlink(places$V2),".html'>",places$V2,"</a>",sep=""),
				paste("<a href='",teamlink(places$V3),".html'>",places$V3,"</a>",sep=""),
				ifelse(places$V4=="","",paste("<a href='",teamlink(places$V4),".html'>",places$V4,"</a>",sep="")),
				ifelse(places$V5=="","",paste("<a href='",teamlink(places$V5),".html'>",places$V5,"</a>",sep="")),
				ifelse(places$V6=="","",paste("<a href='",teamlink(places$V6),".html'>",places$V6,"</a>",sep="")),
				ifelse(places$V7=="","",paste("<a href='",teamlink(places$V7),".html'>",places$V7,"</a>",sep="")),
				ifelse(places$V8=="","",paste("<a href='",teamlink(places$V8),".html'>",places$V8,"</a>",sep="")),
				ifelse(places$V9=="","",paste("<a href='",teamlink(places$V9),".html'>",places$V9,"</a>",sep="")),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/cwsplace.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>CWS Placings</center></h3><br>"))
cat("<table width='90%' align='center'>")
cat(paste("<tr><th>Year</th><th>Champion</th><th>Runner-up</th><th>3rd Place</th><th>3rd/4tth</th>",
				   "<th>5th Place</th><th>5th Place</th><th>7th Place</th><th>7th Place</th></tr>"))
write.table(places["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("</table><br>",sep=""))
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# Championship Series Summary
############################

chserw <- subset(champgm,yr>2002)[c("yr","team","team.2")]
chserw$result <- "W"

chserl <- chserw
chserl$team <- chserl$team.2
chserl$result <- "L"

chser <- rbind(chserw[c("team","result")],chserl[c("team","result")])
sertab <- as.data.frame.matrix(table(chser$team,chser$result))
sertab$team <- rownames(sertab)

chserg <- subset(games,gameno=="Final 1"|gameno=="Final 2"|gameno=="Final 3")
chsergt<- as.data.frame.matrix(table(chserg$team,chserg$result.f))
chsergt$team <- rownames(chsergt)

sertab <- merge(sertab,chsergt,by="team")

sertab <- sertab[order(-sertab$W-sertab$L, -sertab$W, -sertab$WF+sertab$LF), ]
sertab$class <- (1+(1:nrow(sertab))) %% 2

sertab$row <- paste("<tr class='d",sertab$class,"'><td>",paste(
				paste("<a href='",teamlink(sertab$team),".html'>",sertab$team,"</a>",sep=""),
				paste(sertab$W+sertab$L),
				paste(sertab$W),
				paste(sertab$L),
				paste(sertab$WF,sertab$LF,sep="-"),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/chseries.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Championship Series Appearances and Records</center></h3>"))
cat("<br><table width='80%' align='center'>")
cat(paste("<tr><th></th><th>App.</th><th>W</th><th>L</th><th>Game W-L</th></tr>",sep=""))
write.table(sertab["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# Bracket Finals
############################

games88 <- subset(games,regional=="CWS" & yr>=1988)
games20 <- subset(games88,grepl("Game",gameno) & as.numeric(substring(gameno,6,7))<=8 & result=="W")
teams20 <- subset(as.data.frame.matrix(table(games20$key,games20$result)), W==2)
teams20$key <- rownames(teams20)
teams20$yr  <- as.numeric(substr(teams20$key,1,4))
teams20$team<- substr(teams20$key,6,24)

teams20 <- merge(teams20,summ[c("key","WC","LC","WF","LF","crank")],by="key",all.x=T)
teams20$result <- ifelse(teams20$LC==2,"LL",ifelse(teams20$LC==1,"LW","W"))
table(subset(teams20,yr<2020)$result)

bracketf <- subset(games,yr>=1988 & regional=="CWS" & (gameno=="Game 11"|gameno=="Game 12"|gameno=="Game 13"|gameno=="Game 14"))


#sertab <- sertab[order(-sertab$W-sertab$L, -sertab$W, -sertab$WF+sertab$LF), ]
#sertab$class <- (1+(1:nrow(sertab))) %% 2
#
#sertab$row <- paste("<tr class='d",sertab$class,"'><td>",paste(
#				paste("<a href='",teamlink(sertab$team),".html'>",sertab$team,"</a>",sep=""),
#				paste(sertab$W+sertab$L),
#				paste(sertab$W),
#				paste(sertab$L),
#				paste(sertab$WF,sertab$LF,sep="-"),
#			sep="</td><td>"),"</td></tr>",sep="")

#sink(paste("../CWS/chseries.html",sep=""))
#write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
#cat(paste("<br><h3><center>Championship Series Appearances and Records</center></h3>"))
#cat("<br><table width='80%' align='center'>")
#cat(paste("<tr><th></th><th>App.</th><th>W</th><th>L</th><th>Game W-L</th></tr>",sep=""))
#write.table(sertab["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
#cat("</table><br>")
#write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
#sink()

############################
# results by round
############################

disttab <- as.data.frame(table(distw))
disttab$team <- disttab$distw

supersum$SRW <- supersum$W
supersum$SRL <- supersum$L

reggames <- subset(games,regional != "CWS" & grepl("SR",round)==F)
reggames <- reggames[c("yr","team","result")]
reggames$result2 <- ifelse(reggames$yr<1951,paste(reggames$result,"R47",sep=""),
			ifelse(reggames$yr<1975,paste(reggames$result,"R51",sep=""),
			ifelse(reggames$yr<1999,paste(reggames$result,"R75",sep=""),paste(reggames$result,"R99",sep=""))))
regsum <- as.data.frame.matrix(table(reggames$team,reggames$result2))
regsum$team <- rownames(regsum)

byround <- merge(teams[c("team","WR","LR","WS","LS","WX","LX","WT","LT","W","L")],
			regtab[c("team","W75","N75","W99","N99")],by="team",all=T)
byround <- merge(byround,disttab[c("team","Freq")],by="team",all=T)
byround <- merge(byround,supersum[c("team","SRW","SRL")],by="team",all=T)
byround <- merge(byround,regsum,by="team",all.x=T)

byround$regw <- ifelse(is.na(byround$Freq),0,byround$Freq)+byround$W75+byround$N75+byround$W99+byround$N99

byround$class <- 0

byround$row <- paste("<tr class='d",byround$class,"'><td>",paste(
				paste("<a href='",teamlink(byround$team),".html'>",byround$team,"</a>",sep=""),
				ifelse(byround$WR+byround$LR==0,"**",paste(byround$WR,byround$LR,sep="-")),
				ifelse(byround$WS+byround$LS==0,"",paste(byround$WS,byround$LS,sep="-")),
				ifelse(byround$WX+byround$LX==0,"",paste(byround$WX,byround$LX,sep="-")),
				ifelse(byround$regw==0,"",paste(byround$regw)),
				ifelse(is.na(byround$SRW)|byround$SRW==0,"",paste(byround$SRW)),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/byround.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Regional/Super Regional Records by Team</center></h3>"))
cat("<br><table width='80%' align='center'>")
cat(paste("<tr><th></th><th class='hl' colspan='3'>W-L Record</th><th class='hl' colspan='2'>Regionals Won</th>",sep=""))
cat(paste("<tr><th></th><th>Regional</th><th>Super</th><th>Total</th><th>Regional</th><th>Super</th></tr>",sep=""))
write.table(byround["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# by decade
############################

decade <- subset(games,regional=="CWS")
decade$decade <- floor(decade$yr/10)*10
decade$key <- paste(decade$decade,decade$team,sep="_")
summdec <- as.data.frame.matrix(table(decade$key,decade$result.f))
summdec$key <- rownames(summdec)
summdec$decade <- as.numeric(substr(summdec$key,1,4))
summdec$team <- substr(summdec$key,6,24)
summdec$WT <- summdec$WC + summdec$WF
summdec$LT <- summdec$LC + summdec$LF

dranks <- subset(summ, crank<9)[c("yr","team","crank")]
dranks$decade <- floor(dranks$yr/10)*10
dranks$key <- paste(dranks$decade,dranks$team,sep="_")
summdr <- as.data.frame.matrix(table(dranks$key,dranks$crank))
summdr$key <- rownames(summdr)
summdr$decade <- as.numeric(substr(summdr$key,1,4))
summdr$team <- substr(summdr$key,6,24)
summdr$app <- summdr$"1" + summdr$"2" + summdr$"3" + summdr$"4" + summdr$"5" + summdr$"7"

summdec <- merge(summdec,summdr[c("decade","team","app","1","2","3","4","5","7")],by=c("decade","team"))
summdec$rank <- ave(-summdec$WT, summdec$decade, FUN = function(x) rank(x, ties.method = "min") )

summdec <- subset(summdec[order(summdec$decade, -summdec$WT, -summdec$WT+summdec$LT), ],rank<=10)

summdec$row <- paste("<tr><td>",paste(
				paste("<a href='",teamlink(summdec$team),".html'>",summdec$team,"</a>",sep=""),
				summdec$app,
				summdec$WT,
				summdec$LT,
				summdec$"1",
				summdec$"2",
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/decade.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>Best Teams by Decade</center></h3>"))
cat("<br><table width='80%' align='center'>")

for (dec in seq(from=1940, to=2020, by=10)) {
cat(paste("<tr><th class='hl' colspan='6'>",dec,"s</th>",sep=""))
cat(paste("<tr><th></th><th>Apps.</th><th>W</th><th>L</th><th>1ST</th><th>2ND</th></tr>",sep=""))
write.table(subset(summdec,decade==dec)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr><th colspan='6'> </th>",sep=""))
}

cat("</table><br>")
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# streaks
############################

steak <- summ[c("yr","team","crank")]
steak$yr2 <- ifelse(steak$yr>=2021,steak$yr-1,steak$yr)

for (p in 1:nrow(steak)) {
if (p==1) { steak$streak[p] <- 1}
if (p> 1) { steak$streak[p] <- ifelse(steak$yr2[p]-steak$yr2[p-1]!=1,1,steak$streak[p-1]+1) }
if (p==1) { steak$start[p] <- steak$yr[p]}
if (p> 1) { steak$start[p] <- ifelse(steak$yr2[p]-steak$yr2[p-1]!=1,steak$yr[p],steak$start[p-1]) }
if (p>=1) { steak$end[p] <- ifelse(steak$yr2[p+1]-steak$yr2[p]!=1,"end","") }}
yy <- subset(steak,end=="end" & streak>=5)[c("team","streak","start","yr")]
yy <- yy[order(-yy$streak, -yy$start),]

steak2 <- subset(summ[c("yr","team","crank")],crank!=999)
steak2$yr2 <- ifelse(steak2$yr>=2021,steak2$yr-1,steak2$yr)

for (p in 1:nrow(steak2)) {
if (p==1) { steak2$streak[p] <- 1}
if (p> 1) { steak2$streak[p] <- ifelse(steak2$yr2[p]-steak2$yr2[p-1]!=1,1,steak2$streak[p-1]+1) }
if (p==1) { steak2$start[p] <- steak2$yr[p]}
if (p> 1) { steak2$start[p] <- ifelse(steak2$yr2[p]-steak2$yr2[p-1]!=1,steak2$yr[p],steak2$start[p-1]) }
if (p>=1) { steak2$end[p] <- ifelse(steak2$yr2[p+1]-steak2$yr2[p]!=1,"end","") }}
zz <- subset(steak2,end=="end" & streak>=3)[c("team","streak","start","yr")]
zz <- zz[order(-zz$streak, -zz$start),]

yy$class <- ifelse(yy$yr==max(yy$yr),2,0)

yy$row <- paste("<tr class='d",yy$class,"'><td>",paste(
				paste(yy$streak),
				paste("<a href='",teamlink(yy$team),".html'>",yy$team,"</a>",sep=""),
				paste(yy$start),
				paste(yy$yr),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/streakr.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>NCAA Tournament Appearance Streaks</center></h3>"))
cat("<br><table width='80%' align='center'>")
cat(paste("<tr><th>Streak</th><th> </th><th>Start</th><th>End</th></tr>",sep=""))
write.table(yy["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

###

zz$class <- ifelse(zz$yr==max(zz$yr),2,0)

zz$row <- paste("<tr class='d",zz$class,"'><td>",paste(
				paste(zz$streak),
				paste("<a href='",teamlink(zz$team),".html'>",zz$team,"</a>",sep=""),
				paste(zz$start),
				paste(zz$yr),
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../CWS/streak.html",sep=""))
write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h3><center>College World Series Appearance Streaks</center></h3>"))
cat("<br><table width='80%' align='center'>")
cat(paste("<tr><th>Streak</th><th> </th><th>Start</th><th>End</th></tr>",sep=""))
write.table(zz["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")
write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

############################
# run per game, debut teams
############################

runs <- subset(decade, is.na(rf)==F & result=="W")
runs$rf <- as.numeric(runs$rf)
runs$ra <- as.numeric(runs$ra)
runs$runs <- runs$rf + runs$ra

ravg <- aggregate(runs$runs, list(runs$yr), mean)
runf <- aggregate(runs$rf  , list(runs$yr), median)
runa <- aggregate(runs$ra  , list(runs$yr), median)
cbind(ravg,runf,runa)

debut <- subset(summ, WC+LC>0)[c("yr","team","WT","LT","crank")]
debut$fyr <- ave(debut$yr, debut$team, FUN = function(x) min(x) )
debut <- subset(debut, yr==fyr)
debut <- debut[order(debut$fyr, debut$team), ]

############################
# generate statpage
############################

sink(paste("../CWS/stats.html",sep=""))

write.table(header, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th class='hl'><div style='font-size:1.5em;'>Statistics</div></th>","</tr>"))
cat(paste("<tr>","<th>College World Series</th>","</tr>"))
cat(paste("<tr>","<td><a href='byteam1.html'>Sortable College World Series Records</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='cwssum.html'>CWS Records by Round and Finish</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='champgm.html'>Championship Games and Series by Year</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='chseries.html'>Championship Series Records by Team</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='matchupsc.html'>Most Common CWS Matchups</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='streak.html'>CWS Appearance Streaks</a></td>","</tr>"))
cat(paste("<tr>","<th>NCAA Tournament</th>","</tr>"))
cat(paste("<tr>","<td><a href='byteamr1.html'>Sortable NCAA Tournament Records</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='byround.html'>Regional/Super Regional Records by Team</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='matchupsr.html'>Most Common NCAA Tournament Matchups</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='streakr.html'>NCAA Tournament Appearance Streaks</a></td>","</tr>"))
cat(paste("<tr>","<th>Super Regional Era (since 1999)</th>","</tr>"))
cat(paste("<tr>","<td><a href='natseed.html'>National Seed Results by Year</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='supers.html'>Super Regional Results by Year</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='supersum.html'>Super Regional Records by Team</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='regnls2.html'>Regional Finals by Year</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='regteam.html'>Regional Records and Finishes by Team</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='regstat.html'>Regional/Super Regional Statistics by Year</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='regseed.html'>Regional Finishes By Seed and Year</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='hosts1.html'>Success of Regional Hosts</a></td>","</tr>"))
cat(paste("<tr>","<th>Regional Era (1975-1998)</th>","</tr>"))
cat(paste("<tr>","<td><a href='regnls1.html'>Regional Finals by Year</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='regteam2.html'>Regional Records and Finishes by Team</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='regstat2.html'>Regional Statistics by Year</a></td>","</tr>"))
cat(paste("<tr>","<td><a href='hosts2.html'>Success of Regional Hosts</a></td>","</tr>"))
cat(paste("<tr>","<th>District Era (1947-1974)</th>","</tr>"))
cat(paste("<tr>","<td><a href='dists.html'>District Winners</a></td>","</tr>"))
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

