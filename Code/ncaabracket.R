
##########################################

firstword <- function(x) {toupper(gsub("(\\w+).*", "\\1", x))}
teamlink <- function(x){teamx = tolower(gsub("&","_",gsub(".","",gsub("'","",gsub(" ", "",x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE))}

breaks <- function(n) {
if (n=="EAST" ) {n<-"E<br>A<br>S<br>T"}
else if (n=="WEST" ) {n<-"W<br>E<br>S<br>T"}
else if (n=="MIDEAST" ) {n<-"M<br>I<br>D<br>E<br>A<br>S<br>T"}
else if (n=="MIDWEST" ) {n<-"M<br>I<br>D<br>W<br>E<br>S<br>T"}
else if (n=="FAR" ) {n<-"F<br>A<br>R<br>W<br>E<br>S<br>T"}
}


ncaat <- read.csv("../Data/ncaatournament.csv")

#################

for (j in c(1985:2019,2021:2026)) {

bracket <- subset(ncaat, year==j)

#bracket$text <- paste(bracket$team1,bracket$score1,"<br>",bracket$team2,bracket$score2,sep=" ")
#bracket$text <- paste("<b>#",bracket$seed1,"</b> ",bracket$team1," <b>",bracket$score1,"</b><br>",
#			    "<b>#",bracket$seed2,"</b> ",bracket$team2," <b>",bracket$score2,"</b>",sep="")
bracket$text <- paste("<a href='",teamlink(bracket$team1),".html'>",bracket$team1,"</a> <b>",bracket$score1,"</b><br>",
			    "<a href='",teamlink(bracket$team2),".html'>",bracket$team2,"</a> <b>",bracket$score2,"</b>",
				ifelse(bracket$ot!="",paste(" (",bracket$ot,")",sep=""),""),sep="")

fileName <- "./bracket64.html"
HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>Basketball Men</a>",HTML)
HTML <- gsub("YEAR1",paste(j," (<a href='",j,".html'>Games</a>",")",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)
HTML <- gsub("<< <a href='bracket2020.html'>2020</a> ","<< <a href='bracket2019.html'>2019</a> ",HTML)
HTML <- gsub("<a href='bracket2020.html'>2020</a> >>","<a href='bracket2021.html'>2021</a> >>",HTML)
HTML <- gsub("<a href='bracket2025.html'>2025</a> >>","2025 >>",HTML)

HTML <- gsub("YYYY",paste(j),HTML)
REG1 <- paste(subset(bracket, game==301)$name1); HTML <- gsub("REGIONAL 1",firstword(REG1),HTML)
REG2 <- paste(subset(bracket, game==302)$name1); HTML <- gsub("REGIONAL 2",firstword(REG2),HTML)
REG3 <- paste(subset(bracket, game==303)$name1); HTML <- gsub("REGIONAL 3",firstword(REG3),HTML)
REG4 <- paste(subset(bracket, game==304)$name1); HTML <- gsub("REGIONAL 4",firstword(REG4),HTML)

champ <- subset(bracket, game==101)
champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../CBB/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

################

for (j in 1979:1984) {

bracket <- subset(ncaat, year==j)

bracket$text <- paste(ifelse(bracket$game>600|(bracket$game>500 & bracket$year==1979 & bracket$seed1<=6)
							     ,paste("<h6>",bracket$seed1,". </h6>",sep=""),""),"","<a href='",teamlink(bracket$team1),".html'>",bracket$team1,"</a> <b>",bracket$score1,"</b><br>",
				ifelse(bracket$game>500,paste("<h6>",bracket$seed2,". </h6>",sep=""),""),"","<a href='",teamlink(bracket$team2),".html'>",bracket$team2,"</a> <b>",bracket$score2,"</b>",
				ifelse(bracket$ot!="",paste(" (",bracket$ot,")",sep=""),""),sep="")

fileName <- "./bracket48.html"
HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in 601:632) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>Basketball Men</a>",HTML)
HTML <- gsub("YEAR1",paste(j," (<a href='",j,".html'>Games</a>",")",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)

HTML <- gsub("YYYY",paste(j),HTML)
REG1 <- paste(subset(bracket, game==301)$name1); HTML <- gsub("REGIONAL 1",firstword(REG1),HTML)
REG2 <- paste(subset(bracket, game==302)$name1); HTML <- gsub("REGIONAL 2",firstword(REG2),HTML)
REG3 <- paste(subset(bracket, game==303)$name1); HTML <- gsub("REGIONAL 3",firstword(REG3),HTML)
REG4 <- paste(subset(bracket, game==304)$name1); HTML <- gsub("REGIONAL 4",firstword(REG4),HTML)

champ <- subset(bracket, game==101)
champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../CBB/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

################

for (j in 1939:1978) {

bracket <- subset(ncaat, year==j)

bracket$text <- paste("<a href='",teamlink(bracket$team1),".html'>",bracket$team1,"</a> <b>",bracket$score1,"</b><br>",
				"<a href='",teamlink(bracket$team2),".html'>",bracket$team2,"</a> <b>",bracket$score2,"</b>",
				ifelse(bracket$ot!="",paste(" (",bracket$ot,")",sep=""),""),sep="")

if (j>=1953) {fileName <- "./bracket32.html"}
if (j==1952) {fileName <- "./bracket16.html"}
if (j==1951) {fileName <- "./bracket28.html"}
if (j<=1950) {fileName <- "./bracket08.html"}

HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in 501:516) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>Basketball Men</a>",HTML)
HTML <- gsub("YEAR1",paste(j," (<a href='",j,".html'>Games</a>",")",sep=""),HTML)
if (j==1939) {HTML <- gsub("YEAR0","First",HTML)}
if (j>=1939) {HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)}
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)

HTML <- gsub("YYYY",paste(j),HTML)
REG1 <- paste(subset(bracket, game==301)$name1); HTML <- gsub("REGIONAL 1",breaks(firstword(REG1)),HTML)
REG2 <- paste(subset(bracket, game==302)$name1); HTML <- gsub("REGIONAL 2",breaks(firstword(REG2)),HTML)
REG3 <- paste(subset(bracket, game==303)$name1); HTML <- gsub("REGIONAL 3",breaks(firstword(REG3)),HTML)
REG4 <- paste(subset(bracket, game==304)$name1); HTML <- gsub("REGIONAL 4",breaks(firstword(REG4)),HTML)

#champ <- subset(bracket, game==101)
#champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
#CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../CBB/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

