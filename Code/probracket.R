
##########################################

firstword <- function(x) {toupper(gsub("(\\w+).*", "\\1", x))}

breaks <- function(n) {
if (n=="EAST" ) {n<-"E<br>A<br>S<br>T"}
else if (n=="WEST" ) {n<-"W<br>E<br>S<br>T"}
else if (n=="EASTERN" ) {n<-"E<br>A<br>S<br>T"}
else if (n=="WESTERN" ) {n<-"W<br>E<br>S<br>T"}
else if (n=="AFC" ) {n<-"A<br>F<br>C"}
else if (n=="NFC" ) {n<-"N<br>F<br>C"}
else if (n=="AFL" ) {n<-"A<br>F<br>L"}
else if (n=="NFL" ) {n<-"N<br>F<br>L"}
else if (n=="AMERICAN" ) {n<-"A<br> <br>L"}
else if (n=="NATIONAL" ) {n<-"N<br> <br>L"}
else if (n=="SEMIFINALS" ) {n<-" "}
else if (n=="WALES" ) {n<-"W<br>A<br>L<br>E<br>S"}
else if (n=="CAMPBELL" ) {n<-"C<br>A<br>M<br>P<br>B<br>E<br>L<br>L"}
}


pro <- read.csv("../Data/protournament.csv")

################

nfl <- subset(pro, sport=="NFL")

for (j in 1966:2025) {

bracket <- subset(nfl, year==j)

bracket$text <- paste(bracket$team1," <b>",bracket$score1,"</b><br>",
				bracket$team2," <b>",bracket$score2,"</b>",
				ifelse(bracket$ot!="",paste(" (",bracket$ot,")",sep=""),""),sep="")

if (j>=1966) {fileName <- "./probracket22.html"}
if (j>=1967) {fileName <- "./probracket24.html"}
if (j>=1978) {fileName <- "./probracket28.html"}

HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in c(401:408,301:304)) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>NFL</a>",HTML)
HTML <- gsub("YEAR1",paste("<a href='",paste(j),".html'>",paste(j),"</a>",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)
HTML <- gsub("<a href='bracket2026.html'>2026</a> >>","2026 >>",HTML)
HTML <- gsub("<< <a href='bracket1965.html'>1965</a> ","<< First ",HTML)

HTML <- gsub("YYYY",paste(j),HTML)
REG1 <- paste(subset(bracket, game==201)$name1); HTML <- gsub("REGIONAL 1",breaks(firstword(REG1)),HTML)
REG2 <- paste(subset(bracket, game==202)$name1); HTML <- gsub("REGIONAL 2",breaks(firstword(REG2)),HTML)

HTML <- gsub("EVENTEVENT","NFL Playoffs",HTML)
HTML <- gsub("ROUND1","Wild Card",HTML)
HTML <- gsub("ROUND2","Divisional",HTML)
HTML <- gsub("ROUND3","Conference",HTML)
HTML <- gsub("ROUND4","Super Bowl",HTML)

#champ <- subset(bracket, game==101)
#champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
#CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../NFL/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

################

mlb <- subset(pro, sport=="MLB")

for (j in c(1969:1993,1995:2025)) {

bracket <- subset(mlb, year==j)

bracket$text <- paste(bracket$team1," <b>",bracket$score1,"</b><br>",
				bracket$team2," <b>",bracket$score2,"</b>",
				ifelse(bracket$ot!="",paste(" (",bracket$ot,")",sep=""),""),sep="")

if (j>=1969) {fileName <- "./probracket22.html"}
if (j>=1995) {fileName <- "./probracket24.html"}
if (j>=2013) {fileName <- "./probracket28.html"}

HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in c(401:408,301:304)) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>MLB</a>",HTML)
HTML <- gsub("YEAR1",paste("<a href='",paste(j),".html'>",paste(j),"</a>",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)
HTML <- gsub("<< <a href='bracket1994.html'>1994</a> ","<< <a href='bracket1993.html'>1993</a> ",HTML)
HTML <- gsub("<a href='bracket1994.html'>1994</a> >>","<a href='bracket1995.html'>1995</a> >>",HTML)
HTML <- gsub("<a href='bracket2025.html'>2025</a> >>","2025 >>",HTML)
HTML <- gsub("<< <a href='bracket1968.html'>1968</a> ","<< First ",HTML)

HTML <- gsub("YYYY",paste(j),HTML)
REG1 <- paste(subset(bracket, game==201)$name1); HTML <- gsub("REGIONAL 1",breaks(firstword(REG1)),HTML)
REG2 <- paste(subset(bracket, game==202)$name1); HTML <- gsub("REGIONAL 2",breaks(firstword(REG2)),HTML)

HTML <- gsub("EVENTEVENT","MLB Playoffs",HTML)
HTML <- gsub("ROUND1","Wild Card",HTML)
HTML <- gsub("ROUND2","LDS",HTML)
HTML <- gsub("ROUND3","LCS",HTML)
HTML <- gsub("ROUND4","World Series",HTML)

#champ <- subset(bracket, game==101)
#champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
#CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../MLB/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

################

nba <- subset(pro, sport=="NBA")

for (j in 1947:2025) {

bracket <- subset(nba, year==j)

bracket$text <- paste(bracket$team1," <b>",bracket$score1,"</b><br>",
				bracket$team2," <b>",bracket$score2,"</b>",
				ifelse(bracket$ot!="",paste(" (",bracket$ot,")",sep=""),""),sep="")

if (j==1947|j==1948) {fileName <- "./probracket8.html"}
if (j>=1949) {fileName <- "./probracket24.html"}
if (j>=1975) {fileName <- "./probracket28.html"}
if (j==1950) {fileName <- "./probracket16.html"}
if (j==1954) {fileName <- "./probracket22.html"}

HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in c(401:408,301:304)) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>NBA</a>",HTML)
HTML <- gsub("YEAR1",paste("<a href='",paste(j),".html'>",paste(j),"</a>",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)
HTML <- gsub("<a href='bracket2026.html'>2026</a> >>","2025 >>",HTML)
HTML <- gsub("<< <a href='bracket1946.html'>1946</a> ","<< First ",HTML)

HTML <- gsub("YYYY",paste(j),HTML)
REG1 <- paste(subset(bracket, game==201)$name1); HTML <- gsub("REGIONAL 1",breaks(firstword(REG1)),HTML)
REG2 <- paste(subset(bracket, game==202)$name1); HTML <- gsub("REGIONAL 2",breaks(firstword(REG2)),HTML)

HTML <- gsub("EVENTEVENT","NBA Playoffs",HTML)
HTML <- gsub("ROUND1","&nbsp;",HTML)
HTML <- gsub("ROUND2","&nbsp;",HTML)
HTML <- gsub("ROUND3","&nbsp;",HTML)
HTML <- gsub("ROUND4","NBA Finals",HTML)

#champ <- subset(bracket, game==101)
#champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
#CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../NBA/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

################

nhl <- subset(pro, sport=="NHL")

for (j in c(1927:2004,2006:2025)) {

bracket <- subset(nhl, year==j)

bracket$text <- paste(bracket$team1," <b>",bracket$score1,"</b><br>",
				bracket$team2," <b>",bracket$score2,"</b>",
				ifelse(bracket$ot!="",paste(" (",bracket$ot,")",sep=""),""),sep="")

if (j>=1927 & j<=1942) {fileName <- "./probracket8.html"}
if (j>=1943 & j<=1967) {fileName <- "./probracket4.html"}
if (j>=1968 & j<=1970) {fileName <- "./probracket24.html"}
if (j>=1971 & j<=1974) {fileName <- "./probracket8.html"}
if (j>=1975 & j<=1981) {fileName <- "./probracket16.html"}
if (j>=1982 & j<=2020) {fileName <- "./probracket28.html"}
if (j==2021) {fileName <- "./probracket16.html"}
if (j>=2022 & j<=9999) {fileName <- "./probracket28.html"}

HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in c(401:408,301:304)) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>NHL</a>",HTML)
HTML <- gsub("YEAR1",paste("<a href='",paste(j),".html'>",paste(j),"</a>",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)
HTML <- gsub("<< <a href='bracket2005.html'>2005</a> ","<< <a href='bracket2004.html'>2004</a> ",HTML)
HTML <- gsub("<a href='bracket2005.html'>2005</a> >>","<a href='bracket2006.html'>2006</a> >>",HTML)
HTML <- gsub("<a href='bracket2026.html'>2026</a> >>","2025 >>",HTML)
HTML <- gsub("<< <a href='bracket1926.html'>1926</a> ","<< First ",HTML)

HTML <- gsub("YYYY",paste(j),HTML)
REG1 <- paste(subset(bracket, game==201)$name1); HTML <- gsub("REGIONAL 1",breaks(firstword(REG1)),HTML)
REG2 <- paste(subset(bracket, game==202)$name1); HTML <- gsub("REGIONAL 2",breaks(firstword(REG2)),HTML)

HTML <- gsub("EVENTEVENT","Stanley Cup Playoffs",HTML)
HTML <- gsub("ROUND1","&nbsp;",HTML)
HTML <- gsub("ROUND2","&nbsp;",HTML)
HTML <- gsub("ROUND3","&nbsp;",HTML)
HTML <- gsub("ROUND4","Stanley Cup Finals",HTML)

#champ <- subset(bracket, game==101)
#champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
#CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../NHL/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

################

mls <- subset(pro, sport=="MLS")

for (j in c(1996:2025)) {

bracket <- subset(mls, year==j)

bracket$text <- paste(bracket$team1," <b>",bracket$score1,"</b><br>",
				bracket$team2," <b>",bracket$score2,"</b>",
				ifelse(bracket$ot!="",paste(" (",bracket$ot,")",sep=""),""),sep="")

if (j<=2010) {fileName <- "./probracket24.html"}
if (j>=2000 & j<=2002) {fileName <- "./probracket8.html"}
if (j>=2011) {fileName <- "./probracket28.html"}

HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in c(401:408,301:304)) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>MLS</a>",HTML)
HTML <- gsub("YEAR1",paste("<a href='",paste(j),".html'>",paste(j),"</a>",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)
HTML <- gsub("<a href='bracket2025.html'>2025</a> >>","2025 >>",HTML)
HTML <- gsub("<< <a href='bracket1995.html'>1995</a> ","<< First ",HTML)

HTML <- gsub("YYYY",paste(j),HTML)
REG1 <- paste(subset(bracket, game==201)$name1); HTML <- gsub("REGIONAL 1",breaks(firstword(REG1)),HTML)
REG2 <- paste(subset(bracket, game==202)$name1); HTML <- gsub("REGIONAL 2",breaks(firstword(REG2)),HTML)

HTML <- gsub("EVENTEVENT","MLS Cup Playoffs",HTML)
HTML <- gsub("ROUND1","&nbsp;",HTML)
HTML <- gsub("ROUND2","&nbsp;",HTML)
HTML <- gsub("ROUND3","&nbsp;",HTML)
HTML <- gsub("ROUND4","MLS Cup Final",HTML)

#champ <- subset(bracket, game==101)
#champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
#CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../MLS/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}
