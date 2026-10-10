
##########################################

firstword <- function(x) {toupper(gsub("(\\w+).*", "\\1", x))}
teamlink <- function(x){teamx = tolower(gsub("&","_",gsub(".","",gsub("'","",gsub(" ", "",x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE))}

breaks <- function(n) {
if (n=="EAST" ) {n<-"E<br>A<br>S<br>T"}
else if (n=="WEST" ) {n<-"W<br>E<br>S<br>T"}
else if (n=="NORTHEAST" ) {n<-"N<br>E"}
else if (n=="MIDWEST" ) {n<-"M<br>W"}
else if (n=="MIDEAST" ) {n<-"M<br>I<br>D<br>E<br>A<br>S<br>T"}
else if (n=="PACIFIC" ) {n<-"P<br>A<br>C<br>I<br>F<br>I<br>C"}
else if (n=="NORTHWEST" ) {n<-"N<br>O<br>R<br>T<br>H<br>W<br>E<br>S<br>T"}
else if (n=="SOUTHWEST" ) {n<-"S<br>O<br>U<br>T<br>H<br>W<br>E<br>S<br>T"}
else if (n=="SOUTHEAST" ) {n<-"S<br>O<br>U<br>T<br>H<br>E<br>A<br>S<br>T"}
else if (n=="SOUTH" ) {n<-"S<br>O<br>U<br>T<br>H"}
else if (n=="CENTRAL" ) {n<-"C<br>E<br>N<br>T<br>R<br>A<br>L"}
else if (n=="MOUNTAIN" ) {n<-"M<br>O<br>U<br>N<br>T<br>A<br>I<br>N"}
else if (n=="ALAMO" ) {n<-"A<br>L<br>A<br>M<br>O"}
else if (n=="HEMISFAIR" ) {n<-"H<br>E<br>M<br>I<br>S<br>F<br>A<br>I<br>R"}
else if (n=="MERCADO" ) {n<-"M<br>E<br>R<br>C<br>A<br>D<br>O"}
else if (n=="RIVER" ) {n<-"R<br>I<br>V<br>E<br>R<br> <br>W<br>A<br>L<br>K"}
else if (n=="FROZEN" ) {n<-" "}
}


ncaa <- read.csv("../Data/ncaagames.csv")

################

hky <- subset(ncaa, sport=="HKY")

### series ###
hky.1 <- subset(hky, (round==10 | round==20) & name2=="Game 1")
hky.2 <- subset(hky, (round==10 | round==20) & name2=="Game 2")
hky.3 <- subset(hky, (round==10 | round==20) & name2=="Game 3")

hky.4 <- merge(hky.1,hky.2,by=c("sport","year","name1","team1","team2","round","game"),all.x=T)
hky.4 <- merge(hky.4,hky.3,by=c("sport","year","name1","team1","team2","round","game"),all.x=T)

hky.4$score1 <- paste(hky.4$score1.x,"-",hky.4$score1.y,ifelse(is.na(hky.4$score1)==F,paste("-",hky.4$score1,sep=""),""),sep="")
hky.4$score2 <- paste(hky.4$score2.x,"-",hky.4$score2.y,ifelse(is.na(hky.4$score2)==F,paste("-",hky.4$score2,sep=""),""),sep="")
hky.4$ot <- ""
##############

hky <- subset(hky, round!=10 & round!=20)
hky <- rbind(hky[c("sport","year","name1","name2","round","game","team1","team2","score1","score2","ot")],
		 hky.4[c("sport","year","name1","name2","round","game","team1","team2","score1","score2","ot")])

for (j in c(1948:2019,2021:2026)) {

bracket <- subset(hky, year==j)

bracket$text <- paste("<a href='",teamlink(bracket$team1),".html'>",bracket$team1,"</a> <b>",bracket$score1,"</b><br>",
				"<a href='",teamlink(bracket$team2),".html'>",bracket$team2,"</a> <b>",bracket$score2,"</b>",
				ifelse(bracket$ot!="",paste(" (",bracket$ot,")",sep=""),""),sep="")

if (j<=1976) {fileName <- "./probracket4.html"}
else if (j<=1987) {fileName <- "./probracket8.html"}
else if (j<=2002) {fileName <- "./probracket16.html"}
else if (j<=9999) {fileName <- "./bracket16.html"}

HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in c(401:408,301:304)) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>Hockey</a>",HTML)
HTML <- gsub("YEAR1",paste(j," (<a href='",j,".html'>Games</a>",")",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)
HTML <- gsub("<< <a href='bracket2020.html'>2020</a> ","<< <a href='bracket2019.html'>2019</a> ",HTML)
HTML <- gsub("<a href='bracket2020.html'>2020</a> >>","<a href='bracket2021.html'>2021</a> >>",HTML)
HTML <- gsub("<a href='bracket2027.html'>2027</a> >>","2027 >>",HTML)
HTML <- gsub("<< <a href='bracket1947.html'>1947</a> ","<< First ",HTML)

HTML <- gsub("YYYY",paste(j),HTML)

if (j>=2003) {
REG1 <- paste(subset(bracket, game==301)$name1); HTML <- gsub("REGIONAL 1",breaks(firstword(REG1)),HTML)
REG2 <- paste(subset(bracket, game==302)$name1); HTML <- gsub("REGIONAL 2",breaks(firstword(REG2)),HTML)
REG3 <- paste(subset(bracket, game==303)$name1); HTML <- gsub("REGIONAL 3",breaks(firstword(REG3)),HTML)
REG4 <- paste(subset(bracket, game==304)$name1); HTML <- gsub("REGIONAL 4",breaks(firstword(REG4)),HTML)
}

HTML <- gsub("EVENTEVENT","NCAA Hockey Tournament",HTML)
HTML <- gsub("NCAA Basketball Tournament","NCAA Hockey Tournament",HTML)
HTML <- gsub("ROUND1","First Round",HTML)
HTML <- gsub("ROUND2","Quarterfinals",HTML)
HTML <- gsub("ROUND3","Semifinals",HTML)
HTML <- gsub("ROUND4","Championship",HTML)
HTML <- gsub("Final Four","Frozen Four",HTML)

#champ <- subset(bracket, game==101)
#champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
#CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../HKY/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

################

vol <- subset(ncaa, sport=="VOL")

for (j in 1981:2025) {

bracket <- subset(vol, year==j)

bracket$text <- paste("<a href='",teamlink(bracket$team1),".html'>",bracket$team1,"</a> <b>",bracket$score1,"</b><br>",
				"<a href='",teamlink(bracket$team2),".html'>",bracket$team2,"</a> <b>",bracket$score2,"</b>",sep="")

if (j>=2022) {fileName <- "./vbracket642.html"}
if (2000<=j & j <=2021) {fileName <- "./vbracket64.html"}
if (1993<=j & j <=1999) {fileName <- "./bracket48.html"}
if (1981<=j & j <=1992) {fileName <- "./bracket32.html"}

HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in c(601:632,501:516,401:408,301:304)) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>Volleyball</a>",HTML)
HTML <- gsub("YEAR1",paste(j," (<a href='",j,".html'>Games</a>",")",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)
HTML <- gsub("<a href='bracket2026.html'>2026</a> >>","2026 >>",HTML)
HTML <- gsub("<< <a href='bracket1980.html'>1980</a> ","<< First ",HTML)

HTML <- gsub("YYYY",paste(j),HTML)

if (j<=1992) {
REG1 <- paste(subset(bracket, game==301)$name1); HTML <- gsub("REGIONAL 1",breaks(firstword(REG1)),HTML)
REG2 <- paste(subset(bracket, game==302)$name1); HTML <- gsub("REGIONAL 2",breaks(firstword(REG2)),HTML)
REG3 <- paste(subset(bracket, game==303)$name1); HTML <- gsub("REGIONAL 3",breaks(firstword(REG3)),HTML)
REG4 <- paste(subset(bracket, game==304)$name1); HTML <- gsub("REGIONAL 4",breaks(firstword(REG4)),HTML)
}

if (j>=1993 & j<=1999) {
REG1 <- paste(subset(bracket, game==301)$name1); HTML <- gsub("REGIONAL 1",firstword(REG1),HTML)
REG2 <- paste(subset(bracket, game==302)$name1); HTML <- gsub("REGIONAL 2",firstword(REG2),HTML)
REG3 <- paste(subset(bracket, game==303)$name1); HTML <- gsub("REGIONAL 3",firstword(REG3),HTML)
REG4 <- paste(subset(bracket, game==304)$name1); HTML <- gsub("REGIONAL 4",firstword(REG4),HTML)
}

if (j>=2000) {
REG1 <- paste(subset(bracket, game==301)$name1); HTML <- gsub("REGIONAL 1",toupper(REG1),HTML)
REG2 <- paste(subset(bracket, game==302)$name1); HTML <- gsub("REGIONAL 2",toupper(REG2),HTML)
REG3 <- paste(subset(bracket, game==303)$name1); HTML <- gsub("REGIONAL 3",toupper(REG3),HTML)
REG4 <- paste(subset(bracket, game==304)$name1); HTML <- gsub("REGIONAL 4",toupper(REG4),HTML)
HTML <- gsub(" REGIONAL","",HTML)
}

HTML <- gsub("EVENTEVENT","NCAA Volleyball Tournament",HTML)
HTML <- gsub("NCAA Basketball Tournament","NCAA Volleyball Tournament",HTML)
HTML <- gsub("ROUND1","First Round",HTML)
HTML <- gsub("ROUND2","Quarterfinals",HTML)
HTML <- gsub("ROUND3","Semifinals",HTML)
HTML <- gsub("ROUND4","Championship",HTML)
HTML <- gsub("Final Four","Final Four",HTML)

champ <- subset(bracket, game==101)
champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../VOL/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

################

wbb <- subset(ncaa, sport=="WBB")

for (j in c(1982:2019,2021:2026)) {

bracket <- subset(wbb, year==j)

bracket$text <- paste("<a href='",teamlink(bracket$team1),".html'>",bracket$team1,"</a> <b>",bracket$score1,"</b><br>",
				"<a href='",teamlink(bracket$team2),".html'>",bracket$team2,"</a> <b>",bracket$score2,"</b>",
				ifelse(bracket$ot!="",paste(" (",bracket$ot,")",sep=""),""),sep="")

if (j>=1994) {fileName <- "./bracket64.html"}
if (1989<=j & j <=1993) {fileName <- "./bracket48.html"}
if (1987<=j & j <=1988) {fileName <- "./bracket48.html"}
if (1981<=j & j <=1986) {fileName <- "./bracket32.html"}

HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in c(601:632,501:516,401:408,301:304)) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>Basketball Women</a>",HTML)
HTML <- gsub("YEAR1",paste(j," (<a href='",j,".html'>Games</a>",")",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)
HTML <- gsub("<< <a href='bracket2020.html'>2020</a> ","<< <a href='bracket2019.html'>2019</a> ",HTML)
HTML <- gsub("<a href='bracket2020.html'>2020</a> >>","<a href='bracket2021.html'>2021</a> >>",HTML)
HTML <- gsub("<a href='bracket2027.html'>2027</a> >>","2027 >>",HTML)
HTML <- gsub("<< <a href='bracket1981.html'>1981</a> ","<< First ",HTML)

HTML <- gsub("YYYY",paste(j),HTML)

if (j<=1986) {
REG1 <- paste(subset(bracket, game==301)$name1); HTML <- gsub("REGIONAL 1",breaks(firstword(REG1)),HTML)
REG2 <- paste(subset(bracket, game==302)$name1); HTML <- gsub("REGIONAL 2",breaks(firstword(REG2)),HTML)
REG3 <- paste(subset(bracket, game==303)$name1); HTML <- gsub("REGIONAL 3",breaks(firstword(REG3)),HTML)
REG4 <- paste(subset(bracket, game==304)$name1); HTML <- gsub("REGIONAL 4",breaks(firstword(REG4)),HTML)
}

if (j>=1986) {
REG1 <- paste(subset(bracket, game==301)$name1); HTML <- gsub("REGIONAL 1",firstword(REG1),HTML)
REG2 <- paste(subset(bracket, game==302)$name1); HTML <- gsub("REGIONAL 2",firstword(REG2),HTML)
REG3 <- paste(subset(bracket, game==303)$name1); HTML <- gsub("REGIONAL 3",firstword(REG3),HTML)
REG4 <- paste(subset(bracket, game==304)$name1); HTML <- gsub("REGIONAL 4",firstword(REG4),HTML)
}

HTML <- gsub("EVENTEVENT","NCAA Women's Basketball Tournament",HTML)
HTML <- gsub("NCAA Basketball Tournament","NCAA Women's Basketball Tournament",HTML)
HTML <- gsub("ROUND1","First Round",HTML)
HTML <- gsub("ROUND2","Quarterfinals",HTML)
HTML <- gsub("ROUND3","Semifinals",HTML)
HTML <- gsub("ROUND4","Championship",HTML)
HTML <- gsub("Final Four","Final Four",HTML)

champ <- subset(bracket, game==101)
champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../WBB/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

################

mso <- subset(ncaa, sport=="MSO")

for (j in 1959:2025) {

bracket <- subset(mso, year==j)

bracket$text <- paste("<a href='",teamlink(bracket$team1),".html'>",bracket$team1,"</a> <b>",bracket$score1,"</b><br>",
				"<a href='",teamlink(bracket$team2),".html'>",bracket$team2,"</a> <b>",bracket$score2,"</b>",
ifelse(bracket$ot=="OT"|bracket$ot=="2OT"|bracket$ot=="3OT"|bracket$ot=="4OT",paste(" (",bracket$ot,")",sep=""),""),sep="")

bracket$text <- gsub(".123","PK",bracket$text)

if (2001<=j & j <=2020) {fileName <- "./bracket48.html"}
if (1968<=j & j <=2000) {fileName <- "./bracket32.html"}
if (1963<=j & j <=1967) {fileName <- "./probracket16.html"}
if (1959<=j & j <=1962) {fileName <- "./probracket8.html"}

HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in c(601:632,501:516,401:408,301:304)) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>Soccer Men</a>",HTML)
HTML <- gsub("YEAR1",paste(j," (<a href='",j,".html'>Games</a>",")",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)
HTML <- gsub("<a href='bracket2026.html'>2026</a> >>","2026 >>",HTML)
HTML <- gsub("<< <a href='bracket1958.html'>1958</a> ","<< First ",HTML)

HTML <- gsub("YYYY",paste(j),HTML)

if (j>=1959) {
REG1 <- paste(subset(bracket, game==301)$name1); HTML <- gsub("REGIONAL 1","&nbsp;&nbsp;",HTML)
REG2 <- paste(subset(bracket, game==302)$name1); HTML <- gsub("REGIONAL 2","&nbsp;&nbsp;",HTML)
REG3 <- paste(subset(bracket, game==303)$name1); HTML <- gsub("REGIONAL 3","&nbsp;&nbsp;",HTML)
REG4 <- paste(subset(bracket, game==304)$name1); HTML <- gsub("REGIONAL 4","&nbsp;&nbsp;",HTML)
HTML <- gsub(" REGIONAL","",HTML)
}

HTML <- gsub("EVENTEVENT","NCAA Men's Soccer Tournament",HTML)
HTML <- gsub("NCAA Basketball Tournament","NCAA Men's Soccer Tournament",HTML)
HTML <- gsub("ROUND1","First Round",HTML)
HTML <- gsub("ROUND2","Quarterfinals",HTML)
HTML <- gsub("ROUND3","Semifinals",HTML)
HTML <- gsub("ROUND4","Championship",HTML)
HTML <- gsub("Final Four","Final Four",HTML)

champ <- subset(bracket, game==101)
champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../MSO/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

################

wso <- subset(ncaa, sport=="WSO")

for (j in 1982:2025) {

bracket <- subset(wso, year==j)

bracket$text <- paste("<a href='",teamlink(bracket$team1),".html'>",bracket$team1,"</a> <b>",bracket$score1,"</b><br>",
				"<a href='",teamlink(bracket$team2),".html'>",bracket$team2,"</a> <b>",bracket$score2,"</b>",
ifelse(bracket$ot=="OT"|bracket$ot=="2OT"|bracket$ot=="3OT"|bracket$ot=="4OT",paste(" (",bracket$ot,")",sep=""),""),sep="")

bracket$text <- gsub(".123","PK",bracket$text)

if (2022<=j & j <=9999) {fileName <- "./vbracket642.html"}
if (2021<=j & j <=2021) {fileName <- "./wbracket64.html"}
if (2020<=j & j <=2020) {fileName <- "./bracket48.html"}
if (2005<=j & j <=2019) {fileName <- "./wbracket64.html"}
if (1998<=j & j <=2004) {fileName <- "./bracket48.html"}
if (1994<=j & j <=1997) {fileName <- "./bracket32.html"}
if (1963<=j & j <=1993) {fileName <- "./probracket16.html"}

HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in c(601:632,501:516,401:408,301:304)) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>Soccer Women</a>",HTML)
HTML <- gsub("YEAR1",paste(j," (<a href='",j,".html'>Games</a>",")",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)
HTML <- gsub("<a href='bracket2026.html'>2026</a> >>","2026 >>",HTML)
HTML <- gsub("<< <a href='bracket1981.html'>1981</a> ","<< First ",HTML)

HTML <- gsub("YYYY",paste(j),HTML)

if (j>=1959) {
REG1 <- paste(subset(bracket, game==301)$name1); HTML <- gsub("REGIONAL 1","&nbsp;&nbsp;",HTML)
REG2 <- paste(subset(bracket, game==302)$name1); HTML <- gsub("REGIONAL 2","&nbsp;&nbsp;",HTML)
REG3 <- paste(subset(bracket, game==303)$name1); HTML <- gsub("REGIONAL 3","&nbsp;&nbsp;",HTML)
REG4 <- paste(subset(bracket, game==304)$name1); HTML <- gsub("REGIONAL 4","&nbsp;&nbsp;",HTML)
HTML <- gsub(" REGIONAL","",HTML)
}

HTML <- gsub("EVENTEVENT","NCAA Women's Soccer Tournament",HTML)
HTML <- gsub("NCAA Basketball Tournament","NCAA Women's Soccer Tournament",HTML)
HTML <- gsub("ROUND1","First Round",HTML)
HTML <- gsub("ROUND2","Quarterfinals",HTML)
HTML <- gsub("ROUND3","Semifinals",HTML)
HTML <- gsub("ROUND4","Championship",HTML)
HTML <- gsub("Final Four","Final Four",HTML)

champ <- subset(bracket, game==101)
champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../WSO/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

################

lax <- subset(ncaa, sport=="LAX")

for (j in c(1971:2019,2021:2026)) {

bracket <- subset(lax, year==j)

bracket$text <- paste("<a href='",teamlink(bracket$team1),".html'>",bracket$team1,"</a> <b>",bracket$score1,"</b><br>",
				"<a href='",teamlink(bracket$team2),".html'>",bracket$team2,"</a> <b>",bracket$score2,"</b>",
ifelse(bracket$ot=="OT"|bracket$ot=="2OT"|bracket$ot=="3OT"|bracket$ot=="4OT",paste(" (",bracket$ot,")",sep=""),""),sep="")

bracket$text <- gsub(".123","PK",bracket$text)

if (1986<=j & j <=9999) {fileName <- "./probracket16.html"}
if (1000<=j & j <=1985) {fileName <- "./probracket8.html"}

HTML <- readChar(fileName, file.info(fileName)$size)

for (i in 1:nrow(bracket)) {HTML <- gsub(paste("Game ",bracket$game[i],"",sep=""),bracket$text[i],HTML)}
for (i in c(601:632,501:516,401:408,301:304)) {HTML <- gsub(paste("Game ",i,"",sep=""),"&nbsp;<br>&nbsp;",HTML)}

HTML <- gsub("SPORTZ","<a href='./index.html'>LaCrosse</a>",HTML)
HTML <- gsub("YEAR1",paste(j," (<a href='",j,".html'>Games</a>",")",sep=""),HTML)
HTML <- gsub("YEAR0",paste("<a href='bracket",paste(j-1),".html'>",paste(j-1),"</a>",sep=""),HTML)
HTML <- gsub("YEAR2",paste("<a href='bracket",paste(j+1),".html'>",paste(j+1),"</a>",sep=""),HTML)
HTML <- gsub("<< <a href='bracket2020.html'>2020</a> ","<< <a href='bracket2019.html'>2019</a> ",HTML)
HTML <- gsub("<a href='bracket2020.html'>2020</a> >>","<a href='bracket2021.html'>2021</a> >>",HTML)
HTML <- gsub("<a href='bracket2027.html'>2027</a> >>","2027 >>",HTML)
HTML <- gsub("<< <a href='bracket1970.html'>1970</a> ","<< First ",HTML)

HTML <- gsub("YYYY",paste(j),HTML)

if (j>=1959) {
REG1 <- paste(subset(bracket, game==301)$name1); HTML <- gsub("REGIONAL 1","&nbsp;&nbsp;",HTML)
REG2 <- paste(subset(bracket, game==302)$name1); HTML <- gsub("REGIONAL 2","&nbsp;&nbsp;",HTML)
REG3 <- paste(subset(bracket, game==303)$name1); HTML <- gsub("REGIONAL 3","&nbsp;&nbsp;",HTML)
REG4 <- paste(subset(bracket, game==304)$name1); HTML <- gsub("REGIONAL 4","&nbsp;&nbsp;",HTML)
HTML <- gsub(" REGIONAL","",HTML)
}

HTML <- gsub("EVENTEVENT","NCAA Men's LaCrosse Tournament",HTML)
HTML <- gsub("NCAA Basketball Tournament","NCAA Men's LaCrosse Tournament",HTML)
HTML <- gsub("ROUND1","First Round",HTML)
HTML <- gsub("ROUND2","Quarterfinals",HTML)
HTML <- gsub("ROUND3","Semifinals",HTML)
HTML <- gsub("ROUND4","Championship",HTML)
HTML <- gsub("Final Four","Final Four",HTML)

champ <- subset(bracket, game==101)
champ$champion <- ifelse(champ$score1>champ$score2,paste(champ$team1),paste(champ$team2))
CHAMP <- paste(champ$champion); HTML <- gsub("Champion123",CHAMP,HTML)

sink(paste("../LAX/bracket",paste(j),".html",sep=""))
write.table(HTML, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}


