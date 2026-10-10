
############# import data
seasons <- read.csv("../Data/seasons.csv")
playoff <- read.csv("../Data/playoffs.csv")
asg     <- read.csv("../Data/asg.csv")
###########################################

seasons$year <- ifelse(seasons$sport=="NHL"|seasons$sport=="NBA",
			ifelse(seasons$season=="1999-00",2000,as.numeric(paste(substring(seasons$season,1,2),substring(seasons$season,6,7),sep=""))),
			as.numeric(substring(seasons$season,1,4)))
seasons$key1 <- paste(seasons$sport,seasons$year,sep="/")
seasons$key2 <- paste(seasons$sport,seasons$id  ,sep="/")
seasons$key3 <- paste(seasons$sport,seasons$year,seasons$league,seasons$div,sep="/")

seasons$seedn<- gsub("(\\D*)(\\d*)","\\2", seasons$seed)
seasons$gp   <- seasons$w+seasons$l+ifelse(is.na(seasons$t),0,seasons$t)+ifelse(is.na(seasons$otl),0,seasons$otl)
seasons$pctn <- (seasons$w+ifelse(is.na(seasons$t),0,seasons$t)/2) / seasons$gp
seasons$pctn <- ifelse(seasons$sport=="NFL" & as.numeric(substring(seasons$season,1,4))<1972,seasons$w/(seasons$w+seasons$l),seasons$pctn)
seasons$pct  <- sprintf("%.3f", round(seasons$pctn,3))
seasons$pts  <- ifelse(seasons$sport=="NHL",2*seasons$w+seasons$t+ifelse(is.na(seasons$otl),0,seasons$otl),
			ifelse(seasons$sport=="MLS",3*seasons$w+seasons$t,
			ifelse(seasons$sport=="NFL" & as.numeric(substring(seasons$season,1,4))<1972,seasons$w/(seasons$w+seasons$l),
			seasons$w-seasons$l)))

seasons$crank <- ave(-seasons$pts, seasons$key3, FUN = function(x) rank(x, ties.method = "min") )
seasons$lrank <- ave(-seasons$pts, seasons$key1, FUN = function(x) rank(x, ties.method = "min") )
seasons$maxpt <- ave( seasons$pts, seasons$key3, FUN = function(x) max(x) )

#seasons$mu <- ave( seasons$pts, seasons$key1, FUN = function(x) mean(x) )
#seasons$sd <- ave( seasons$pts, seasons$key1, FUN = function(x) sd(x) )
seasons$zz <- (seasons$pctn-0.5)*(seasons$gp)^(0.5)
seasons$zrank <- ave(-seasons$zz, seasons$sport, FUN = function(x) rank(x, ties.method = "min") )

seasons$gb <- ifelse(seasons$maxpt>seasons$pts,(seasons$maxpt-seasons$pts)/2,"--")

seasons$col1 <- seasons$w
seasons$col2 <- seasons$l
seasons$col3 <- ifelse(seasons$sport=="MLB"|seasons$sport=="NBA",seasons$pct,
			ifelse(is.na(seasons$t),"",ifelse(is.na(seasons$otl),seasons$t,paste(seasons$t,seasons$otl,sep="/"))))
seasons$col4 <- ifelse(seasons$sport=="MLB"|seasons$sport=="NBA",seasons$gb ,ifelse(seasons$sport=="NFL",seasons$pct,seasons$pts))

seasons$sow <- ifelse(seasons$sport=="MLS" & seasons$year<2000,seasons$t,0)
seasons$t   <- ifelse(seasons$sport=="MLS" & seasons$year<2000,0,seasons$t)

#############################################

ids <- unique(seasons[c("sport","year","team","id","seedn")])
ids$id1 <- ids$id; ids$id2 <- ids$id; ids$team1 <- ids$team; ids$team2 <- ids$team; ids$seed1 <- ids$seedn; ids$seed2 <- ids$seedn;
playoff <- merge(playoff,ids[c("sport","year","team1","id1","seed1")],by=c("sport","year","team1"),all.x=TRUE)
playoff <- merge(playoff,ids[c("sport","year","team2","id2","seed2")],by=c("sport","year","team2"),all.x=TRUE)
playoff$key1 <- paste(playoff$sport,playoff$year,sep="/")
playoff$group <- ifelse(playoff$round==1,99,ifelse(playoff$event=="Semis",98,ifelse(playoff$event=="Quarters",97,0)))
	playoff$score2 <- ifelse(is.na(playoff$score2)==TRUE,"",playoff$score2)

playoff$sortn <- ifelse(playoff$seed1==1|playoff$seed1==8,1,
			ifelse(playoff$seed1==4|playoff$seed1==5,2,
			ifelse(playoff$seed1==3|playoff$seed1==6,3,
			ifelse(playoff$seed1==2|playoff$seed1==7,4,99))))

#subset(playoff, is.na(id1)==TRUE); #subset(playoff, is.na(id2)==TRUE);
#############################################

asg <- merge(asg,ids[c("sport","year","team1","id1")],by=c("sport","year","team1"),all.x=TRUE)
asg <- merge(asg,ids[c("sport","year","team2","id2")],by=c("sport","year","team2"),all.x=TRUE)
#############################################

rounds <- playoff[c("sport","year","round","id1","team1","score1","id2","team2","score2","ot")]

rounds.w <- rounds
rounds.w$id <- rounds$id1
  rounds.w$pk <- ifelse(rounds$score1==rounds$score2,ifelse(grepl("(a)",rounds$ot),"a","p"),"")
rounds.w$score <- ifelse(rounds$score2!="",paste(rounds$score1,rounds.w$pk,"-",rounds$score2,sep=""),paste(rounds$score1)) 
rounds.w$result <- "W"
rounds.w <- rounds.w[c("sport","year","round","id","result","score")]

rounds.l <- rounds
rounds.l$id <- rounds$id2
  rounds.l$pk <- ifelse(rounds$score1==rounds$score2,ifelse(grepl("(a)",rounds$ot),"a","p"),"")
rounds.l$score <- ifelse(rounds$score2!="",paste(rounds$score2,"-",rounds$score1,rounds.w$pk,sep=""),"") 
rounds.l$result <- "L"
rounds.l <- rounds.l[c("sport","year","round","id","result","score")]

rounds <- rbind(rounds.w,rounds.l)
rounds$final <- ifelse(rounds$round==1 ,rounds$score,"")
rounds$finalr<- ifelse(rounds$round==1 ,rounds$result,"")
rounds$semif <- ifelse(rounds$round==4 ,rounds$score,"")
rounds$semifr<- ifelse(rounds$round==4 ,rounds$result,"")
rounds$quart <- ifelse(rounds$round==8 ,rounds$score,"")
rounds$prelm <- ifelse(rounds$round==16,rounds$score,"")
rounds$covid <- ifelse(rounds$round==32,rounds$score,"")

#####

seasons <- merge(seasons,subset(rounds,final!="")[c("sport","year","id","final","finalr")],by=c("sport","year","id"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,semif!="")[c("sport","year","id","semif","semifr")],by=c("sport","year","id"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,quart!="")[c("sport","year","id","quart")],by=c("sport","year","id"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,prelm!="")[c("sport","year","id","prelm")],by=c("sport","year","id"),all.x=TRUE)
seasons <- merge(seasons,subset(rounds,covid!="")[c("sport","year","id","covid")],by=c("sport","year","id"),all.x=TRUE)
seasons$finalr <- ifelse(is.na(seasons$finalr),"",seasons$finalr)
seasons$semifr <- ifelse(is.na(seasons$semifr),"",seasons$semifr)

seasons$class1 <- ifelse(is.na(seasons$covid)&is.na(seasons$prelm)&is.na(seasons$quart)&is.na(seasons$semif)&is.na(seasons$final),0,1)

seasons$class2 <- ifelse(seasons$finalr=="W",3,
			ifelse(seasons$finalr=="L"|seasons$semifr=="W"|(seasons$sport=="MLB"&seasons$year<1910&seasons$drank=="1st"),2,
			ifelse(is.na(seasons$prelm)&is.na(seasons$quart)&is.na(seasons$semif)&is.na(seasons$final),0,1)))


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

# create summary for given year
YR <- as.numeric(substring(u.yr[i],5,8)); LG <- substring(u.yr[i],1,3); YR0 <- YR-1; YR2 <- YR+1
if (LG=="NHL" & YR0 == 2005) {YR0 <- 2004}; if (LG=="NHL" & YR2 == 2005) {YR2 <- 2006};

SEAS <- ifelse(LG=="NBA"|LG=="NHL",paste(YR -1,sprintf("%02d", YR %%100),sep="-"),paste(YR ));
SEAS0<- ifelse(LG=="NBA"|LG=="NHL",paste(YR0-1,sprintf("%02d", YR0%%100),sep="-"),paste(YR0));
SEAS2<- ifelse(LG=="NBA"|LG=="NHL",paste(YR2-1,sprintf("%02d", YR2%%100),sep="-"),paste(YR2));

headerx <- gsub("ABC123",LG,header)
summ.yr <- subset(seasons, key1 == u.yr[i])

summ.yr <- summ.yr[order(summ.yr$league, summ.yr$div, -summ.yr$pts, summ.yr$drank),]

summ.yr$row <- paste("<tr class='d",summ.yr$class1,"'><td>",paste(
				paste("<div style='text-align:left'><a href='",summ.yr$id,".html'>",summ.yr$team,"</a>",
					ifelse(summ.yr$seedn!="",paste(" <h5>[",summ.yr$seedn,"]</h5>",sep=""),""),"</div>",sep=""),
				summ.yr$col1,
				summ.yr$col2,
				summ.yr$col3,
				summ.yr$col4,
				paste(ifelse(is.na(summ.yr$covid)==FALSE,paste("[",summ.yr$covid,"] ",sep=""),""),
					ifelse(is.na(summ.yr$prelm)==FALSE,summ.yr$prelm,""),sep=""),
				ifelse(is.na(summ.yr$quart)==FALSE,summ.yr$quart,""),
				ifelse(is.na(summ.yr$semif)==FALSE,summ.yr$semif,""),
				ifelse(is.na(summ.yr$final)==FALSE,summ.yr$final,""),
			sep="</td><td>"),"</td></tr>",sep="")

play.yr <- subset(playoff, key1 == u.yr[i])
play.yr <- play.yr[order(play.yr$group, play.yr$name1, -play.yr$round, play.yr$event, play.yr$sortn),]

     if (LG=="MLB" & YR>=2013) {thth <- "</th><th>W</th><th>L</th><th>Pct</th><th>GB</th><th>WC</th><th>LDS</th><th>LCS</th><th>WS</th>"}
else if (LG=="MLB" & (YR>=1994|YR==1981)) {thth <- "</th><th>W</th><th>L</th><th>Pct</th><th>GB</th><th></th><th>LDS</th><th>LCS</th><th>WS</th>"}
else if (LG=="MLB" & YR>=1969) {thth <- "</th><th>W</th><th>L</th><th>Pct</th><th>GB</th><th></th><th></th><th>LCS</th><th>WS</th>"}
else if (LG=="MLB" & YR>=1800) {thth <- "</th><th>W</th><th>L</th><th>Pct</th><th>GB</th><th></th><th></th><th></th><th>WS</th>"}

else if (LG=="MLS" & YR>=2011) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th>WC</th><th>CSF</th><th>CF</th><th>Cup</th>"}
else if (LG=="MLS" & (YR==2000|YR==2001)) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th></th><th>QF</th><th>SF</th><th>Cup</th>"}
else if (LG=="MLS" & YR>=2000) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th></th><th>CSF</th><th>CF</th><th>Cup</th>"}
else if (LG=="MLS") {thth <- "</th><th>W</th><th>L</th><th>SOW</th><th>Pts</th><th></th><th>CSF</th><th>CF</th><th>Cup</th>"}

else if (LG=="NBA" & YR>=1984) {thth <- "</th><th>W</th><th>L</th><th>Pct</th><th>GB</th><th>CQF</th><th>CSF</th><th>CF</th><th>NBAF</th>"}
else if (LG=="NBA" & YR>=1975) {thth <- "</th><th>W</th><th>L</th><th>Pct</th><th>GB</th><th>1R</th><th>CSF</th><th>CF</th><th>NBAF</th>"}
else if (LG=="NBA" & YR>=1971) {thth <- "</th><th>W</th><th>L</th><th>Pct</th><th>GB</th><th></th><th>CSF</th><th>CF</th><th>NBAF</th>"}
else if (LG=="NBA" & YR==1954) {thth <- "</th><th>W</th><th>L</th><th>Pct</th><th>GB</th><th></th><th>RR</th><th>DF</th><th>NBAF</th>"}
else if (LG=="NBA" & (YR>=1951|YR==1949)) {thth <- "</th><th>W</th><th>L</th><th>Pct</th><th>GB</th><th></th><th>DSF</th><th>DF</th><th>NBAF</th>"}
else if (LG=="NBA" & YR==1950) {thth <- "</th><th>W</th><th>L</th><th>Pct</th><th>GB</th><th>DSF</th><th>DF</th><th>SF</th><th>NBAF</th>"}
else if (LG=="NBA" & YR>=1900) {thth <- "</th><th>W</th><th>L</th><th>Pct</th><th>GB</th><th></th><th>1R</th><th>SF</th><th>NBAF</th>"}

else if (LG=="NHL" & YR>=2005) {thth <- "</th><th>W</th><th>L</th><th>OTL</th><th>Pts</th><th>CQF</th><th>CSF</th><th>CF</th><th>SCF</th>"}
else if (LG=="NHL" & YR>=2000) {thth <- "</th><th>W</th><th>L</th><th>T/OTL</th><th>Pts</th><th>CQF</th><th>CSF</th><th>CF</th><th>SCF</th>"}
else if (LG=="NHL" & YR>=1994) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th>CQF</th><th>CSF</th><th>CF</th><th>SCF</th>"}
else if (LG=="NHL" & YR>=1994) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th>CQF</th><th>CSF</th><th>CF</th><th>SCF</th>"}
else if (LG=="NHL" & YR>=1982) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th>DSF</th><th>DF</th><th>CF</th><th>SCF</th>"}
else if (LG=="NHL" & YR>=1975) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th>R1</th><th>QF</th><th>SF</th><th>SCF</th>"}
else if (LG=="NHL" & YR>=1971) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th></th><th>QF</th><th>SF</th><th>SCF</th>"}
else if (LG=="NHL" & YR>=1968) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th></th><th>CSF</th><th>CF</th><th>SCF</th>"}
else if (LG=="NHL" & YR>=1943) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th></th><th></th><th>SF</th><th>SCF</th>"}
else if (LG=="NHL" & YR>=1927) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th></th><th>QF</th><th>SF</th><th>SCF</th>"}
else if (LG=="NHL" & YR>=1926) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th></th><th>SF</th><th>NHL</th><th>SCF</th>"}
else if (LG=="NHL" & YR>=1900) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pts</th><th></th><th></th><th>NHL</th><th>SCF</th>"}

else if (LG=="NFL" & YR>=1978) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pct</th><th>WC</th><th>DR</th><th>CH</th><th>SB</th>"}
else if (LG=="NFL" & YR>=1967) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pct</th><th></th><th>DR</th><th>CH</th><th>SB</th>"}
else if (LG=="NFL" & YR>=1966) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pct</th><th></th><th></th><th>CH</th><th>SB</th>"}
else if (LG=="NFL" & YR>=1900) {thth <- "</th><th>W</th><th>L</th><th>T</th><th>Pct</th><th></th><th></th><th>CH</th><th></th>"}

if (nrow(play.yr)>0) {
for (p in 1:nrow(play.yr)) {
if (p==1) { play.yr$flag[p] <- 2}
if (p >1) { play.yr$flag[p] <- ifelse(play.yr$name2[p] != play.yr$name2[p-1]|play.yr$name1[p] != play.yr$name1[p-1]
							,ifelse(play.yr$name1[p] != play.yr$name1[p-1],2,1),0)}}

play.yr$row <- paste(ifelse(play.yr$flag==1,"<tr><th colspan='5'></th></tr>",""),"<tr><td>",paste(
				ifelse(play.yr$flag>=1,paste("<b>",play.yr$name2,"</b>",sep=""),""),
				ifelse(is.na(play.yr$id1)==FALSE,paste("<a href='",play.yr$id1,".html'>",play.yr$team1,"</a>",sep=""),paste(play.yr$team1)),
				ifelse(play.yr$score2!="",paste(play.yr$score1,play.yr$score2,sep=" - "),paste(play.yr$score1)),
				ifelse(is.na(play.yr$id2)==FALSE,paste("<a href='",play.yr$id2,".html'>",play.yr$team2,"</a>",sep=""),paste(play.yr$team2)),
				play.yr$ot,
			sep="</td><td>"),"</td></tr>") }

sink(paste("../",u.yr[i],".html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",SEAS,"</center></h2>"))
cat(paste("<center><a href='",YR0,".html'> << ",SEAS0,"</a> | ",
	            "<a href='",YR2,".html'>",SEAS2," >> </a></center><br>",sep=""))

cat("<br><table width='80%' align='center'>")
cat("<col width='36%'><col width='7%'><col width='7%'><col width='7%'><col width='7%'><col width='9%'><col width='9%'><col width='9%'><col width='9%'>")

u.lg  <- unique(summ.yr$league2)
for (j in 1:length(u.lg)) {
cat(paste("<tr><th colspan='9' class='hl'>",u.lg[j],"</th></tr>"))
u.div <- subset(summ.yr,u.lg[j]==summ.yr$league2); u.div <- unique(u.div$div);

for (k in 1:length(u.div)) {
cat(paste("<tr>","<th>",u.div[k],thth,"</tr>"))
write.table(subset(summ.yr,u.lg[j]==summ.yr$league2 & u.div[k]==summ.yr$div)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}}

cat("</table><br>")

if (nrow(play.yr)>0) {
cat("<!--Insert Bracket--!>")
cat("<center><h3>Playoffs</h3></center>")
cat(paste("<br><center><a href='bracket",paste(YR),".html'>Playoff Bracket</a></center>",sep=""))
cat("<br><table width='80%' align='center'>")

u.rd  <- unique(play.yr$name1)

for (m in 1:length(u.rd)) {
cat(paste("<tr>","<th colspan='5' class='hl'>",u.rd[m],"</th>","</tr>"))
write.table(subset(play.yr,u.rd[m]==play.yr$name1)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
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

# create summary for given year
TM <- substring(u.tm[i],5,7); LG <- substring(u.tm[i],1,3); 
headerx <- gsub("ABC123",LG,header)
summ.tm <- subset(seasons, key2 == u.tm[i])
summ.tm <- summ.tm[order(-summ.tm$year),]

if (LG=="MLS") {
 summ.tm$col1 <- ifelse(1996<=summ.tm$year & summ.tm$year<=1999,paste(summ.tm$col1,summ.tm$col3,sep="/"),summ.tm$col1)
 summ.tm$col3 <- ifelse(1996<=summ.tm$year & summ.tm$year<=1999,"*",summ.tm$col3)
}

summ.tm$row <- paste("<tr class='d",summ.tm$class2,"'><td>",paste(
				paste("<a href='",summ.tm$year,".html'>",summ.tm$season,"</a>",sep=""),
				paste("<div style='text-align:left'>",summ.tm$team,"</div>",sep=""),
				paste(summ.tm$league,summ.tm$div,sep=" "),
				summ.tm$drank,
				summ.tm$col1,
				summ.tm$col2,
				summ.tm$col3,
				summ.tm$col4,
				paste(ifelse(is.na(summ.tm$covid)==FALSE,paste("[",summ.tm$covid,"] ",sep=""),""),
					ifelse(is.na(summ.tm$prelm)==FALSE,summ.tm$prelm,""),sep=""),
				ifelse(is.na(summ.tm$quart)==FALSE,summ.tm$quart,""),
				ifelse(is.na(summ.tm$semif)==FALSE,summ.tm$semif,""),
				ifelse(is.na(summ.tm$final)==FALSE,summ.tm$final,""),
			sep="</td><td>"),"</td></tr>",sep="")

     if (LG=="MLB") {thth <- "<th>W</th><th>L</th><th>Pct</th><th>GB</th><th>WC</th><th>LDS</th><th>LCS</th><th>WS</th>"}
else if (LG=="NBA") {thth <- "<th>W</th><th>L</th><th>Pct</th><th>GB</th><th>CQF</th><th>CSF</th><th>CF</th><th>NBAF</th>"}
else if (LG=="NFL") {thth <- "<th>W</th><th>L</th><th>T</th><th>Pct</th><th>WC</th><th>DR</th><th>CH</th><th>SB</th>"}
else if (LG=="NHL") {thth <- "<th>W</th><th>L</th><th>T</th><th>Pts</th><th>CQF</th><th>CSF</th><th>CF</th><th>SCF</th>"}
else if (LG=="MLS") {thth <- "<th>W</th><th>L</th><th>T</th><th>Pts</th><th>WC</th><th>CSF</th><th>CF</th><th>Cup</th>"}

play.tm <- subset(playoff, sport == LG & (id1==TM | id2==TM) )
play.tm <- play.tm[order(-play.tm$year, -play.tm$round),]

if (nrow(play.tm)>0) {
for (n in 1:nrow(play.tm)) {
if (n==1) { play.tm$flag[n] <- 2}
if (n >1) { play.tm$flag[n] <- ifelse(play.tm$year[n] != play.tm$year[n-1],1,0)}}

play.tm$row <- paste(ifelse(play.tm$flag==1,"<tr><th colspan='6'></th></tr>",""),"<tr><td>",paste(
				ifelse(play.tm$flag>=1,paste("<a href='",play.tm$year,".html'><b>",play.tm$year,"</b></a>",sep=""),""),
				ifelse(play.tm$sport=="MLB",ifelse(play.tm$name2!="",paste(play.tm$name2),paste(play.tm$name1)),
					paste(play.tm$name1,play.tm$name2)),
				ifelse(is.na(play.tm$id1)==FALSE,paste("<a href='",play.tm$id1,".html'>",play.tm$team1,"</a></div>",sep=""),paste(play.tm$team1)),
				ifelse(play.tm$score2!="",paste(play.tm$score1,play.tm$score2,sep=" - "),paste(play.tm$score1)),
				ifelse(is.na(play.tm$id2)==FALSE,paste("<a href='",play.tm$id2,".html'>",play.tm$team2,"</a></div>",sep=""),paste(play.tm$team2)),
				play.tm$ot,
			sep="</td><td>"),"</td></tr>",sep="") }

sink(paste("../",u.tm[i],".html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",summ.tm$team[1],"</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat("<col width='7%'><col width='22%'><col width='13%'><col width='5%'><col width='5%'><col width='5%'><col width='5%'><col width='5%'><col width='7%'><col width='7%'><col width='7%'><col width='7%'>")
cat(paste("<tr>","<th>Season</th><th>Team</th><th>Division</th><th>Place</th>",thth,"</tr>",sep=""))
write.table(summ.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

if (nrow(play.tm)>0) {
cat("<center><h3>Playoffs</h3></center>")
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
names(byyear)[1] <- "key1"
byyear$year  <- as.numeric(substring(byyear$key1,5,8))
byyear$sport <- substring(byyear$key1,1,3);

final <- subset(playoff, round==1)
byyear <- merge(byyear,final,by=c("key1","sport","year"),all.x=TRUE)

teams.yr <- as.data.frame.matrix(table(seasons$key1,rep("no.teams",length(seasons$key1)) ))
teams.yr$key1 <- rownames(teams.yr)
byyear <- merge(byyear,teams.yr,by="key1",all.x=TRUE)

semis <- subset(playoff, round==4)[c("key1","id1","team1","event")]
semis$id3 <- semis$id1; semis$team3 <- semis$team1; 
semis$id4 <- semis$id1; semis$team4 <- semis$team1;
semis.1 <- subset(semis, event=="NLCS"|event=="EF"|event=="NFLCH"|event=="NFCCH"|event=="WalesF")
semis.1 <- semis.1[c("key1","id3","team3")]
semis.2 <- subset(semis, event=="ALCS"|event=="WF"|event=="AFLCH"|event=="AFCCH"|event=="CampbellF")
semis.2 <- semis.2[c("key1","id4","team4")]

pennants <- subset(seasons, sport=="MLB" & (drank=="1st"|drank=="*1st") & year<1969)[c("key1","key2","id","team","league")]
pennants$id3 <- pennants$id; pennants$team3 <- pennants$team; 
pennants$id4 <- pennants$id; pennants$team4 <- pennants$team;
pennants.1 <- subset(pennants, league=="NL")
pennants.1 <- pennants.1[c("key1","id3","team3")]
pennants.2 <- subset(pennants, league=="AL")
pennants.2 <- pennants.2[c("key1","id4","team4")]

semis.1 <- rbind(semis.1,pennants.1); semis.2 <- rbind(semis.2,pennants.2); 

byyear <- merge(byyear,semis.1,by="key1",all.x=TRUE)
byyear <- merge(byyear,semis.2,by="key1",all.x=TRUE)
byyear$score <- ifelse(byyear$score1!="" & byyear$score2!="",paste(byyear$score1,byyear$score2,sep=" - "),"")
byyear$class <- (1+(1:nrow(byyear))) %% 2

byyear$row <- paste("<tr class='d",byyear$class,"'><td>",paste(
	paste("<a href='",byyear$year,".html'>",byyear$year ,"</a>",sep=""),
	ifelse(is.na(byyear$no.teams),"",byyear$no.teams),
	ifelse(is.na(byyear$id1),ifelse(is.na(byyear$team1),"",paste(byyear$team1)),paste("<a href='",byyear$id1,".html'>", byyear$team1,"</a>",sep="")),
	ifelse(is.na(byyear$score),"",byyear$score),
	ifelse(is.na(byyear$id2),ifelse(is.na(byyear$team2),"",paste(byyear$team2)),paste("<a href='",byyear$id2,".html'>", byyear$team2,"</a>",sep="")),
	ifelse(is.na(byyear$team3),"",paste("<a href='",byyear$id3,".html'>", byyear$team3,"</a>",sep="")),
	ifelse(is.na(byyear$team4),"",paste("<a href='",byyear$id4,".html'>", byyear$team4,"</a>",sep="")),
			sep="</td><td>"),"</td></tr>",sep="")

byyear <- byyear[order(-byyear$year),]

u.sport <- unique(byyear$sport)

for (i in 1:length(u.sport))
{

     if (u.sport[i]=="MLB") {thth <- "<th>NL Champion</th><th>AL Champion</th>"}
else if (u.sport[i]=="NBA") {thth <- "<th>East Champion</th><th>West Champion</th>"}
else if (u.sport[i]=="NFL") {thth <- "<th>NFL/NFC Champion</th><th>AFL/AFC Champion</th>"}
else if (u.sport[i]=="NHL") {thth <- "<th>East/Wales Champion</th><th>West/Campbell Champion</th>"}
else if (u.sport[i]=="MLS") {thth <- "<th>East Champion</th><th>West Champion</th>"}

sink(paste("../",u.sport[i],"/byyear.html",sep=""))

headerx <- gsub("ABC123",u.sport[i],header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",u.sport[i],"</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Season</th><th>Teams</th><th>Champion</th><th></th><th>Runner-Up</th>",thth,"</tr>",sep=""))
write.table(subset(byyear,sport==u.sport[i])["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# by team page ###############
##########################################

seasons$count <- 1
byteam <- as.data.frame(u.tm)
names(byteam)[1] <- "key2"

seas <- aggregate(seasons$count, list(seasons$key2), sum); names(seas)[names(seas)=="x"] <- "Years"
wins <- aggregate(seasons$w    , list(seasons$key2), sum); names(wins)[names(wins)=="x"] <- "W";   summ <- merge(seas,wins,by="Group.1");
loss <- aggregate(seasons$l    , list(seasons$key2), sum); names(loss)[names(loss)=="x"] <- "L";   summ <- merge(summ,loss,by="Group.1");
ties <- aggregate(seasons$t    , list(seasons$key2), sum, na.rm = T); names(ties)[names(ties)=="x"] <- "T";   summ <- merge(summ,ties,by="Group.1");
otls <- aggregate(seasons$otl  , list(seasons$key2), sum, na.rm = T); names(otls)[names(otls)=="x"] <- "OTL"; summ <- merge(summ,otls,by="Group.1");
sows <- aggregate(seasons$sow  , list(seasons$key2), sum); names(sows)[names(sows)=="x"] <- "SOW";   summ <- merge(summ,sows,by="Group.1");
fyr  <- aggregate(seasons$year , list(seasons$key2), min); names(fyr )[names(fyr )=="x"] <- "FYr"; summ <- merge(summ,fyr ,by="Group.1");
lyr  <- aggregate(seasons$year , list(seasons$key2), max); names(lyr )[names(lyr )=="x"] <- "LYr"; summ <- merge(summ,lyr ,by="Group.1");

byteam <- merge(byteam,summ,by.x="key2",by.y="Group.1")

rounds$key2 <- paste(rounds$sport,rounds$id,sep="/")
rounds$resultf <- paste(rounds$result,rounds$round,sep="")
potab <- as.data.frame.matrix(table(rounds$key2,rounds$resultf))
potab$key2 <- rownames(potab)

tmpens <- as.data.frame.matrix(table(pennants$key2,pennants$league))
tmpens$key2 <- rownames(tmpens)

byteam <- merge(byteam,potab,by="key2",all.x=TRUE)
byteam <- merge(byteam,tmpens[c("key2","AL","NL")],by="key2",all.x=TRUE)

last.plo <- aggregate(rounds$year , list(rounds$key2), max); names(last.plo)[names(last.plo)=="x"] <- "last.plo";
last.win <- aggregate(subset(rounds,result=="W")$year , list(subset(rounds,result=="W")$key2), max); names(last.win)[names(last.win)=="x"] <- "last.win";
last.fin <- aggregate(subset(rounds,resultf=="W4"|resultf=="L1")$year , list(subset(rounds,resultf=="W4"|resultf=="L1")$key2), max); names(last.fin)[names(last.fin)=="x"] <- "last.fin";
last.chp <- aggregate(subset(rounds,resultf=="W1")$year , list(subset(rounds,resultf=="W1")$key2), max); names(last.chp)[names(last.chp)=="x"] <- "last.chp";

last <- merge(last.plo,last.win,by="Group.1",all.x=TRUE)
last <- merge(last    ,last.fin,by="Group.1",all.x=TRUE)
last <- merge(last    ,last.chp,by="Group.1",all.x=TRUE)
byteam <- merge(byteam,last,by.x="key2",by.y="Group.1",all.x=TRUE)

names <- unique(subset(seasons,(sport!="NHL" & year>=2025)|(sport=="NHL" & year>=2026))[,c("key2","sport","id","team")])
byteam <- merge(byteam,names,by="key2")
byteam$cyr <- ifelse(byteam$sport=="NFL",2025,2025)

byteam$powins <- byteam$W1 + byteam$W4 + byteam$W8 + byteam$W16 + byteam$W32
byteam$poloss <- byteam$L1 + byteam$L4 + byteam$L8 + byteam$L16 + byteam$L32
byteam$W      <- byteam$W + byteam$SOW
byteam$T      <- byteam$T + byteam$OTL
byteam$fins   <- byteam$W4 + ifelse(byteam$sport=="MLB" & is.na(byteam$AL)==FALSE,byteam$AL+byteam$NL,0)


byteam$class <- (1+(1:nrow(byteam))) %% 2
byteam$row <- paste("<tr class='d",byteam$class,"'><td>",paste(
				paste("<a href='",byteam$id,".html'>",byteam$team,"</a>",sep=""),
				byteam$Years,
				ifelse(is.na(byteam$W1),0,byteam$W1),
				byteam$W,
				byteam$L,
				ifelse(byteam$T>0,byteam$T,""),
				paste(ifelse(is.na(byteam$powins)==FALSE,byteam$powins,0),ifelse(is.na(byteam$poloss)==FALSE,byteam$poloss,0),sep="-"),
				ifelse(byteam$W1 +byteam$L1 >0&is.na(byteam$W1 )==F,paste(byteam$W1 ,byteam$L1 ,sep="-"),""),
				ifelse(is.na(byteam$last.chp),paste("<h5>[",byteam$cyr-byteam$FYr+1,"]</h5>",sep=""),byteam$cyr-byteam$last.chp),
			sep="</td><td>"),"</td></tr>",sep="")

u.sport <- unique(byteam$sport)

for (i in 1:length(u.sport))
{

     if (u.sport[i]=="MLB") {thth <- "<tr><th colspan='3'></th><th colspan='3' class='hl2'>All-Time Record</th><th colspan='2' class='hl2'>Playoff Record</th><th rowspan='2'>Years Since Last Championship</th></tr><tr><th></th><th>Years</th><th>World Series</th><th>W</th><th>L</th><th></th><th>All Rounds</th><th>World Series</th></tr>"}
else if (u.sport[i]=="NFL") {thth <- "<tr><th colspan='3'></th><th colspan='3' class='hl2'>All-Time Record</th><th colspan='2' class='hl2'>Playoff Record</th><th rowspan='2'>Years Since Last Championship</th></tr><tr><th></th><th>Years</th><th>Super Bowls</th><th>W</th><th>L</th><th>T</th><th>All Rounds</th><th>Super Bowl</th></tr>"}
else if (u.sport[i]=="NBA") {thth <- "<tr><th colspan='3'></th><th colspan='3' class='hl2'>All-Time Record</th><th colspan='2' class='hl2'>Playoff Record</th><th rowspan='2'>Years Since Last Championship</th></tr><tr><th></th><th>Years</th><th>Championships</th><th>W</th><th>L</th><th></th><th>All Rounds</th><th>NBA Finals</th></tr>"}
else if (u.sport[i]=="NHL") {thth <- "<tr><th colspan='3'></th><th colspan='3' class='hl2'>All-Time Record</th><th colspan='2' class='hl2'>Playoff Record</th><th rowspan='2'>Years Since Last Championship</th></tr><tr><th></th><th>Years</th><th>Stanley Cups</th><th>W</th><th>L</th><th>T</th><th>All Rounds</th><th>Cup Finals</th></tr>"}
else if (u.sport[i]=="MLS") {thth <- "<tr><th colspan='3'></th><th colspan='3' class='hl2'>All-Time Record</th><th colspan='2' class='hl2'>Playoff Record</th><th rowspan='2'>Years Since Last Championship</th></tr><tr><th></th><th>Years</th><th>MLS Cups</th><th>W</th><th>L</th><th>T</th><th>All Rounds</th><th>MLS Cup</th></tr>"}

sink(paste("../",u.sport[i],"/byteam.html",sep=""))

headerx <- gsub("ABC123",u.sport[i],header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",u.sport[i],"</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat("<col width='30%'><col width='10%'><col width='10%'><col width='8%'><col width='8%'><col width='8%'><col width='8%'><col width='8%'><col width='10%'>")
cat(paste(thth))
write.table(subset(byteam,sport==u.sport[i])["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# index page #################
##########################################

for (i in 1:length(u.sport))
{
byyear2 <- subset(byyear,sport==u.sport[i]) 
byyear2 <- byyear2[order(-byyear2$year),]
byyear2 <- byyear2[1:10,]

byteam2 <- subset(byteam,sport==u.sport[i])
byteam2 <- byteam2[order(-byteam2$W1, -byteam2$fins, -byteam2$powins, -byteam2$poloss),]
byteam2 <- byteam2[1:10,]

byyear2$row <- paste("<tr><td>",paste(
	paste("<a href='",byyear2$year,".html'>",byyear2$year ,"</a>",sep=""),
	paste("<a href='",byyear2$id1,".html'>", byyear2$team1,"</a>",sep=""),
	byyear2$score,
	paste("<a href='",byyear2$id2,".html'>", byyear2$team2,"</a>",sep=""),
			sep="</td><td>"),"</td></tr>",sep="")

byteam2$row <- paste("<tr><td>",paste(
				paste("<a href='",byteam2$id,".html'>",byteam2$team,"</a>",sep=""),
				byteam2$W1,
				byteam2$fins,
				paste(ifelse(is.na(byteam2$powins)==FALSE,byteam2$powins,0),ifelse(is.na(byteam2$poloss)==FALSE,byteam2$poloss,0),sep="-"),
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
cat(paste("<tr>","<th>Team</th><th>Championships</th><th>Finals</th><th>Playoff W-L</th>","</tr>"))
write.table(byteam2["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr>","<td colspan='4'><a href='byteam.html'>All Teams</a></td>","</tr>"))
cat("</table><br>")

cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th class='hl'><div style='font-size:1.5em;'>Event Results</div></th>","</tr>"))

if (u.sport[i]=="MLB") {
	cat(paste("<tr>","<td><a href='worldseries.html'>World Series</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='alcs.html'>American League Championship Series</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='nlcs.html'>National League Championship Series</a></td>","</tr>")) }
if (u.sport[i]=="NFL") {
	cat(paste("<tr>","<td><a href='superbowls.html'>Super Bowl</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='afcchamps.html'>AFL/AFC Championship Game</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='nfcchamps.html'>NFL/NFC Championship Game</a></td>","</tr>")) 
	cat(paste("<tr>","<td><a href='allstar.html'>Pro Bowl</a></td>","</tr>")) }
if (u.sport[i]=="NBA") {
	cat(paste("<tr>","<td><a href='nbafinals.html'>NBA Finals</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='eastfinal.html'>Eastern Conference Finals</a></td>","</tr>"))
	cat(paste("<tr>","<td><a href='westfinal.html'>Western Conference Finals</a></td>","</tr>")) }
if (u.sport[i]=="NHL") {
	cat(paste("<tr>","<td><a href='stanleycup.html'>Stanley Cup Finals</a></td>","</tr>")) }
if (u.sport[i]=="MLS") {
	cat(paste("<tr>","<td><a href='mlscup.html'>MLS Cup</a></td>","</tr>")) 
	cat(paste("<tr>","<td><a href='usocup.html'>U.S Open Cup</a></td>","</tr>")) }
if (u.sport[i]!="NFL") {
	cat(paste("<tr>","<td><a href='allstar.html'>All-Star Game</a></td>","</tr>")) }

cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# event pages ################
##########################################

worldseries	<- subset(playoff, sport=="MLB" & event=="WS")
alcs		<- subset(playoff, sport=="MLB" & event=="ALCS")
nlcs		<- subset(playoff, sport=="MLB" & event=="NLCS")

superbowls 	<- subset(playoff, sport=="NFL" & event=="SB")
afc.champs 	<- subset(playoff, sport=="NFL" & (event=="AFLCH"|event=="AFCCH"))
nfc.champs 	<- subset(playoff, sport=="NFL" & (event=="NFLCH"|event=="NFCCH"))

nba.finals 	<- subset(playoff, sport=="NBA" & event=="Finals")
east.final 	<- subset(playoff, sport=="NBA" & event=="EF" & year>=1951)
west.final 	<- subset(playoff, sport=="NBA" & event=="WF" & year>=1951)

stanleycup	<- subset(playoff, sport=="NHL" & event=="Finals")

mls.cup	<- subset(playoff, sport=="MLS" & event=="MLSCup")

asg.mlb <- subset(asg, sport=="MLB")
asg.nfl <- subset(asg, sport=="NFL")
asg.nba <- subset(asg, sport=="NBA")
asg.nhl <- subset(asg, sport=="NHL")
asg.mls <- subset(asg, sport=="MLS" & event=="MLSASG")
uso.cup <- subset(asg, sport=="MLS" & event=="USOCUP")


##################################################################################################
singlepage <- function(gamesx,sportx,htmlx,titlex,thx) {

gamesx <- gamesx[order(-gamesx$year),]
gamesx$class <- (1+(1:nrow(gamesx))) %% 2

gamesx$row <- paste("<tr class='d",gamesx$class,"'><td>",paste(
			paste("<a href='",gamesx$year,".html'>",gamesx$year ,"</a>",sep=""),
			ifelse(is.na(gamesx$id1),paste(gamesx$team1),paste("<a href='",gamesx$id1,".html'>", gamesx$team1,"</a>",sep="")),
			paste(gamesx$score1,gamesx$score2,sep=" - "),
			ifelse(is.na(gamesx$id2),paste(gamesx$team2),paste("<a href='",gamesx$id2,".html'>", gamesx$team2,"</a>",sep="")),
			paste(gamesx$ot),
			paste(gamesx$location),
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

singlepage(worldseries,"MLB","worldseries.html","World Series","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")
singlepage(alcs,"MLB","alcs.html","American League Championship Series","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")
singlepage(nlcs,"MLB","nlcs.html","National League Championship Series","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")

singlepage(superbowls,"NFL","superbowls.html","Super Bowls","<tr><th>Season</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th>Location</th></tr>")
singlepage(afc.champs,"NFL","afcchamps.html" ,"AFL/AFC Championship Games","<tr><th>Season</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")
singlepage(nfc.champs,"NFL","nfcchamps.html" ,"NFL/NFC Championship Games","<tr><th>Season</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")

singlepage(nba.finals,"NBA","nbafinals.html","NBA Finals","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")
singlepage(east.final,"NBA","eastfinal.html","Eastern Conference Finals","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")
singlepage(west.final,"NBA","westfinal.html","Western Conference Finals","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")

singlepage(stanleycup,"NHL","stanleycup.html","Stanley Cup Finals","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")

singlepage(mls.cup,"MLS","mlscup.html","MLS Cup","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")
singlepage(uso.cup,"MLS","usocup.html","U.S. Open Cup","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th></th></tr>")

singlepage(asg.mlb,"MLB","allstar.html","MLB All-Star Game","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th>Location</th></tr>")
singlepage(asg.nfl,"NFL","allstar.html","NFL Pro Bowl","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th>Location</th></tr>")
singlepage(asg.nba,"NBA","allstar.html","NBA All-Star Game","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th>Location</th></tr>")
singlepage(asg.nhl,"NHL","allstar.html","NHL All-Star Game","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th>Location</th></tr>")
singlepage(asg.mls,"MLS","allstar.html","MLS All-Star Game","<tr><th>Year</th><th>Winner</th><th>Score</th><th>Loser</th><th></th><th>Location</th></tr>")

##################################################################################################

top100 <- subset(seasons, zrank<=100)[c("sport","team","season","w","l","t","otl","pctn","pts","zz","zrank")]
top100 <- top100[order(top100$sport,top100$zrank),]

