
############# import data
seasons <- read.csv("../Data/cbseasons.csv")
###########################################

teamlink <- function(x){teamx = tolower(gsub("&","_",gsub(".","",gsub("'","",gsub(" ", "",x, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE))}

seasons$key3 <- paste(seasons$Year,seasons$Conference,seasons$Division,sep="/")

seasons$gp   <- seasons$W+seasons$L+seasons$T
seasons$pctn <- (seasons$W+seasons$T/2) / seasons$gp
seasons$pct  <- sprintf("%.3f", round(seasons$pctn,3))

seasons$cpctn <- ifelse(seasons$PC>0,(seasons$WC+seasons$TC/2) / seasons$PC,0)
seasons$cpct  <- sprintf("%.3f", round(seasons$cpctn,3))
seasons$pts  <- seasons$WC-seasons$LC

seasons$crank1 <- ave(-round(seasons$cpctn,3), seasons$key3, FUN = function(x) rank(x, ties.method = "min") )
seasons$crank2 <- ave(-round(seasons$cpctn,3), seasons$key3, FUN = function(x) rank(x, ties.method = "max") )
seasons$crank <- ifelse(seasons$crank1!=seasons$crank2,paste(seasons$crank1,":",seasons$crank2,sep=""),seasons$crank1)
seasons$lrank <- ave(-round(seasons$pctn ,3) , seasons$Year, FUN = function(x) rank(x, ties.method = "random") )
seasons$maxpt <- ave( seasons$pts, seasons$key3, FUN = function(x) max(x) )

seasons$gb <- ifelse(seasons$maxpt>seasons$pts,sprintf("%.1f",(seasons$maxpt-seasons$pts)/2),"--")

seasons$col1 <- paste(seasons$W,"-",seasons$L,ifelse(seasons$T>0,paste("-",seasons$T,sep=""),""),sep="")
seasons$col2 <- ifelse(seasons$Conference!="Indep"&seasons$Conference!="Indepyyy", 
			paste(seasons$WC,"-",seasons$LC,ifelse(seasons$TC>0,paste("-",seasons$TC,sep=""),""),sep=""),"")
seasons$col3 <- ifelse(seasons$WT+seasons$LT>0,paste(seasons$WT,"-",seasons$LT,sep=""),"")
seasons$col4 <- ifelse(seasons$WN+seasons$LN>0,paste(seasons$WN,"-",seasons$LN,sep=""),"")

##########################################
############# generate header / footer ###
##########################################

header <- vector()
header[1] <- '<html><head><title>College Baseball</title>'
header[2] <- '<link rel="stylesheet" type="text/css" href="styles.css"></head>' 
header[3] <- '<body><div id="red"><h1>College Baseball</h1></div>'
header[4] <- '<div id="head"><table class="head"><tr class="head">
<td class="head" style="text-align:left; width:75%;"><!--Insert Link--> &nbsp;</td>
<td class="head" style="text-align:right; width:25%;">
<a href="byyear.html">Years</a> - <a href="byteam.html">Teams</a> - <a href="index.html">Home</a>
</td></tr></table></div>'

footer <- vector()
footer[1] <- '<div id="foot"> <a class="foot" href="../index.html">AndyHulme.Net</a></div></body></html>'

##########################################
############# yearly pages ###############
##########################################

u.yr <- unique(seasons$Year)

for (i in 1:length(u.yr))
{ 
print(paste(i,u.yr[i]))

# create summary for given year
YR <- u.yr[i]; YR0 <- YR-1; YR2 <- YR+1

SEAS <- paste(YR );
SEAS0<- paste(YR0);
SEAS2<- paste(YR2);

headerx <- gsub("ABC123","BSB",header)
summ.yr <- subset(seasons, Year == u.yr[i])

summ.yr <- summ.yr[order(summ.yr$Conference, summ.yr$Division, -summ.yr$cpctn, -summ.yr$pts, summ.yr$School),]

summ.yr$class1 <- 0

summ.yr$row <- paste("<tr class='d",summ.yr$class1,"'><td>",paste(
				paste("<div style='text-align:left'><a href='",teamlink(summ.yr$School),".html'>",summ.yr$School,"</a>",
					"</div>",sep=""),
				summ.yr$col2,
				ifelse(summ.yr$Conference!="Indep" & summ.yr$Conference!="Indepyyy",summ.yr$gb,""),
				summ.yr$col1,
				summ.yr$pct,
				summ.yr$col3,
				summ.yr$col4,
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../BSB/",u.yr[i],".html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",SEAS,"</center></h2>"))
cat(paste("<center><a href='",YR0,".html'> << ",SEAS0,"</a> | ",
	            "<a href='",YR2,".html'>",SEAS2," >> </a></center><br>",sep=""))

cat("<br><table width='80%' align='center'>")
cat("<col width='32%'><col width='12%'><col width='12%'><col width='12%'><col width='12%'><col width='10%'><col width='10%'>")

u.lg  <- unique(summ.yr$Conference)
for (j in 1:length(u.lg)) {
cat(paste("<tr><th colspan='7' class='hl'>",u.lg[j],"<a name='",teamlink(u.lg[j]),"'></a>","</th></tr>",sep=""))
u.div <- subset(summ.yr,u.lg[j]==summ.yr$Conference); u.div <- unique(u.div$Division);

for (k in 1:length(u.div)) {
cat(paste("<tr>","<th>",u.div[k],"<th>Conf</th><th>GB</th><th>Overall</th><th>Pct</th><th>ConfT</th><th>NCAAT</th>","</tr>"))
write.table(subset(summ.yr,u.lg[j]==summ.yr$Conference & u.div[k]==summ.yr$Division)["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
cat(paste("<tr><td colspan='7'>&nbsp;</td></tr>"))
}

cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# team pages #################
##########################################

u.tm <- unique(seasons$School)

for (i in 1:length(u.tm))
{
print(paste(i,u.tm[i]))

# create summary for given team
headerx <- gsub("ABC123","BSB",header)
summ.tm <- subset(seasons, School == u.tm[i])
summ.tm <- summ.tm[order(-summ.tm$Year),]

summ.tm$class2 <- (0+(1:nrow(summ.tm))) %% 2

summ.tm$row <- paste("<tr class='d",summ.tm$class2,"'><td>",paste(
				paste("<a href='",summ.tm$Year,".html'>",summ.tm$Year,"</a>",sep=""),
				summ.tm$Conference,
				summ.tm$Division,
				summ.tm$col1,
				summ.tm$pct,
				summ.tm$col2,
				ifelse(summ.tm$Conference!="Indep"&summ.tm$Conference!="Indepyyy"&summ.tm$Year!=2020,
		paste("<a href='",summ.tm$Year,".html#",teamlink(summ.tm$Conference),"'>",summ.tm$crank,"</a>",sep=""),""),
				ifelse(summ.tm$Conference!="Indep"&summ.tm$Conference!="Indepyyy"&summ.tm$Year!=2020,summ.tm$gb,""),
				summ.tm$col3,
				summ.tm$col4,
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../BSB/",teamlink(u.tm[i]),".html",sep=""))

write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>",summ.tm$School[1],"</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat("<col width='8%'><col width='12%'><col width='10%'><col width='10%'><col width='10%'><col width='10%'><col width='10%'><col width='10%'><col width='10%'><col width='10%'>")
cat(paste("<tr>","<th>Year</th><th>Conference</th><th>Division</th><th>Overall</th><th>Pct</th><th>Conf</th><th>Place</th><th>GB</th><th>ConfT</th><th>NCAAT</th></tr>",sep=""))
write.table(summ.tm["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()
}

##########################################
############# by year page ###############
##########################################

byyear <- as.data.frame(u.yr)
names(byyear)[1] <- "Year"

final <- subset(seasons, lrank==1)
byyear <- merge(byyear,final[c("Year","School","col1","pct","col4")],by=c("Year"),all.x=TRUE)

teams.yr <- as.data.frame.matrix(table(seasons$Year,rep("no.teams",length(seasons$Year)) ))
teams.yr$Year <- rownames(teams.yr)
byyear <- merge(byyear,teams.yr,by="Year",all.x=TRUE)

byyear$class <- (1+(1:nrow(byyear))) %% 2

byyear$row <- paste("<tr class='d",byyear$class,"'><td>",paste(
	paste("<a href='",byyear$Year,".html'>",byyear$Year ,"</a>",sep=""),
	ifelse(is.na(byyear$no.teams),"",byyear$no.teams),
	paste("<div style='text-align:left'><a href='",teamlink(byyear$School),".html'>",byyear$School,"</a>",
					"</div>",sep=""),
	byyear$col1,
	byyear$pct,
	byyear$col4,
			sep="</td><td>"),"</td></tr>",sep="")

byyear <- byyear[order(-byyear$Year),]

sink(paste("../BSB/byyear.html",sep=""))

headerx <- gsub("ABC123","BSB",header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>College Baseball</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th>Season</th><th>Teams</th><th>Best Record</th><th>W-L</th><th>Pct</th><th>NCAAT</th></tr>",sep=""))
write.table(byyear["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

##########################################
############# by team page ###############
##########################################

seasons$count <- 1
byteam <- as.data.frame(u.tm)
names(byteam)[1] <- "School"

seas <- aggregate(seasons$count, list(seasons$School), sum); names(seas)[names(seas)=="x"] <- "Years"
wins <- aggregate(seasons$W    , list(seasons$School), sum); names(wins)[names(wins)=="x"] <- "W";   summ <- merge(seas,wins,by="Group.1");
loss <- aggregate(seasons$L    , list(seasons$School), sum); names(loss)[names(loss)=="x"] <- "L";   summ <- merge(summ,loss,by="Group.1");
ties <- aggregate(seasons$T    , list(seasons$School), sum, na.rm = T); names(ties)[names(ties)=="x"] <- "T";   summ <- merge(summ,ties,by="Group.1");
fyr  <- aggregate(seasons$Year , list(seasons$School), min); names(fyr )[names(fyr )=="x"] <- "FYr"; summ <- merge(summ,fyr ,by="Group.1");
lyr  <- aggregate(seasons$Year , list(seasons$School), max); names(lyr )[names(lyr )=="x"] <- "LYr"; summ <- merge(summ,lyr ,by="Group.1");

byteam <- merge(byteam,summ,by.x="School",by.y="Group.1")

byteam$class <- (0+(1:nrow(byteam))) %% 2
byteam$row <- paste("<tr class='d",byteam$class,"'><td>",paste(
				paste("<div style='text-align:left'><a href='",teamlink(byteam$School),".html'>",byteam$School,"</a>",
					"</div>",sep=""),				
				byteam$Years,
				byteam$W,
				byteam$L,
				ifelse(byteam$T>0,byteam$T,""),
				byteam$FYr,
				byteam$LYr,
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../BSB/byteam.html",sep=""))

headerx <- gsub("ABC123","BSB",header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<br><h2><center>College Baseball Teams</center></h2>"))

cat("<br><table width='80%' align='center'>")
cat("<col width='30%'><col width='12%'><col width='12%'><col width='12%'><col width='12%'><col width='11%'><col width='11%'>")
cat(paste("<tr>","<th>School</th><th>Years</th><th>Wins</th><th>Losses</th><th>Ties</th><th>First</th><th>Last</th></tr>",sep=""))
write.table(byteam["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

##########################################
############# index page #################
##########################################

byyear2 <- byyear[order(-byyear$Year),]
byyear2 <- byyear2[1:10,]

byteam2 <- byteam[order(-byteam$W, -byteam$L),]
byteam2 <- byteam2[1:10,]

byyear2$row <- paste("<tr><td>",paste(
	paste("<a href='",byyear2$Year,".html'>",byyear2$Year ,"</a>",sep=""),
	paste("<div style='text-align:left'><a href='",teamlink(byyear2$School),".html'>",byyear2$School,"</a>",
					"</div>",sep=""),
	byyear2$col1,
			sep="</td><td>"),"</td></tr>",sep="")

byteam2$row <- paste("<tr><td>",paste(
				paste("<div style='text-align:left'><a href='",teamlink(byteam2$School),".html'>",byteam2$School,"</a>",
					"</div>",sep=""),
				byteam2$W,
				byteam2$L,
			sep="</td><td>"),"</td></tr>",sep="")

sink(paste("../BSB/index.html",sep=""))

headerx <- gsub("ABC123","BSB",header)
write.table(headerx, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)

cat("<br><table width='80%' align='center'>")
cat(paste("<tr>","<th colspan='3' class='hl'><div style='font-size:1.5em;'>Results by Year</div></th>","</tr>"))
cat(paste("<tr>","<th>Year</th><th>Best Record</th><th>W-L</th>","</tr>"))
write.table(byyear2["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr>","<td colspan='4'><a href='byyear.html'>All Years</a></td>","</tr>"))
cat("</table><br>")

cat("<table width='80%' align='center'>")
cat(paste("<tr>","<th colspan='3' class='hl'><div style='font-size:1.5em;'>Results by Team</div></th>","</tr>"))
cat(paste("<tr>","<th>Team</th><th>W</th><th>L</th>","</tr>"))
write.table(byteam2["row"], file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
cat(paste("<tr>","<td colspan='4'><a href='byteam.html'>All Teams</a></td>","</tr>"))
cat("</table><br>")

write.table(footer, file = "", quote = FALSE, row.names = FALSE, col.names = FALSE)
sink()

