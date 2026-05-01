 
setwd("C:/Users/andyh/Documents/OMAHASERIES/Code")
library("readxl")

########## College World Series ######################

cwshtml <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/cwshtml.xlsx",sheet = "cwshtml")
cwshtml <- as.data.frame(cwshtml) 
write.csv(cwshtml, file = "../Data/cwshtml.csv", quote = FALSE, row.names = FALSE, na="")

cwsteams <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/cwsteams.xlsx",sheet = "cwsteams",col_types="text")
cwsteams <- as.data.frame(cwsteams) 
write.csv(cwsteams, file = "../Data/cwsteams.csv", quote = FALSE, row.names = FALSE, na="")

source("./cwshtml.r")
source("./cwsbracket.r")
source("./cwsbracket2.r")

source("./cbseasons.r")
source("./cbase.r")

########## Pro Sports / World Cup ######################

seasons <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Scores.xlsx",sheet = "Seasons",col_types="text")
seasons <- as.data.frame(seasons) 
write.csv(seasons, file = "../Data/seasons.csv", quote = FALSE, row.names = FALSE, na="")

playoffs <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Scores.xlsx",sheet = "Playoffs",col_types="text")
playoffs <- as.data.frame(playoffs) 
write.csv(playoffs, file = "../Data/playoffs.csv", quote = FALSE, row.names = FALSE, na="")

asg <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Scores.xlsx",sheet = "ASG")
asg <- as.data.frame(asg) 
write.csv(asg, file = "../Data/asg.csv", quote = FALSE, row.names = FALSE, na="")

pro <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/protournament.xlsx",sheet = "proexport",col_types="text")
pro <- as.data.frame(pro) 
write.csv(pro, file = "../Data/protournament.csv", quote = FALSE, row.names = FALSE, na="")

worldcup <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Scores.xlsx",sheet = "WorldCup")
worldcup <- as.data.frame(worldcup) 
write.csv(worldcup, file = "../Data/worldcup.csv", quote = FALSE, row.names = FALSE, na="")

source("almanac.r")
source("probracket.r")
source("worldcup.r")

########## College Football ######################
source("cfgames.r")
source("cfseasons.r")

seasons <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/CollegeFB.xlsx",sheet = "seasons")
seasons <- as.data.frame(seasons) 
#write.csv(seasons, file = "../Data/cfseasons.csv", quote = FALSE, row.names = FALSE, na="")

bowls <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/CollegeFB.xlsx",sheet = "bowls")
bowls <- as.data.frame(bowls) 
write.csv(bowls, file = "../Data/cfbowls.csv", quote = FALSE, row.names = FALSE, na="")

polls <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/CollegeFB.xlsx",sheet = "polls")
polls <- as.data.frame(polls) 
write.csv(polls, file = "../Data/cfpolls.csv", quote = FALSE, row.names = FALSE, na="")

weekly <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/CollegeFB.xlsx",sheet = "weekly")
weekly <- as.data.frame(weekly) 
write.csv(weekly, file = "../Data/cfweekly.csv", quote = FALSE, row.names = FALSE, na="")

champs <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/CollegeFB.xlsx",sheet = "champs")
champs <- as.data.frame(champs) 
write.csv(champs, file = "../Data/cfchamps.csv", quote = FALSE, row.names = FALSE, na="")

playoffs <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/CollegeFB.xlsx",sheet = "playoffs")
playoffs <- as.data.frame(playoffs) 
write.csv(playoffs, file = "../Data/cfplayoffs.csv", quote = FALSE, row.names = FALSE, na="")

source("cfalmanac.r") 

########## Mens NCAA Basketball Tournament ######################

ncaat <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/Scores.xlsx",sheet = "NCAAT")
ncaat <- as.data.frame(ncaat) 
write.csv(ncaat, file = "../Data/ncaat.csv", quote = FALSE, row.names = FALSE, na="")

bracket <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/ncaatournament.xlsx",sheet = "NCAAT")
bracket <- as.data.frame(bracket) 
write.csv(bracket, file = "../Data/ncaatournament.csv", quote = FALSE, row.names = FALSE, na="")

source("ncaat.r")
source("ncaabracket.r")

########## Other NCAA Sports ######################

ncaagames <- read_excel("C://Users/andyh/OneDrive/Documents/SPORTSDATA/NCAAGames.xlsx",sheet = "games")
ncaagames <- as.data.frame(ncaagames) 
write.csv(ncaagames, file = "../Data/ncaagames.csv", quote = FALSE, row.names = FALSE, na="")

source("ncaasports.r")
source("ncaasoft.r")
source("sportsbracket.r")



