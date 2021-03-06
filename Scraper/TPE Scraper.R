library(rvest)
library(rlist)
library(tidyverse)
library(gdata)
library(reshape)

links <- list()
num <- seq(0,45,15)
tm <- list('62',
           '160',
           '163',
           '74',
           '65',
           '71',
           '59',
           '68',
           '138',
           '140',
           '152',
           '155',
           '175',
           '177')
n.TPE <- data.frame('Topic.Title'=character(), League = character(), Team_Name = character())

abbr <- list('SAS','DV','FL','FLA','UTA','NYV','OBX','PRO','VAN','CAN','NAS')

for (n in num){
  for (t in tm){
  url <- paste("http://probaseballexperience.jcink.net/index.php?showforum=",t,"&st=",n,sep="")
  links <- list.append(url,links)
}}

for (l in rev(links)){

  roster <- read_html(l)
  players <- roster %>% html_node(xpath = '//*[@id="topic-list"]/form[1]/table') %>% html_table(header = TRUE)
  players <- players[3]
  colnames(players)[colnames(players) == 'Topic Title'] <- 'Topic.Title'
  if (dim(players)[1]==0) {
    blank <- data.frame('Topic.Title' = NA)
    players <- rbind(blank,players)
  }else{
    players <- players
  }
  # league_name <- roster %>% html_node(xpath = '//*[@id="navstrip"]/a[2]') %>% html_text() 
  # League <- ifelse(substr(league_name,1,1)=="P","PBE","MiLPBE")
  Team_Name <- roster %>% html_node(xpath = '//*[@id="navstrip"]/a[3]') %>% html_text()
  l.TPE <- cbind(players, Team_Name)
  n.TPE <- rbind(l.TPE,n.TPE)

}
TPE <- n.TPE
TPE <- TPE[complete.cases(TPE), ]
TPE <- TPE[!grepl("GM", TPE$Topic.Title),]
TPE <- data.frame(lapply(TPE, function(x) {
  gsub("\\[|\\]", "", x)
  }))

TPE$Created.Season <- trim(substr(TPE$Topic.Title,1,3))
find.string <- paste(unlist(abbr), collapse = "|")
TPE$Topic.Title <-  gsub(find.string, replacement = "", x = TPE$Topic.Title)

TPE <- separate(TPE,Topic.Title,c("player","brkt"),sep=" - ",remove = FALSE)
TPE$player <- trim(substr(TPE$player,4,length(TPE$player)))
TPE <- separate(TPE,brkt,"positon",sep = " ",remove = FALSE)
TPE <- separate(TPE,brkt,c("remove","TPE"),sep = ": ",remove = FALSE)
TPE$remove <- NULL
TPE$Topic.Title <- NULL
TPE$brkt <- NULL



TPE[] <- lapply(TPE, as.character)
TPE$TPE <- as.numeric(TPE$TPE)
Team_Name <- unique(TPE$Team_Name)
lg <- data.frame(number = 1:4, League = "MiLPBE")
lg2 <- data.frame(number = 1:10, League = "PBE")
lg <- rbind(lg,lg2)
lg$number <- NULL
tmlg <- cbind(Team_Name, lg)
TPE <- merge(TPE,tmlg,all.x = TRUE)
PBE_TPE <- subset(TPE,League == "PBE")
MiLPBE_TPE <- subset(TPE,League == "MiLPBE")

#write.csv(PBE_TPE,"~/Documents/GitHub/PBE/Scraper/PBE_TPE.csv",row.names = FALSE)
#write.csv(MiLPBE_TPE,"~/Documents/GitHub/PBE/Scraper/MiLPBE_TPE",row.names = FALSE)
