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
           '155')
n.TPE <- data.frame('Topic.Title'=character(), League = character(), Team_Name = character())

abbr <- list('SAS','DV','FL','FLA','UTA','NYV','OBX','PRO','VAN')

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
  league_name <- roster %>% html_node(xpath = '//*[@id="navstrip"]/a[2]') %>% html_text() 
  League <- ifelse(substr(league_name,1,1)=="P","PBE","MiLPBE")
  Team_Name <- roster %>% html_node(xpath = '//*[@id="navstrip"]/a[3]') %>% html_text()
  l.TPE <- cbind(players, League, Team_Name)
  n.TPE <- rbind(l.TPE,n.TPE)

}
TPE <- n.TPE
TPE <- TPE[complete.cases(TPE), ]
TPE <- TPE[!grepl("GM", TPE$Topic.Title),]
TPE <- data.frame(lapply(TPE, function(x) {
  gsub("\\[|\\]", "", x)
  }))



TPE$created.season <- trim(substr(TPE$Topic.Title,1,3))
find.string <- paste(unlist(abbr), collapse = "|")
TPE$Topic.Title <-  gsub(find.string, replacement = "", x = TPE$Topic.Title)

TPE <- separate(TPE,Topic.Title,c("player","brkt"),sep=" - ",remove = FALSE)
TPE$player <- trim(substr(TPE$player,4,length(TPE$player)))
TPE <- separate(TPE,brkt,"positon",sep = " ",remove = FALSE)
TPE <- separate(TPE,brkt,c("remove","TPE"),sep = ": ",remove = FALSE)
TPE$remove <- NULL


