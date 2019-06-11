library(rvest)
library(rlist)

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
TPE <- data.frame(`Topic Title`=character(), League = character(), Team_Name = character())

for (n in num){
  for (t in tm){
  url <- paste("http://probaseballexperience.jcink.net/index.php?showforum=",t,"&st=",n,sep="")
  links <- list.append(url,links)
}}

for (l in rev(links)){

  roster <- read_html(l)
  players <- roster %>% html_node(xpath = '//*[@id="topic-list"]/form[1]/table') %>% html_table(header = TRUE)
  league_name <- roster %>% html_node(xpath = '//*[@id="navstrip"]/a[2]') %>% html_text() 
  League <- ifelse(substr(league_name,1,1)=="P","PBE","MiLPBE")
  Team_Name <- roster %>% html_node(xpath = '//*[@id="navstrip"]/a[3]') %>% html_text()
  players <- players[3]
  l.TPE <- cbind(players, League, Team_Name)
  TPE <- rbind(l.TPE,TPE)

}
