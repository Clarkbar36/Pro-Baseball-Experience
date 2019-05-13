library(dplyr)
library(tidyverse)
library(tidyr)
setwd("~/Documents/GitHub/PBE/Exports/")

games <- read.csv('games.csv', header = TRUE, sep = ',')

games$game_id <- as.numeric(games$game_id)

all_teams <- read.csv('teams.csv',header = TRUE, sep = ',')

teams <- subset(all_teams,all_teams$league_id != 1000)
teams$team_name <- paste(teams$name,teams$nickname)
teams_lookup <- teams[,c(1,3,10,28)]
teams_home <- teams_lookup
colnames(teams_home)[colnames(teams_home) == 'team_id'] <- 'home_team'
teams_away <- teams_lookup
colnames(teams_away)[colnames(teams_away) == 'team_id'] <- 'away_team'

m_games <- merge(games,teams_home,by='home_team',all=TRUE)
colnames(m_games)[colnames(m_games) == 'abbr'] <- 'home_abbr'
colnames(m_games)[colnames(m_games) == 'team_name'] <- 'home_team_name'
colnames(m_games)[colnames(m_games) == 'division_id'] <- 'home_division_id'

m_games <- merge(m_games,teams_away,by='away_team',all=TRUE)
colnames(m_games)[colnames(m_games) == 'abbr'] <- 'away_abbr'
colnames(m_games)[colnames(m_games) == 'team_name'] <- 'away_team_name'
colnames(m_games)[colnames(m_games) == 'division_id'] <- 'away_division_id'

m_games$home <- 'home'
m_games$away <- 'away'

m_games <- subset(m_games,m_games$played == 1 & m_games$game_type == 0 & m_games$league_id != 1000)


m_games$home_win <- ifelse(m_games$runs1 > m_games$runs0, 1, 0)
m_games$home_loss <- ifelse(m_games$runs1 < m_games$runs0, 1, 0) 
m_games$away_win <- ifelse(m_games$runs0 > m_games$runs1, 1, 0)
m_games$away_loss <- ifelse(m_games$runs0 < m_games$runs1, 1, 0) 

home_box <- m_games[,c(2:4,6:7,11,13,15,23:25,29,31:32)]
away_box <- m_games[,c(1,3,6:7,11,12,14,26:28,30,33:34)]
box <- cbind(home_box,away_box)
colnames(box)[colnames(box) == 'runs1'] <- 'runs_home'
colnames(box)[colnames(box) == 'hits1'] <- 'hits_home'
colnames(box)[colnames(box) == 'runs0'] <- 'runs_away'
colnames(box)[colnames(box) == 'hits0'] <- 'hits_away'
box <- box[,-c(16:19)] 


#write.csv(box,"R_Code_Exports/box.csv",row.names = FALSE)

home <- m_games[,c(2:4,6:7,11,13,15,23:25,29,31:32)]
colnames(home)[colnames(home) == 'home_team'] <- 'team_id'
colnames(home)[colnames(home) == 'runs1'] <- 'runs'
colnames(home)[colnames(home) == 'hits1'] <- 'hits'
colnames(home)[colnames(home) == 'home_abbr'] <- 'team_abbr'
colnames(home)[colnames(home) == 'home_team_name'] <- 'team_name'
colnames(home)[colnames(home) == 'home'] <- 'home/away'
colnames(home)[colnames(home) == 'home_win'] <- 'win'
colnames(home)[colnames(home) == 'home_loss'] <- 'loss'
colnames(home)[colnames(home) == 'home_division_id'] <- 'division_id'

away <- m_games[,c(1,3:4,6:7,11,12,14,26:28,30,33:34)]
colnames(away)[colnames(away) == 'away_team'] <- 'team_id'
colnames(away)[colnames(away) == 'runs0'] <- 'runs'
colnames(away)[colnames(away) == 'hits0'] <- 'hits'
colnames(away)[colnames(away) == 'away_abbr'] <- 'team_abbr'
colnames(away)[colnames(away) == 'away_team_name'] <- 'team_name'
colnames(away)[colnames(away) == 'away'] <- 'home/away'
colnames(away)[colnames(away) == 'away_win'] <- 'win'
colnames(away)[colnames(away) == 'away_loss'] <- 'loss'
colnames(away)[colnames(away) == 'away_division_id'] <- 'division_id'

all_games <- rbind(home,away)

divisions <- read.csv("divisions.csv")
divisions$divisions_id <- paste(divisions$league_id,"-",divisions$division_id)
divisions <- divisions[c(4,6)]
all_games$divisions_id <- paste(all_games$league_id,"-",all_games$division_id)
all_games <- merge(all_games,divisions,by='divisions_id')

all_games$divisions_id <- NULL
all_games$division_id <- NULL
colnames(all_games)[colnames(all_games) == 'name'] <- 'division'

leagues <- data.frame(league_id = c(100,101), league_abbr = c("PBE","MiPBE") )
all_games <- merge(all_games, leagues, all.x = TRUE)

all_games <- all_games[order(all_games[,1],all_games[,3],all_games[,4]),]

all_games <- all_games %>% group_by(team_abbr) %>% mutate(ttl_wins = cumsum(win))
all_games <- all_games %>% group_by(team_abbr) %>% mutate(ttl_losses = cumsum(loss))
all_games$below.500 <- all_games$ttl_wins - all_games$ttl_losses
all_games$winloss <- paste(all_games$win, "-", all_games$loss)

#write.csv(all_games,"R_Code_Exports/PBE_Standings.csv",row.names = FALSE)

# league_averages <- all_games
# league_averages <- league_averages[c(4,6:8,13:14)]
# league_averages <- aggregate(. ~ date, league_averages, sum)
# league_averages <- league_averages[order(as.Date(league_averages$date)),]
# 
# league_averages$Average_Innings <- round(cumsum(league_averages$innings)/12,2)
# league_averages$Average_Runs <- round(cumsum(league_averages$runs)/12,2)
# league_averages$Average_Hits <- round(cumsum(league_averages$hits)/12,2)
# league_averages$Average_Wins <- round(cumsum(league_averages$win)/12,2)
# league_averages$Average_Loss <- round(cumsum(league_averages$loss)/12,2)

#write.csv(league_averages,"~/Box/Alissa Private Folder/Alex/SDMB1/R_Code_Exports/League Averages.csv",row.names = FALSE)

