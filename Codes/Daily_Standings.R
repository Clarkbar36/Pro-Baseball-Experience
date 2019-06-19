library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(Hmisc)
#setwd("~/Documents/GitHub/PBE/Exports/")

ds.games <- read.csv('Exports/games.csv', header = TRUE, sep = ',')

ds.games$game_id <- as.numeric(ds.games$game_id)

ds.all_teams <- read.csv('Exports/teams.csv',header = TRUE, sep = ',')

ds.teams <- subset(ds.all_teams,ds.all_teams$league_id != 1000)
ds.teams$team_name <- paste(ds.teams$name,ds.teams$nickname)
ds.teams_lookup <- ds.teams[,c(1,3,10,28)]
ds.teams_home <- ds.teams_lookup
colnames(ds.teams_home)[colnames(ds.teams_home) == 'team_id'] <- 'home_team'
ds.teams_away <- ds.teams_lookup
colnames(ds.teams_away)[colnames(ds.teams_away) == 'team_id'] <- 'away_team'

ds.m_games <- merge(ds.games,ds.teams_home,by='home_team',all=TRUE)
colnames(ds.m_games)[colnames(ds.m_games) == 'abbr'] <- 'home_abbr'
colnames(ds.m_games)[colnames(ds.m_games) == 'team_name'] <- 'home_team_name'
colnames(ds.m_games)[colnames(ds.m_games) == 'division_id'] <- 'home_division_id'

ds.m_games <- merge(ds.m_games,ds.teams_away,by='away_team',all=TRUE)
colnames(ds.m_games)[colnames(ds.m_games) == 'abbr'] <- 'away_abbr'
colnames(ds.m_games)[colnames(ds.m_games) == 'team_name'] <- 'away_team_name'
colnames(ds.m_games)[colnames(ds.m_games) == 'division_id'] <- 'away_division_id'

ds.m_games$home <- 'home'
ds.m_games$away <- 'away'

ds.m_games <- subset(ds.m_games,ds.m_games$played == 1 & ds.m_games$game_type == 0 & ds.m_games$league_id != 1000)


ds.m_games$home_win <- ifelse(ds.m_games$runs1 > ds.m_games$runs0, 1, 0)
ds.m_games$home_loss <- ifelse(ds.m_games$runs1 < ds.m_games$runs0, 1, 0) 
ds.m_games$away_win <- ifelse(ds.m_games$runs0 > ds.m_games$runs1, 1, 0)
ds.m_games$away_loss <- ifelse(ds.m_games$runs0 < ds.m_games$runs1, 1, 0) 

ds.home_box <- ds.m_games[,c(2:4,6:7,11,13,15,23:25,29,31:32)]
ds.away_box <- ds.m_games[,c(1,3,6:7,11,12,14,26:28,30,33:34)]
ds.box <- cbind(ds.home_box,ds.away_box)
colnames(ds.box)[colnames(ds.box) == 'runs1'] <- 'runs_home'
colnames(ds.box)[colnames(ds.box) == 'hits1'] <- 'hits_home'
colnames(ds.box)[colnames(ds.box) == 'runs0'] <- 'runs_away'
colnames(ds.box)[colnames(ds.box) == 'hits0'] <- 'hits_away'
ds.box <- ds.box[,-c(16:19)] 


#write.csv(box,"R_Code_Exports/box.csv",row.names = FALSE)

ds.home <- ds.m_games[,c(2:4,6:7,11,13,15,23:25,29,31:32)]
colnames(ds.home)[colnames(ds.home) == 'home_team'] <- 'team_id'
colnames(ds.home)[colnames(ds.home) == 'runs1'] <- 'runs'
colnames(ds.home)[colnames(ds.home) == 'hits1'] <- 'hits'
colnames(ds.home)[colnames(ds.home) == 'home_abbr'] <- 'team_abbr'
colnames(ds.home)[colnames(ds.home) == 'home_team_name'] <- 'team_name'
colnames(ds.home)[colnames(ds.home) == 'home'] <- 'home/away'
colnames(ds.home)[colnames(ds.home) == 'home_win'] <- 'win'
colnames(ds.home)[colnames(ds.home) == 'home_loss'] <- 'loss'
colnames(ds.home)[colnames(ds.home) == 'home_division_id'] <- 'division_id'

ds.away <- ds.m_games[,c(1,3:4,6:7,11,12,14,26:28,30,33:34)]
colnames(ds.away)[colnames(ds.away) == 'away_team'] <- 'team_id'
colnames(ds.away)[colnames(ds.away) == 'runs0'] <- 'runs'
colnames(ds.away)[colnames(ds.away) == 'hits0'] <- 'hits'
colnames(ds.away)[colnames(ds.away) == 'away_abbr'] <- 'team_abbr'
colnames(ds.away)[colnames(ds.away) == 'away_team_name'] <- 'team_name'
colnames(ds.away)[colnames(ds.away) == 'away'] <- 'home/away'
colnames(ds.away)[colnames(ds.away) == 'away_win'] <- 'win'
colnames(ds.away)[colnames(ds.away) == 'away_loss'] <- 'loss'
colnames(ds.away)[colnames(ds.away) == 'away_division_id'] <- 'division_id'

ds.all_games <- rbind(ds.home,ds.away)

ds.divisions <- read.csv("Exports/divisions.csv")
ds.divisions$divisions_id <- paste(ds.divisions$league_id,"-",ds.divisions$division_id)
ds.divisions <- ds.divisions[c(4,6)]
ds.all_games$divisions_id <- paste(ds.all_games$league_id,"-",ds.all_games$division_id)
ds.all_games <- merge(ds.all_games,ds.divisions,by='divisions_id')

ds.all_games$divisions_id <- NULL
ds.all_games$division_id <- NULL
colnames(ds.all_games)[colnames(ds.all_games) == 'name'] <- 'division'

ds.leagues <- data.frame(league_id = c(100,101), league_abbr = c("PBE","MiLPBE") )
ds.all_games <- merge(ds.all_games, ds.leagues, all.x = TRUE)

ds.all_games <- ds.all_games[order(ds.all_games[,1],ds.all_games[,3],ds.all_games[,4]),]


#create daily standings columns
ds.all_games <- ds.all_games %>% group_by(team_abbr) %>% mutate(ttl_wins = cumsum(win))
ds.all_games <- ds.all_games %>% group_by(team_abbr) %>% mutate(ttl_losses = cumsum(loss))
ds.all_games$below.500 <- ds.all_games$ttl_wins - ds.all_games$ttl_losses
ds.all_games$winloss <- paste(ds.all_games$ttl_wins, "-", ds.all_games$ttl_losses)
ds.all_games$ID <- paste(ds.all_games$game_id, "-", ds.all_games$loss)
RA_tbl <- ds.all_games[c(3,7,12)]
RA_tbl$ID <- paste(RA_tbl$game_id, "-",RA_tbl$win) 
RA_tbl <- RA_tbl[c(2,4)]
colnames(RA_tbl)[colnames(RA_tbl) == 'runs'] <- 'RA'
ds.all_games <- merge(ds.all_games,RA_tbl,all.x = TRUE)
ds.all_games$ID <- NULL
ds.all_games <- ds.all_games %>% group_by(team_abbr) %>% mutate(ttl_ra = cumsum(RA))
ds.all_games <- ds.all_games %>% group_by(team_abbr) %>% mutate(ttl_runs = cumsum(runs))
ds.all_games <- ds.all_games %>% group_by(team_abbr) %>% mutate(ttl_hits = cumsum(hits))
ds.all_games <- ds.all_games %>% group_by(team_abbr) %>% mutate(ttl_rs_sq = cumsum(runs)^2)
ds.all_games <- ds.all_games %>% group_by(team_abbr) %>% mutate(ttl_ra_sq = cumsum(RA)^2)
ds.all_games$gp <- ds.all_games$ttl_wins + ds.all_games$ttl_losses
ds.all_games$wp <- ds.all_games$ttl_rs_sq/(ds.all_games$ttl_rs_sq + ds.all_games$ttl_ra_sq)
ds.all_games$pythag_win <- round(ds.all_games$wp*ds.all_games$gp,0)
ds.all_games$pythag_loss <- round(ds.all_games$gp - ds.all_games$pythag_win, 0)
ds.all_games$pythag_record <- paste(ds.all_games$pythag_win, "-", ds.all_games$pythag_loss)


ds.color <- read_excel("Misc/Colors.xlsx")
ds.color <- ds.color[c(1,3)]
ds.all_games <- merge(ds.all_games, ds.color, all.x = TRUE)



ds.tm <- read.csv("Exports/teams.csv")
ds.tm$`Team Name` <- paste(ds.tm$name,ds.tm$nickname)
ds.tm <- ds.tm[c(1,3,8,28)]
colnames(ds.tm)[colnames(ds.tm) == 'abbr'] <- 'Team Abbr'

ds.lg <- read.csv("Exports/leagues.csv")
ds.lg <- ds.lg[c(1,3)]
colnames(ds.lg)[colnames(ds.lg) == 'abbr'] <- 'League'

ds.stdp <- read.csv("Exports/team_record.csv")
ds.stdp <- ds.stdp[c(1,5,6)]
ds.stdp <- subset(ds.stdp,ds.stdp$team_id %nin% c(7,8,12,13,14,15,22,23))
colnames(ds.stdp)[colnames(ds.stdp) == 'pos'] <- 'Division Standing'

ds.lkup <- merge(ds.tm,ds.lg, all.x = TRUE)
ds.lkup <- merge(ds.lkup, ds.stdp)
ds.lkup <- ds.lkup %>% 
  group_by(league_id) %>% 
  mutate(`League Standing` = round(rank(-pct),0)) %>% 
  ungroup
ds.lkup <- ds.lkup[c(1,6,8)]

ds.all_games <- merge(ds.all_games,ds.lkup,all.x = TRUE)


ds.all_games$date <- as.Date(ds.all_games$date)
season <- substring(max(ds.all_games$date),1,4)
ds.all_games$season <- season

write.csv(ds.all_games,paste("R_Code_Exports/",season,"_PBE_Standings.csv",sep=""),row.names = FALSE)

filenames = paste(paste("R_Code_Exports/",2027:2028,"_PBE_Standings",sep=""), '.csv', sep = '') 
wab.all_games <- lapply(filenames,function(i){
  read.csv(i, header=TRUE, stringsAsFactors = FALSE)
})
wab.all_games <- bind_rows(wab.all_games)
wab.all_games$date <- as.Date(wab.all_games$date, "%Y-%m-%d")
ssn <- unique(wab.all_games$season)


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

