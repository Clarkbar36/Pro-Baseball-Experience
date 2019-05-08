library(dplyr)
library(tidyr)
library(Hmisc)
setwd("~/Box/Alissa Private Folder/Alex/PBE/Exports")

## Teams ##
all_teams <- read.csv('teams.csv',header = TRUE, sep = ',')
teams <- subset(all_teams,all_teams$league_id != 1000)
teams$team_name <- paste(teams$name,teams$nickname)
teams_lookup <- teams[,c(1,3,28)]

## Records History ##
team_history <- read.csv('team_history.csv',header=TRUE, sep=',')
team_history_record <- read.csv('team_history_record.csv',header=TRUE,sep=',')
team_history <- subset(team_history,team_history$league_id != 1000)
team_history <- team_history[,c(1,2,9:11,13:14)]
team_history_record <- subset(team_history_record,team_history_record$league_id != 1000)


## Players ##
combined.players <- read.csv('players.csv',header = TRUE)
combined.players$full_name <- paste(combined.players$first_name,combined.players$last_name)
player_lookup <- combined.players[,c(1,99)]

hitter_player_lookup <- player_lookup
colnames(hitter_player_lookup)[colnames(hitter_player_lookup) == 'player_id'] <- 'best_hitter_id'
colnames(hitter_player_lookup)[colnames(hitter_player_lookup) == 'full_name'] <- 'best_hitter_name'

pitcher_player_lookup <- player_lookup
colnames(pitcher_player_lookup)[colnames(pitcher_player_lookup) == 'player_id'] <- 'best_pitcher_id'
colnames(pitcher_player_lookup)[colnames(pitcher_player_lookup) == 'full_name'] <- 'best_pitcher_name'

rookie_player_lookup <- player_lookup
colnames(rookie_player_lookup)[colnames(rookie_player_lookup) == 'player_id'] <- 'best_rookie_id'
colnames(rookie_player_lookup)[colnames(rookie_player_lookup) == 'full_name'] <- 'best_rookie_name'

## Division ##
divisions <- read.csv("divisions.csv")
divisions$divisions_id <- paste(divisions$league_id,"-",divisions$division_id)
divisions <- divisions[c(4,6)]
team_history_record$divisions_id <- paste(team_history_record$league_id,"-",team_history_record$division_id)
team_history_record <- merge(team_history_record,divisions,by='divisions_id',all.x = TRUE)
team_history_record$divisions_id <- NULL
team_history_record$division_id <- NULL
colnames(team_history_record)[colnames(team_history_record) == 'name'] <- 'division'

## Combine ##
records <- merge(team_history_record,teams_lookup,by = "team_id")
records <- merge(records, team_history, by =c("team_id","year"))
records <- merge(records,hitter_player_lookup, all.x = TRUE)
records <- merge(records,pitcher_player_lookup, all.x = TRUE)
records <- merge(records,rookie_player_lookup, all.x = TRUE)

leagues <- data.frame(league_id = c(100,101), league_abbr = c("PBE","MiPBE") )
records <- merge(records, leagues, all.x = TRUE)

#eliminate first 2 seasons of Armadillos and Mounites, bot seasons
records$id <- paste(records$team_id,"-",records$year,sep = "")
records <- subset(records, records$id %nin% c("17-2021","17-2022","19-2021","19-2022"))
records$id <- NULL

records <- records %>% group_by(league_id) %>% mutate(Lg_Average_Wins = round(mean(w),0)) %>% ungroup()



## Write ##
write.csv(records, "~/Box/Alissa Private Folder/Alex/PBE/R_Code_Exports/records.csv", row.names = FALSE)
