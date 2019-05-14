library(dplyr)
library(tidyr)
library(Hmisc)
#setwd("~/Documents/GitHub/PBE/Exports")

## Teams ##
r.all_teams <- read.csv('Exports/teams.csv',header = TRUE, sep = ',')
r.teams <- subset(r.all_teams,r.all_teams$league_id != 1000)
r.teams$team_name <- paste(r.teams$name,r.teams$nickname)
r.teams_lookup <- r.teams[,c(1,3,28)]

## Records History ##
r.team_history <- read.csv('Exports/team_history.csv',header=TRUE, sep=',')
r.team_history_record <- read.csv('Exports/team_history_record.csv',header=TRUE,sep=',')
r.team_history <- subset(r.team_history,r.team_history$league_id != 1000)
r.team_history <- r.team_history[,c(1,2,9:11,13:14)]
r.team_history_record <- subset(r.team_history_record,r.team_history_record$league_id != 1000)


## Players ##
r.combined.players <- read.csv('Exports/players.csv',header = TRUE)
r.combined.players$full_name <- paste(r.combined.players$first_name,r.combined.players$last_name)
r.player_lookup <- r.combined.players[,c(1,99)]

r.hitter_player_lookup <- r.player_lookup
colnames(r.hitter_player_lookup)[colnames(r.hitter_player_lookup) == 'player_id'] <- 'best_hitter_id'
colnames(r.hitter_player_lookup)[colnames(r.hitter_player_lookup) == 'full_name'] <- 'best_hitter_name'

r.pitcher_player_lookup <- r.player_lookup
colnames(r.pitcher_player_lookup)[colnames(r.pitcher_player_lookup) == 'player_id'] <- 'best_pitcher_id'
colnames(r.pitcher_player_lookup)[colnames(r.pitcher_player_lookup) == 'full_name'] <- 'best_pitcher_name'

r.rookie_player_lookup <- r.player_lookup
colnames(r.rookie_player_lookup)[colnames(r.rookie_player_lookup) == 'player_id'] <- 'best_rookie_id'
colnames(r.rookie_player_lookup)[colnames(r.rookie_player_lookup) == 'full_name'] <- 'best_rookie_name'

## Division ##
r.divisions <- read.csv("Exports/divisions.csv")
r.divisions$divisions_id <- paste(r.divisions$league_id,"-",r.divisions$division_id)
r.divisions <- r.divisions[c(4,6)]
r.team_history_record$divisions_id <- paste(r.team_history_record$league_id,"-",r.team_history_record$division_id)
r.team_history_record <- merge(r.team_history_record,r.divisions,by='divisions_id',all.x = TRUE)
r.team_history_record$divisions_id <- NULL
r.team_history_record$division_id <- NULL
colnames(r.team_history_record)[colnames(r.team_history_record) == 'name'] <- 'division'

## Combine ##
r.records <- merge(r.team_history_record,r.teams_lookup,by = "team_id")
r.records <- merge(r.records, r.team_history, by =c("team_id","year"))
r.records <- merge(r.records,r.hitter_player_lookup, all.x = TRUE)
r.records <- merge(r.records,r.pitcher_player_lookup, all.x = TRUE)
r.records <- merge(r.records,r.rookie_player_lookup, all.x = TRUE)

r.leagues <- data.frame(league_id = c(100,101), league_abbr = c("PBE","MiPBE") )
r.records <- merge(r.records, r.leagues, all.x = TRUE)

#eliminate first 2 seasons of Armadillos and Mounites, bot seasons
r.records$id <- paste(r.records$team_id,"-",r.records$year,sep = "")
r.records <- subset(r.records, r.records$id %nin% c("17-2021","17-2022","19-2021","19-2022"))
r.records$id <- NULL

r.records <- r.records %>% group_by(league_id) %>% mutate(Lg_Average_Wins = round(mean(w),0)) %>% ungroup()

ds.color <- read_excel("Misc/Colors.xlsx")
ds.color <- ds.color[c(1,3,5)]
r.records <- merge(r.records, ds.color, all.x = TRUE)

r.volatility <- r.records %>% group_by(league_abbr,team_name) %>% summarise(Volatility = round(sd(w),2),`Average Wins` = round(mean(w),2))

## Write ##
#write.csv(records, "R_Code_Exports/records.csv", row.names = FALSE)


