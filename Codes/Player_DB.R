library(dplyr)
library(tidyr)
setwd("~/Box/Alissa Private Folder/Alex/SDMB1/Newest_Sim")

filenames = paste(paste('players', '_', 1:2, sep = ''), '.csv', sep = '') 
x_pl <- read.csv('players.csv',header = TRUE)
x_pl1 <- do.call(rbind, lapply(filenames, read.csv, header = FALSE))
colnames(x_pl1) <- colnames(x_pl)
combined.players <- rbind(x_pl,x_pl1)
combined.players$full_name <- paste(combined.players$first_name,combined.players$last_name)
pl_name_lookup <- combined.players[c(1,99)]
pl_info <- combined.players[c(1:4,6:7,9,16,17,27:34,38,46,99)]
pl_info <- subset(pl_info,pl_info$retired != 1 & pl_info$age < 45)
pl_info$retired <- NULL

x_bat_handeness <- data_frame("bats" = c(1,2,3), "bats_hand" = c("R","L","S"))
x_throw_handeness <- data_frame("throws" = c(1,2), "throws_hand" = c("R","L"))

pl_info <- merge(pl_info, x_bat_handeness, by = "bats")
pl_info <- merge(pl_info, x_throw_handeness, by = "throws")

all_teams <- read.csv("teams.csv",header = TRUE)
all_teams$team_name <- paste(all_teams$name,all_teams$nickname)
teams_lookup <- all_teams[,c(1,3,8,10,12,13,16,28)]
pl_info <- merge(pl_info, teams_lookup, by = "team_id", all.x = TRUE)
parent_teams_lookup <- teams_lookup[c(1:3,8)]
parent_teams_lookup <- subset(parent_teams_lookup, parent_teams_lookup$league_id == 100)
parent_teams_lookup$league_id <- NULL
colnames(parent_teams_lookup)[colnames(parent_teams_lookup) == 'team_id'] <- 'parent_team_id'
colnames(parent_teams_lookup)[colnames(parent_teams_lookup) == 'abbr'] <- 'parent_team_abbr'
colnames(parent_teams_lookup)[colnames(parent_teams_lookup) == 'team_name'] <- 'parent_team_name'
pl_info <- merge(pl_info, parent_teams_lookup, by = "parent_team_id", all.x = TRUE)


divisions <- data_frame('division_id' = 0:2,"division" = c("Adams","Zotti","Signorino"), "league_id" = c(100,100,100) )
pl_info$league_id.x <- NULL
colnames(pl_info)[colnames(pl_info) == 'league_id.y'] <- 'league_id'
pl_info <- merge(pl_info, divisions, by = c("division_id","league_id"), all.x = TRUE)

leagues <- read.csv("leagues.csv", header = TRUE)
leagues <- leagues[c(1,2,3)]
colnames(leagues)[colnames(leagues) == 'name'] <- 'league_name'
colnames(leagues)[colnames(leagues) == 'abbr'] <- 'league_abbr'
pl_info <- merge(pl_info,leagues,by = "league_id", all.x = TRUE)


positions <- data.frame('position' = 1:10, 'position_name' = c('P','C','1B','2B','3B','SS','LF','CF','RF','DH'))
pl_info <- merge(pl_info,positions,by = "position", all.x = TRUE)

pl_info$org_id <- ifelse(pl_info$parent_team_id == 0, pl_info$team_id, pl_info$parent_team_id)
pl_info$org_id <- ifelse(pl_info$org_id > 12, 0, pl_info$org_id)

org_lookup <- teams_lookup[c(1:3,8)]
org_lookup <- subset(org_lookup, org_lookup$league_id == 100)
org_lookup$league_id <- NULL
colnames(org_lookup)[colnames(org_lookup) == 'team_id'] <- 'org_id'
colnames(org_lookup)[colnames(org_lookup) == 'abbr'] <- 'org_abbr'
colnames(org_lookup)[colnames(org_lookup) == 'team_name'] <- 'org_name'
pl_info <- merge(pl_info, org_lookup, by = "org_id", all.x = TRUE)


x_pl_batting <- read.csv('players_batting.csv', header =TRUE)
x_overall_pl_batting <- x_pl_batting[c(1,6:12,27:33)]
x_right_pl_batting <- x_pl_batting[c(1,13:19)]
x_left_pl_batting <- x_pl_batting[c(1,20:26)]

x_pl_fielding <- read.csv('players_fielding.csv', header=TRUE)
x_pl_fielding <- x_pl_fielding[c(1,6:14)]

x_pl_pitching <- read.csv('players_pitching.csv', header = TRUE)
x_overall_pl_pitching <- x_pl_pitching[c(1,6:11,24:29,54:58)]
x_right_pl_pitching <- x_pl_pitching[c(1,12:17)]
x_left_pl_pitching <- x_pl_pitching[c(1,18:23)]
x_pl_pitches <- x_pl_pitching[c(1,30:53)]

x_pl_values <- read.csv('players_value.csv', header = TRUE)
x_overall_pl_values <- x_pl_values[c(1,6:7,10:11,14:16,19:21,42:43)]
x_right_pl_values <- x_pl_values[c(1,9,13,18)]
x_left_pl_values <- x_pl_values[c(1,8,12,17)]

overall <- merge(x_overall_pl_batting, x_overall_pl_pitching, by = "player_id",all.x = TRUE)
overall <- merge(overall, x_pl_fielding, by= "player_id")
overall <- merge(overall, x_overall_pl_values, by="player_id",all.x = TRUE)
overall_pl_info <- merge(pl_info,overall,by="player_id",all.x = TRUE)
write.csv(overall_pl_info,"~/Box/Alissa Private Folder/Alex/SDMB1/R_Code_Exports/Overall.csv", row.names =  FALSE)

right <- merge(x_right_pl_batting, x_right_pl_pitching, by="player_id",all.x = TRUE)
right <- merge(right, x_right_pl_values, by="player_id",all.x = TRUE)
#right_splits_pl_info <- merge(pl_info, right, by="player_id",all.x = TRUE)
write.csv(right,"~/Box/Alissa Private Folder/Alex/SDMB1/R_Code_Exports/Right.csv", row.names = FALSE)

left <- merge(x_left_pl_batting, x_left_pl_pitching, by="player_id",all.x = TRUE)
left <- merge(left, x_left_pl_values, by="player_id",all.x = TRUE)
#left_splits_pl_info <- merge(pl_info, left, by="player_id",all.x = TRUE)
write.csv(left,"~/Box/Alissa Private Folder/Alex/SDMB1/R_Code_Exports/Left.csv", row.names = FALSE)
