library(dplyr)
setwd("~/Documents/GitHub/PBE/Exports/")
records <- read.csv("team_record.csv")
games_played <- max(records$g)
x_pl <- read.csv('players.csv',header = TRUE)
x_pl$full_name <- paste(x_pl$first_name,x_pl$last_name)
positions <- data.frame('position' = 1:10, 'position_name' = c('P','C','1B','2B','3B','SS','LF','CF','RF','DH'))
combined.players <- merge(x_pl,positions,by = "position", all.x = TRUE)
pl_name_lookup <- combined.players[c(2,99,100)]

draft <- combined.players[c(2,63,65:66,70,99:100)]

player_career_batting  <- read.csv('players_career_batting_stats.csv',header = TRUE)
player_career_batting <- subset(player_career_batting,player_career_batting$split_id == 1)
bats <- player_career_batting[,c(1:3,5,9:11,12:14,16:21,23:24,27:28,32)]
colnames(bats)[colnames(bats) == 'k'] <- 'SO'
colnames(bats)[colnames(bats) == 'd'] <- 'Dbl'
colnames(bats)[colnames(bats) == 't'] <- 'Trp'
colnames(bats)[colnames(bats) == 't'] <- 'Trp'
bats <- subset(bats, bats$league_id ==100)
bats$year <- NULL
bats$team_id <- NULL
bats$league_id <- NULL
bats <- bats %>%
  group_by(player_id) %>% 
  summarise_all(funs(sum))



player_career_fielding <- read.csv('players_career_fielding_stats.csv',header = TRUE)
fielding <- player_career_fielding[c(1:4,37)]
fielding <- subset(fielding, fielding$league_id ==100)
fielding$year <- NULL
fielding$team_id <- NULL
fielding$league_id <- NULL
fielding <- fielding %>%
  group_by(player_id) %>% 
  summarise_all(funs(sum))


player_career_pitching <- read.csv('players_career_pitching_stats.csv',header = TRUE)
player_career_pitching <- subset(player_career_pitching,player_career_pitching$split_id == 1)
pitch <- player_career_pitching[,c(1:3,5,8:12,15:17,24:26,30,32,40,44,45,48,55)]
colnames(pitch)[colnames(pitch) == 'bb'] <- 'walks'
colnames(pitch)[colnames(pitch) == 'ab'] <- 'p_ab'
colnames(pitch)[colnames(pitch) == 'tb'] <- 'p_tb'
colnames(pitch)[colnames(pitch) == 'r'] <- 'p_r'
colnames(pitch)[colnames(pitch) == 'sf'] <- 'p_sf'
pitch <- subset(pitch, pitch$league_id ==100)
pitch$year <- NULL
pitch$team_id <- NULL
pitch$league_id <- NULL
pitch <- pitch %>%
  group_by(player_id) %>% 
  summarise_all(funs(sum))

all <- merge(bats,pitch,by=c('player_id'),all=TRUE)
all <- merge(all,fielding,by=c('player_id'),all=TRUE)
all[is.na(all)] <- 0
all$war <- round(all$war.x + all$war.y,2)
all$war.x <- NULL
all$war.y <- NULL

all <- all[c(1,36)]
all.draft <- merge(all, draft, all.x = TRUE)
all_teams <- read.csv("teams.csv",header = TRUE)
all_teams$team_name <- paste(all_teams$name,all_teams$nickname)


team_lookup <- all_teams[c(1,28)]
colnames(team_lookup)[colnames(team_lookup) == 'team_id'] <- 'draft_team_id'

all.draft <- merge(all.draft, team_lookup, all.x = TRUE)
write.csv(all.draft,"~/Documents/GitHub/PBE/R_Code_Exports/PBE_Drafts.csv",row.names = FALSE)
