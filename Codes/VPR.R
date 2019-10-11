suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(stringr))
suppressMessages(library(RColorBrewer))
setwd("~/Documents/GitHub/PBE/")

x_pl <- read.csv('Exports/players.csv',header = TRUE)
x_pl$full_name <- paste(x_pl$first_name,x_pl$last_name)
positions <- data.frame('position' = 1:10, 'position_name' = c('P','C','1B','2B','3B','SS','LF','CF','RF','DH'))
combined.players <- merge(x_pl,positions,by = "position", all.x = TRUE)
pl_name_lookup <- combined.players %>%
  select(player_id, team_id, league_id, full_name, position_name) %>%
  filter(league_id != 0)

leagues <- read.csv("Exports/leagues.csv", header = TRUE)
leagues <- leagues %>%
  select(league_id, abbr) %>%
  rename(league_abbr = abbr)
pl_name_lookup <- merge(pl_name_lookup,leagues,all.x = TRUE)

all_teams <- read.csv("Exports/teams.csv",header = TRUE)
all_teams$team_name <- paste(all_teams$name,all_teams$nickname)
team_lookup <- all_teams %>%
  select(team_id, abbr, team_name) %>%
  rename(team_abbr = abbr)
pl_name_lookup <- merge(pl_name_lookup, team_lookup, all.x = TRUE)

pitch_war <- read.csv('Exports/players_career_pitching_stats.csv')
pitch_war <- pitch_war %>% 
  filter(year == 2030, split_id == 1) %>%
  select(player_id, war)

pitching_games <- read.csv('Exports/players_game_pitching_stats.csv')

current_pitching <- pitching_games %>% 
  filter(split_id == 1, gs == 1, league_id != 1000) %>%
  mutate(ERA = round(9*(er/ip),2), 
         ES = ifelse(ip >= 6 & ERA <= 3,1,0),
         PS = ifelse(ERA > 4.50,1,0), 
         NS = ifelse(ES == 0 & PS == 0, 1,0))

volatility <- current_pitching %>%
  group_by(player_id) %>%
  summarise(Games_Started = sum(gs),
            IPS = round(sum(ip)/sum(gs),2),
            ES = sum(ES),
            NS = sum(NS),
            PS = sum(PS),
            volatility_percent = round((ES+PS)/Games_Started,2),
            VPR = round(ES/PS,2))

volatility$VPR <- sapply(volatility$VPR, function(x) ifelse(is.infinite(x), 999, x))

volatility <- merge(volatility, pl_name_lookup)
volatility <- merge(volatility, pitch_war, all.x = TRUE)

volatility <- volatility %>%
  select(full_name, team_name, team_abbr, league_abbr, Games_Started, IPS, ES, NS, PS, VPR, volatility_percent, war) %>%
  arrange(league_abbr, -VPR, -volatility_percent) %>%
  rename(Name = full_name, Team = team_name, `Team Abbr` = team_abbr, League = league_abbr, `Games Started` = Games_Started, `Innings per Start` = IPS, `Excellent Starts` = ES, `Neutral Starts` = NS, `Poor Starts` = PS, `Volatility %` = volatility_percent, WAR = war)

write.csv(volatility, 'R_Code_Exports/VPR.csv', row.names = FALSE)

tm_volatility <- current_pitching %>%
  group_by(team_id, league_id) %>%
  summarise(Games_Started = sum(gs),
            IPS = round(sum(ip)/sum(gs),2),
            ES = sum(ES),
            NS = sum(NS),
            PS = sum(PS),
            volatility_percent = round((ES+PS)/Games_Started,2),
            VPR = round(ES/PS,2))
tm_volatility <- merge(tm_volatility,team_lookup)
tm_volatility <- merge(tm_volatility,leagues)

tm_volatility <- tm_volatility %>%
  select(team_name, team_abbr, `league abbr`, Games_Started, IPS, ES, NS, PS, VPR, volatility_percent) %>%
  arrange(`league abbr`, -VPR, -volatility_percent) %>%
  rename(Team = team_name, `Team Abbr` = team_abbr, League = `league abbr`, `Games Started` = Games_Started, `Innings per Start` = IPS, `Excellent Starts` = ES, `Neutral Starts` = NS, `Poor Starts` = PS, `Volatility %` = volatility_percent)

write.csv(tm_volatility, 'R_Code_Exports/tm_VPR.csv', row.names = FALSE)
