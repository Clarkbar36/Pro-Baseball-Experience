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


names <- colnames(pl_name_lookup) 
hitter_lookup <-  pl_name_lookup 
colnames(hitter_lookup) <- paste("Hitter", names, sep = "_")

pitcher_lookup <-  pl_name_lookup 
colnames(pitcher_lookup) <- paste("Pitcher", names, sep = "_")
pitcher_lookup <- pitcher_lookup %>% select(-Pitcher_position_name)

at_bats <- read.csv("Exports/players_at_bat_batting_stats.csv")
at_bats <- at_bats %>% rename(Hitter_player_id = player_id, Hitter_team_id = team_id, Pitcher_player_id = opponent_player_id)
at_bats <- merge(at_bats, hitter_lookup, all.x = TRUE)
at_bats <- merge(at_bats, pitcher_lookup, all.x = TRUE)

results <- read.csv("results.csv")
at_bats <- merge(at_bats, results, all.x = TRUE)
at_bats <- at_bats %>% drop_na()
at_bats <- at_bats %>% mutate(RISP = ifelse(base2 == 1 | base3 == 1, 1,0), at_bat = 1)
at_bats$RISP <- as.integer(at_bats$RISP)
at_bats$at_bat <- as.integer(at_bats$at_bat)
games <- read.csv('Exports/games.csv', header = TRUE, sep = ',')
games <- subset(games,games$played == 1 & games$game_type == 0 & games$league_id != 1000)
games <- games %>% select(game_id) %>% distinct()

at_bats <- at_bats %>% filter(game_id %in% games$game_id)

hitter <- "Nate Piazza"
pitcher <- "Henry Chadwick"

pv_ab_filtered <- at_bats %>% filter(Hitter_full_name == hitter & Pitcher_full_name == pitcher) %>%
  group_by(Hitter_full_name, Hitter_position_name, Hitter_league_abbr, Hitter_team_abbr, Hitter_team_name, 
           Pitcher_full_name, Pitcher_league_abbr, Pitcher_team_abbr, Pitcher_team_name) %>%
  summarise(ABs = sum(at_bat), Hits = sum(hit), Singles = sum(sngl), Doubles = sum(dbl), Triples = sum(trpl), 
            Homeruns = sum(hr), Walks = sum(bb), Stolen_Bases = sum(sb), RBIs = sum(rbi), Extra_Base_Hits = sum(ebh), 
            Strikeouts = sum(k), Sacrifice_Fly = sum(sac), Batting_Average = round(Hits/ABs,3), 
            On_Base_Percentage = round((Hits+Walks)/(ABs+Walks+Sacrifice_Fly),3), Groundouts = sum(go), Flyouts = sum(fo))



pv_player_singles <- sample_n(singles, pv_ab_filtered$Singles[1])
pv_player_doubles <- sample_n(doubles, pv_ab_filtered$Doubles[1])
pv_player_triples <- sample_n(triples, pv_ab_filtered$Triples[1])
pv_player_homeruns <- sample_n(homeruns,pv_ab_filtered$Homeruns[1])
pv_player_flyouts <- sample_n(flyouts,pv_ab_filtered$Flyouts[1])
pv_player_groundouts <- sample_n(groundouts,pv_ab_filtered$Groundouts[1])

plot_hits <- bind_rows(pv_player_homeruns,pv_player_singles, pv_player_doubles, pv_player_triples,
                       pv_player_flyouts, pv_player_groundouts)

hit_theme <- plot_hits %>% select(type,shape,color) %>% distinct() %>% arrange(type) %>% mutate(size = 2)
hit_shapes <- as.integer(hit_theme$shape)
hit_colors <- as.character(hit_theme$color)
hit_size <- as.integer(hit_theme$size)

plot_hits %>%
  ggplot(aes(x=plt_x, y=plt_y, color = type, shape = type, size = type )) +
  ggtitle(paste(pitcher, "vs", hitter, sep = " ")) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.title=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_shape_manual(values = hit_shapes) +
  scale_color_manual(values = hit_colors) +
  scale_size_manual(values = hit_size) +
  geom_mlb_stadium(stadium_segments = "all") +
  coord_fixed() +
  geom_spraychart()
