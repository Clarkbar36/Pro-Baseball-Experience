suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(stringr))
suppressMessages(library(RColorBrewer))

#setwd("~/Documents/GitHub/PBE/Exports/")
c.records <- read.csv("Exports/team_record.csv")
c.games_played <- max(c.records$g)
c.x_pl <- read.csv('Exports/players.csv',header = TRUE)
c.x_pl$full_name <- paste(c.x_pl$first_name,c.x_pl$last_name)
c.positions <- data.frame('position' = 1:10, 'position_name' = c('P','C','1B','2B','3B','SS','LF','CF','RF','DH'))
c.combined.players <- merge(c.x_pl,c.positions,by = "position", all.x = TRUE)
c.pl_name_lookup <- c.combined.players[c(2,99,100)]

c.player_career_batting  <- read.csv('Exports/players_career_batting_stats.csv',header = TRUE)
c.player_career_batting <- subset(c.player_career_batting,c.player_career_batting$split_id == 1)
c.bats <- c.player_career_batting[,c(1:3,5,9:11,12:14,16:21,23:24,27:28,32)]
colnames(c.bats)[colnames(c.bats) == 'k'] <- 'SO'
colnames(c.bats)[colnames(c.bats) == 'd'] <- 'Dbl'
colnames(c.bats)[colnames(c.bats) == 't'] <- 'Trp'
colnames(c.bats)[colnames(c.bats) == 't'] <- 'Trp'
c.bats$year <- NULL
c.bats$team_id <- NULL
c.bats <- subset(c.bats,c.bats$league_id != 1000)
c.bats <- c.bats %>%
  group_by(player_id, league_id) %>% 
  summarise_all(funs(sum))



c.player_career_fielding <- read.csv('Exports/players_career_fielding_stats.csv',header = TRUE)
c.fielding <- c.player_career_fielding[c(1:4,37)]
c.fielding$year <- NULL
c.fielding$team_id <- NULL
c.fielding <- subset(c.fielding,c.fielding$league_id != 1000)
c.fielding <- c.fielding %>%
  group_by(player_id, league_id) %>% 
  summarise_all(funs(sum))


c.player_career_pitching <- read.csv('Exports/players_career_pitching_stats.csv',header = TRUE)
c.player_career_pitching <- subset(c.player_career_pitching,c.player_career_pitching$split_id == 1)
c.pitch <- c.player_career_pitching[,c(1:3,5,8:12,15:17,22,24:26,30,32,40,44,45,48,55)]
colnames(c.pitch)[colnames(c.pitch) == 'bb'] <- 'walks'
colnames(c.pitch)[colnames(c.pitch) == 'ab'] <- 'p_ab'
colnames(c.pitch)[colnames(c.pitch) == 'tb'] <- 'p_tb'
colnames(c.pitch)[colnames(c.pitch) == 'r'] <- 'p_r'
colnames(c.pitch)[colnames(c.pitch) == 'sf'] <- 'p_sf'
colnames(c.pitch)[colnames(c.pitch) == 'g'] <- 'p_g'
c.pitch$year <- NULL
c.pitch$team_id <- NULL
c.pitch <- subset(c.pitch,c.pitch$league_id != 1000)
c.pitch <- c.pitch %>%
  group_by(player_id,league_id) %>% 
  summarise_all(list(sum))

c.all <- merge(c.bats,c.pitch,by=c('player_id','league_id'),all=TRUE)
c.all <- merge(c.all,c.fielding,by=c('player_id','league_id'),c.all=TRUE)
c.all[is.na(c.all)] <- 0
c.all$war <- round(c.all$war.x + c.all$war.y,2)
c.all$war.x <- NULL
c.all$war.y <- NULL

c.all <- subset(c.all,c.all$league_id<1000)

c.all$avg <- round(c.all$h/c.all$ab,3)
c.all$obp <- round((c.all$h + c.all$bb + c.all$ibb + c.all$hp)/(c.all$ab + c.all$bb + c.all$ibb + c.all$hp + c.all$sf),3)
c.all$sin <- c.all$h - c.all$Dbl - c.all$Trp - c.all$hr
c.all$rc <- round(((c.all$h+(c.all$bb + c.all$ibb + c.all$hp))*((1*c.all$sin)+(2*c.all$Dbl)+(3*c.all$Trp)+(4*c.all$hr)))/(c.all$ab+(c.all$bb + c.all$ibb + c.all$hp)),0)
c.all$tb <- (1*c.all$sin)+(2*c.all$Dbl)+(3*c.all$Trp)+(4*c.all$hr)
c.all$slg <- round(c.all$tb/c.all$ab,3)
c.all$ops <- c.all$obp + c.all$slg
c.all$iso <- c.all$slg - c.all$avg
c.all$b_babip <- round((c.all$h - c.all$hr)/(c.all$ab - c.all$SO - c.all$hr+c.all$sf),3)
c.all$bat_k_pcnt <- round(c.all$SO / c.all$pa,2)
c.all$bat_bb_pcnt <- round(c.all$bb / c.all$pa,2)
c.all$bat_k_bb_pcnt <- c.all$bat_k_pcnt - c.all$bat_bb_pcnt
c.all$era <- round(((9*c.all$er)/c.all$ip),3)
c.all$whip <- round((c.all$ha + c.all$walks)/c.all$ip,3)
c.all$p_babip <- round((c.all$ha - c.all$hra)/(c.all$p_ab - c.all$k - c.all$hra+c.all$p_sf),3)
c.all$fip <- round(((13*c.all$hra+3*c.all$walks-2*c.all$k)/c.all$ip)+3.1,2)
c.all$HR_per_9 <- round(9 * (c.all$hra/c.all$ip),2)
c.all$R_per_9 <- round(9 * (c.all$p_r/c.all$ip),2)
c.all$H_per_9 <- round(9 * (c.all$ha/c.all$ip),2)
c.all$K_per_9 <- round(9 * (c.all$k/c.all$ip),2)
c.all$BB_per_9 <- round(9 * (c.all$walks/c.all$ip),2)
c.all$pit_k_pcnt <- round(c.all$k / c.all$p_ab,2)
c.all$pit_bb_pcnt <- round(c.all$walks / c.all$p_ab,2)
c.all$pit_k_bb_pcnt <- c.all$pit_k_pcnt - c.all$pit_bb_pcnt
c.all$win_percent <- round(c.all$w / (c.all$w + c.all$l),3)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

c.all <- merge(c.all, c.pl_name_lookup, by = 'player_id')
c.all$full_name <- str_replace_all(c.all$full_name,"-","")

c.leagues <- read.csv("Exports/leagues.csv", header = TRUE)
c.leagues <- c.leagues[c(1,3)]
colnames(c.leagues)[colnames(c.leagues) == 'abbr'] <- 'league_abbr'
c.all <- merge(c.all,c.leagues,all.x = TRUE)
c.all[is.nan(c.all)] <- 0
c.all$zr[is.na(c.all$zr)] <- 0

c.all$winloss <- paste(c.all$w,"-",c.all$l)
c.all$name_pos <- paste(c.all$full_name, "-",c.all$position_name)

c.all$url <- paste("http://www.pbesim.com/players/player_",c.all$player_id,".html", sep = "")

c.all$pitch_hit <- ifelse(c.all$position_name != 'P','Hitter','Pitcher')

c.all.hit <- subset(c.all,c.all$pitch_hit == 'Hitter')
c.all.pitch <- subset(c.all, c.all$pitch_hit == "Pitcher")

c.all.hit <- c.all.hit[-c(19:36,51:63,67)]
colnames(c.all.hit)[colnames(c.all.hit) == 'ab'] <- 'At Bats'
colnames(c.all.hit)[colnames(c.all.hit) == 'h'] <- 'Hits'
colnames(c.all.hit)[colnames(c.all.hit) == 'SO'] <- 'Strikeouts'
colnames(c.all.hit)[colnames(c.all.hit) == 'pa'] <- 'Plate Apperances'
colnames(c.all.hit)[colnames(c.all.hit) == 'g'] <- 'Games'
colnames(c.all.hit)[colnames(c.all.hit) == 'Dbl'] <- 'Doubles'
colnames(c.all.hit)[colnames(c.all.hit) == 'Trp'] <- 'Triples'
colnames(c.all.hit)[colnames(c.all.hit) == 'hr'] <- 'Homeruns'
colnames(c.all.hit)[colnames(c.all.hit) == 'r'] <- 'Runs'
colnames(c.all.hit)[colnames(c.all.hit) == 'rbi'] <- 'RBI'
colnames(c.all.hit)[colnames(c.all.hit) == 'sb'] <- 'Stolen Bases'
colnames(c.all.hit)[colnames(c.all.hit) == 'bb'] <- 'Walks'
colnames(c.all.hit)[colnames(c.all.hit) == 'zr'] <- 'Zone Rating'
colnames(c.all.hit)[colnames(c.all.hit) == 'war'] <- 'WAR'
colnames(c.all.hit)[colnames(c.all.hit) == 'avg'] <- 'Average'
colnames(c.all.hit)[colnames(c.all.hit) == 'obp'] <- 'OBP'
colnames(c.all.hit)[colnames(c.all.hit) == 'sin'] <- 'Single'
colnames(c.all.hit)[colnames(c.all.hit) == 'rc'] <- 'Runs Created'
colnames(c.all.hit)[colnames(c.all.hit) == 'tb'] <- 'Total Bases'
colnames(c.all.hit)[colnames(c.all.hit) == 'slg'] <- 'SLG'
colnames(c.all.hit)[colnames(c.all.hit) == 'ops'] <- 'OPS'
colnames(c.all.hit)[colnames(c.all.hit) == 'iso'] <- 'ISO'
colnames(c.all.hit)[colnames(c.all.hit) == 'b_babip'] <- 'BABIP'
colnames(c.all.hit)[colnames(c.all.hit) == 'bat_k_pcnt'] <- 'K Percent'
colnames(c.all.hit)[colnames(c.all.hit) == 'bat_bb_pcnt'] <- 'BB Percent'
colnames(c.all.hit)[colnames(c.all.hit) == 'bat_k_bb_pcnt'] <- 'K-BB Percent'

c.all.pitch <- c.all.pitch[-c(3:18,37,39:50)]
colnames(c.all.pitch)[colnames(c.all.pitch) =='ip']<-'Innings Pitched'
colnames(c.all.pitch)[colnames(c.all.pitch) =='ha']<-'Hits Allowed'
colnames(c.all.pitch)[colnames(c.all.pitch) =='k']<-'Strikeouts'
colnames(c.all.pitch)[colnames(c.all.pitch) =='walks']<-'Walks'
colnames(c.all.pitch)[colnames(c.all.pitch) =='p_r']<-'Runs Allowed'
colnames(c.all.pitch)[colnames(c.all.pitch) =='er']<-'Earned Runs'
colnames(c.all.pitch)[colnames(c.all.pitch) =='w']<-'Wins'
colnames(c.all.pitch)[colnames(c.all.pitch) =='l']<-'Loss'
colnames(c.all.pitch)[colnames(c.all.pitch) =='s']<-'Saves'
colnames(c.all.pitch)[colnames(c.all.pitch) =='hra']<-'Homeruns Allowed'
colnames(c.all.pitch)[colnames(c.all.pitch) =='qs']<-'Quality Start'
colnames(c.all.pitch)[colnames(c.all.pitch) =='cg']<-'Complete Game'
colnames(c.all.pitch)[colnames(c.all.pitch) =='sho']<-'Shutout'
colnames(c.all.pitch)[colnames(c.all.pitch) =='hld']<-'Hold'
colnames(c.all.pitch)[colnames(c.all.pitch) =='war']<-'WAR'
colnames(c.all.pitch)[colnames(c.all.pitch) =='era']<-'ERA'
colnames(c.all.pitch)[colnames(c.all.pitch) =='whip']<-'WHIP'
colnames(c.all.pitch)[colnames(c.all.pitch) =='p_babip']<-'BABIP'
colnames(c.all.pitch)[colnames(c.all.pitch) =='fip']<-'FIP'
colnames(c.all.pitch)[colnames(c.all.pitch) =='HR_per_9']<-'HR per 9'
colnames(c.all.pitch)[colnames(c.all.pitch) =='R_per_9']<-'R per 9'
colnames(c.all.pitch)[colnames(c.all.pitch) =='H_per_9']<-'Hits per 9'
colnames(c.all.pitch)[colnames(c.all.pitch) =='K_per_9']<-'Ks per 9'
colnames(c.all.pitch)[colnames(c.all.pitch) =='BB_per_9']<-'BB per 9'
colnames(c.all.pitch)[colnames(c.all.pitch) =='pit_k_pcnt']<-'K percent'
colnames(c.all.pitch)[colnames(c.all.pitch) =='pit_bb_pcnt']<-'BB percent'
colnames(c.all.pitch)[colnames(c.all.pitch) =='pit_k_bb_pcnt']<-'K-BB percent'
colnames(c.all.pitch)[colnames(c.all.pitch) =='winloss']<-'Win-Loss'
colnames(c.all.pitch)[colnames(c.all.pitch) =='win_percent']<-'Win Percent'


#write.csv(all,'Documents/GitHub/PBE/R_Code_Exports/All_Time_Stats.csv', row.names = FALSE)
