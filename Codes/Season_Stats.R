suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(stringr))
suppressMessages(library(RColorBrewer))
#setwd("~/Documents/GitHub/PBE/")
s.records <- read.csv("Exports/team_record.csv")
s.games_played <- max(s.records$g)
s.x_pl <- read.csv('Exports/players.csv',header = TRUE)
s.x_pl$full_name <- paste(s.x_pl$first_name,s.x_pl$last_name)
s.positions <- data.frame('position' = 1:10, 'position_name' = c('P','C','1B','2B','3B','SS','LF','CF','RF','DH'))
s.combined.players <- merge(s.x_pl,s.positions,by = "position", all.x = TRUE)
s.pl_name_lookup <- s.combined.players[c(2,5,99,100)]

s.player_career_batting  <- read.csv('Exports/players_career_batting_stats.csv',header = TRUE)
s.player_career_batting <- subset(s.player_career_batting,s.player_career_batting$split_id == 1)
s.bats <- s.player_career_batting[,c(1:3,5,9:11,12:14,16:21,23:25,27:28,32)]
colnames(s.bats)[colnames(s.bats) == 'k'] <- 'SO'
colnames(s.bats)[colnames(s.bats) == 'd'] <- 'Dbl'
colnames(s.bats)[colnames(s.bats) == 't'] <- 'Trp'
#bats <- subset(bats, bats$ab >=100)


s.player_career_fielding <- read.csv('Exports/players_career_fielding_stats.csv',header = TRUE)
s.fielding <- s.player_career_fielding[c(1:4,37)]
s.fielding <- s.fielding %>%
  group_by(player_id, year, team_id, league_id) %>% 
  summarise_all(funs(sum))

 
s.player_career_pitching <- read.csv('Exports/players_career_pitching_stats.csv',header = TRUE)
s.player_career_pitching <- subset(s.player_career_pitching,s.player_career_pitching$split_id == 1)
s.pitch <- s.player_career_pitching[,c(1:3,5,8:12,15:17,22,24:26,30,32,40,44,45,48,55)]
colnames(s.pitch)[colnames(s.pitch) == 'bb'] <- 'walks'
colnames(s.pitch)[colnames(s.pitch) == 'ab'] <- 'p_ab'
colnames(s.pitch)[colnames(s.pitch) == 'tb'] <- 'p_tb'
colnames(s.pitch)[colnames(s.pitch) == 'r'] <- 'p_r'
colnames(s.pitch)[colnames(s.pitch) == 'sf'] <- 'p_sf'
colnames(s.pitch)[colnames(s.pitch) == 'g'] <- 'p_g'
#pitch <- subset(pitch,pitch$ip>=10)

s.all <- merge(s.bats,s.pitch,by=c('player_id','year','team_id','league_id'),all=TRUE)
s.all <- merge(s.all,s.fielding,by=c('player_id','year','team_id','league_id'),all=TRUE)
s.all[is.na(s.all)] <- 0
s.all$war <- round(s.all$war.x + s.all$war.y,2)
s.all$war.x <- NULL
s.all$war.y <- NULL
s.detail_all <- s.all
s.all <- s.all %>%
  group_by(player_id, year, league_id) %>% 
  summarise(team_id = max(team_id),
            ab = sum(ab),
            h = sum(h),
            SO = sum(SO),
            pa = sum(pa),
            pitches_seen = sum(pitches_seen),
            g = sum(g),
            Dbl = sum(Dbl),
            Trp = sum(Trp),
            hr = sum(hr),
            r = sum(r),
            rbi = sum(rbi),
            sb = sum(sb),
            bb = sum(bb),
            ibb = sum(ibb),
            sf = sum(sf),
            gdp = sum(gdp),
            hp = sum(hp),
            ip = sum(ip),
            p_ab = sum(p_ab),
            p_tb = sum(p_tb),
            ha = sum(ha),
            k = sum(k),
            walks = sum(walks),
            p_r = sum(p_r),
            er = sum(er),
            w = sum(w),
            l = sum(l),
            s = sum(s),
            p_sf = sum(p_sf),
            p_g = sum(p_g),
            hra = sum(hra),
            qs = sum(qs),
            cg = sum(cg),
            sho = sum(sho),
            hld = sum(hld),
            zr = sum(zr),
            war = sum(war))


#all <- subset(all,all$league_id == 100)
s.max_year <- max(s.all$year)
s.all$games <- 162
s.all$games_remaining <- ifelse(s.all$year == s.max_year, s.all$games - s.games_played, 0)
s.all$pa_pace <- round(ifelse(s.all$year == s.max_year, ((s.all$pa / s.all$g) * s.all$games_remaining) + s.all$pa, s.all$pa),0)
s.all$avg <- round(s.all$h/s.all$ab,3)
s.all$obp <- round((s.all$h + s.all$bb + s.all$ibb + s.all$hp)/(s.all$ab + s.all$bb + s.all$ibb + s.all$hp + s.all$sf),3)
s.all$sin <- s.all$h - s.all$Dbl - s.all$Trp - s.all$hr
s.all$rc <- round(((s.all$h+(s.all$bb + s.all$ibb + s.all$hp))*((1*s.all$sin)+(2*s.all$Dbl)+(3*s.all$Trp)+(4*s.all$hr)))/(s.all$ab+(s.all$bb + s.all$ibb + s.all$hp)),0)
s.all$tb <- (1*s.all$sin)+(2*s.all$Dbl)+(3*s.all$Trp)+(4*s.all$hr)
s.all$slg <- round(s.all$tb/s.all$ab,3)
s.all$ops <- s.all$obp + s.all$slg
s.all$iso <- s.all$slg - s.all$avg
s.all$b_babip <- round((s.all$h - s.all$hr)/(s.all$ab - s.all$SO - s.all$hr+s.all$sf),3)
s.all$bat_k_pcnt <- round(s.all$SO / s.all$pa,2)
s.all$bat_bb_pcnt <- round(s.all$bb / s.all$pa,2)
s.all$bat_k_bb_pcnt <- s.all$bat_k_pcnt - s.all$bat_bb_pcnt
s.all$era <- round(((9*s.all$er)/s.all$ip),3)
s.all$whip <- round((s.all$ha + s.all$walks)/s.all$ip,3)
s.all$p_babip <- round((s.all$ha - s.all$hra)/(s.all$p_ab - s.all$k - s.all$hra+s.all$p_sf),3)
s.all$fip <- round(((13*s.all$hra+3*s.all$walks-2*s.all$k)/s.all$ip)+3.1,2)
s.all$HR_per_9 <- round(9 * (s.all$hra/s.all$ip),2)
s.all$R_per_9 <- round(9 * (s.all$p_r/s.all$ip),2)
s.all$H_per_9 <- round(9 * (s.all$ha/s.all$ip),2)
s.all$K_per_9 <- round(9 * (s.all$k/s.all$ip),2)
s.all$BB_per_9 <- round(9 * (s.all$walks/s.all$ip),2)
s.all$pit_k_pcnt <- round(s.all$k / s.all$p_ab,2)
s.all$pit_bb_pcnt <- round(s.all$walks / s.all$p_ab,2)
s.all$pit_k_bb_pcnt <- s.all$pit_k_pcnt - s.all$pit_bb_pcnt
s.all$win_percent <- round(s.all$w / (s.all$w + s.all$l),3)


is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

s.all <- merge(s.all, s.pl_name_lookup, by = 'player_id')
s.all$full_name <- str_replace_all(s.all$full_name,"-","")

s.all <- subset(s.all,s.all$league_id<1000)

s.leagues <- read.csv("Exports/leagues.csv", header = TRUE)
s.leagues <- s.leagues[c(1,3)]
colnames(s.leagues)[colnames(s.leagues) == 'abbr'] <- 'league_abbr'
s.all <- merge(s.all,s.leagues,all.x = TRUE)


s.all_teams <- read.csv("Exports/teams.csv",header = TRUE)
s.all_teams$team_name <- paste(s.all_teams$name,s.all_teams$nickname)
s.team_lookup <- s.all_teams[c(1,3,28)]
s.all <- merge(s.all, s.team_lookup, all.x = TRUE)

s.all$team_name <- ifelse(is.na(s.all$team_name), "Free Agent",s.all$team_name) 

s.all[is.nan(s.all)] <- 0
s.all$zr[is.na(s.all$zr)] <- 0

s.all$winloss <- paste(s.all$w,"-",s.all$l)
s.all$team_name_pos <- paste(s.all$abbr,"-",s.all$full_name, "-",s.all$position_name)

s.all$url <- paste("http://www.pbesim.com/players/player_",s.all$player_id,".html", sep = "")

s.all$pitch_hit <- ifelse(s.all$position_name != 'P','Hitter','Pitcher')

s.all.hit <- subset(s.all,s.all$pitch_hit == 'Hitter')
s.all.hit$role <- NULL
s.all.pitch <- subset(s.all, s.all$pitch_hit == "Pitcher")


s.all.hit <- s.all.hit[-c(21:39,57:69,75)]

colnames(s.all.hit)[colnames(s.all.hit) == 'ab'] <- 'At Bats'
colnames(s.all.hit)[colnames(s.all.hit) == 'h'] <- 'Hits'
colnames(s.all.hit)[colnames(s.all.hit) == 'SO'] <- 'Strikeouts'
colnames(s.all.hit)[colnames(s.all.hit) == 'pa'] <- 'Plate Appearances'
colnames(s.all.hit)[colnames(s.all.hit) == 'g'] <- 'Games'
colnames(s.all.hit)[colnames(s.all.hit) == 'Dbl'] <- 'Doubles'
colnames(s.all.hit)[colnames(s.all.hit) == 'Trp'] <- 'Triples'
colnames(s.all.hit)[colnames(s.all.hit) == 'hr'] <- 'Homeruns'
colnames(s.all.hit)[colnames(s.all.hit) == 'r'] <- 'Runs'
colnames(s.all.hit)[colnames(s.all.hit) == 'rbi'] <- 'RBI'
colnames(s.all.hit)[colnames(s.all.hit) == 'sb'] <- 'Stolen Bases'
colnames(s.all.hit)[colnames(s.all.hit) == 'bb'] <- 'Walks'
colnames(s.all.hit)[colnames(s.all.hit) == 'zr'] <- 'Zone Rating'
colnames(s.all.hit)[colnames(s.all.hit) == 'war'] <- 'WAR'
colnames(s.all.hit)[colnames(s.all.hit) == 'avg'] <- 'Average'
colnames(s.all.hit)[colnames(s.all.hit) == 'obp'] <- 'OBP'
colnames(s.all.hit)[colnames(s.all.hit) == 'sin'] <- 'Single'
colnames(s.all.hit)[colnames(s.all.hit) == 'rc'] <- 'Runs Created'
colnames(s.all.hit)[colnames(s.all.hit) == 'tb'] <- 'Total Bases'
colnames(s.all.hit)[colnames(s.all.hit) == 'slg'] <- 'SLG'
colnames(s.all.hit)[colnames(s.all.hit) == 'ops'] <- 'OPS'
colnames(s.all.hit)[colnames(s.all.hit) == 'iso'] <- 'ISO'
colnames(s.all.hit)[colnames(s.all.hit) == 'b_babip'] <- 'BABIP'
colnames(s.all.hit)[colnames(s.all.hit) == 'bat_k_pcnt'] <- 'K Percent'
colnames(s.all.hit)[colnames(s.all.hit) == 'bat_bb_pcnt'] <- 'BB Percent'
colnames(s.all.hit)[colnames(s.all.hit) == 'bat_k_bb_pcnt'] <- 'K-BB Percent'
colnames(s.all.hit)[colnames(s.all.hit) == 'position_name'] <- 'Position'
colnames(s.all.hit)[colnames(s.all.hit) == 'gdp'] <- 'GDP'

s.all.h.cnames <- colnames(s.all.hit[c(5:8,10:17,20:22,26:37)])
s.all.h.cnames <- sort(s.all.h.cnames)

s.all.pitch$pitcher_position <- ifelse(s.all.pitch$s + s.all.pitch$hld >0,"RP","SP")
s.all.pitch$role <- NULL
s.all.pitch$team_name_pos <- paste(s.all.pitch$abbr,"-", s.all.pitch$full_name, "-", s.all.pitch$pitcher_position)
s.all.pitch <- s.all.pitch[-c(5:21,40,42:56)]
colnames(s.all.pitch)[colnames(s.all.pitch) =='ip']<-'Innings Pitched'
colnames(s.all.pitch)[colnames(s.all.pitch) =='ha']<-'Hits Allowed'
colnames(s.all.pitch)[colnames(s.all.pitch) =='k']<-'Strikeouts'
colnames(s.all.pitch)[colnames(s.all.pitch) =='walks']<-'Walks'
colnames(s.all.pitch)[colnames(s.all.pitch) =='p_r']<-'Runs Allowed'
colnames(s.all.pitch)[colnames(s.all.pitch) =='er']<-'Earned Runs'
colnames(s.all.pitch)[colnames(s.all.pitch) =='w']<-'Wins'
colnames(s.all.pitch)[colnames(s.all.pitch) =='l']<-'Loss'
colnames(s.all.pitch)[colnames(s.all.pitch) =='s']<-'Saves'
colnames(s.all.pitch)[colnames(s.all.pitch) =='hra']<-'Homeruns Allowed'
colnames(s.all.pitch)[colnames(s.all.pitch) =='qs']<-'Quality Start'
colnames(s.all.pitch)[colnames(s.all.pitch) =='cg']<-'Complete Game'
colnames(s.all.pitch)[colnames(s.all.pitch) =='sho']<-'Shutout'
colnames(s.all.pitch)[colnames(s.all.pitch) =='hld']<-'Hold'
colnames(s.all.pitch)[colnames(s.all.pitch) =='war']<-'WAR'
colnames(s.all.pitch)[colnames(s.all.pitch) =='era']<-'ERA'
colnames(s.all.pitch)[colnames(s.all.pitch) =='whip']<-'WHIP'
colnames(s.all.pitch)[colnames(s.all.pitch) =='p_babip']<-'BABIP'
colnames(s.all.pitch)[colnames(s.all.pitch) =='fip']<-'FIP'
colnames(s.all.pitch)[colnames(s.all.pitch) =='HR_per_9']<-'HR per 9'
colnames(s.all.pitch)[colnames(s.all.pitch) =='R_per_9']<-'R per 9'
colnames(s.all.pitch)[colnames(s.all.pitch) =='H_per_9']<-'Hits per 9'
colnames(s.all.pitch)[colnames(s.all.pitch) =='K_per_9']<-'Ks per 9'
colnames(s.all.pitch)[colnames(s.all.pitch) =='BB_per_9']<-'BB per 9'
colnames(s.all.pitch)[colnames(s.all.pitch) =='pit_k_pcnt']<-'K percent'
colnames(s.all.pitch)[colnames(s.all.pitch) =='pit_bb_pcnt']<-'BB percent'
colnames(s.all.pitch)[colnames(s.all.pitch) =='pit_k_bb_pcnt']<-'K-BB percent'
colnames(s.all.pitch)[colnames(s.all.pitch) =='winloss']<-'Win-Loss'
colnames(s.all.pitch)[colnames(s.all.pitch) =='win_percent']<-'Win Percent'
colnames(s.all.pitch)[colnames(s.all.pitch) =='pitcher_position']<-'Position'

s.all.p.cnames <- colnames(s.all.pitch[c(5,8:15,18:36)])
s.all.p.cnames <- sort(s.all.p.cnames)

#write.csv(all,'~/Documents/GitHub/PBE/R_Code_Exports/Season_Stats.csv', row.names = FALSE)
