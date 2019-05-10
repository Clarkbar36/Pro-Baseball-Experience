suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(stringr))
suppressMessages(library(RColorBrewer))
suppressMessages(library(Hmisc))

setwd("~/Documents/GitHub/PBE/Exports/")

tm <- read.csv("teams.csv")
tm$`Team Name` <- paste(tm$name,tm$nickname)
tm <- tm[c(1,3,8,28)]
colnames(tm)[colnames(tm) == 'abbr'] <- 'Team Abbr'

lg <- read.csv("leagues.csv")
lg <- lg[c(1,3)]
colnames(lg)[colnames(lg) == 'abbr'] <- 'League'

stdp <- read.csv("team_record.csv")
stdp <- stdp[c(1,5,6)]
stdp <- subset(stdp,stdp$team_id %nin% c(7,8,12,13,14,15,22,23))
colnames(stdp)[colnames(stdp) == 'pos'] <- 'All - Division Standing'

lkup <- merge(tm,lg, all.x = TRUE)
lkup <- merge(lkup, stdp)
lkup <- lkup %>% 
  group_by(league_id) %>% 
  mutate(`All - League Standing` = round(rank(-pct),0)) %>% 
  ungroup
lkup$pct <- NULL

tm_pitch <- read.csv("team_pitching_stats.csv")
tm_pitch <- subset(tm_pitch, tm_pitch$split_id == 1)
tm_pitch$Walks <- tm_pitch$bb + tm_pitch$iw + tm_pitch$hp
tm_pitch <- tm_pitch[-c(2,4:6,13,19,20,21,25:29,31:37,44:45,56,58:63,65,66)]
{
  colnames(tm_pitch)[colnames(tm_pitch) =='ip'] <- 'Pitching - Innings Pitched'
  colnames(tm_pitch)[colnames(tm_pitch) =='bf'] <- 'Pitching - Batters Faced'
  colnames(tm_pitch)[colnames(tm_pitch) =='tb'] <- 'Pitching - Total Bases'
  colnames(tm_pitch)[colnames(tm_pitch) =='ha'] <- 'Pitching - Hits Allowed'
  colnames(tm_pitch)[colnames(tm_pitch) =='k'] <- 'Pitching - Strikeouts'
  colnames(tm_pitch)[colnames(tm_pitch) =='rs'] <- 'Pitching - Run Support'
  colnames(tm_pitch)[colnames(tm_pitch) =='r'] <- 'Pitching - Runs'
  colnames(tm_pitch)[colnames(tm_pitch) =='er'] <- 'Pitching - Earned Runs'
  colnames(tm_pitch)[colnames(tm_pitch) =='gb'] <- 'Pitching - Ground Balls'
  colnames(tm_pitch)[colnames(tm_pitch) =='fb'] <- 'Pitching - Fly Balls'
  colnames(tm_pitch)[colnames(tm_pitch) =='pi'] <- 'Pitching - Production Index'
  colnames(tm_pitch)[colnames(tm_pitch) =='w'] <- 'Pitching - Wins'
  colnames(tm_pitch)[colnames(tm_pitch) =='l'] <- 'Pitching - Loss'
  colnames(tm_pitch)[colnames(tm_pitch) =='s'] <- 'Pitching - Saves'
  colnames(tm_pitch)[colnames(tm_pitch) =='hra'] <- 'Pitching - Homeruns Allowed'
  colnames(tm_pitch)[colnames(tm_pitch) =='qs'] <- 'Pitching - Qualty Starts'
  colnames(tm_pitch)[colnames(tm_pitch) =='svo'] <- 'Pitching - Save Opportunities'
  colnames(tm_pitch)[colnames(tm_pitch) =='bs'] <- 'Pitching - Blown Saves'
  colnames(tm_pitch)[colnames(tm_pitch) =='ra'] <- 'Pitching - Run Average'
  colnames(tm_pitch)[colnames(tm_pitch) =='cg'] <- 'Pitching - Complete Game'
  colnames(tm_pitch)[colnames(tm_pitch) =='sho'] <- 'Pitching - Shutout'
  colnames(tm_pitch)[colnames(tm_pitch) =='hld'] <- 'Pitching - Holds'
  colnames(tm_pitch)[colnames(tm_pitch) =='r9'] <- 'Pitching - Runs per 9'
  colnames(tm_pitch)[colnames(tm_pitch) =='avg'] <- 'Pitching - Opponent Average'
  colnames(tm_pitch)[colnames(tm_pitch) =='obp'] <- 'Pitching - Opponent OBP'
  colnames(tm_pitch)[colnames(tm_pitch) =='slg'] <- 'Pitching - Opponent SLG'
  colnames(tm_pitch)[colnames(tm_pitch) =='ops'] <- 'Pitching - Opponent OPS'
  colnames(tm_pitch)[colnames(tm_pitch) =='h9'] <- 'Pitching - Hits per 9'
  colnames(tm_pitch)[colnames(tm_pitch) =='k9'] <- 'Pitching - Ks per 9'
  colnames(tm_pitch)[colnames(tm_pitch) =='hr9'] <- 'Pitching - Homerun per 9'
  colnames(tm_pitch)[colnames(tm_pitch) =='bb9'] <- 'Pitching - Walks per 9'
  colnames(tm_pitch)[colnames(tm_pitch) =='fip'] <- 'Pitching - FIP'
  colnames(tm_pitch)[colnames(tm_pitch) =='era'] <- 'Pitching - ERA'
  colnames(tm_pitch)[colnames(tm_pitch) =='whip'] <- 'Pitching - WHIP'
  colnames(tm_pitch)[colnames(tm_pitch) =='gbfbp'] <- 'Pitching - Groundball-Flyball Percent'
  colnames(tm_pitch)[colnames(tm_pitch) =='kbb'] <- 'Pitching - KBB'
  colnames(tm_pitch)[colnames(tm_pitch) =='babip'] <- 'Pitching - BABIP'
  colnames(tm_pitch)[colnames(tm_pitch) =='Walks'] <- 'Pitching - Walks'
}


tm_field <- read.csv("team_fielding_stats_stats.csv")
tm_field <- subset(tm_field, tm_field$league_id >0)
tm_field <- tm_field[-c(2,4:8,15:20,23,24)]
{
  colnames(tm_field)[colnames(tm_field) =='tc'] <- 'Fielding - Total Chances'
  colnames(tm_field)[colnames(tm_field) =='a'] <- 'Fielding - Assists'
  colnames(tm_field)[colnames(tm_field) =='po'] <- 'Fielding - Put-Outs'
  colnames(tm_field)[colnames(tm_field) =='e'] <- 'Fielding - Errors'
  colnames(tm_field)[colnames(tm_field) =='dp'] <- 'Fielding - Double Plays'
  colnames(tm_field)[colnames(tm_field) =='tp'] <- 'Fielding - Triple Plays'
  colnames(tm_field)[colnames(tm_field) =='pct'] <- 'Fielding - Put-Out Pct'
  colnames(tm_field)[colnames(tm_field) =='range'] <- 'Fielding - Range'
}


tm_bat <- read.csv("team_batting_stats.csv")
tm_bat <- subset(tm_bat, tm_bat$league_id %in% c(100,101))
tm_bat$batter.walks <- tm_bat$bb + tm_bat$ibb + tm_bat$hp
tm_bat <- tm_bat[-c(2,4,5,10,11,19:24,26,27,29,36,38,39)]
{
  colnames(tm_bat)[colnames(tm_bat) =='pa'] <- 'Batting - Plate Apperances'
  colnames(tm_bat)[colnames(tm_bat) =='ab'] <- 'Batting - At-Bats'
  colnames(tm_bat)[colnames(tm_bat) =='h'] <- 'Batting - Hits'
  colnames(tm_bat)[colnames(tm_bat) =='k'] <- 'Batting - Strikeouts'
  colnames(tm_bat)[colnames(tm_bat) =='d'] <- 'Batting - Doubles'
  colnames(tm_bat)[colnames(tm_bat) =='t'] <- 'Batting - Triples'
  colnames(tm_bat)[colnames(tm_bat) =='hr'] <- 'Batting - Homeruns'
  colnames(tm_bat)[colnames(tm_bat) =='sb'] <- 'Batting - Stolen Bases'
  colnames(tm_bat)[colnames(tm_bat) =='cs'] <- 'Batting - Caught Stealing'
  colnames(tm_bat)[colnames(tm_bat) =='rbi'] <- 'Batting - RBIs'
  colnames(tm_bat)[colnames(tm_bat) =='r'] <- 'Batting - Runs'
  colnames(tm_bat)[colnames(tm_bat) =='gdp'] <- 'Batting - Grounded in Double Play'
  colnames(tm_bat)[colnames(tm_bat) =='ebh'] <- 'Batting - Extra Base Hits'
  colnames(tm_bat)[colnames(tm_bat) =='avg'] <- 'Batting - Average'
  colnames(tm_bat)[colnames(tm_bat) =='obp'] <- 'Batting - OBP'
  colnames(tm_bat)[colnames(tm_bat) =='slg'] <- 'Batting - SLG'
  colnames(tm_bat)[colnames(tm_bat) =='rc'] <- 'Batting - Runs Created'
  colnames(tm_bat)[colnames(tm_bat) =='rc27'] <- 'Batting - RC27'
  colnames(tm_bat)[colnames(tm_bat) =='iso'] <- 'Batting - ISO'
  colnames(tm_bat)[colnames(tm_bat) =='ops'] <- 'Batting - OPS'
  colnames(tm_bat)[colnames(tm_bat) =='batter.walks'] <- 'Batting - Walks'
}

all.stats <- merge(tm_bat,tm_pitch)
all.stats <- merge(all.stats,tm_field)
all.stats <- merge(all.stats,lkup,all.x = TRUE)

cnames <- colnames(all.stats[c(3:69,72:74)])
cnames <- sort(cnames)

gms <- read.csv("games.csv")
gms <- gms %>% filter(played == 1)
gms <- max(as.Date(gms$date))

# x <- 'Pitching - ERA'
# y <- 'Batting - Homeruns'
# l <- 'MiLPBE'
# # 
# # 
# tm.scatter <- function(l,x,y){
# num.x <- which( colnames(all.stats)==x)
# num.y <- which( colnames(all.stats)==y)
# 
# p.all.stats <- subset(all.stats,all.stats$League == l)
# p.all.stats <- p.all.stats[c(70,as.numeric(num.x),as.numeric(num.y))]
# colnames(p.all.stats) <- c("t","x","y")
# # 
# # 
# pbe.colors <- c('#97162B',
#                 '#D0D02B',
#                 '#0E1540',
#                 '#FF6700',
#                 '#005CAD',
#                 '#87795E',
#                 '#2C0060',
#                 '#183013')
# milpbe.colors <- c('#007EF3',
#                    '#86572C',
#                    '#6C0000',
#                    '#115376')
# # if(l == 'PBE'){
# p <-  ggplot(p.all.stats, aes(x=x, y=y,color=t))+
#   geom_point() +
#   ggtitle(paste(y,"by",x), subtitle =paste(l))  +
#   theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
#   xlab(x) + ylab(y) +
#   geom_text(label=p.all.stats$t, vjust = -.5) +
#   theme(legend.position = "none") +
#   if(l=='PBE'){
#     scale_colour_manual(values=pbe.colors)
#   } else{
#     scale_colour_manual(values=milpbe.colors)
#   }
# p

# } else {
#   p <-  ggplot(p.all.stats, aes(x=x, y=y,color=t))+
#     geom_point() + 
#     ggtitle(paste(y,"by",x), subtitle =paste(l))  +
#     theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
#     xlab(x) + ylab(y) +
#     geom_text(label=p.all.stats$t, vjust = -.5) +
#     theme(legend.position = "none") +
#     geom_text(label=p.all.stats$t, vjust = -.5) +
#     scale_colour_manual(values=milpbe.colors)
#   p
#   
# }
# }
# 
# tm.scatter(l,x,y)
# 