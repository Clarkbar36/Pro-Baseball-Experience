library(dplyr)
library(ggplot2)
library(ggthemes)
setwd("~/Box/Alissa Private Folder/Alex/PBE/Exports/")
player_career_batting  <- read.csv('players_career_batting_stats.csv',header = TRUE)
player_career_batting <- subset(player_career_batting,player_career_batting$split_id == 1)
player_career_pitching <- read.csv('players_career_pitching_stats.csv',header = TRUE)
player_career_pitching <- subset(player_career_pitching,player_career_pitching$split_id == 1)
all.service.time <- merge(player_career_pitching,player_career_batting,by=c('player_id','year','team_id','league_id'),all=TRUE)
all.service.time <- subset(all.service.time,all.service.time$league_id != 1000)


service_time <- all.service.time[c(1,2,4)]
service_time <-unique(service_time)
service_time$year <- NULL
service_time$constant <- 1
service_time <- service_time %>%
  group_by(player_id, league_id) %>% 
  summarise(service.time=n())

ttl_service_time <- service_time[c(1,3)] %>%
  group_by(player_id) %>% 
  summarise(service.time=sum(service.time))
ttl_service_time$label <- "Total"

mipbe_st <- subset(service_time,league_id == "101")
mipbe_st$league_id <- NULL
mipbe_st$label <- "MIPBE"

pbe_st <- subset(service_time,league_id == "100")
pbe_st$league_id <- NULL
pbe_st$label <- "PBE"

service_time_lookup <- bind_rows(list(ttl_service_time,mipbe_st,pbe_st))

source("~/Box/Alissa Private Folder/Alex/PBE/Codes/All-Time_Stats.R")


position_change <- combined.players[c(2,5,100)]
pitcher_type <- subset(position_change,position_change$position_name == "P")
all_other_pos <- subset(position_change,position_change$position_name != "P")
pitcher_type$position_name <- ifelse(pitcher_type$role %in% c(12,13),"CL",ifelse(pitcher_type$role == 11,"SP",pitcher_type$position_name))
pos_lookup <- bind_rows(list(pitcher_type,all_other_pos))
pos_lookup$role <- NULL
pos_lookup <- unique(pos_lookup)
final.service.time <- merge(service_time_lookup,pos_lookup,all.x = TRUE)
final.service.time$position_name <- factor(final.service.time$position_name,levels = c("SP","CL","C", "1B", "2B","3B","SS","LF","CF","RF","DH"))

status <- read.csv("players_roster_status.csv")
status <- status[c(1,8)]
status <- subset(status,status$is_active == 1)

ttlServ <- subset(final.service.time,final.service.time$label == "Total")
ttlServ <- subset(ttlServ,ttlServ$player_id %in% status$player_id)
mipbe <- subset(final.service.time,final.service.time$label == "MIPBE")
pbe <- subset(final.service.time,final.service.time$label == "PBE")
brkdnServ <- subset(final.service.time,final.service.time$label != "Total")

write.csv(ttlServ,"~/Box/Alissa Private Folder/Alex/PBE/R_Code_Exports/total_service_time.csv",row.names = FALSE)

write.csv(brkdnServ,"~/Box/Alissa Private Folder/Alex/PBE/R_Code_Exports/brokendown_service_time.csv",row.names = FALSE)


write.csv(mipbe,"~/Box/Alissa Private Folder/Alex/PBE/R_Code_Exports/mipbe_service_time.csv",row.names = FALSE)


write.csv(pbe,"~/Box/Alissa Private Folder/Alex/PBE/R_Code_Exports/pbe_service_time.csv",row.names = FALSE)







# mipbe_plot <- subset(final.service.time,final.service.time$label == "MIPBE")
# pbe_plot <- subset(final.service.time,final.service.time$label == "PBE")
ttlS_plot <- subset(final.service.time,final.service.time$label == "Total")
# 
# ## Combine Box Plot
# p <- ggplot(data = final.service.time, aes(x=position_name, y=service.time)) + 
#   geom_boxplot(aes(fill=label),outlier.size = .75,outlier.colour = "orange",outlier.alpha = .55) +
#   scale_y_continuous(breaks = round(seq(min(final.service.time$service.time), max(final.service.time$service.time), by = 1),1))
# 
# p + ggtitle("Service Time by Position", subtitle = "Plot by Clarkbar36") + xlab("1st Position") + ylab("Service Time (years)") + labs(fill = "League") + theme(plot.title = element_text(hjust = 0.5)) +theme(plot.subtitle = element_text(hjust = 0.5)) + scale_colour_solarized()
# 
# ## MIPBE Only Plot
# p1 <- ggplot(data = test1, aes(x=position_name, y=service.time)) + 
#   geom_boxplot(aes(fill=label),outlier.size = .75,outlier.colour = "orange",outlier.alpha = .55) +
#   scale_y_continuous(breaks = round(seq(min(final.service.time$service.time), max(final.service.time$service.time), by = 1),1))
# 
# p1 + ggtitle("Service Time by Position - MIPBE", subtitle = "Plot by Clarkbar36") + xlab("1st Position") + ylab("Service Time (years)") + theme(plot.title = element_text(hjust = 0.5)) +theme(plot.subtitle = element_text(hjust = 0.5)) + scale_colour_solarized()
# 
# 
# 
# test1 <- subset(final.service.time,final.service.time$label != "Total")
# test2 <- subset(final.service.time,final.service.time$label == "Total" & final.service.time$position_name == "CF")
# summary(test2)
# p2 <- ggplot(data = test1, aes(x=position_name, y=service.time)) + geom_bar(aes(fill = label),position = position_dodge(), stat = "summary", fun.y = "mean") +  
#   scale_y_continuous(breaks = round(seq(min(final.service.time$service.time), max(final.service.time$service.time), by = 1),1)) + facet_grid(. ~ label) + 
#   
#   p2 +  ggtitle("Service Time by Position", subtitle = "Plot by Clarkbar36") + xlab("1st Position") + ylab("Service Time (years)") + theme(plot.title = element_text(hjust = 0.5)) +theme(plot.subtitle = element_text(hjust = 0.5)) + scale_colour_solarized()


