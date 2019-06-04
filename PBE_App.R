#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

suppressMessages(library(shiny))
suppressMessages(library(tidyverse))
suppressMessages(library(DT))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(stringr))
suppressMessages(library(RColorBrewer))
suppressMessages(library(shinydashboard))
suppressMessages(library(readxl))
suppressMessages(library(directlabels))
suppressMessages(library(Hmisc))
suppressMessages(library(gridExtra))
suppressMessages(library(ggthemes))
suppressMessages(library(ggrepel))
library(rsconnect)

source("Codes/All-Time_Stats.R",local = TRUE)
source("Codes/Season_Stats.R",local = TRUE)
source("Codes/Team_Scatter.R",local = TRUE)
source("Codes/Daily_Standings.R",local = TRUE)
source("Codes/Records.R",local = TRUE)
# Define UI for application that draws a histogram

all.time.hitter.leaderboard <- function(p,x,y,z){
  # subset hitter dataframe by league
  if (p=="All"){
    hit.plt.df <- subset(c.all.hit,c.all.hit$league_abbr == z)
  }else if (p=='OF'){
    hit.plt.df <- subset(c.all.hit,c.all.hit$league_abbr == z & c.all.hit$Position %in% c('LF','CF','RF'))
  }else{
    hit.plt.df <- subset(c.all.hit,c.all.hit$league_abbr == z & c.all.hit$Position == p)  
  }
  # if PBE subset dataframe by plate appearances greater than or equal to 760, if MiLPBE subset by PA greater than or equal to 470
  if(x %in% c('Average','OBP','SLG','OPS','ISO','BABIP','K Percent','BB Percent','K-BB Percent','Strikeouts','GDP')){
    mean_pa <- round(mean(hit.plt.df$`Plate Apperances`),0)
    hit.plt.df <- subset(hit.plt.df,hit.plt.df$`Plate Apperances`>=mean_pa)
  } else {
    hit.plt.df <- hit.plt.df
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(hit.plt.df)==x)
  
  
  # if statistic is K percent or K-BB percent, take the bottom obs, players with lower k-percents are better
  if(x %in% c("K Percent", "K-BB Percent",'Strikeouts','GDP')){
    hit.plt.df <- top_n(hit.plt.df, n=y, -hit.plt.df[num])
  } else {
    hit.plt.df <- top_n(hit.plt.df, n=y, hit.plt.df[num])
  }
  
  # condense dataframe down to 2 columns: name & position, the statistic variable column
  hit.plt.df <- hit.plt.df[c(37,as.numeric(num))]
  
  #rename columns to x,y for easier plotting
  colnames(hit.plt.df) <- c("pl.x","pl.y")
  hit.plt.df <- filter(hit.plt.df, pl.y != 0)
  
  # plotting funtion, if statistic is k percetn or k-bb percent, plot in reverse order
  if(x %in% c("K Percent", "K-BB Percent",'Strikeouts','GDP')){
    pl <-  ggplot(hit.plt.df, aes(x=reorder(pl.x,-pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(hit.plt.df$pl.x),"Hitters All-Time -",p), subtitle =paste(x,"-", z)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=hit.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#5FC3E4", high =  "#E55D87")  +
      coord_flip()
    pl
  } else {
    pl <-  ggplot(hit.plt.df, aes(x=reorder(pl.x,pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(hit.plt.df$pl.x),"Hitters All-Time -",p), subtitle =paste(x,"-", z))  +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=hit.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#E55D87", high =  "#5FC3E4")  +
      coord_flip()
    pl
  }
}

s.hitter.leaderboard <- function(p,w,x,y,z){
  # subset hitter dataframe by league
  if (p=="All"){
    s.hit.plt.df <- subset(s.all.hit,s.all.hit$league_abbr == z & s.all.hit$year == w)
  }else if(p=='OF'){
    s.hit.plt.df <- subset(s.all.hit,s.all.hit$league_abbr == z & s.all.hit$year == w & s.all.hit$Position %in% c('LF','CF','RF'))  
  }else{
    s.hit.plt.df <- subset(s.all.hit,s.all.hit$league_abbr == z & s.all.hit$year == w & s.all.hit$Position == p)
  }
  
  # if PBE subset dataframe by plate appearances greater than or equal to 760, if MiLPBE subset by PA greater than or equal to 470
  if(x %in% c('Average','OBP','SLG','OPS','ISO','BABIP','K Percent','BB Percent','K-BB Percent','Strikeouts','GDP')){
    mean_pa <- round(mean(s.hit.plt.df$`Plate Apperances`),0)
    s.hit.plt.df <- subset(s.hit.plt.df,s.hit.plt.df$`Plate Apperances`>=mean_pa)
  } else {
    s.hit.plt.df <- s.hit.plt.df
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(s.hit.plt.df)==x)
  
  
  # if statistic is K percent or K-BB percent, take the bottom obs, players with lower k-percents are better
  if(x %in% c("K Percent", "K-BB Percent",'Strikeouts','GDP')){
    s.hit.plt.df <- top_n(s.hit.plt.df, n=y, -s.hit.plt.df[num])
  } else {
    s.hit.plt.df <- top_n(s.hit.plt.df, n=y, s.hit.plt.df[num])
  }
  
  # condense dataframe down to 2 columns: team & name & position, the statistic variable column
  s.hit.plt.df <- s.hit.plt.df[c(43,as.numeric(num))]
  
  #rename columns to x,y for easier plotting
  colnames(s.hit.plt.df) <- c("pl.x","pl.y")
  s.hit.plt.df <- filter(s.hit.plt.df, pl.y != 0)
  
  # plotting funtion, if statistic is k percetn or k-bb percent, plot in reverse order
  if(x %in% c("K Percent", "K-BB Percent",'Strikeouts','GDP')){
    pl <-  ggplot(s.hit.plt.df, aes(x=reorder(pl.x,-pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste(w,"Season -","Top",length(s.hit.plt.df$pl.x), "Hitters -",p), subtitle =paste(x,"-", z)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.hit.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#5FC3E4", high =  "#E55D87")  +
      coord_flip()
    pl
  } else {
    pl <-  ggplot(s.hit.plt.df, aes(x=reorder(pl.x,pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste(w,"Season -","Top",length(s.hit.plt.df$pl.x), "Hitters -",p), subtitle =paste(x,"-", z)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.hit.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#E55D87", high =  "#5FC3E4") +
      coord_flip()
    pl
  }
}

all.time.pitcher.leaderboard <- function(p,x,y,z){
  # subset pitcher dataframe by league
  if(p=='All'){
  pitch.plt.df <- subset(c.all.pitch,c.all.pitch$league_abbr == z)
  }else{
  pitch.plt.df <- subset(c.all.pitch,c.all.pitch$league_abbr == z & c.all.pitch$Position == p)  
  }
  
  # if PBE subset dataframe by Innings Pitched greater than or equal to 400, if MiLPBE subset by IP greater than or equal to 160
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Hits Allowed','Homeruns Allowed','Runs Allowed')){
    mean_ip <- round(mean(pitch.plt.df$`Innings Pitched`),0)
    pitch.plt.df <- subset(pitch.plt.df,pitch.plt.df$`Innings Pitched`>=mean_ip)
  } else {
    pitch.plt.df <- pitch.plt.df
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(pitch.plt.df)==x)
  
  
  # if statistic is a ratio, reverse order
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Hits Allowed','Homeruns Allowed','Runs Allowed')){
    pitch.plt.df <- top_n(pitch.plt.df, n=y, -pitch.plt.df[num])
  } else {
    pitch.plt.df <- top_n(pitch.plt.df, n=y, pitch.plt.df[num])
  }
  
  # condense dataframe down to 2 columns: name & position, the statistic variable column
  pitch.plt.df <- pitch.plt.df[c(39,as.numeric(num))]
  
  #rename columns to x,y for easier plotting
  colnames(pitch.plt.df) <- c("pl.x","pl.y")
  pitch.plt.df <- filter(pitch.plt.df, pl.y != 0)
  
  # plotting funtion, if statistic is ratio, plot in reverse order
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Hits Allowed','Homeruns Allowed','Runs Allowed')){
    pl <-  ggplot(pitch.plt.df, aes(x=reorder(pl.x,-pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(pitch.plt.df$pl.x),"Pitchers All-Time -",p), subtitle =paste(x,"-", z)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=pitch.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#5FC3E4", high =  "#E55D87")  +
      coord_flip()
    pl
  } else {
    pl <-  ggplot(pitch.plt.df, aes(x=reorder(pl.x,pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(pitch.plt.df$pl.x),"Pitchers All-Time -",p), subtitle =paste(x,"-", z))  +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=pitch.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#E55D87", high =  "#5FC3E4") +
      coord_flip()
    pl
  }
}

s.pitcher.leaderboard <- function(p,w,x,y,z){
  # subset pitcher dataframe by league
  if(p=="All"){
  s.pitch.plt.df <- subset(s.all.pitch,s.all.pitch$league_abbr == z & s.all.pitch$year == w)
  }else{
  s.pitch.plt.df <- subset(s.all.pitch,s.all.pitch$league_abbr == z & s.all.pitch$year == w & s.all.pitch$Position == p)  
  }
  # if PBE subset dataframe by plate appearances greater than or equal to 760, if MiLPBE subset by PA greater than or equal to 470
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Hits Allowed','Homeruns Allowed','Runs Allowed')){
    mean_ip <- round(mean(s.pitch.plt.df$`Innings Pitched`),0)
    s.pitch.plt.df <- subset(s.pitch.plt.df,s.pitch.plt.df$`Innings Pitched`>=mean_ip)
  } else {
    s.pitch.plt.df <- s.pitch.plt.df
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(s.pitch.plt.df)==x)
  
  
  # if statistic is K percent or K-BB percent, take the bottom obs, players with lower k-percents are better
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Hits Allowed','Homeruns Allowed','Runs Allowed')){
    s.pitch.plt.df <- top_n(s.pitch.plt.df, n=y, -s.pitch.plt.df[num])
  } else {
    s.pitch.plt.df <- top_n(s.pitch.plt.df, n=y, s.pitch.plt.df[num])
  }
  
  # condense dataframe down to 2 columns: team & name & position, the statistic variable column
  s.pitch.plt.df <- s.pitch.plt.df[c(43,as.numeric(num))]
  
  #rename columns to x,y for easier plotting
  colnames(s.pitch.plt.df) <- c("pl.x","pl.y")
  s.pitch.plt.df <- filter(s.pitch.plt.df, pl.y != 0)
  
  # plotting funtion, if statistic is k percetn or k-bb percent, plot in reverse order
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Hits Allowed','Homeruns Allowed','Runs Allowed')){
    pl <-  ggplot(s.pitch.plt.df, aes(x=reorder(pl.x,-pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste(w,"Season -","Top",length(s.pitch.plt.df$pl.x), "Pitchers -",p), subtitle =paste(x,"-", y)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.pitch.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#5FC3E4", high =  "#E55D87")  +
      coord_flip()
    pl
  } else {
    pl <-  ggplot(s.pitch.plt.df, aes(x=reorder(pl.x,pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste(w,"Season -","Top",length(s.pitch.plt.df$pl.x),"Pitchers -",p), subtitle =paste(x,"-", y)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.pitch.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#E55D87", high =  "#5FC3E4") +
      coord_flip()
    pl
  }
}

tm.scatter <- function(l,x,y){
  num.x <- which( colnames(all.stats)==x)
  num.y <- which( colnames(all.stats)==y)
  
  p.all.stats <- subset(all.stats,all.stats$League == l)
  p.all.stats <- p.all.stats[c(70,as.numeric(num.x),as.numeric(num.y))]
  colnames(p.all.stats) <- c("t","x","y")
  
  
  pbe.colors <- c('#97162B',
                  '#D0D02B',
                  '#0E1540',
                  '#FF6700',
                  '#005CAD',
                  '#87795E',
                  '#2C0060',
                  '#183013')
  milpbe.colors <- c('#007EF3',
                     '#86572C',
                     '#6C0000',
                     '#115376')
  if(l == 'PBE'){
    p <-  ggplot(p.all.stats, aes(x=x, y=y,color=t))+
      geom_point(aes(size=5)) + 
      ggtitle(paste(y,"by",x), subtitle =paste(l))  +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      xlab(x) + ylab(y) +
      geom_text(label=p.all.stats$t, vjust = -.75) +
      theme(legend.position = "none") +
      scale_colour_manual(values=pbe.colors)
    p
  } else {
    p <-  ggplot(p.all.stats, aes(x=x, y=y,color=t))+
      geom_point(aes(size=5)) + 
      ggtitle(paste(y,"by",x), subtitle =paste(l))  +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      xlab(x) + ylab(y) +
      geom_text(label=p.all.stats$t, vjust = -.75) +
      theme(legend.position = "none") +
      scale_colour_manual(values=milpbe.colors)
    p
    
  }
}

tm.tbl <- function(l,x,y){
  
  num.x <- which( colnames(all.stats)==x)
  num.y <- which( colnames(all.stats)==y)
  
  p.all.stats <- subset(all.stats,all.stats$League == l)
  p.all.stats <- p.all.stats[c(71,as.numeric(num.y),as.numeric(num.x))]
  t.all.stats <- p.all.stats
  t.all.stats
}

WinsAB_plot <- function(l){
  #For use in new seasons
  # filenames = paste(paste("R_Code_Exports/",2027,"_PBE_Standings",sep=""), '.csv', sep = '') 
  # ds.all_games <- do.call(rbind, lapply(filenames, read.csv, header = TRUE))

  daily <- subset(ds.all_games,ds.all_games$league_abbr == l)
  p.daily <- daily[c(4,9,14,18,31,34)]
  season <- unique(p.daily$season)
  colnames(p.daily) <- c("x","t","d","y","c","s")
 
  
  pbe.colors <- c('#97162B',
                  '#D0D02B',
                  '#0E1540',
                  '#FF6700',
                  '#005CAD',
                  '#87795E',
                  '#2C0060',
                  '#183013')
  milpbe.colors <- c('#007EF3',
                     '#86572C',
                     '#6C0000',
                     '#115376')
  
  
  if(l == 'PBE'){
    p <-  ggplot(p.daily, aes(x=x, y=y, color = t))+
      geom_line() +
      ggtitle("Games Above/Below .500", subtitle = paste(season,"Season -",l)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      ylab("Games Above/Below .500") + xlab("Date") +
      geom_dl(aes(label=t, color=t), method = list("last.points",cex = .75,hjust = -.15, vjust = -.75)) +
      theme(legend.position = "none") + 
      scale_colour_manual(values=pbe.colors) +
      guides(colour=FALSE) +
      theme_bw()
    p
    
  } else {
    p <-  ggplot(p.daily, aes(x=x, y=y, color = t))+
      geom_line() +
      ggtitle("Games Above/Below .500", subtitle = paste(season,"Season -",l)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      ylab("Games Above/Below .500") + xlab("Date") +
      geom_dl(aes(label=t, color=t), method = list("last.points",cex = .75,hjust = -.15, vjust = -.75)) +
      theme(legend.position = "none") + 
      scale_colour_manual(values=milpbe.colors) +
      guides(colour=FALSE) +
      theme_bw()
    p
    
  }
}
  
WAB.tbl <- function(l){
  tbl.ds <- subset(ds.all_games,ds.all_games$league_abbr == l)
  tbl.ds$date <- as.Date(tbl.ds$date)
  tbl.ds <- tbl.ds %>% group_by(team_id) %>% filter(date == max(date))
  tbl.ds <- tbl.ds[c(10,14,18,19,21:23,30,32,33)]
  colnames(tbl.ds)[colnames(tbl.ds) == 'team_name'] <-'Team Name'
  colnames(tbl.ds)[colnames(tbl.ds) == 'division'] <-'Division'
  colnames(tbl.ds)[colnames(tbl.ds) == 'below.500'] <-'Games Above/Below .500'
  colnames(tbl.ds)[colnames(tbl.ds) == 'winloss'] <-'Win-Loss'
  colnames(tbl.ds)[colnames(tbl.ds) == 'ttl_ra'] <-'Total Runs Against'
  colnames(tbl.ds)[colnames(tbl.ds) == 'ttl_runs'] <-'Total Runs'
  colnames(tbl.ds)[colnames(tbl.ds) == 'ttl_hits'] <-'Total Hits'
  colnames(tbl.ds)[colnames(tbl.ds) == 'pythag_record'] <-'Pythag Record'
  tbl.ds <- tbl.ds [c(1,9,10,6,5,7,3,4,8)]
  tbl.ds <- tbl.ds[order(tbl.ds$`League Standing`),]
  tbl.ds
}  

records.plot <- function(t){
  pl.records <- subset(r.records,r.records$team_name == t)
  pl.color <- as.character(unique(pl.records$`Primary Color`))
  pl.tcolor <- as.character(unique(pl.records$`Tertiary Color`))

  
  
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = .75)),
    colhead = list(fg_params=list(cex = .75)),
    rowhead = list(fg_params=list(cex = .75)))
  records.tbl <- pl.records %>% group_by(team_name) %>% summarise(`Seasons Played` = n(), `Playoff Apperances` = sum(made_playoffs), Championships = sum(won_playoffs), `Team Average Wins` = round(mean(w),0))
  colnames(records.tbl)[colnames(records.tbl) == 'team_name'] <- 'Team'
  
  
  p <- ggplot(pl.records,aes(x=year,y=w,group=team_name)) +
    geom_line(colour = pl.color) +
    geom_point(shape = 23, size = 3, fill = pl.tcolor, colour = "black") +
    ggtitle("Wins by Season", subtitle = paste(unique(pl.records$team_name),"-",unique(pl.records$league_abbr))) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
    ylab("Wins") + xlab("Year") +
    scale_x_continuous(breaks=seq(from = 2017,to = 2026,by = 1)) +
    geom_text(x=mean(pl.records$year), y=mean(pl.records$Lg_Average_Wins), label=paste("League Aveage Wins -",unique(pl.records$Lg_Average_Wins)), vjust = -.5, size = 5) +
    geom_line(aes(x=year,y=Lg_Average_Wins),linetype = "longdash") +
    theme_bw()
  p     
}

vol.tbl <- function(t){
  lg.lup <- subset(r.volatility,r.volatility$team_name == t)
  lg.lup <- lg.lup[[1]] 
  r.volatility <- subset(r.volatility,r.volatility$league_abbr == lg.lup)
  colnames(r.volatility)[colnames(r.volatility) == 'league_abbr'] <-'League'
  colnames(r.volatility)[colnames(r.volatility) == 'team_name'] <-'Team'
  r.volatility
}

season.tbl <- function(t){
  season.info <- subset(r.records,r.records$team_name == t)
  season.info <- season.info[c(6,18,21:23,28,11)]
  colnames(season.info)[colnames(season.info) == 'year'] <-'Year'
  colnames(season.info)[colnames(season.info) == 'team_name'] <-'Team'
  colnames(season.info)[colnames(season.info) == 'best_hitter_name'] <-'Best Hitter'
  colnames(season.info)[colnames(season.info) == 'best_pitcher_name'] <-'Best Pitcher'
  colnames(season.info)[colnames(season.info) == 'best_rookie_name'] <-'Best Rookie'
  colnames(season.info)[colnames(season.info) == 'winloss'] <- 'Win-Loss'
  colnames(season.info)[colnames(season.info) == 'pos'] <-'Final Divison Standing'
  season.info <- season.info[order(-season.info$Year),]
  season.info
}

records.tbl <- function(t){
  pl.records <- subset(r.records,r.records$team_name == t)
  records.tbl <- pl.records %>% group_by(team_name) %>% summarise(`Seasons Played` = n(), `Playoff Apperances` = sum(made_playoffs), Championships = sum(won_playoffs), `Team Average Wins` = round(mean(w),0))
  colnames(records.tbl)[colnames(records.tbl) == 'team_name'] <- 'Team'
  records.tbl
}

all.hit.scatter <- function(l,p,x,y){
  num.x <- which( colnames(c.all.hit)==x)
  num.y <- which( colnames(c.all.hit)==y)
  
  if (p=='All'){
    c.pl.scatter <- subset(c.all.hit,c.all.hit$league_abbr == l)
  }else if (p == 'OF'){
    c.pl.scatter <- subset(c.all.hit,c.all.hit$league_abbr == l & c.all.hit$Position %in% c('LF','CF','RF')) 
  } else{
    c.pl.scatter <- subset(c.all.hit,c.all.hit$league_abbr == l & c.all.hit$Position == p) 
  }
  
  if(x %in% c('Average','OBP','SLG','OPS','ISO','BABIP','K Percent','BB Percent','K-BB Percent','Strikeouts') | y %in% c('Average','OBP','SLG','OPS','ISO','BABIP','K Percent','BB Percent','K-BB Percent','Strikeouts') ){
    mean_pa <- round(mean(c.pl.scatter$`Plate Apperances`),0)
    c.pl.scatter <- subset(c.pl.scatter,c.pl.scatter$`Plate Apperances`>=mean_pa)
  } else {
    c.pl.scatter <- c.pl.scatter
  } 
  
  c.pl.scatter <- c.pl.scatter[c(37,as.numeric(num.x),as.numeric(num.y))]
  colnames(c.pl.scatter) <- c("pl","x","y")
  
  if(p=='All'){
    l.pl <- subset(c.pl.scatter,c.pl.scatter$x >= quantile(c.pl.scatter$x,.97) |  c.pl.scatter$y >= quantile(c.pl.scatter$y,.97))
  }else{
    l.pl <- subset(c.pl.scatter,c.pl.scatter$x >= quantile(c.pl.scatter$x,.75) |  c.pl.scatter$y >= quantile(c.pl.scatter$y,.75))   
  }
  
  
  pl <-  ggplot(c.pl.scatter, aes(x=x, y=y,label=pl))+
    geom_point(aes(colour = x)) +
    scale_colour_gradient(low = "Orange", high = "#3945D7") +
    ggtitle(paste("All-Time Hitters",y,"by",x), subtitle =paste(l,"-",p))  +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
    xlab(x) + ylab(y) +
    geom_text_repel(
      arrow = arrow(length = unit(0.01, 'npc')),
      point.padding = unit(.80, "lines"),
      box.padding = unit(.60, "lines"),
      force = 2,
      data=l.pl) +
    theme(legend.position = "none")
  
  pl
}

s.hit.scatter <- function(z,l,p,x,y){
  num.x <- which( colnames(s.all.hit)==x)
  num.y <- which( colnames(s.all.hit)==y)
  
  if (p=='All'){
    s.pl.scatter <- subset(s.all.hit,s.all.hit$league_abbr == l & s.all.hit$year == z)
  }else if (p == 'OF'){
    s.pl.scatter <- subset(s.all.hit,s.all.hit$league_abbr == l & s.all.hit$Position %in% c('LF','CF','RF') & s.all.hit$year == z) 
  } else{
    s.pl.scatter <- subset(s.all.hit,s.all.hit$league_abbr == l & s.all.hit$Position == p & s.all.hit$year == z) 
  }
  
  if(x %in% c('Average','OBP','SLG','OPS','ISO','BABIP','K Percent','BB Percent','K-BB Percent','Strikeouts') | y %in% c('Average','OBP','SLG','OPS','ISO','BABIP','K Percent','BB Percent','K-BB Percent','Strikeouts') ){
    mean_pa <- round(mean(s.pl.scatter$`Plate Apperances`),0)
    s.pl.scatter <- subset(s.pl.scatter,s.pl.scatter$`Plate Apperances`>=mean_pa)
  } else {
    s.pl.scatter <- s.pl.scatter
  } 
  
  s.pl.scatter <- s.pl.scatter[c(43,as.numeric(num.x),as.numeric(num.y))]
  colnames(s.pl.scatter) <- c("pl","x","y")
  
  if(p=='All'){
    l.pl <- subset(s.pl.scatter,s.pl.scatter$x >= quantile(s.pl.scatter$x,.97) |  s.pl.scatter$y >= quantile(s.pl.scatter$y,.97))
  }else{
    l.pl <- subset(s.pl.scatter,s.pl.scatter$x >= quantile(s.pl.scatter$x,.75) |  s.pl.scatter$y >= quantile(s.pl.scatter$y,.75))   
  }
  
  
  pl <-  ggplot(s.pl.scatter, aes(x=x, y=y,label=pl))+
    geom_point(aes(colour = x)) +
    scale_colour_gradient(low = "Orange", high = "#3945D7") +
    ggtitle(paste(z,"- Season Hitters",y,"by",x), subtitle =paste(l,"-",p))  +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
    xlab(x) + ylab(y) +
    geom_text_repel(
      arrow = arrow(length = unit(0.01, 'npc')),
      point.padding = unit(.80, "lines"),
      box.padding = unit(.60, "lines"),
      force = 2,
      data=l.pl) +
    theme(legend.position = "none")
  
  pl
}

all.pitch.scatter <- function(l,p,x,y){
  num.x <- which( colnames(c.all.pitch)==x)
  num.y <- which( colnames(c.all.pitch)==y)
  
  
  if (p=='All'){
    c.p.pl.scatter <- subset(c.all.pitch,c.all.pitch$league_abbr == l)
  } else{
    c.p.pl.scatter <- subset(c.all.pitch,c.all.pitch$league_abbr == l & c.all.pitch$Position == p) 
  }
  
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Win Percent','K percent', 'K-BB percent') | y %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Win Percent','K percent', 'K-BB percent')){
    mean_ip <- round(mean(c.p.pl.scatter$`Innings Pitched`),0)
    c.p.pl.scatter <- subset(c.p.pl.scatter,c.p.pl.scatter$`Innings Pitched`>=mean_ip)
  } else {
    c.p.pl.scatter<- c.p.pl.scatter
  }
  
  c.p.pl.scatter <- c.p.pl.scatter[c(39,as.numeric(num.x),as.numeric(num.y))]
  colnames(c.p.pl.scatter) <- c("pl","x","y")
  if(p=='All'){
    l.pl <- subset(c.p.pl.scatter,c.p.pl.scatter$x >= quantile(c.p.pl.scatter$x,.97) |  c.p.pl.scatter$y >= quantile(c.p.pl.scatter$y,.97))
  }else{
    l.pl <- subset(c.p.pl.scatter,c.p.pl.scatter$x >= quantile(c.p.pl.scatter$x,.93) |  c.p.pl.scatter$y >= quantile(c.p.pl.scatter$y,.93))   
  }
  
  
  pl <-  ggplot(c.p.pl.scatter, aes(x=x, y=y,label=pl))+
    geom_point(aes(colour = x)) +
    scale_colour_gradient(low = "Orange", high = "#3945D7") +
    ggtitle(paste("All-Time Pitchers",y,"by",x), subtitle =paste(l,"-",p))  +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
    xlab(x) + ylab(y) +
    geom_text_repel(
      arrow = arrow(length = unit(0.01, 'npc')),
      point.padding = unit(.80, "lines"),
      box.padding = unit(.60, "lines"),
      force = 2,
      data=l.pl) +
    theme(legend.position = "none")
  
  pl
}

s.pitch.scatter <- function(z,l,p,x,y){
  num.x <- which( colnames(s.all.pitch)==x)
  num.y <- which( colnames(s.all.pitch)==y)
  
  
  if (p=='All'){
    s.p.pl.scatter <- subset(s.all.pitch,s.all.pitch$league_abbr == l & s.all.pitch$year == z)
  } else{
    s.p.pl.scatter <- subset(s.all.pitch,s.all.pitch$league_abbr == l & s.all.pitch$Position == p & s.all.pitch$year == z) 
  }
  
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Win Percent','K percent', 'K-BB percent') | y %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Win Percent','K percent', 'K-BB percent')){
    mean_ip <- round(mean(s.p.pl.scatter$`Innings Pitched`),0)
    s.p.pl.scatter <- subset(s.p.pl.scatter,s.p.pl.scatter$`Innings Pitched`>=mean_ip)
  } else {
    s.p.pl.scatter<- s.p.pl.scatter
  }
  
  s.p.pl.scatter <- s.p.pl.scatter[c(43,as.numeric(num.x),as.numeric(num.y))]
  colnames(s.p.pl.scatter) <- c("pl","x","y")
  if(p=='All'){
    l.pl <- subset(s.p.pl.scatter,s.p.pl.scatter$x >= quantile(s.p.pl.scatter$x,.97) |  s.p.pl.scatter$y >= quantile(s.p.pl.scatter$y,.97))
  }else{
    l.pl <- subset(s.p.pl.scatter,s.p.pl.scatter$x >= quantile(s.p.pl.scatter$x,.93) |  s.p.pl.scatter$y >= quantile(s.p.pl.scatter$y,.93))   
  }
  
  
  pl <-  ggplot(s.p.pl.scatter, aes(x=x, y=y,label=pl))+
    geom_point(aes(colour = x)) +
    scale_colour_gradient(low = "Orange", high = "#3945D7") +
    ggtitle(paste(z,"- Season Pitchers",y,"by",x), subtitle =paste(l,"-",p))  +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
    xlab(x) + ylab(y) +
    geom_text_repel(
      arrow = arrow(length = unit(0.01, 'npc')),
      point.padding = unit(.80, "lines"),
      box.padding = unit(.60, "lines"),
      force = 2,
      data=l.pl) +
    theme(legend.position = "none")
  
  pl
} 


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "PBE",dropdownMenu(type = "notifications",
                                                     messageItem(
                                                       from = "Built by: Clarkbar36",
                                                       message = paste("Data is updated through:",gms),
                                                       icon("beer")         
                                                     )))  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar("THE HUB:",
  sidebarMenu(
    menuItem("Wins Above/Below .500", tabName = "WinsAB", icon = icon("chart-line")),
    menuItem("Hitter Leaderboard", tabName = "HitterL", icon = icon("chart-bar")),
    menuItem("Pitcher Leaderboard", tabName = "PitcherL", icon = icon("chart-bar")),
    menuItem("Wins by Season", tabName = "Records", icon = icon("chart-line")),
    menuItem("Team Scatter", tabName = "TmSctpl", icon = icon("baseball-ball")),
    menuItem("Hitter Scatter", tabName = "HSctpl", icon = icon("baseball-ball")),
    menuItem("Pitcher Scatter", tabName = "PSctpl", icon = icon("baseball-ball")),
    "________________________",
    menuItem("Link: PBE Forum", icon = icon("link"),
             href = "http://probaseballexperience.jcink.net/index.php?act=idx"),
    menuItem("Link: PBE Main Index", icon = icon("link"),
             href = "http://www.pbesim.com"),
    menuItem("Link: PBE Player Compendium", icon = icon("link"),
             href = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTVnwILj2aFQWiepL0YNHsRwnuckMdwfVPUkUA3rytPpYzprgHqL0u5bMCQZv4XrR3WdRAVS_F_6_nI/pubhtml"),
    menuItem("Link: Github", icon = icon("link"),
             href = "https://github.com/Clarkbar36/Pro-Baseball-Experience")
    
  ))
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "WinsAB",
            fluidRow(
              column(width = 6,
                     selectInput("dslg",
                                 "League:",
                                 c('PBE','MiLPBE'))
            )),
            fluidRow(
              column(width = 6,plotOutput("wins_AB")),
                     column(width = 3,dataTableOutput("winsWB_table"))
              )
    ),
    
    tabItem(tabName = "HitterL",
            fluidRow(
              column(width = 3,
                     selectInput("stat",
                                 "Hitter Statistic:",
                                 c('WAR',
                                   'At Bats',
                                   'Average',
                                   'BABIP',
                                   'BB Percent',
                                   'Doubles',
                                   'Games',
                                   'GDP',
                                   'Hits',
                                   'Homeruns',
                                   'ISO',
                                   'K Percent',
                                   'K-BB Percent',
                                   'OBP',
                                   'OPS',
                                   'Plate Apperances',
                                   'RBI',
                                   'Runs',
                                   'Runs Created',
                                   'Single',
                                   'SLG',
                                   'Stolen Bases',
                                   'Strikeouts',
                                   'Total Bases',
                                   'Triples',
                                   'Walks',
                                   'Zone Rating'),selected = 'WAR'),
                     numericInput("obs",
                                  "Top n:",
                                  10, min = 1, max = 100)
                     
              ),
            
              column(width = 3,
                     selectInput("lg",
                                 "League:",
                                 c('PBE','MiLPBE')),
                     sliderInput("year",
                                 "Season",
                                 min = 2017, max = 2027, step = 1,value = 2027,sep = "")),
              column(width = 6,
                     selectInput("pos",
                                 "Position:",
                                 c('All','C','1B','2B','SS','3B','LF','CF','RF','OF','DH'), selected = 'All'))
                     
              ),
              
            
            fluidRow(
              column(width = 6,
                     plotOutput("all.time.hitter_plot")
              ),
              column( width = 6,
                      plotOutput("season.hitter_plot")
              )
            )
    ),
    
    tabItem(tabName = "PitcherL",
            fluidRow(
              column(width = 3,
                     selectInput("pstat",
                                 "Pitcher Statistic:",
                                 c('WAR',
                                   'BABIP',
                                   'BB per 9',
                                   'BB percent',
                                   'Complete Game',
                                   'Earned Runs',
                                   'ERA',
                                   'FIP',
                                   'Hits Allowed',
                                   'Hits per 9',
                                   'Hold',
                                   'Homeruns Allowed',
                                   'HR per 9',
                                   'Innings Pitched',
                                   'K percent',
                                   'Ks per 9',
                                   'K-BB percent',
                                   'Loss',
                                   'Quality Start',
                                   'R per 9',
                                   'Runs Allowed',
                                   'Saves',
                                   'Shutout',
                                   'Strikeouts',
                                   'Walks',
                                   'WHIP',
                                   'Win Percent',
                                   'Wins'), selected = 'WAR'),
                     numericInput("pobs",
                                  "Top n:",
                                  10, min = 1, max = 100)
              ),
              column(width = 3,
                     selectInput("plg",
                                 "League:",
                                 c('PBE','MiLPBE')),
                     sliderInput("pyear",
                                 "Season",
                                 min = 2017, max = 2027, step = 1,value = 2027,sep = "")),
              column(width = 3,
                    selectInput("spos",
                                "Position:",
                                c('All','RP','SP'), selected = 'All'))
              ),
            fluidRow(
              column(width = 6,
                     plotOutput("all.time.pitcher_plot")
              ),
              column( width = 6,
                      plotOutput("season.pitcher_plot")
              )
            )
            
    ),
    
    tabItem(tabName = "Records",
            fluidRow(
              column(width = 6,
                     selectInput('rtm', 'Team', tm.names,selectize=FALSE))),
            fluidRow(
              column(width = 6,plotOutput("records_plt"),dataTableOutput("vol_tbl")),
              column(width = 6,dataTableOutput("record_tbl"),dataTableOutput("season_tbl")))
    ),
    
    tabItem(tabName = "TmSctpl",
            fluidRow(
              column(width = 4,
                     selectInput('ysct', 'Y-Axis Statistic', c(Choose='Batting - Homeruns',cnames), selectize=FALSE)),
              column(width = 4,
                     selectInput('xsct', 'X-Axis Statistic', c(Choose='All - League Standing',cnames), selectize=FALSE)),
              column(width = 2,
                     selectInput("splg",
                                 "League:",
                                 c('PBE','MiLPBE')))
            ),
            fluidRow(
              column(width = 8, plotOutput("team_scatter_plot")),
              column(width = 3,dataTableOutput("team_table"))
    )
  ),
  
  tabItem(tabName = "HSctpl",
          fluidRow(
            column(width = 4,
                   selectInput('hysct', 'Y-Axis Statistic', c(Choose='Homeruns',c.all.h.cnames), selectize=FALSE),
                   selectInput("plsplg",
                               "League:",
                               c('PBE','MiLPBE'))
                   ),
            column(width = 4,
                   selectInput('hxsct', 'X-Axis Statistic', c(Choose='WAR',c.all.h.cnames), selectize=FALSE),
                   sliderInput("plyear",
                               "Season",
                               min = 2017, max = 2027, step = 1,value = 2027,sep = "")),
            column(width = 4,
                   selectInput("hpos",
                               "Position:",
                               c('All','C','1B','2B','SS','3B','LF','CF','RF','OF','DH'), selected = 'All'))
          ),
          fluidRow(
            column(width = 6, plotOutput("c_h_pl_scatter_plot")),
            column(width = 6, plotOutput("s_h_pl_scatter_plot"))
          )
  ),
  
  tabItem(tabName = "PSctpl",
          fluidRow(
            column(width = 4,
                   selectInput('pysct', 'Y-Axis Statistic', c(Choose='WAR',c.all.p.cnames), selectize=FALSE),
                   selectInput("pplsplg",
                               "League:",
                               c('PBE','MiLPBE'))
            ),
            column(width = 4,
                   selectInput('pxsct', 'X-Axis Statistic', c(Choose='Strikeouts',c.all.p.cnames), selectize=FALSE),
                   sliderInput("plpyear",
                               "Season",
                               min = 2017, max = 2027, step = 1,value = 2027,sep = "")),
            column(width = 4,
                   selectInput("ppos",
                               "Position:",
                               c('All','RP','SP'), selected = 'All'))
          ),
          fluidRow(
            column(width = 6, plotOutput("c_p_pl_scatter_plot")),
            column(width = 6, plotOutput("s_p_pl_scatter_plot"))
          )
  )
)
)



ui <- dashboardPage(title = 'Pro Baseball Experience Hub', header, sidebar, body, skin='purple')


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$all.time.hitter_plot <-  renderPlot({
    #input$submit
    all.time.hitter.leaderboard(x = input$stat, y = input$obs, z = input$lg, p = input$pos)
    
  },height = 500)
  
  output$season.hitter_plot <-  renderPlot({
    
    s.hitter.leaderboard(w = input$year, x = input$stat, y = input$obs, z = input$lg, p = input$pos)
    
  },height = 500) 
  
  output$all.time.pitcher_plot <-  renderPlot({
    #input$submit
    all.time.pitcher.leaderboard(x = input$pstat, y = input$pobs, z = input$plg, p = input$spos)
    
  },height = 500)
  
  output$season.pitcher_plot <-  renderPlot({
    #input$submit
    s.pitcher.leaderboard(w = input$pyear, x = input$pstat, y = input$pobs, z = input$plg, p = input$spos)
    
  },height = 500)
  
  output$team_scatter_plot <-  renderPlot({
    #input$submit
    tm.scatter(l = input$splg,x = input$xsct,y = input$ysct)
    
  },height = 500)
  
  output$team_table <-  renderDataTable(options = list(dom = 'tip',paging = FALSE),rownames= FALSE,{
    #input$submit
    tm.tbl(l = input$splg,x = input$xsct,y = input$ysct)
    
  })
  
  output$wins_AB <-  renderPlot({
    #input$submit
    WinsAB_plot(l = input$dslg)
    
  },height = 600)
  
  output$winsWB_table <-  renderDataTable(options = list(dom = 'tip',paging = FALSE),rownames= FALSE,{
    #input$submit
    WAB.tbl(l = input$dslg)
    
  })
  
  output$records_plt <-  renderPlot({
    #input$submit
    records.plot(t = input$rtm)
    
  },height = 400)
  
  output$vol_tbl <-  renderDataTable(options = list(dom = 'tip',paging = FALSE),rownames= FALSE,{
    #input$submit
    vol.tbl(t = input$rtm)
    
  })
  
  output$season_tbl <-  renderDataTable(options = list(dom = 'tip',paging = FALSE),rownames= FALSE,{
    #input$submit
    season.tbl(t = input$rtm)
    
  })
  
  output$record_tbl <-  renderDataTable(options = list(dom = 'tip',paging = FALSE),rownames= FALSE,{
    #input$submit
    records.tbl(t = input$rtm)
    
  })
  
  output$c_h_pl_scatter_plot <-  renderPlot({
    #input$submit
    all.hit.scatter(l = input$plsplg,p = input$hpos,x = input$hxsct,y = input$hysct)
    
  },height = 500)
  
  output$s_h_pl_scatter_plot <-  renderPlot({
    
    s.hit.scatter(l = input$plsplg,p = input$hpos,x = input$hxsct,y = input$hysct, z = input$plyear)
    
  },height = 500) 
  
  output$c_p_pl_scatter_plot <-  renderPlot({
    #input$submit
    all.pitch.scatter(l = input$pplsplg, p = input$ppos ,x = input$pxsct,y = input$pysct)
    
  },height = 500)
  
  output$s_p_pl_scatter_plot <-  renderPlot({
    
    s.pitch.scatter(l = input$pplsplg,p = input$ppos,x = input$pxsct,y = input$pysct, z = input$plpyear)
    
  },height = 500) 
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

