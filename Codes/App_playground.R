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

source("/Codes/All-Time_Stats.R",local = TRUE)
source("/Codes/Season_Stats.R",local = TRUE)
#All time Hitter Graph Function

# hitter - Set variables to subset and graph by
hit.statistic <- "Stolen Bases" 
hit.obs <- 10
hit.lg <- "PBE"


#hitter function
hitter.leaderboard <- function(x,y,z){
  # subset hitter dataframe by league
  hit.plt.df <- subset(c.all.hit,c.all.hit$league_abbr == hit.lg)
  
  # if PBE subset dataframe by plate appearances greater than or equal to 760, if MiLPBE subset by PA greater than or equal to 470
  if(hit.lg == "PBE"){
    hit.plt.df <- subset(hit.plt.df,hit.plt.df$`Plate Apperances`>=760)
  } else {
    hit.plt.df <- subset(hit.plt.df,hit.plt.df$`Plate Apperances`>=470)
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(hit.plt.df)==hit.statistic)
  
  
  # if statistic is K percent or K-BB percent, take the bottom obs, players with lower k-percents are better
  if(hit.statistic %in% c("K Percent", "K-BB Percent")){
    hit.plt.df <- top_n(hit.plt.df, n=hit.obs, -hit.plt.df[num])
  } else {
    hit.plt.df <- top_n(hit.plt.df, n=hit.obs, hit.plt.df[num])
  }
  
  # condense dataframe down to 2 columns: name & position, the statistic variable column
  hit.plt.df <- hit.plt.df[c(36,as.numeric(num))]
  
  #rename columns to x,y for easier plotting
  colnames(hit.plt.df) <- c("x","y")
  
  # plotting funtion, if statistic is k percetn or k-bb percent, plot in reverse order
  if(hit.statistic %in% c("K Percent", "K-BB Percent")){
  p <-  ggplot(hit.plt.df, aes(x=reorder(x,-y), y=y,fill=y))+
    geom_bar(stat='identity')+
    ggtitle(paste("Top",length(hit.plt.df$x),"All-Time"), subtitle =paste(hit.statistic,"-", hit.lg)) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
    geom_text(data=hit.plt.df,aes(x=x,y=y,label=y),size = 3, hjust=1, colour = "white") +
    scale_y_continuous(toupper(hit.statistic)) +
    theme(axis.title.y = element_blank()) +
    theme(legend.position = "none") +
    scale_fill_gradient(low = "red", high =  "dark blue") +
    coord_flip()
  p
  } else {
    p <-  ggplot(hit.plt.df, aes(x=reorder(x,y), y=y,fill=y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(hit.plt.df$x),"All-Time"), subtitle =paste(hit.statistic,"-", hit.lg))  +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=hit.plt.df,aes(x=x,y=y,label=y),size = 3, hjust=1, colour = "white") +
      scale_y_continuous(toupper(hit.statistic)) +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "red", high =  "dark blue") +
      coord_flip()
    p
  }
}

# hitter - run function to create graph
hitter.leaderboard(x = hit.statistic, y = hit.obs, z = hit.lg)

# ---------------------------------------------------------------------------------------
#All time Pitcher Graph Function

# pitcher - Set variables to subset and graph by
pitch.statistic <- "Win Percent" 
pitch.obs <- 10
pitch.lg <- "PBE"

#pitcher function
pitcher.leaderboard <- function(x,y,z){
  # subset pitcher dataframe by league
  pitch.plt.df <- subset(c.all.pitch,c.all.pitch$league_abbr == pitch.lg)
  
  # if PBE subset dataframe by Innings Pitched greater than or equal to 400, if MiLPBE subset by IP greater than or equal to 160
  if(pitch.statistic %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Win Percent')){
    mean_ip <- round(mean(pitch.plt.df$`Innings Pitched`),0)
    pitch.plt.df <- subset(pitch.plt.df,pitch.plt.df$`Innings Pitched`>=mean_ip)
  } else {
    pitch.plt.df <- pitch.plt.df
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(pitch.plt.df)==pitch.statistic)
  
  
  # if statistic is a ratio, reverse order
  if(pitch.statistic %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent')){
    pitch.plt.df <- top_n(pitch.plt.df, n=pitch.obs, -pitch.plt.df[num])
  } else {
    pitch.plt.df <- top_n(pitch.plt.df, n=pitch.obs, pitch.plt.df[num])
  }
  
  # condense dataframe down to 2 columns: name & position, the statistic variable column
  pitch.plt.df <- pitch.plt.df[c(39,as.numeric(num))]
  
  #rename columns to x,y for easier plotting
  colnames(pitch.plt.df) <- c("x","y")
  
  # plotting funtion, if statistic is ratio, plot in reverse order
  if(pitch.statistic %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent')){
    p <-  ggplot(pitch.plt.df, aes(x=reorder(x,-y), y=y,fill=y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(pitch.plt.df$x),"All-Time"), subtitle =paste(pitch.statistic,"-", pitch.lg)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=pitch.plt.df,aes(x=x,y=y,label=y),size = 3, hjust=1, colour = "white") +
      scale_y_continuous(toupper(pitch.statistic)) +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "red", high =  "dark blue") +
      coord_flip()
    p
  } else {
    p <-  ggplot(pitch.plt.df, aes(x=reorder(x,y), y=y,fill=y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(pitch.plt.df$x),"All-Time"), subtitle =paste(pitch.statistic,"-", pitch.lg))  +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=pitch.plt.df,aes(x=x,y=y,label=y),size = 3, hjust=1, colour = "white") +
      scale_y_continuous(toupper(pitch.statistic)) +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "red", high =  "dark blue") +
      coord_flip()
    p
  }
}

# pitcher - run function to create graph
pitcher.leaderboard(x = pitcher.statistic, y = pitcher.obs, z = pitcher.lg)

# ---------------------------------------------------------------------------------------
#Season Hitter Graph Function

# hitter - Set variables to subset and graph by
s.hit.statistic <- "K Percent" 
s.hit.obs <- 15
s.hit.lg <- "PBE"
s.hit.year <- 2026


#hitter function
s.hitter.leaderboard <- function(w,x,y,z){
  # subset hitter dataframe by league
  s.hit.plt.df <- subset(s.all.hit,s.all.hit$league_abbr == s.hit.lg & s.all.hit$year == s.hit.year)
  
  # if PBE subset dataframe by plate appearances greater than or equal to 760, if MiLPBE subset by PA greater than or equal to 470
  if(s.hit.lg == "PBE"){
    mean_pa <- round(mean(s.hit.plt.df$`Plate Apperances`),0)
    s.hit.plt.df <- subset(s.hit.plt.df,s.hit.plt.df$`Plate Apperances`>=mean_pa)
  } else {
    s.hit.plt.df <- subset(s.hit.plt.df,s.hit.plt.df$`Plate Apperances`>=mean_pa)
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(s.hit.plt.df)==s.hit.statistic)
  
  
  # if statistic is K percent or K-BB percent, take the bottom obs, players with lower k-percents are better
  if(s.hit.statistic %in% c("K Percent", "K-BB Percent")){
    s.hit.plt.df <- top_n(s.hit.plt.df, n=s.hit.obs, -s.hit.plt.df[num])
  } else {
    s.hit.plt.df <- top_n(s.hit.plt.df, n=s.hit.obs, s.hit.plt.df[num])
  }
  
  # condense dataframe down to 2 columns: team & name & position, the statistic variable column
  s.hit.plt.df <- s.hit.plt.df[c(42,as.numeric(num))]
  
  #rename columns to x,y for easier plotting
  colnames(s.hit.plt.df) <- c("x","y")
  
  # plotting funtion, if statistic is k percetn or k-bb percent, plot in reverse order
  if(s.hit.statistic %in% c("K Percent", "K-BB Percent")){
    p <-  ggplot(s.hit.plt.df, aes(x=reorder(x,-y), y=y,fill=y))+
      geom_bar(stat='identity')+
      ggtitle(paste(s.hit.year,"Season -","Top",length(s.hit.plt.df$x)), subtitle =paste(s.hit.statistic,"-", s.hit.lg)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.hit.plt.df,aes(x=x,y=y,label=y),size = 3, hjust=1, colour = "white") +
      scale_y_continuous(toupper(s.hit.statistic)) +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "red", high =  "dark blue") +
      coord_flip()
    p
  } else {
    p <-  ggplot(s.hit.plt.df, aes(x=reorder(x,y), y=y,fill=y))+
      geom_bar(stat='identity')+
      ggtitle(paste(s.hit.year,"Season -","Top",length(s.hit.plt.df$x)), subtitle =paste(s.hit.statistic,"-", s.hit.lg)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.hit.plt.df,aes(x=x,y=y,label=y),size = 3, hjust=1, colour = "white") +
      scale_y_continuous(toupper(s.hit.statistic)) +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "red", high =  "dark blue") +
      coord_flip()
    p
  }
}

# hitter - run function to create graph
s.hitter.leaderboard(w = s.hit.year, x = s.hit.statistic, y = s.hit.obs, z = s.hit.lg)

# ---------------------------------------------------------------------------------------
#Season Pitcher Graph Function

# pitcher - Set variables to subset and graph by
s.pitch.statistic <- "Win Percent" 
s.pitch.obs <- 15
s.pitch.lg <- "PBE"
s.pitch.year <- 2026


#pitcher function
s.pitcher.leaderboard <- function(w,x,y,z){
  # subset pitcher dataframe by league
  s.pitch.plt.df <- subset(s.all.pitch,s.all.pitch$league_abbr == s.pitch.lg & s.all.pitch$year == s.pitch.year)
  
  # if PBE subset dataframe by plate appearances greater than or equal to 760, if MiLPBE subset by PA greater than or equal to 470
  if(s.pitch.statistic %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Win Percent')){
    mean_ip <- round(mean(s.pitch.plt.df$`Innings Pitched`),0)
    s.pitch.plt.df <- subset(s.pitch.plt.df,s.pitch.plt.df$`Innings Pitched`>=mean_ip)
  } else {
    s.pitch.plt.df <- s.pitch.plt.df
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(s.pitch.plt.df)==s.pitch.statistic)
  
  
  # if statistic is K percent or K-BB percent, take the bottom obs, players with lower k-percents are better
  if(s.pitch.statistic %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent')){
    s.pitch.plt.df <- top_n(s.pitch.plt.df, n=s.pitch.obs, -s.pitch.plt.df[num])
  } else {
    s.pitch.plt.df <- top_n(s.pitch.plt.df, n=s.pitch.obs, s.pitch.plt.df[num])
  }
  
  # condense dataframe down to 2 columns: team & name & position, the statistic variable column
  s.pitch.plt.df <- s.pitch.plt.df[c(43,as.numeric(num))]
  
  #rename columns to x,y for easier plotting
  colnames(s.pitch.plt.df) <- c("x","y")
  
  # plotting funtion, if statistic is k percetn or k-bb percent, plot in reverse order
  if(s.pitch.statistic %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent')){
    p <-  ggplot(s.pitch.plt.df, aes(x=reorder(x,-y), y=y,fill=y))+
      geom_bar(stat='identity')+
      ggtitle(paste(s.pitch.year,"Season -","Top",length(s.pitch.plt.df$x)), subtitle =paste(s.pitch.statistic,"-", s.pitch.lg)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.pitch.plt.df,aes(x=x,y=y,label=y),size = 3, hjust=1, colour = "white") +
      scale_y_continuous(toupper(s.pitch.statistic)) +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "red", high =  "dark blue") +
      coord_flip()
    p
  } else {
    p <-  ggplot(s.pitch.plt.df, aes(x=reorder(x,y), y=y,fill=y))+
      geom_bar(stat='identity')+
      ggtitle(paste(s.pitch.year,"Season -","Top",length(s.pitch.plt.df$x)), subtitle =paste(s.pitch.statistic,"-", s.pitch.lg)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.pitch.plt.df,aes(x=x,y=y,label=y),size = 3, hjust=1, colour = "white") +
      scale_y_continuous(toupper(s.pitch.statistic)) +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "red", high =  "dark blue") +
      coord_flip()
    p
  }
}

# pitcher - run function to create graph
s.pitcher.leaderboard(w = s.pitch.year, x = s.pitch.statistic, y = s.pitch.obs, z = s.pitch.lg)


# Daily Standings

l <- 'PBE'


daily <- subset(ds.all_games,ds.all_games$league_abbr == l)
p.daily <- daily[c(4,9,14,18,31)]
colnames(p.daily) <- c("x","t","d","y","c")
p.daily$x <- as.Date(p.daily$x)
season <- substring(max(p.daily$x),1,4)


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
   ggtitle("Games Above/Below .500", subtitle = paste(l)) +
   theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
   ylab("Games Above/Below .500") + xlab("Date") +
   geom_dl(aes(label=t, color=t), method = list("last.points",cex = .75,hjust = .5, vjust = -.5)) +
   theme(legend.position = "none") + 
   scale_colour_manual(values=pbe.colors)
 p

 } else {
     p <-  ggplot(p.daily, aes(x=x, y=y, color = t))+
       geom_line() +
       ggtitle("Games Above/Below .500", subtitle = paste(l)) +
       theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
       ylab("Games Above/Below .500") + xlab("Date") +
       geom_dl(aes(label=t, color=t), method = list("last.points",cex = .75,hjust = .5, vjust = -.5)) +
       theme(legend.position = "none") + 
       scale_colour_manual(values=milpbe.colors)
p

 }
tbl.ds <- subset(ds.all_games,ds.all_games$league_abbr == l)
tbl.ds$date <- as.Date(tbl.ds$date)
tbl.ds <- tbl.ds %>% filter(date == max(date))
tbl.ds <- tbl.ds[c(10,14,18,19,21:23,30,32,33)]
colnames(tbl.ds)[colnames(tbl.ds) == 'team_name'] <-'Team Name'
colnames(tbl.ds)[colnames(tbl.ds) == 'division'] <-'Division'
colnames(tbl.ds)[colnames(tbl.ds) == 'below.500'] <-'Games Above/Below .500'
colnames(tbl.ds)[colnames(tbl.ds) == 'winloss'] <-'Win-Loss'
colnames(tbl.ds)[colnames(tbl.ds) == 'ttl_ra'] <-'Total Runs Against'
colnames(tbl.ds)[colnames(tbl.ds) == 'ttl_runs'] <-'Total Runs'
colnames(tbl.ds)[colnames(tbl.ds) == 'ttl_hits'] <-'Total Hits'
colnames(tbl.ds)[colnames(tbl.ds) == 'pythag_record'] <-'Pythag Record'
tbl.ds <- tbl.ds [c(1,2,9,10,6,5,7,4,8,3)]


## Records
tm = "Death Valley Scorpions"
pl.records <- subset(r.records,r.records$team_name == tm)
pl.color <- as.character(unique(pl.records$`Primary Color`))
pl.tcolor <- as.character(unique(pl.records$`Tertiary Color`))


mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = .75)),
  colhead = list(fg_params=list(cex = .75)),
  rowhead = list(fg_params=list(cex = .75)))
records.tbl <- pl.records %>% group_by(team_name) %>% summarise(`Seasons Played` = n(), `Playoff Apperances` = sum(made_playoffs), Championships = sum(won_playoffs), `Team Average Wins` = round(mean(w),0))
colnames(records.tbl)[colnames(records.tbl) == 'team_name'] <- 'Team'


p <- ggplot(pl.records,aes(x=year,y=w,group=abbr)) +
  geom_line(colour = pl.color) +
  geom_point(shape = 23, size = 3, fill = pl.tcolor, colour = "black") +
  ggtitle("Wins by Season", subtitle = paste(unique(pl.records$team_name),"-",unique(pl.records$league_abbr))) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
  ylab("Wins") + xlab("Year") +
  scale_x_continuous(breaks=seq(min(pl.records$year),max(pl.records$year),1)) +
  geom_text(x=mean(pl.records$year), y=mean(pl.records$Lg_Average_Wins), label=paste("League Aveage Wins -",unique(pl.records$Lg_Average_Wins)), vjust = -.5, size = 4) +
  geom_line(aes(x=year,y=Lg_Average_Wins),linetype = "longdash") +
  annotation_custom(tableGrob(records.tbl, theme = mytheme, rows = NULL), xmin=min(pl.records$year), xmax=mean(pl.records$year), ymin=mean(pl.records$w)+10, ymax=max(pl.records$w))
 
  
p    

## Volatility table
colnames(r.volatility)[colnames(r.volatility) == 'league_abbr'] <-'League'
colnames(r.volatility)[colnames(r.volatility) == 'team_name'] <-'Team'
r.volatility

## Season Table
season.info <- subset(r.records,r.records$team_name == tm)
season.info <- season.info[c(6,18,21:23,28,11)]
colnames(season.info)[colnames(season.info) == 'year'] <-'Year'
colnames(season.info)[colnames(season.info) == 'team_name'] <-'Team'
colnames(season.info)[colnames(season.info) == 'best_hitter_name'] <-'Best Hitter'
colnames(season.info)[colnames(season.info) == 'best_pitcher_name'] <-'Best Pitcher'
colnames(season.info)[colnames(season.info) == 'best_rookie_name'] <-'Best Rookie'
colnames(season.info)[colnames(season.info) == 'winloss'] <- 'Win-Loss'
colnames(season.info)[colnames(season.info) == 'pos'] <-'Final Divison Standing'
season.info <- season.info[order(season.info$Year),] 
season.info

lg.lup <- subset(r.volatility,r.volatility$team_name == tm)
lg.lup <- lg.lup[1] 
