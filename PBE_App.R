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
library(rsconnect)

source("Codes/All-Time_Stats.R",local = TRUE)
source("Codes/Season_Stats.R",local = TRUE)
source("Codes/Team_Scatter.R",local = TRUE)

# Define UI for application that draws a histogram

all.time.hitter.leaderboard <- function(x,y,z){
  # subset hitter dataframe by league
  hit.plt.df <- subset(c.all.hit,c.all.hit$league_abbr == z)
  
  # if PBE subset dataframe by plate appearances greater than or equal to 760, if MiLPBE subset by PA greater than or equal to 470
  if(x %in% c('Average','OBP','SLG','OPS','ISO','BABIP','K Percent','BB Percent','K-BB Percent','Strikeouts')){
    mean_pa <- round(mean(hit.plt.df$`Plate Apperances`),0)
    hit.plt.df <- subset(hit.plt.df,hit.plt.df$`Plate Apperances`>=mean_pa)
  } else {
    hit.plt.df <- hit.plt.df
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(hit.plt.df)==x)
  
  
  # if statistic is K percent or K-BB percent, take the bottom obs, players with lower k-percents are better
  if(x %in% c("K Percent", "K-BB Percent",'Strikeouts')){
    hit.plt.df <- top_n(hit.plt.df, n=y, -hit.plt.df[num])
  } else {
    hit.plt.df <- top_n(hit.plt.df, n=y, hit.plt.df[num])
  }
  
  # condense dataframe down to 2 columns: name & position, the statistic variable column
  hit.plt.df <- hit.plt.df[c(36,as.numeric(num))]
  
  #rename columns to x,y for easier plotting
  colnames(hit.plt.df) <- c("pl.x","pl.y")
  hit.plt.df <- filter(hit.plt.df, pl.y != 0)
  
  # plotting funtion, if statistic is k percetn or k-bb percent, plot in reverse order
  if(x %in% c("K Percent", "K-BB Percent",'Strikeouts')){
    p <-  ggplot(hit.plt.df, aes(x=reorder(pl.x,-pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(hit.plt.df$pl.x),"Batters All-Time"), subtitle =paste(x,"-", z)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=hit.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#5FC3E4", high =  "#E55D87")  +
      coord_flip()
    p
  } else {
    p <-  ggplot(hit.plt.df, aes(x=reorder(pl.x,pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(hit.plt.df$pl.x),"Batters All-Time"), subtitle =paste(x,"-", z))  +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=hit.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#E55D87", high =  "#5FC3E4")  +
      coord_flip()
    p
  }
}

s.hitter.leaderboard <- function(w,x,y,z){
  # subset hitter dataframe by league
  s.hit.plt.df <- subset(s.all.hit,s.all.hit$league_abbr == z & s.all.hit$year == w)
  
  # if PBE subset dataframe by plate appearances greater than or equal to 760, if MiLPBE subset by PA greater than or equal to 470
  if(x %in% c('Average','OBP','SLG','OPS','ISO','BABIP','K Percent','BB Percent','K-BB Percent','Strikeouts')){
    mean_pa <- round(mean(s.hit.plt.df$`Plate Apperances`),0)
    s.hit.plt.df <- subset(s.hit.plt.df,s.hit.plt.df$`Plate Apperances`>=mean_pa)
  } else {
    s.hit.plt.df <- s.hit.plt.df
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(s.hit.plt.df)==x)
  
  
  # if statistic is K percent or K-BB percent, take the bottom obs, players with lower k-percents are better
  if(x %in% c("K Percent", "K-BB Percent",'Strikeouts')){
    s.hit.plt.df <- top_n(s.hit.plt.df, n=y, -s.hit.plt.df[num])
  } else {
    s.hit.plt.df <- top_n(s.hit.plt.df, n=y, s.hit.plt.df[num])
  }
  
  # condense dataframe down to 2 columns: team & name & position, the statistic variable column
  s.hit.plt.df <- s.hit.plt.df[c(42,as.numeric(num))]
  
  #rename columns to x,y for easier plotting
  colnames(s.hit.plt.df) <- c("pl.x","pl.y")
  s.hit.plt.df <- filter(s.hit.plt.df, pl.y != 0)
  
  # plotting funtion, if statistic is k percetn or k-bb percent, plot in reverse order
  if(x %in% c("K Percent", "K-BB Percent",'Strikeouts')){
    p <-  ggplot(s.hit.plt.df, aes(x=reorder(pl.x,-pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste(w,"Season -","Top",length(s.hit.plt.df$pl.x), "Batters"), subtitle =paste(x,"-", z)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.hit.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#5FC3E4", high =  "#E55D87")  +
      coord_flip()
    p
  } else {
    p <-  ggplot(s.hit.plt.df, aes(x=reorder(pl.x,pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste(w,"Season -","Top",length(s.hit.plt.df$pl.x), "Batters"), subtitle =paste(x,"-", z)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.hit.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#E55D87", high =  "#5FC3E4") +
      coord_flip()
    p
  }
}


all.time.pitcher.leaderboard <- function(x,y,z){
  # subset pitcher dataframe by league
  pitch.plt.df <- subset(c.all.pitch,c.all.pitch$league_abbr == z)
  
  # if PBE subset dataframe by Innings Pitched greater than or equal to 400, if MiLPBE subset by IP greater than or equal to 160
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Win Percent','K percent', 'K-BB percent')){
    mean_ip <- round(mean(pitch.plt.df$`Innings Pitched`),0)
    pitch.plt.df <- subset(pitch.plt.df,pitch.plt.df$`Innings Pitched`>=mean_ip)
  } else {
    pitch.plt.df <- pitch.plt.df
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(pitch.plt.df)==x)
  
  
  # if statistic is a ratio, reverse order
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent')){
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
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent')){
    p <-  ggplot(pitch.plt.df, aes(x=reorder(pl.x,-pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(pitch.plt.df$pl.x),"Pitchers All-Time"), subtitle =paste(x,"-", z)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=pitch.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#5FC3E4", high =  "#E55D87")  +
      coord_flip()
    p
  } else {
    p <-  ggplot(pitch.plt.df, aes(x=reorder(pl.x,pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(pitch.plt.df$pl.x),"Pitchers All-Time"), subtitle =paste(x,"-", z))  +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=pitch.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#E55D87", high =  "#5FC3E4") +
      coord_flip()
    p
  }
}


s.pitcher.leaderboard <- function(w,x,y,z){
  # subset pitcher dataframe by league
  s.pitch.plt.df <- subset(s.all.pitch,s.all.pitch$league_abbr == z & s.all.pitch$year == w)
  
  # if PBE subset dataframe by plate appearances greater than or equal to 760, if MiLPBE subset by PA greater than or equal to 470
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent', 'Win Percent', 'K percent', 'K-BB percent')){
    mean_ip <- round(mean(s.pitch.plt.df$`Innings Pitched`),0)
    s.pitch.plt.df <- subset(s.pitch.plt.df,s.pitch.plt.df$`Innings Pitched`>=mean_ip)
  } else {
    s.pitch.plt.df <- s.pitch.plt.df
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(s.pitch.plt.df)==x)
  
  
  # if statistic is K percent or K-BB percent, take the bottom obs, players with lower k-percents are better
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent')){
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
  if(x %in% c('ERA', 'WHIP','BABIP','FIP','HR per 9','R per 9','Hits per 9','BB per 9','BB percent')){
    p <-  ggplot(s.pitch.plt.df, aes(x=reorder(pl.x,-pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste(w,"Season -","Top",length(s.pitch.plt.df$pl.x), "Pitchers"), subtitle =paste(x,"-", y)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.pitch.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#5FC3E4", high =  "#E55D87")  +
      coord_flip()
    p
  } else {
    p <-  ggplot(s.pitch.plt.df, aes(x=reorder(pl.x,pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste(w,"Season -","Top",length(s.pitch.plt.df$pl.x),"Pitchers"), subtitle =paste(x,"-", y)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.pitch.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.y = element_text(face="bold", size = 10))+
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#E55D87", high =  "#5FC3E4") +
      coord_flip()
    p
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
    menuItem("Hitter Leaderboard", tabName = "HitterL", icon = icon("chart-bar")),
    menuItem("Pitcher Leaderboard", tabName = "PitcherL", icon = icon("chart-bar")),
    menuItem("Team Scatter", tabName = "TmSctpl", icon = icon("baseball-ball")),
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
                                   'Walks'),selected = 'WAR'),
                     numericInput("obs",
                                  "Top n:",
                                  10, min = 1, max = 100)
              ),
              column(width = 6,
                     selectInput("lg",
                                 "League:",
                                 c('PBE','MiLPBE')),
                     sliderInput("year",
                                 "Season",
                                 min = 2017, max = 2027, step = 1,value = 2027,sep = "")
                     
              )),
            
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
              column(width = 6,
                     selectInput("plg",
                                 "League:",
                                 c('PBE','MiLPBE')),
                     sliderInput("pyear",
                                 "Season",
                                 min = 2017, max = 2027, step = 1,value = 2027,sep = "")
              )),
            fluidRow(
              column(width = 6,
                     plotOutput("all.time.pitcher_plot")
              ),
              column( width = 6,
                      plotOutput("season.pitcher_plot")
              )
            )
            
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
              column(width = 10, plotOutput("team_scatter_plot")
              )),
            fluidRow(
              dataTableOutput("team_table")
            )
    )
  )
)



ui <- dashboardPage(title = 'Pro Baseball Experience Hub', header, sidebar, body, skin='purple')


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$all.time.hitter_plot <-  renderPlot({
    #input$submit
    all.time.hitter.leaderboard(x = input$stat, y = input$obs, z = input$lg)
    
  })
  
  output$season.hitter_plot <-  renderPlot({
    
    s.hitter.leaderboard(w = input$year, x = input$stat, y = input$obs, z = input$lg)
    
  }) 
  
  output$all.time.pitcher_plot <-  renderPlot({
    #input$submit
    all.time.pitcher.leaderboard(x = input$pstat, y = input$pobs, z = input$plg)
    
  })
  
  output$season.pitcher_plot <-  renderPlot({
    #input$submit
    s.pitcher.leaderboard(w = input$pyear, x = input$pstat, y = input$pobs, z = input$plg)
    
  })
  
  output$team_scatter_plot <-  renderPlot({
    #input$submit
    tm.scatter(l = input$splg,x = input$xsct,y = input$ysct)
    
  })
  
  output$team_table <-  renderDataTable(options = list(dom = 'tip',paging = FALSE),rownames= FALSE,{
    #input$submit
    tm.tbl(l = input$splg,x = input$xsct,y = input$ysct)
    
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

