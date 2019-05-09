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
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(stringr))
suppressMessages(library(RColorBrewer))

source("~/Documents/GitHub/PBE/Codes/All-Time_Stats.R",local = TRUE)
source("~/Documents/GitHub/PBE/Codes/Season_Stats.R",local = TRUE)

# Define UI for application that draws a histogram

all.time.hitter.leaderboard <- function(x,y,z){
  # subset hitter dataframe by league
  hit.plt.df <- subset(c.all.hit,c.all.hit$league_abbr == z)
  
  # if PBE subset dataframe by plate appearances greater than or equal to 760, if MiLPBE subset by PA greater than or equal to 470
  if(x %in% c('Average','OBP','SLG','OPS','ISO','BABIP','K Percent','BB Percent','K-BB Percent')){
    mean_pa <- round(mean(hit.plt.df$`Plate Apperances`),0)
    hit.plt.df <- subset(hit.plt.df,hit.plt.df$`Plate Apperances`>=mean_pa)
  } else {
    hit.plt.df <- hit.plt.df
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(hit.plt.df)==x)
  
  
  # if statistic is K percent or K-BB percent, take the bottom obs, players with lower k-percents are better
  if(x %in% c("K Percent", "K-BB Percent")){
    hit.plt.df <- top_n(hit.plt.df, n=y, -hit.plt.df[num])
  } else {
    hit.plt.df <- top_n(hit.plt.df, n=y, hit.plt.df[num])
  }
  
  # condense dataframe down to 2 columns: name & position, the statistic variable column
  hit.plt.df <- hit.plt.df[c(36,as.numeric(num))]
  
  #rename columns to x,y for easier plotting
  colnames(hit.plt.df) <- c("pl.x","pl.y")
  
  # plotting funtion, if statistic is k percetn or k-bb percent, plot in reverse order
  if(x %in% c("K Percent", "K-BB Percent")){
    p <-  ggplot(hit.plt.df, aes(x=reorder(pl.x,-pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(hit.plt.df$pl.x),"All-Time"), subtitle =paste(x,"-", z)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=hit.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#E55D87", high =  "#5FC3E4")  +
      coord_flip()
    p
  } else {
    p <-  ggplot(hit.plt.df, aes(x=reorder(pl.x,pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste("Top",length(hit.plt.df$pl.x),"All-Time"), subtitle =paste(x,"-", z))  +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=hit.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
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
  if(x %in% c('Average','OBP','SLG','OPS','ISO','BABIP','K Percent','BB Percent','K-BB Percent')){
    mean_pa <- round(mean(s.hit.plt.df$`Plate Apperances`),0)
    s.hit.plt.df <- subset(s.hit.plt.df,s.hit.plt.df$`Plate Apperances`>=mean_pa)
  } else {
    s.hit.plt.df <- s.hit.plt.df
  }
  
  # find which column number the statistic variable is in the dataframe
  num <- which( colnames(s.hit.plt.df)==x)
  
  
  # if statistic is K percent or K-BB percent, take the bottom obs, players with lower k-percents are better
  if(x %in% c("K Percent", "K-BB Percent")){
    s.hit.plt.df <- top_n(s.hit.plt.df, n=y, -s.hit.plt.df[num])
  } else {
    s.hit.plt.df <- top_n(s.hit.plt.df, n=y, s.hit.plt.df[num])
  }
  
  # condense dataframe down to 2 columns: team & name & position, the statistic variable column
  s.hit.plt.df <- s.hit.plt.df[c(42,as.numeric(num))]
  
  #rename columns to x,y for easier plotting
  colnames(s.hit.plt.df) <- c("pl.x","pl.y")
  
  # plotting funtion, if statistic is k percetn or k-bb percent, plot in reverse order
  if(x %in% c("K Percent", "K-BB Percent")){
    p <-  ggplot(s.hit.plt.df, aes(x=reorder(pl.x,-pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste(w,"Season -","Top",length(s.hit.plt.df$pl.x)), subtitle =paste(x,"-", z)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.hit.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#E55D87", high =  "#5FC3E4") +
      coord_flip()
    p
  } else {
    p <-  ggplot(s.hit.plt.df, aes(x=reorder(pl.x,pl.y), y=pl.y,fill=pl.y))+
      geom_bar(stat='identity')+
      ggtitle(paste(w,"Season -","Top",length(s.hit.plt.df$pl.x)), subtitle =paste(x,"-", z)) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      geom_text(data=s.hit.plt.df,aes(x=pl.x,y=pl.y,label=pl.y),size = 4, hjust=1, colour = "black") +
      scale_y_continuous(toupper(x)) +
      theme(axis.title.y = element_blank()) +
      theme(legend.position = "none") +
      scale_fill_gradient(low = "#E55D87", high =  "#5FC3E4") +
      coord_flip()
    p
  }
}



ui <- fluidPage(
   

   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("stat",
                     "Hitter Statistic:",
                     c('At Bats',
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
                       'Walks',
                       'WAR')),
         numericInput("obs",
                      "Top n:",
                      10, min = 1, max = 100),
         selectInput("lg",
                     "Hitter League:",
                     c('PBE','MiLPBE')),
         sliderInput("year",
                     "Season",
                     min = 2017, max = 2026, step = 1,value = 2026)
         #actionButton("submit", "Submit")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("all.time.hitter_plot"),
         plotOutput("season.hitter_plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$all.time.hitter_plot <-  renderPlot({
     #input$submit
     all.time.hitter.leaderboard(x = input$stat, y = input$obs, z = input$lg)

   })
   
   output$season.hitter_plot <-  renderPlot({
  
   s.hitter.leaderboard(w = input$year, x = input$stat, y = input$obs, z = input$lg)

   }) 
}
# Run the application 
shinyApp(ui = ui, server = server)

