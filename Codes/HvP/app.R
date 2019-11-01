#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(GeomMLBStadiums)
HittervsAll <- function(hitter){
    
    hva_ab_filtered <- at_bats %>% filter(Hitter_full_name == hitter) %>%
        group_by(Hitter_full_name, Hitter_position_name, Hitter_league_abbr, Hitter_team_abbr, Hitter_team_name) %>%
        summarise(ABs = sum(at_bat), Hits = sum(hit), Singles = sum(sngl), Doubles = sum(dbl), Triples = sum(trpl), 
                  Homeruns = sum(hr), Walks = sum(bb), Stolen_Bases = sum(sb), RBIs = sum(rbi), Extra_Base_Hits = sum(ebh), 
                  Strikeouts = sum(k), Sacrifice_Fly = sum(sac), Batting_Average = round(Hits/ABs,3), 
                  On_Base_Percentage = round((Hits+Walks)/(ABs+Walks+Sacrifice_Fly),3), Groundouts = sum(go), Flyouts = sum(fo))
    
    
    
    hva_player_singles <- sample_n(singles, hva_ab_filtered$Singles[1])
    hva_player_doubles <- sample_n(doubles, hva_ab_filtered$Doubles[1])
    hva_player_triples <- sample_n(triples, hva_ab_filtered$Triples[1])
    hva_player_homeruns <- sample_n(homeruns,hva_ab_filtered$Homeruns[1])
    hva_player_flyouts <- sample_n(flyouts,hva_ab_filtered$Flyouts[1])
    hva_player_groundouts <- sample_n(groundouts,hva_ab_filtered$Groundouts[1])
    
    plot_hits <- bind_rows(hva_player_homeruns,hva_player_singles, hva_player_doubles, hva_player_triples,
                           hva_player_flyouts, hva_player_groundouts)
    
    hit_theme <- plot_hits %>% select(type,shape,color) %>% distinct() %>% arrange(type) %>% mutate(size = 2)
    hit_shapes <- as.integer(hit_theme$shape)
    hit_colors <- as.character(hit_theme$color)
    hit_size <- as.integer(hit_theme$size)
    
    plt <- plot_hits %>%
        ggplot(aes(x=plt_x, y=plt_y, color = type, shape = type, size = type )) +
        ggtitle(paste(hitter,"vs All Pitchers", sep = " ")) +
        theme(axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              plot.title = element_text(hjust = 0.5),
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
    
    plt
}

HittervsAllTBL <- function(hitter){
    
    hva_ab_filtered <- at_bats %>% filter(Hitter_full_name == hitter) %>%
        group_by(Hitter_full_name, Hitter_position_name, Hitter_league_abbr, Hitter_team_abbr, Hitter_team_name) %>%
        summarise(ABs = sum(at_bat), Hits = sum(hit), Singles = sum(sngl), Doubles = sum(dbl), Triples = sum(trpl), 
                  Homeruns = sum(hr), Walks = sum(bb), Stolen_Bases = sum(sb), RBIs = sum(rbi), Extra_Base_Hits = sum(ebh), 
                  Strikeouts = sum(k), Sacrifice_Fly = sum(sac), Batting_Average = round(Hits/ABs,3), 
                  On_Base_Percentage = round((Hits+Walks)/(ABs+Walks+Sacrifice_Fly),3), Groundouts = sum(go), Flyouts = sum(fo))
    hva_ab_filtered
}

PitchervsAll <- function(pitcher){
    
    pva_ab_filtered <- at_bats %>% filter(Pitcher_full_name == pitcher) %>%
        group_by(Pitcher_full_name, Pitcher_league_abbr, Pitcher_team_abbr, Pitcher_team_name) %>%
        summarise(ABs = sum(at_bat), Hits = sum(hit), Singles = sum(sngl), Doubles = sum(dbl), Triples = sum(trpl), 
                  Homeruns = sum(hr), Walks = sum(bb), Stolen_Bases = sum(sb), RBIs = sum(rbi), Extra_Base_Hits = sum(ebh), 
                  Strikeouts = sum(k), Sacrifice_Fly = sum(sac), Batting_Average = round(Hits/ABs,3), 
                  On_Base_Percentage = round((Hits+Walks)/(ABs+Walks+Sacrifice_Fly),3), Groundouts = sum(go), Flyouts = sum(fo))
    
    
    
    pva_player_singles <- sample_n(singles, pva_ab_filtered$Singles[1])
    pva_player_doubles <- sample_n(doubles, pva_ab_filtered$Doubles[1])
    pva_player_triples <- sample_n(triples, pva_ab_filtered$Triples[1])
    pva_player_homeruns <- sample_n(homeruns,pva_ab_filtered$Homeruns[1])
    pva_player_flyouts <- sample_n(flyouts,pva_ab_filtered$Flyouts[1])
    pva_player_groundouts <- sample_n(groundouts,pva_ab_filtered$Groundouts[1])
    
    plot_hits <- bind_rows(pva_player_homeruns,pva_player_singles, pva_player_doubles, pva_player_triples,
                           pva_player_flyouts, pva_player_groundouts)
    
    hit_theme <- plot_hits %>% select(type,shape,color) %>% distinct() %>% arrange(type) %>% mutate(size = 2)
    hit_shapes <- as.integer(hit_theme$shape)
    hit_colors <- as.character(hit_theme$color)
    hit_size <- as.integer(hit_theme$size)
    
    plt <- plot_hits %>%
        ggplot(aes(x=plt_x, y=plt_y, color = type, shape = type, size = type )) +
        ggtitle(paste(pitcher,"vs All Hitters", sep = " ")) +
        theme(axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              plot.title = element_text(hjust = 0.5),
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
    
    plt
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("test"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('hitter_select', 'Select Hitter', c(hitter_names), selectize=FALSE),
            selectInput('pitcher_select', 'Select Pitcher', c(pitcher_names), selectize=FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("hit"),
           dataTableOutput("hit_tbl"),
           plotOutput("pitch")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$hit <- renderPlot({
        HittervsAll(hitter = input$hitter_select)
    })
    
    output$hit_tbl <-  renderDataTable(options = list(dom = 't',paging = FALSE,searching = FALSE),{
        #input$submit
        HittervsAllTBL(hitter = input$hitter_select)
        
    })
    
    output$pitch <- renderPlot({
        PitchervsAll(pitcher = input$pitcher_select)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
