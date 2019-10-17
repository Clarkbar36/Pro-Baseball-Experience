suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(stringr))
suppressMessages(library(RColorBrewer))
library(GeomMLBStadiums)
library(sp)
setwd("~/Documents/GitHub/PBE/")

stadiums <- MLBStadiumsPathData

# Single Coordinates

generic <-  stadiums %>% filter(team == "generic")

# extra Base Hits
generic_exbh <- generic %>% filter(segment != "infield_outer" & y >= 155)
generic_outfield <- generic %>% filter(segment == "outfield_outer")
generic_doubles_triples <- union(generic_exbh,generic_outfield)

simulated_hits = data.frame(plt_x = runif(50000, 31, 219),
                            plt_y = runif(50000, 17, 200))


exbh <- generic_doubles_triples %>% select(x,y)
exbh_if_poly = Polygon(exbh)

in_bounds <- sp::point.in.polygon(simulated_hits$plt_x,simulated_hits$plt_y,exbh_if_poly@coords[,1],exbh_if_poly@coords[,2])
in_bounds <- data.frame(in_poly = in_bounds)
simulated_hits <- bind_cols(simulated_hits,in_bounds)

trpl_dbl <- simulated_hits %>% 
  filter(in_poly == 1 & plt_y <= 98)

trp_dbl_type = sample(c('2B','3B'),size = nrow(trpl_dbl),prob = c(0.5,0.5),replace = TRUE)
trpl_dbl$type = trp_dbl_type
trpl_dbl$shape <- 16
trpl_dbl$color <- "#34F10F"

doubles <- trpl_dbl %>% filter(type == "2B")
triples <- trpl_dbl %>% filter(type == "3B")

# triples %>%
#   ggplot(aes(x=plt_x, y=plt_y, label = in_poly )) + 
#   geom_mlb_stadium(stadium_segments = "all") + 
#   coord_fixed() +
#   geom_spraychart()

# singles
singl1 <- simulated_hits %>% 
  filter(in_poly == 1 & plt_y >= 100 & plt_y <= 155)

# singl1 %>%
#   ggplot(aes(x=plt_x, y=plt_y, label = in_poly )) + 
#   geom_mlb_stadium(stadium_segments = "all") + 
#   coord_fixed() +
#   geom_spraychart()

generic <-  stadiums %>% filter(team == "generic")
generic_single <- generic %>% filter(segment == "infield_outer" & y >= 155)

simulated_singles = data.frame(plt_x = runif(10000, 81, 169),
                               plt_y = runif(10000, 155, 197))
sing <- generic_single %>% select(x,y)
sing_if_poly = Polygon(sing)

sing_in_bounds <- sp::point.in.polygon(simulated_singles$plt_x,simulated_singles$plt_y,sing_if_poly@coords[,1],sing_if_poly@coords[,2])
sing_in_bounds <- data.frame(in_poly = sing_in_bounds)
simulated_singles <- bind_cols(simulated_singles,sing_in_bounds)

singl2 <- simulated_singles %>% 
  filter(in_poly == 1)

singles <- union(singl1, singl2)

singles$type <- "1B"
singles$shape <- 16
singles$color <- "#0F87F1"

# singles %>%
#   ggplot(aes(x=plt_x, y=plt_y, label = in_poly )) + 
#   geom_mlb_stadium(stadium_segments = "all") + 
#   coord_fixed() +
#   geom_spraychart()


# Home runs

generic_hr <-  stadiums %>% filter(team == "generic" & segment == "outfield_outer" & y <= 100) 
# plot(generic_hr$x, generic_hr$y)
simulated_hrs <- data.frame(plt_x = runif(50000,30,220),
                            plt_y = runif(50000,10,95))

hr <- generic_hr %>% select(x,y)
hr_if_poly = Polygon(hr)

hr_in_bounds <- sp::point.in.polygon(simulated_hrs$plt_x,simulated_hrs$plt_y,hr_if_poly@coords[,1],hr_if_poly@coords[,2])
hr_in_bounds <- data.frame(in_poly = hr_in_bounds)
simulated_hrs <- bind_cols(simulated_hrs,hr_in_bounds)

homeruns <- simulated_hrs %>%
  filter(in_poly == 0) %>%
  mutate(type = "HR", shape = 15, color = "#F10FEA")

# outs %>%
#   ggplot(aes(x=plt_x, y=plt_y, label = in_poly )) +
#   geom_mlb_stadium(stadium_segments = "all") +
#   coord_fixed() +
#   geom_spraychart()

flyouts <- bind_rows(doubles)
flyouts$type <- "FO"
flyouts$shape <- 4
flyouts$color <- "#F1200F"

groundouts <- bind_rows(singles)
groundouts$type <- "GO"
groundouts$shape <- 4
groundouts$color <- "#F1200F"

sf <- bind_rows(doubles)
sf$type <- "SF"
sf$shape <- 4
sf$color <- "#F1200F"
