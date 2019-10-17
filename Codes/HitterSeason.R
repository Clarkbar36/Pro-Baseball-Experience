whole_hitter <- "Eduardo Dinero"

whole_ab_filtered <- at_bats %>% filter(Hitter_full_name == tm_hitter) %>%
  group_by(Hitter_full_name, Hitter_position_name, Hitter_league_abbr, Hitter_team_abbr, Hitter_team_name) %>%
  summarise(ABs = sum(at_bat), Hits = sum(hit), Singles = sum(sngl), Doubles = sum(dbl), Triples = sum(trpl), 
            Homeruns = sum(hr), Walks = sum(bb), Stolen_Bases = sum(sb), RBIs = sum(rbi), Extra_Base_Hits = sum(ebh), 
            Strikeouts = sum(k), Sacrifice_Fly = sum(sac), Batting_Average = round(Hits/ABs,3), 
            On_Base_Percentage = round((Hits+Walks)/(ABs+Walks+Sacrifice_Fly),3))

whole_player_singles <- sample_n(singles, whole_ab_filtered$Singles[1])
whole_player_doubles <- sample_n(doubles, whole_ab_filtered$Doubles[1])
whole_player_triples <- sample_n(triples, whole_ab_filtered$Triples[1])
whole_player_homeruns <- sample_n(homeruns,whole_ab_filtered$Homeruns[1])
whole_player_outs <- sample_n(outs,(whole_ab_filtered$ABs - whole_ab_filtered$Hits))
whole_player_sf <- sample_n(sf,(whole_ab_filtered$Sacrifice_Fly))

plot_hits <- bind_rows(whole_player_homeruns,whole_player_singles, whole_player_doubles, whole_player_triples, whole_player_outs, whole_player_sf)
plot_hits %>%
  ggplot(aes(x=plt_x, y=plt_y, color = type, shape = type )) +
  ggtitle(paste(tm_hitter, sep = " ")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_shape_manual(values=c(1,2,7,5,4,6))+
  geom_mlb_stadium(stadium_segments = "all") +
  coord_fixed() +
  geom_spraychart()
