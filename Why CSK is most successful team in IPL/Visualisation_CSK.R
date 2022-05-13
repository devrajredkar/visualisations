install.packages("readxl") 
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("fmsb")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("ggalt")
install.packages("ggrepel")


library(readxl) # For Importing Excel File
library(dplyr) # For Data Manipulation
library(tidyr) # For Data Manipulation
library(stringr) # For Data Manipulation
library(fmsb) # For Radarchart
library(ggplot2) # For Plotting
library(gridExtra) # For Combining Two Plots into one
library(ggalt) # For Dumbbell Plot
library(ggrepel) # For Text Labels and Boxes on Plots


# ------------------------------- Importing Data -------------------------------

# Importing IPL MatchbyMatch Data

ipl_match_data <- read_excel("ipl_all_seasons_summary.xlsx")


# Importing IPL Points Table Data

ipl_points_table_data <- read_excel("ipl_all_seasons_points_table.xlsx")


# Importing IPL Batting Scorecard Data

ipl_batting_scorecard_data <- read_excel("ipl_all_seasons_batting_card.xlsx")


# Importing IPL Bowling Scorecard Data

ipl_bowling_scorecard_data <- read_excel("ipl_all_seasons_bowling_card.xlsx")


# ------------------------------- Data Pre-Processing --------------------------

# Calculating Matches Played By Teams

home_matches <- ipl_match_data %>%
                  filter(toss_won != "no toss") %>%
                  select(season, id, home_team, toss_won) %>%
                  group_by(home_team) %>%
                  count(home_team)

away_matches <- ipl_match_data %>%
                  filter(toss_won != "no toss") %>%
                  select(season, id, away_team, toss_won) %>%
                  group_by(away_team) %>%
                  count(away_team)

matches_data <- home_matches %>%
                inner_join(away_matches, by = c("home_team" = "away_team"), 
                           suffix = c("_mts_home", "_mts_away")) %>%
                mutate(total_matches = sum(n_mts_home, n_mts_away)) %>%
                arrange(desc(total_matches)) %>%
                rename(team = "home_team", home_matches_played = "n_mts_home", 
                       away_matches_played = "n_mts_away")


# Calculating Home Wins, Away Wins, Total Wins and Win Percentage

home_wins <-  ipl_match_data %>%
                filter(toss_won != "no toss") %>%
                filter(winner != "No Result") %>%
                select(season, id, home_team, winner) %>%
                mutate(is_home_win = case_when(home_team == winner ~ 1, 
                                               home_team != winner ~0 )) %>%
                group_by(home_team) %>%
                summarise(total_home_wins = sum(is_home_win))

away_wins <-  ipl_match_data %>%
                filter(toss_won != "no toss") %>%
                filter(winner != "No Result") %>%
                select(season, id, away_team, winner) %>%
                mutate(is_away_win = case_when(away_team == winner ~ 1, 
                                               away_team != winner ~0 )) %>%
                group_by(away_team) %>%
                summarise(total_away_wins = sum(is_away_win))

wins_data <-  home_wins %>%
                inner_join(away_wins, by = c("home_team" = "away_team")) %>%
                mutate(total_wins = total_home_wins + total_away_wins) %>%
                arrange(desc(total_wins)) %>%
                rename(team = "home_team")

win_percentage <- matches_data %>%
                    inner_join(wins_data, by = "team") %>%
                    mutate(win_percent = total_wins / total_matches * 100) %>%
                    arrange(desc(win_percent))


# Calculating Loss

matches_lost_data <-  ipl_match_data %>%
                        filter(toss_won != "no toss") %>%
                        filter(winner != "No Result") %>%
                        select(id, home_team, away_team, winner) %>%
                        mutate(loser = case_when(winner == home_team ~ away_team, 
                                                 winner == away_team ~ home_team)) %>%
                        group_by(loser) %>%
                        count(loser) %>%
                        arrange(desc(n)) %>%
                        rename(team = "loser", total_loss = "n")

# No of Titles Won

titles_won <- ipl_match_data %>%
                filter(str_detect(description, pattern = "Final")) %>%
                filter(!grepl('Elimination', description) & 
                       !grepl('Semi', description) & 
                       !grepl('Qualifying', description)) %>%
                select(season, description, winner) %>%
                group_by(winner) %>%
                count(winner) %>%
                arrange(desc(n)) %>%
                rename(team = "winner", no_of_titles = "n")


# Fours and Sixes Hit by Teams

fours_sixes <-  ipl_batting_scorecard_data %>%
                  select(current_innings, fours, sixes) %>%
                  drop_na() %>%
                  group_by(current_innings) %>%
                  summarise(fours = sum(fours), sixes = sum(sixes)) %>%
                  rename(team = "current_innings")


# Calculating Wickets Taken By Teams

home_wickets <- ipl_match_data %>%
                  filter(toss_won != "no toss") %>%
                  drop_na() %>%
                  select(season, id, home_team, home_wickets) %>%
                  group_by(home_team) %>%
                  summarise(home_wickets = sum(home_wickets))

away_wickets <- ipl_match_data %>%
                  filter(toss_won != "no toss") %>%
                  drop_na() %>%
                  select(season, id, away_team, away_wickets) %>%
                  group_by(away_team) %>%
                  summarise(away_wickets = sum(away_wickets))

wickets_data <- home_wickets %>%
                  inner_join(away_wickets, by = c("home_team" = "away_team"), 
                             suffix = c("_wts_home", "_wts_away")) %>%
                  mutate(total_wickets = home_wickets + away_wickets) %>%
                  arrange(desc(total_wickets)) %>%
                  rename(team = "home_team")


# Calculating Runs Scored By Teams

home_runs <-  ipl_match_data %>%
                filter(toss_won != "no toss") %>%
                drop_na() %>%
                select(season, id, home_team, home_runs) %>%
                group_by(home_team) %>%
                summarise(home_runs = sum(home_runs))

away_runs <-  ipl_match_data %>%
                filter(toss_won != "no toss") %>%
                drop_na() %>%
                select(season, id, away_team, away_runs) %>%
                group_by(away_team) %>%
                summarise(away_runs = sum(away_runs))

runs_data <-  home_runs %>%
                inner_join(away_runs, by = c("home_team" = "away_team"), 
                           suffix = c("_runs_home", "_runs_away")) %>%
                mutate(total_runs = home_runs + away_runs) %>%
                arrange(desc(total_runs)) %>%
                rename(team = "home_team")

data <- win_percentage %>%
          select(team, total_matches, total_wins, total_home_wins, 
          total_away_wins, win_percent) %>%
          left_join(titles_won, by = "team") %>%
          replace_na(list(no_of_titles = 0)) %>%
          left_join(fours_sixes, by = "team") %>%
          left_join(wickets_data, by = "team") %>%
          left_join(runs_data, by = "team") %>%
          select(-c(home_wickets,away_wickets, home_runs, away_runs)) %>%
          filter(team == "CSK")


# -------------------------- Figure 1: Dumbbell Chart --------------------------

main_data <-  win_percentage %>%
                select(team, total_matches, total_wins, total_home_wins, 
                       total_away_wins, win_percent) %>%
                left_join(titles_won, by = "team") %>%
                replace_na(list(no_of_titles = 0)) %>%
                left_join(fours_sixes, by = "team") %>%
                left_join(wickets_data, by = "team") %>%
                left_join(runs_data, by = "team") %>%
                select(-c(home_wickets,away_wickets, home_runs, away_runs)) %>%
                filter(team %in% c("CSK", "DC", "KKR", "MI", "PBKS", "RCB", "RR", "SRH")) %>%
                ungroup()

min1 <- main_data %>%
          arrange(total_matches) %>%
          slice(1:1) %>%
          mutate(parameter = "min_value", col = "total_matches") %>% 
          select(parameter, total_matches, team, col) 

min2 <- main_data %>%
          arrange(total_wins) %>%
          slice(1:1) %>%
          mutate(parameter = "min_value", col = "total_wins") %>% 
          select(parameter, total_wins, team, col) 

min3 <- main_data %>%
          arrange(total_home_wins) %>%
          slice(1:1) %>%
          mutate(parameter = "min_value", col = "total_home_wins") %>% 
          select(parameter, total_home_wins, team, col) 

min4 <- main_data %>%
          arrange(total_away_wins) %>%
          slice(1:1) %>%
          mutate(parameter = "min_value", col = "total_away_wins") %>% 
          select(parameter, total_away_wins, team, col) 

min5 <- main_data %>%
          arrange(win_percent) %>%
          slice(1:1) %>%
          mutate(parameter = "min_value", win_percent_new = round(win_percent,0), 
                 col = "win_percent") %>% 
          select(parameter, win_percent = win_percent_new, team, col) 

minimum_values <- min1 %>%
                    left_join(min2, by = "parameter") %>%
                    left_join(min3, by = "parameter") %>%
                    left_join(min4, by = "parameter") %>%
                    left_join(min5, by = "parameter")

max1 <- main_data %>%
          arrange(desc(total_matches)) %>%
          slice(1:1) %>%
          mutate(parameter = "max_value", col = "total_matches") %>% 
          select(parameter, total_matches, team, col) 

max2 <- main_data %>%
          arrange(desc(total_wins)) %>%
          slice(1:1) %>%
          mutate(parameter = "max_value", col = "total_wins") %>% 
          select(parameter, total_wins, team, col) 

max3 <- main_data %>%
          arrange(desc(total_home_wins)) %>%
          slice(1:1) %>%
          mutate(parameter = "max_value", col = "total_home_wins") %>% 
          select(parameter, total_home_wins, team, col) 

max4 <- main_data %>%
          arrange(desc(total_away_wins)) %>%
          slice(1:1) %>%
          mutate(parameter = "max_value", col = "total_away_wins") %>% 
          select(parameter, total_away_wins, team, col) 

max5 <- main_data %>%
          arrange(desc(win_percent)) %>%
          slice(1:1) %>%
          mutate(parameter = "max_value", col = "win_percent") %>% 
          select(parameter, win_percent, team, col) 

maximum_values <- max1 %>%
                    left_join(max2, by = "parameter") %>%
                    left_join(max3, by = "parameter") %>%
                    left_join(max4, by = "parameter") %>%
                    left_join(max5, by = "parameter")

min_data <- minimum_values %>%
              select(parameter, total_matches, total_wins, total_home_wins, 
                     total_away_wins, win_percent) %>%
              pivot_longer(!parameter, names_to = "variable", 
                           values_to = "value_min")

max_data <- maximum_values %>%
              select(parameter, total_matches, total_wins, total_home_wins,
                     total_away_wins, win_percent) %>%
              pivot_longer(!parameter, names_to = "variable", 
                           values_to = "value_max")

csk_data <- data %>%
              select(-no_of_titles,-total_runs) %>%
              pivot_longer(!team, names_to = "variable", values_to = "value_csk")


dumbbell_plot_data <- min_data %>%
                        inner_join(max_data, by = "variable") %>%
                        inner_join(csk_data, by = "variable") %>%
                        select(variable, value_min, value_max, value_csk) %>%
                        rename(Minimum = "value_min", Maximum = "value_max",
                               CSK = "value_csk") 

min_teams <- rbind(min1[,c(3,4)],min2[,c(3,4)],min3[,c(3,4)],min4[,c(3,4)], 
                   min5[,c(3,4)])
max_teams <- rbind(max1[,c(3,4)],max2[,c(3,4)],max3[,c(3,4)],max4[,c(3,4)], 
                   max5[,c(3,4)])

dumbbell_plot_data_v2 <-  dumbbell_plot_data %>%
  left_join(min_teams, by = c("variable" = "col")) %>%
  left_join(max_teams, by = c("variable" = "col")) %>%
  rename(min_team_name = "team.x", max_team_name = "team.y") %>%
  mutate(Minimum_Label = paste("Min:", Minimum, "by", min_team_name),
         Maximum_Label = paste("Max:", Maximum, "by", max_team_name),
         CSK_Label = paste("CSK:", CSK))

dumbbell_plot <-  dumbbell_plot_data_v2 %>%
                    ggplot(aes(x = Minimum, xend = Maximum,
                               y = factor(variable,c('total_away_wins', 
                                                     'total_home_wins',
                                                     'win_percent','total_wins',
                                                     'total_matches')))) +
                    geom_dumbbell(size = 4) +
                    scale_x_continuous(limits = c(0,250), 
                                       breaks = seq(0, 250, by = 20)) +
                    geom_point(aes(x = CSK, y = variable, color = 'value1', 
                                   size = "value2")) +
                    scale_colour_manual(values = c("value1" = "yellow")) +
                    scale_size_manual(values = c("value2" = 3)) +
                    theme_bw() +
                    theme(plot.title = element_text(face = "bold", hjust = 0.5),
        
                          panel.grid.minor = element_blank(),
                          panel.grid.major.x = element_line(),
                          panel.grid.major.y = element_line(),
                          panel.border = element_rect(color = "black", size = 2),
        
                          axis.ticks = element_blank(),
                          axis.text.y = element_text(face = "bold", size = 10),
        
                          legend.position = "none") +
                    labs(x = "Value", y = "Parameters",
                         title = "Comparison of CSK with Minimum and Maximum Value for Each Parameter") +
                    scale_y_discrete(labels=c("Total Away Wins", "Total Home Wins", 
                                              "Win Percent","Total Wins", 
                                              "Total Matches Played")) +
  
                    geom_label_repel(data = dumbbell_plot_data_v2,
                                     aes(x = CSK, y = variable, label = CSK_Label),
                                     nudge_y  = 0.2) +
  
                    geom_label_repel(data = dumbbell_plot_data_v2,
                                     aes(x = Minimum, y = variable,
                                         label = Minimum_Label),
                                     nudge_y  = -0.1, nudge_x = -20) +
  
                    geom_label_repel(data = dumbbell_plot_data_v2,
                                     aes(x = Maximum, y = variable,
                                         label = Maximum_Label),
                                     nudge_y  = -0.1, nudge_x = 20)



# -------------------------- Figure 2: Bar Charts ------------------------------

home_captain <- ipl_match_data %>%
                  filter(toss_won != "no toss") %>%
                  select(id, home_team, home_captain) %>%
                  group_by(home_team, home_captain) %>%
                  count(home_captain) %>%
                  arrange(desc(n)) %>%
                  rename(home_matches_as_captain = "n")

away_captain <- ipl_match_data %>%
                  filter(toss_won != "no toss") %>%
                  select(id, away_team, away_captain) %>%
                  group_by(away_team, away_captain) %>%
                  count(away_captain) %>%
                  arrange(desc(n)) %>%
                  rename(away_matches_as_captain = "n")

captains <- away_captain %>%
              left_join(home_captain, by = c("away_team" = "home_team", 
                                             "away_captain" = "home_captain")) %>%
              replace_na(list(home_matches_as_captain = 0)) %>%
              mutate(total_matches_as_captain = away_matches_as_captain + 
                       home_matches_as_captain) %>%
              arrange(desc(total_matches_as_captain)) %>%
              filter(total_matches_as_captain > 5) %>%
              rename(team = "away_team", captain = "away_captain") %>%
              arrange(team, desc(total_matches_as_captain)) 

no_of_captains_data <-  captains %>%
                          select(team, captain)  %>%
                          group_by(team) %>%
                          count(team) %>%
                          rename(no_of_captains = "n") %>%
                          filter(team %in% c("CSK", "DC", "KKR", "MI", "PBKS", 
                                             "RCB", "RR", "SRH")) %>%
                          arrange(no_of_captains)

titles_won_full_list <- no_of_captains_data %>%
                          left_join(titles_won, by = "team" ) %>%
                          replace_na(list(no_of_titles = 0)) 

plot1 <-  ggplot(data=titles_won_full_list, aes(x = team, y = no_of_captains,
                                                fill = team)) +
          geom_bar(stat = "identity", position = "identity", width  = 0.5) +
          scale_y_continuous(limits = c(0,10), breaks = seq(1, 10, by = 1)) +
          labs(y = "Total Captains", 
               title = "Total Captains and Championships for Each Team") +
          theme_minimal() +
          theme(plot.title = element_text(face = "bold", hjust = 0.5),
                plot.background = element_blank(),
        
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
        
                legend.position = "none",
        
                axis.text.x = element_text(face = "bold", color = "black",
                                           size = 10,margin = margin(t = -10, 
                                                                     r = 0,
                                                                     b = 0, 
                                                                     l = 0,
                                                                     unit = "pt")),
                axis.text.y = element_text(face = "bold", color = "black"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(face = "bold", color = "black",
                                            margin = margin(t = 0, r =5,
                                                            b = 0, l = 0,
                                                            unit = "pt")),
                aspect.ratio = 2/5) +
                scale_fill_manual(values = c("CSK" = "Yellow3", "KKR" = "Purple",
                                             "MI"  = "royalblue", "SRH" = "Orange2",
                                             "DC"  = "navy","RCB" = "Red4",
                                             "PBKS"= "Red","RR"  = "hotpink"))

plot2 <-  ggplot(data=titles_won_full_list, aes(x = team, y = no_of_titles,
                                                fill = team)) +
          geom_bar(stat = "identity", position = "identity", width  = 0.5) +
          scale_y_continuous(limits = c(0,5), breaks = seq(0, 5, by = 1)) +
          labs(x = "Teams", y = "Total Championships") +
          theme_minimal() +
          theme(plot.title = element_text(face = "bold", hjust = 0.5),
                plot.background = element_blank(),
        
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
        
                legend.position = "none",
                aspect.ratio = 2/5,
        
                axis.text.x = element_text(face = "bold", color = "black",
                                           size = 10,
                                           margin = margin(t = -10, r = 0,b = 0,
                                                           l = 0,unit = "pt")),
                axis.text.y = element_text(face = "bold", color = "black"),
                axis.title.x = element_text(face = "bold", color = "black",
                                            margin = margin(t = 10, r = 0,b = 0,
                                                            l = 0,unit = "pt")),
                axis.title.y = element_text(face = "bold", color = "black",
                                            margin = margin(t = 0, r =10,b = 0,
                                                            l = 0,unit = "pt"))) +
                scale_fill_manual(values = c("CSK" = "Yellow3", "KKR" = "Purple",
                                             "MI"  = "royalblue", "SRH" = "Orange2",
                                             "DC"  = "navy", "RCB" = "Red4",
                                             "PBKS"= "Red","RR"  = "hotpink"))

bar_chart <- grid.arrange(plot1, plot2, nrow=2)


# -------------------------- Figure 3: Radar Chart -----------------------------

points_table <- ipl_points_table_data %>%
                  select(season, name, short_name, rank,matchpoints) %>%
                  filter(name == "Chennai Super Kings") %>%
                  select(season, rank) %>%
                  arrange(season) %>%
                  pivot_wider(names_from = season, values_from = rank) %>%
                  rbind(1) %>%
                  rbind(8)

points_table <- rbind(points_table[c(3),], points_table[c(2),], points_table[c(1),])

spider_chart <- radarchart(points_table,
                           axistype = 1,
                           seg = 7, 
                           title = "CSK's Season-wise Rank in Points Table",
                           plwd = 4,
                           axislabcol = "black",
                           cglcol = "grey",
                           cglty = 1,
                           caxislabels = seq(1,8,1),
                           pcol = "yellow3",
                           pfcol = rgb(1,1,0.5,0.4))



# -------------------------- Figure 4: Line Chart ------------------------------

home_runs_match_wise <- ipl_match_data %>%
                          filter(toss_won != "no toss") %>%
                          filter(winner != "No Result") %>%
                          select(id, season, home_team, home_runs) %>%
                          arrange(home_runs) %>%
                          rename(team = "home_team", runs = "home_runs") %>%
                          filter(team %in% c("CSK", "DC", "KKR", "MI", "PBKS",
                                             "RCB", "RR", "SRH"))

away_runs_match_wise <- ipl_match_data %>%
                          filter(toss_won != "no toss") %>%
                          filter(winner != "No Result") %>%
                          select(id, season, away_team, away_runs) %>%
                          arrange(away_runs) %>%
                          rename(team = "away_team", runs = "away_runs") %>%
                          filter(team %in% c("CSK", "DC", "KKR", "MI", "PBKS",
                                             "RCB", "RR", "SRH"))

inns <- rbind(home_runs_match_wise, away_runs_match_wise) %>%
          arrange(season, id, desc(runs)) %>%
          group_by(season, team) %>%
          count(team)

runs <- rbind(home_runs_match_wise, away_runs_match_wise) %>%
          arrange(season, id, desc(runs)) %>%
          group_by(season, team) %>%
          summarise(runs = sum(runs))

line_plot_data <- runs %>%
                    inner_join(inns, by = c("team","season")) %>%
                    mutate(runs_per_inns = runs / n) %>%
                    arrange(season, desc(runs_per_inns)) %>%
                    filter(team %in% c("CSK", "KKR", "MI", "SRH"))

line_plot <-  ggplot(line_plot_data, aes(x = season, y = runs_per_inns, 
                                         color = team)) +
              geom_line() +
              geom_point(aes(x = season, y = runs_per_inns)) +
              scale_x_continuous(limits = c(2008,2021), breaks = seq(2008, 2021, by = 1)) +
              scale_y_continuous(limits = c(130,180), breaks = seq(130, 180, by = 5)) +
              scale_color_manual(values = c("CSK" = "Yellow3","KKR" = "Purple",
                                            "MI"  = "royalblue","SRH" = "Orange2")) +
              labs(x = "Year", y = "Average Runs Per Innings",
                   title = "Analysing Runs Per Innings for Top 4 IPL Teams",
                   color = "Teams",
                   caption = "Data for CSK not available for 2016 and 2017 due to non-participation") +
              theme_bw() +
              theme(axis.title = element_text(face = "bold", colour = "black"),
        
                    plot.title = element_text(face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0.9),
        
                    panel.grid.minor = element_blank(),
                    panel.border = element_rect(color = "black", size = 2),
        
                    legend.position = c(0.96,0.13),
                    legend.background = element_rect(fill = "white", 
                                                     color = "black"))

# ------------------------------------------------------------------------------



