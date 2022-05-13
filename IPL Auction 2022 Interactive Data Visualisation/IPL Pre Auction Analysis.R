ipl_auction_players <- read_excel("IPL_2022_Auction_Players_List.xlsx")
ipl_auction_teams   <- read_excel("IPL_Team_Salary_Data.xlsx")

library(dplyr)

# Age Distribution
p1 <- ipl_auction_players %>%
        select(age) %>%
        group_by(age) %>%
        count(age) %>%
        plot_ly(type='bar', x = ~age, y = ~n, text = ~n, 
                textposition = 'outside', hoverinfo = "text") %>%
        layout(yaxis= list(title = "Number of Players", showticklabels = FALSE),
               xaxis= list(title = "Age", dtick = 1 ),
               title="<b>Players Age Distribution<b>") 

# ------------------------------------------------------------------------------

# Country Wise Players Status

players_count_country_wise <- ipl_auction_players %>%
                                select(country, status) %>%
                                group_by(country,status) %>%
                                count(country) %>%
                                ungroup() %>%
                                pivot_wider(names_from = status, values_from = n) 

players_count_country_wise <- mutate_if(players_count_country_wise, is.numeric, ~replace(., is.na(.), 0))

players_count_country_wise <- players_count_country_wise %>%
                                mutate(total = Capped + Uncapped + Associate)

text <- paste("Player Count:",players_count_country_wise$Capped,"
        ", paste(ta,":",as.character(players_count_country_wise$Uncapped),sep=""),sep="")




p2 <- plot_ly(data = players_count_country_wise, type='bar', 
              x = ~Capped, y = ~reorder(country,total),
              name = "Capped",
              hovertemplate = "%{x} Players:") %>%
        add_trace(x = ~Uncapped, name = "Uncapped") %>%
        add_trace(x = ~Associate, name = "Associate") %>%
        add_annotations(data = players_count_country_wise, x = ~total, y = ~country,
                        text = ~total, showarrow = FALSE, xanchor = 'left') %>%
        layout(barmode = "stack", 
               yaxis= list(title = "Country", ticksuffix = "  "),
               xaxis= list(title = "Count of Players"),
               legend = list(x = 0.9, y = 0.5),
               title="<b>Count of Players from Each Country and their Status<b>")
  
# ------------------------------------------------------------------------------


# State Associations

p3 <- ipl_auction_players %>%
        select(state) %>%
        group_by(state) %>%
        count(state) %>%
        drop_na() %>%
        arrange(desc(n)) %>%
        ungroup() %>% 
        top_n(10) %>%
        plot_ly(type='bar', x = ~reorder(state, desc(n)), y = ~n, 
                text = ~n , textposition = 'outside',hoverinfo = "text") %>%
          layout(yaxis= list(title = "Number of Players", showticklabels = FALSE),
                 xaxis= list(title = "State Association"),
                 title="<b>Top 10 State Associations of Indian Players<b>")

# ------------------------------------------------------------------------------


# IPL Veterans
p4 <- ipl_auction_players %>%
        select(first_name, last_name, skill, ipl_teams, last_season_team, ipl_caps) %>%
        arrange(desc(ipl_caps)) %>% 
        top_n(12, wt = ipl_caps) %>%
        plot_ly(type='bar', x = ~reorder(last_name, desc(ipl_caps)), 
                y = ~ipl_caps,
                text = ~ipl_caps,
                textposition = 'outside',
          
                hovertext = ~paste("Skill:", skill, "\n", 
                                   "IPL Teams:", ipl_teams, "\n", 
                                   "Last Season Team:", last_season_team),
                hoverinfo = "text") %>%
                layout(yaxis= list(title = "IPL Matches Played", showticklabels = FALSE),
                       xaxis= list(title = "Players"),
                       title="<b>IPL Veterans in the Auction<b>")  

# ------------------------------------------------------------------------------

#Bowling Styles

bowling_styles <- ipl_auction_players %>%
                    select(last_name, bowling_style) %>%
                    drop_na() %>%
                    mutate(new_bowling_style = str_replace(bowling_style, pattern = "ARM ", "ARM,")) %>%
                    mutate(bowling_arm = sub('\\s*,.*','', new_bowling_style)) %>%
                    mutate(bowling_type = gsub(".*,","",new_bowling_style)) %>%
                    select(bowling_type, bowling_arm) %>%
                    group_by(bowling_type, bowling_arm) %>%
                    count(bowling_type) %>%
                    ungroup() %>%
                    pivot_wider(names_from = "bowling_arm", values_from = "n" )


bowling_styles <- mutate_if(bowling_styles, is.numeric, ~replace(., is.na(.), 0))

bowling_styles <- bowling_styles %>%
                    rename(left_arm = `LEFT ARM`, right_arm = `RIGHT ARM`) %>%
                    mutate(total = left_arm + right_arm) %>%
                    arrange(desc(total))


p5 <- plot_ly(data = bowling_styles, type='bar', 
              x = ~reorder(bowling_type,desc(total)), y = ~right_arm,
              name = "Right Arm",
              hovertemplate = "%{y} are") %>%
        add_trace(y = ~left_arm, name = "Left Arm") %>%
        add_annotations(data = bowling_styles, x = ~bowling_type, y = ~total + 3,
                        text = ~total, showarrow = FALSE) %>%
        layout(barmode = "stack", yaxis= list(title = "Number of Bowlers", showticklabels = FALSE),
               xaxis= list(title = "Bowling Style"),legend = list(x = 0.9, y = 0.9),
               title="<b>Bowling Styles<b>")

# ------------------------------------------------------------------------------

# Country Wise Players Skill

players_count_country_skill_wise <- ipl_auction_players %>%
                                      select(country, skill) %>%
                                      group_by(country,skill) %>%
                                      count(country) %>%
                                      ungroup() %>%
                                      pivot_wider(names_from = skill, values_from = n) 

players_count_country_skill_wise <- mutate_if(players_count_country_skill_wise, is.numeric, ~replace(., is.na(.), 0))

players_count_country_skill_wise <- players_count_country_skill_wise %>%
                                      mutate(total = `ALL-ROUNDER` + BATSMAN + BOWLER + WICKETKEEPER) %>%
                                      arrange(desc(total))



# ------------------------------------------------------------------------------



# Skills Distribution

DF <- ipl_auction_players %>%
            select(skill, country) %>%
            group_by(skill, country) %>%
            count(skill) %>%
            ungroup() 

as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE){
  require(data.table)
  
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    DT[, root := "Total"]  
  }
  
  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
  
  if(is.null(value_column) && add_root){
    setcolorder(DT, c("root", colNamesDF))
  } else if(!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }
  
  hierarchyList <- list()
  
  for(i in seq_along(hierarchy_columns)){
    current_columns <- colNamesDT[1:i]
    if(is.null(value_column)){
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    setnames(currentDT, length(current_columns), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parent_columns) := NULL]
  return(hierarchyDT)
}


sunburstDF <- as.sunburstDF(DF, value_column = "n", add_root = TRUE)


p6 <- plot_ly(data = sunburstDF, ids = ~ids, labels= ~labels, parents = ~parents, 
              values= ~values, type='sunburst', branchvalues = 'total') %>%
      layout(title="<b>Players Skill Count (Country-Wise)<b>")


# ------------------------------------------------------------------------------

# Team Data

ipl_auction_teams


p7 <- plot_ly(data = ipl_auction_teams,
              type = 'scatterpolar',
              fill = 'toself',
              mode = "markers",
              r = ipl_auction_teams$salary_cap_remaining, 
              theta = ipl_auction_teams$team_short_name,
              text = ~team_short_name,
              hoverinfo = "text",
              hovertext = ~paste("Amount Remaining:", salary_cap_remaining, "Crs"),
              name = 'Amount Remaining for Each Team',
              marker = list(size = 7.5)) %>%
        
        add_trace(r = ipl_auction_teams$player_slots_available,
                  theta = ipl_auction_teams$team_short_name,
                  name = 'Players Slots Available',
                  hoverinfo = "text",
                  hovertext = ~paste("Player Slots Available:", player_slots_available))  %>%

        add_trace(r = ipl_auction_teams$overseas_players_slots_available,
                  theta = ipl_auction_teams$team_short_name,
                  name = 'Overseas Players Slots Available',
                  hoverinfo = "text",
                  hovertext = ~paste("Overseas Player Slots Available:", overseas_players_slots_available))  %>%
        layout(polar = list(radialaxis = list(visible = F)),
               title="<b>Budget Remaining for Teams and Player Slots Availability<b>")


# ------------------------------------------------------------------------------

# Integrating with Plotly


Sys.setenv("plotly_username"="Devrajr")
Sys.setenv("plotly_api_key"="Jm5KvDtw6FwJVzQMwVow")

api_create(p1, filename = "ipl-players-age")

api_create(p2, filename = "IPL Auction 2022: Players Country and Status")

api_create(p3, filename = "IPL Auction 2022: Players State Associations")

api_create(p4, filename = "IPL Auction 2022: Veterans in Auction")

api_create(p5, filename = "IPL Auction 2022: Bowlers")

api_create(p6, filename = "IPL Auction 2022: Players Skill and Nationality")

api_create(p7, filename = "IPL Auction 2022: Teams Data")



