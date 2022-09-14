library(shiny)
library(shinydashboard)
library(ranger)
library(ggplot2)
library(dplyr)
library(markdown)


#Read in data
#with_tm_totals_yr df
df <- read.csv("per_game.csv", stringsAsFactors = F)

usg_df <- read.csv("usage_df.csv", stringsAsFactors = F)

#Read in models
point_mod <- readRDS("points_rf_mod.RDS")

assist_mod <- readRDS("assist_rf_mod.RDS")

rebound_mod <- readRDS("rebounds_rf_mod.RDS")

usage_mod <- readRDS("usage_rf_mod.RDS")

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$pointbox <- renderValueBox({
        #Get position
        is_pg <- ifelse(input$position == "PG", 1, 0)
        is_sg <- ifelse(input$position == "SG", 1, 0)
        is_sf <- ifelse(input$position == "SF", 1, 0)
        is_pf <- ifelse(input$position == "PF", 1, 0)
        is_c <- ifelse(input$position == "C", 1, 0)
        #Get dataframe
        pred_df <- data.frame(conference_name = as.factor(input$conference), ranking = as.numeric(input$rank), pg = is_pg, 
                              sg = is_sg, sf = is_sf, pf = is_pf, c = is_c, inches = input$height)
        #Get predictions
        point_pred_val <- predict(point_mod, pred_df)$predictions
        
        #point_pred_df$pointpred <- point_pred_val
        
        #Do same for assists and rebounds
        
        #rebound_pred_val <- predict(rebound_mod, pred_df)$predictions
        
        #assist_pred_val <- predict(assist_mod, pred_df)$predictions
        
        
        #Get final data frame
        #final_df <- data.frame(Points = point_pred_val, Rebounds = rebound_pred_val, Assists = assist_pred_val)
        #final_df
        valueBox(
            round(point_pred_val,2), "Points Per Game", icon = icon("signal", lib = "glyphicon"), color = "blue"
        )
    })
    output$reboundsbox <- renderValueBox({
        #Get position
        is_pg <- ifelse(input$position == "PG", 1, 0)
        is_sg <- ifelse(input$position == "SG", 1, 0)
        is_sf <- ifelse(input$position == "SF", 1, 0)
        is_pf <- ifelse(input$position == "PF", 1, 0)
        is_c <- ifelse(input$position == "C", 1, 0)
        #Get dataframe
        pred_df <- data.frame(conference_name = as.factor(input$conference), ranking = as.numeric(input$rank), pg = is_pg, 
                              sg = is_sg, sf = is_sf, pf = is_pf, c = is_c, inches = input$height)
        #Get predictions
        #point_pred_val <- predict(point_mod, pred_df)$predictions
        
        #point_pred_df$pointpred <- point_pred_val
        
        #Do same for assists and rebounds
        
        rebound_pred_val <- predict(rebound_mod, pred_df)$predictions
        
        #assist_pred_val <- predict(assist_mod, pred_df)$predictions
        
        
        #Get final data frame
        #final_df <- data.frame(Points = point_pred_val, Rebounds = rebound_pred_val, Assists = assist_pred_val)
        #final_df
        valueBox(
            round(rebound_pred_val,2), "Rebounds Per Game", icon = icon("eject", lib = "glyphicon"), color = "red"
        )
    })
    output$assistbox <- renderValueBox({
        #Get position
        is_pg <- ifelse(input$position == "PG", 1, 0)
        is_sg <- ifelse(input$position == "SG", 1, 0)
        is_sf <- ifelse(input$position == "SF", 1, 0)
        is_pf <- ifelse(input$position == "PF", 1, 0)
        is_c <- ifelse(input$position == "C", 1, 0)
        #Get dataframe
        pred_df <- data.frame(conference_name = as.factor(input$conference), ranking = as.numeric(input$rank), pg = is_pg, 
                              sg = is_sg, sf = is_sf, pf = is_pf, c = is_c, inches = input$height)
        #Get predictions
        #point_pred_val <- predict(point_mod, pred_df)$predictions
        
        #point_pred_df$pointpred <- point_pred_val
        
        #Do same for assists and rebounds
        
        #rebound_pred_val <- predict(rebound_mod, pred_df)$predictions
        
        assist_pred_val <- predict(assist_mod, pred_df)$predictions
        
        
        #Get final data frame
        #final_df <- data.frame(Points = point_pred_val, Rebounds = rebound_pred_val, Assists = assist_pred_val)
        #final_df
        valueBox(
            round(assist_pred_val,2), "Assists Per Game", icon = icon("screenshot", lib = "glyphicon"), color = "yellow"
        )
    })
    output$usagebox <- renderValueBox({
        #Get position
        is_pg <- ifelse(input$position == "PG", 1, 0)
        is_sg <- ifelse(input$position == "SG", 1, 0)
        is_sf <- ifelse(input$position == "SF", 1, 0)
        is_pf <- ifelse(input$position == "PF", 1, 0)
        is_c <- ifelse(input$position == "C", 1, 0)
        #Get dataframe
        pred_df <- data.frame(conference_name = as.factor(input$conference), ranking = as.numeric(input$rank), pg = is_pg, 
                              sg = is_sg, sf = is_sf, pf = is_pf, c = is_c, inches = input$height)
        #Get predictions
        #point_pred_val <- predict(point_mod, pred_df)$predictions
        
        #point_pred_df$pointpred <- point_pred_val
        
        #Do same for assists and rebounds
        
        usage_pred_val <- predict(usage_mod, pred_df)$predictions
        
        #assist_pred_val <- predict(assist_mod, pred_df)$predictions
        
        
        #Get final data frame
        #final_df <- data.frame(Points = point_pred_val, Rebounds = rebound_pred_val, Assists = assist_pred_val)
        #final_df
        valueBox(
            paste0(round(usage_pred_val,2),"%"), "Usage", icon = icon("repeat", lib = "glyphicon"), color = "green"
        )
    })
    output$pointsplot <- renderPlot({
        is_pg <- ifelse(input$position == "PG", 1, 0)
        is_sg <- ifelse(input$position == "SG", 1, 0)
        is_sf <- ifelse(input$position == "SF", 1, 0)
        is_pf <- ifelse(input$position == "PF", 1, 0)
        is_c <- ifelse(input$position == "C", 1, 0)
        #Get dataframe
        pred_df <- data.frame(conference_name = as.factor(input$conference), ranking = as.numeric(input$rank), pg = is_pg, 
                              sg = is_sg, sf = is_sf, pf = is_pf, c = is_c, inches = input$height)
        #Get predictions
        point_pred_val <- predict(point_mod, pred_df)$predictions
        points_avg <- aggregate(points~player_id+season, data = df, FUN = mean)
        points_avg$points <- round(points_avg$points)
        sub_rank <- points_avg %>%
            filter(points > 0)
        sub_rank$cuts <- cut(sub_rank$points,40,labels = FALSE)
        colored_cut <- unique(sub_rank$cuts[sub_rank$points == round(point_pred_val)])
        sub_rank$color <- ifelse(sub_rank$cuts == colored_cut, T, F)
        ggplot(sub_rank) + geom_histogram(aes(x = points, fill = color), binwidth = 1,
                                             breaks = seq(0,30,1),
                                             color = "black", boundary = 0.5) + ylab("Count") +
                                             xlab("Points") + ggtitle("Distribution of Points") + 
                                             theme(legend.position="none") + scale_fill_manual(values=c("#999999", "dodgerblue2"))
        
    })
    output$pointsplot_pos <- renderPlot({
        is_pg <- ifelse(input$position == "PG", 1, 0)
        is_sg <- ifelse(input$position == "SG", 1, 0)
        is_sf <- ifelse(input$position == "SF", 1, 0)
        is_pf <- ifelse(input$position == "PF", 1, 0)
        is_c <- ifelse(input$position == "C", 1, 0)
        #Get dataframe
        pred_df <- data.frame(conference_name = as.factor(input$conference), ranking = as.numeric(input$rank), pg = is_pg, 
                              sg = is_sg, sf = is_sf, pf = is_pf, c = is_c, inches = input$height)
        #Get predictions
        point_pred_val <- predict(point_mod, pred_df)$predictions
        if(input$position == "PG") {
            by_pos <- df %>%
                filter(pg == 1)
        }
        else if(input$position == "SG") {
            by_pos <- df %>%
                filter(sg == 1)
        }
        else if(input$position == "SF") {
            by_pos <- df %>%
                filter(sf == 1)
        }
        else if(input$position == "PF") {
            by_pos <- df %>%
                filter(pf == 1)
        }
        else if(input$position == "C") {
            by_pos <- df %>%
                filter(c == 1)
        }
        points_avg <- aggregate(points~player_id+season, data = by_pos, FUN = mean)
        points_avg$points <- round(points_avg$points)
        sub_rank <- points_avg %>%
            filter(points > 0)
        sub_rank$cuts <- cut(sub_rank$points,40,labels = FALSE)
        colored_cut <- unique(sub_rank$cuts[sub_rank$points == round(point_pred_val)])
        sub_rank$color <- ifelse(sub_rank$cuts == colored_cut, T, F)
        ggplot(sub_rank) + geom_histogram(aes(x = points, fill = color), binwidth = 1,
                                          breaks = seq(0,30,1),
                                          color = "black", boundary = 0.5) + ylab("Count") +
            xlab("Points") + ggtitle("Distribution of Points") + 
            theme(legend.position="none") + scale_fill_manual(values=c("#999999", "dodgerblue2"))
        
    })
    output$reboundsplot <- renderPlot({
        is_pg <- ifelse(input$position == "PG", 1, 0)
        is_sg <- ifelse(input$position == "SG", 1, 0)
        is_sf <- ifelse(input$position == "SF", 1, 0)
        is_pf <- ifelse(input$position == "PF", 1, 0)
        is_c <- ifelse(input$position == "C", 1, 0)
        #Get dataframe
        pred_df <- data.frame(conference_name = as.factor(input$conference), ranking = as.numeric(input$rank), pg = is_pg, 
                              sg = is_sg, sf = is_sf, pf = is_pf, c = is_c, inches = input$height)
        #Get predictions
        rebound_pred_val <- predict(rebound_mod, pred_df)$predictions
        rebounds_avg <- aggregate(rebounds~player_id+season, data = df, FUN = mean)
        rebounds_avg$rebounds <- round(rebounds_avg$rebounds)
        sub_rank <- rebounds_avg %>%
            filter(rebounds > 0)
        sub_rank$cuts <- cut(sub_rank$rebounds,25,labels = FALSE)
        colored_cut <- unique(sub_rank$cuts[sub_rank$rebounds == round(rebound_pred_val)])
        sub_rank$color <- ifelse(sub_rank$cuts == colored_cut, T, F)
        ggplot(sub_rank) + geom_histogram(aes(x = rebounds, fill = color), binwidth = 1,
                                          breaks = seq(0,25,1),
                                          color = "black", boundary = 1) + ylab("Count") +
            xlab("Rebounds") + ggtitle("Distribution of Rebounds") + 
            theme(legend.position="none") + scale_fill_manual(values=c("#999999", "red"))
        #sub_rank %>% select(rebounds, cuts, color)
    })
    output$reboundsplot_pos <- renderPlot({
        is_pg <- ifelse(input$position == "PG", 1, 0)
        is_sg <- ifelse(input$position == "SG", 1, 0)
        is_sf <- ifelse(input$position == "SF", 1, 0)
        is_pf <- ifelse(input$position == "PF", 1, 0)
        is_c <- ifelse(input$position == "C", 1, 0)
        #Get dataframe
        pred_df <- data.frame(conference_name = as.factor(input$conference), ranking = as.numeric(input$rank), pg = is_pg, 
                              sg = is_sg, sf = is_sf, pf = is_pf, c = is_c, inches = input$height)
        #Get predictions
        rebound_pred_val <- predict(rebound_mod, pred_df)$predictions
        if(input$position == "PG") {
            by_pos <- df %>%
                filter(pg == 1)
        }
        else if(input$position == "SG") {
            by_pos <- df %>%
                filter(sg == 1)
        }
        else if(input$position == "SF") {
            by_pos <- df %>%
                filter(sf == 1)
        }
        else if(input$position == "PF") {
            by_pos <- df %>%
                filter(pf == 1)
        }
        else if(input$position == "C") {
            by_pos <- df %>%
                filter(c == 1)
        }
        rebounds_avg <- aggregate(rebounds~player_id+season, data = by_pos, FUN = mean)
        rebounds_avg$rebounds <- round(rebounds_avg$rebounds)
        sub_rank <- rebounds_avg %>%
            filter(rebounds > 0)
        sub_rank$cuts <- cut(sub_rank$rebounds,25,labels = FALSE)
        colored_cut <- unique(sub_rank$cuts[sub_rank$rebounds == round(rebound_pred_val)])
        sub_rank$color <- ifelse(sub_rank$cuts == colored_cut, T, F)
        ggplot(sub_rank) + geom_histogram(aes(x = rebounds, fill = color), binwidth = 1,
                                          breaks = seq(0,25,1),
                                          color = "black", boundary = 1) + ylab("Count") +
            xlab("Rebounds") + ggtitle("Distribution of Rebounds") + 
            theme(legend.position="none") + scale_fill_manual(values=c("#999999", "red"))
        #sub_rank %>% select(rebounds, cuts, color)
    })
    output$assistsplot <- renderPlot({
        is_pg <- ifelse(input$position == "PG", 1, 0)
        is_sg <- ifelse(input$position == "SG", 1, 0)
        is_sf <- ifelse(input$position == "SF", 1, 0)
        is_pf <- ifelse(input$position == "PF", 1, 0)
        is_c <- ifelse(input$position == "C", 1, 0)
        #Get dataframe
        pred_df <- data.frame(conference_name = as.factor(input$conference), ranking = as.numeric(input$rank), pg = is_pg, 
                              sg = is_sg, sf = is_sf, pf = is_pf, c = is_c, inches = input$height)
        #Get predictions
        assist_pred_val <- predict(assist_mod, pred_df)$predictions
        assists_avg <- aggregate(assists~player_id+season, data = df, FUN = mean)
        assists_avg$assists <- as.numeric(round(assists_avg$assists))
        sub_rank <- assists_avg %>%
            filter(assists > 0)
        sub_rank$cuts <- cut(sub_rank$assists,18,labels = FALSE)
        colored_cut <- unique(sub_rank$cuts[sub_rank$assists == round(assist_pred_val)])
        sub_rank$color <- ifelse(sub_rank$cuts == colored_cut, T, F)
        ggplot(sub_rank) + geom_histogram(aes(x = assists, fill = color), binwidth = 1,
                                          breaks = seq(0,18,1),
                                          color = "black", boundary = 1) + ylab("Count") +
            xlab("Assists") + ggtitle("Distribution of Assists") + 
            theme(legend.position="none") + scale_fill_manual(values=c("#999999", "goldenrod2"))
        #sub_rank %>% select(rebounds, cuts, color)
    })
    output$assistsplot_pos <- renderPlot({
        is_pg <- ifelse(input$position == "PG", 1, 0)
        is_sg <- ifelse(input$position == "SG", 1, 0)
        is_sf <- ifelse(input$position == "SF", 1, 0)
        is_pf <- ifelse(input$position == "PF", 1, 0)
        is_c <- ifelse(input$position == "C", 1, 0)
        #Get dataframe
        pred_df <- data.frame(conference_name = as.factor(input$conference), ranking = as.numeric(input$rank), pg = is_pg, 
                              sg = is_sg, sf = is_sf, pf = is_pf, c = is_c, inches = input$height)
        #Get predictions
        assist_pred_val <- predict(assist_mod, pred_df)$predictions
        if(input$position == "PG") {
            by_pos <- df %>%
                filter(pg == 1)
        }
        else if(input$position == "SG") {
            by_pos <- df %>%
                filter(sg == 1)
        }
        else if(input$position == "SF") {
            by_pos <- df %>%
                filter(sf == 1)
        }
        else if(input$position == "PF") {
            by_pos <- df %>%
                filter(pf == 1)
        }
        else if(input$position == "C") {
            by_pos <- df %>%
                filter(c == 1)
        }
        by_pos$assists <- as.numeric(by_pos$assists)
        assists_avg <- aggregate(assists~player_id+season, data = by_pos, FUN = mean)
        assists_avg$assists <- round(as.numeric(assists_avg$assists))
        sub_rank <- assists_avg %>%
            filter(assists > 0)
        sub_rank$cuts <- cut(sub_rank$assists,18,labels = FALSE)
        colored_cut <- unique(sub_rank$cuts[sub_rank$assists == round(assist_pred_val)])
        sub_rank$color <- ifelse(sub_rank$cuts == colored_cut, T, F)
        ggplot(sub_rank) + geom_histogram(aes(x = assists, fill = color), binwidth = 1,
                                          breaks = seq(0,18,1),
                                          color = "black", boundary = 1) + ylab("Count") +
            xlab("Assists") + ggtitle("Distribution of Assists") + 
            theme(legend.position="none") + scale_fill_manual(values=c("#999999", "goldenrod2"))
        #sub_rank %>% select(rebounds, cuts, color)
    })
    output$usageplot <- renderPlot({
        is_pg <- ifelse(input$position == "PG", 1, 0)
        is_sg <- ifelse(input$position == "SG", 1, 0)
        is_sf <- ifelse(input$position == "SF", 1, 0)
        is_pf <- ifelse(input$position == "PF", 1, 0)
        is_c <- ifelse(input$position == "C", 1, 0)
        #Get dataframe
        pred_df <- data.frame(conference_name = as.factor(input$conference), ranking = as.numeric(input$rank), pg = is_pg, 
                              sg = is_sg, sf = is_sf, pf = is_pf, c = is_c, inches = input$height)
        #Get predictions
        usage_pred_val <- predict(usage_mod, pred_df)$predictions
        sub_rank <- usg_df %>%
            filter(usage > 0)
        sub_rank$usage <- round(sub_rank$usage)
        sub_rank$cuts <- cut(sub_rank$usage,110,labels = FALSE)
        colored_cut <- unique(sub_rank$cuts[sub_rank$usage == round(usage_pred_val)])
        sub_rank$color <- ifelse(sub_rank$cuts == colored_cut, T, F)
        ggplot(sub_rank) + geom_histogram(aes(x = usage, fill = color), binwidth = 1,
                                          breaks = seq(0,50,1),
                                          color = "black", boundary = 1) + ylab("Count") +
            xlab("Usage") + ggtitle("Distribution of Usage") + 
            theme(legend.position="none") + scale_fill_manual(values=c("#999999", "springgreen3"))
        #sub_rank %>% select(rebounds, cuts, color)
    })
    output$usageplot_pos <- renderPlot({
        is_pg <- ifelse(input$position == "PG", 1, 0)
        is_sg <- ifelse(input$position == "SG", 1, 0)
        is_sf <- ifelse(input$position == "SF", 1, 0)
        is_pf <- ifelse(input$position == "PF", 1, 0)
        is_c <- ifelse(input$position == "C", 1, 0)
        #Get dataframe
        pred_df <- data.frame(conference_name = as.factor(input$conference), ranking = as.numeric(input$rank), pg = is_pg, 
                              sg = is_sg, sf = is_sf, pf = is_pf, c = is_c, inches = input$height)
        #Get predictions
        usage_pred_val <- predict(usage_mod, pred_df)$predictions
        if(input$position == "PG") {
            by_pos <- usg_df %>%
                filter(pg == 1)
        }
        else if(input$position == "SG") {
            by_pos <- usg_df %>%
                filter(sg == 1)
        }
        else if(input$position == "SF") {
            by_pos <- usg_df %>%
                filter(sf == 1)
        }
        else if(input$position == "PF") {
            by_pos <- usg_df %>%
                filter(pf == 1)
        }
        else if(input$position == "C") {
            by_pos <- usg_df %>%
                filter(c == 1)
        }
        sub_rank <- by_pos %>%
            filter(usage > 0)
        sub_rank$usage <- round(sub_rank$usage)
        sub_rank$cuts <- cut(sub_rank$usage,110,labels = FALSE)
        colored_cut <- unique(sub_rank$cuts[sub_rank$usage == round(usage_pred_val)])
        sub_rank$color <- ifelse(sub_rank$cuts == colored_cut, T, F)
        ggplot(sub_rank) + geom_histogram(aes(x = usage, fill = color), binwidth = 1,
                                          breaks = seq(0,50,1),
                                          color = "black", boundary = 1) + ylab("Count") +
            xlab("Usage") + ggtitle("Distribution of Usage") + 
            theme(legend.position="none") + scale_fill_manual(values=c("#999999", "springgreen3"))
        #sub_rank %>% select(rebounds, cuts, color)
    })
    # output$first_line <- renderText({
    #     "Hello test"
    # })
}

