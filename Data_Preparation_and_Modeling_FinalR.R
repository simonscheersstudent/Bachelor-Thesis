##################################
#####################
###### Data Preparation WC 20222 Daten
#####################
##################################

##################
##### set up #####
##################

library(xgboost)
library(caret)
library(tidyjson)
library(tidyverse)
library(rjson)
library(jsonlite)
library(progress)
library(data.table)
library(dplyr)
library(stats)
library(ggplot2)
library("geometry")
install.packages("Metrics")
library(Metrics)
library(Matrix)
library(ranger)
install.packages("comets")
library("comets")
library(coin)

### set wd to folder where this file is saved and make sure the data is stored in this folder as well
setwd("~/Documents/UNI/Bachelor_Thesis") 



##############################
##### Data Preprocessing #####
##############################

### function to extract event data info from the json
extract_tracking_info_event <- function(data_list) {
  
  ### initializing progress bar
  progressbar <- progress_bar$new(
    total = length(data_list),
    format = "Extracting Event Data [:bar] :percent | ETA: :eta",
  )
  progressbar$tick(0)
  
  
  
  
  ### getting first list item since only one
  x <- data_list[[1]]
  
  ### Extract all relevant elements (same length)
  game_id <- if(!is.null(x$gameId)) x$gameId else NA
  periods <- if(!is.null(x$gameEvents$period)) x$gameEvents$period else NA
  elapsed <- if(!is.null(round(x$gameEvents$startGameClock/60,2))) round(x$gameEvents$startGameClock/60,2) else NA 
  home_team <- if(!is.null(x$gameEvents$homeTeam)) x$gameEvents$homeTeam else NA
  event_id <- if(!is.null(x$gameEventId)) x$gameEventId else NA
  player_id <- if(!is.null(x$possessionEvents$shooterPlayerId)) x$possessionEvents$shooterPlayerId else NA
  player_name <- if(!is.null(x$possessionEvents$shooterPlayerName)) x$possessionEvents$shooterPlayerName else NA
  gk_id <- if(!is.null(x$possessionEvents$keeperPlayerId)) x$possessionEvents$keeperPlayerId else NA
  gk_name <- if(!is.null(x$possessionEvents$keeperPlayerName)) x$possessionEvents$keeperPlayerName else NA
  event_types <- if(!is.null(x$gameEvents$gameEventType)) x$gameEvents$gameEventType else NA
  sub_out_id <- if(!is.null(x$gameEvents$playerOffId)) x$gameEvents$playerOffId else NA
  sub_in_id <- if(!is.null(x$gameEvents$playerOnId)) x$gameEvents$playerOnId else NA
  shot_types <- if(!is.null(x$possessionEvents$shotType)) x$possessionEvents$shotType else NA
  shot_outcome <- if(!is.null(x$possessionEvents$shotOutcomeType)) x$possessionEvents$shotOutcomeType else NA 
  goals <- if(!is.null(shot_outcome)) shot_outcome == "G" else NA
  penalty <- if(!is.null(x$gameEvents$setpieceType)) x$gameEvents$setpieceType == "P" else NA
  free_kick <- if(!is.null(x$gameEvents$setpieceType)) x$gameEvents$setpieceType == "F" else NA
  
  ### Add data to output table
  out <- data.table(
    game_id = game_id,
    period = periods,
    gametime = elapsed,
    event_id = event_id,
    event_type = event_types,
    home_team = home_team,
    sub_out_id = sub_out_id,
    sub_in_id = sub_in_id,
    shooter_id = player_id,
    player_name = player_name,
    gk_id = gk_id,
    gk_name = gk_name,
    shot_type = shot_types,
    penalty=penalty,
    free_kick=free_kick,
    shot_outcome = shot_outcome,
    goal = goals
  )
  progressbar$tick()
  
  return(out)
}

### function to extract tracking data info from the json
extract_tracking_info <- function(data_list) {
  
  ### initializing progress bar
  progressbar <- progress_bar$new(
    total = length(data_list),
    format = "Extracting Tracking Data [:bar] :percent | ETA: :eta",
  )
  progressbar$tick(0)
  
  ### initializing output list
  output <- list()
  
  ### Game ID
  game_id <- if (!is.null(data_list[[1]]$gameRefId) && length(data_list[[1]]$gameRefId) > 0) data_list[[1]]$gameRefId else NA
  
  
  for (i in seq_along(data_list)) {
    x <- data_list[[i]]
    
    ### Event Info
    frame_num <- if (!is.null(x$frameNum) && length(x$frameNum) > 0) x$frameNum else NA
    possession_type <- if (!is.null(x$possession_event$possession_event_type) && length(x$possession_event$possession_event_type) > 0) x$possession_event$possession_event_type else NA
    game_event_type <- if (!is.null(x$game_event$game_event_type) && length(x$game_event$game_event_type) > 0) x$game_event$game_event_type else NA
    event_id <- if(!is.null(x$game_event_id)) x$game_event_id else NA
    
    # Raw ball positions
    ball_x <- if (!is.null(x$balls$x)) x$balls$x else NA
    ball_y <- if (!is.null(x$balls$y)) x$balls$y else NA
    #ball_z <- if (!is.null(x$balls$z)) x$balls$z else NA
    
    # Period and time
    period <- if (!is.null(x$period)) x$period else NA
    elapsed <- if (!is.null(x$periodClock$seconds)) x$periodClock$seconds else NA
    
    # Game minute
    game_minute <- if (!is.na(x$period) && !is.na(x$periodElapsedTime)) {
      offset <- switch(
        as.character(x$period),
        "1" = 0,
        "2" = 45,
        "3" = 90,
        "4" = 105,
        0
      )
      round(x$periodElapsedTime / 60 + offset,2)
    } else {
      NA
    }
    
    ### player info
    ##Player Id of event, for later to identify shooter
    player_id <- if (!is.null(x$game_event$player_id)) x$game_event$player_id else NA
    
    ##player name
    player_name <- if (!is.null(x$game_event$player_name)) x$game_event$player_name else NA
    
    ##shirt num of current player
    jerseyNum <- if (!is.null(x$game_event$shirt_number)) x$game_event$shirt_number else NA
    
    ## home_team bool 0 or 1, 1 is home team 0 is away team
    home_team <- if (!is.null(x$game_event$home_team)) x$game_event$home_team else NA
    
    ## position info
    position <- if (!is.null(x$game_event$position_group_type)) x$game_event$position_group_type else NA
    
    ### output dt
    output[[i]] <- data.table(
      
      ###general info
      game_id = game_id,
      
      ### index for searching 
      ind = i,
      
      #time info
      frameNum = frame_num,
      gametime = game_minute,
      period = period,
      
      #event info
      event_id = event_id,
      possession_event_type = possession_type,
      game_event_type = game_event_type,
      
      active_player_id = player_id,
      active_player_name = player_name,
      active_jerseyNum = jerseyNum,
      active_home_team = home_team,
      active_position = position,
      
      #ball info
      ball_x = ball_x,
      ball_y = ball_y
      #ball_z = ball_z,
    )
    
    progressbar$tick()
  }
  out <- rbindlist(output)
  return(out)
}

### extracts the players position information (dt_long is here the json output)
get_players_positions <- function(dt_long){
  
  progressbar <- progress_bar$new(
    total = length(dt_long),
    format = "Extracting Player Position Data [:bar] :percent | ETA: :eta",
  )
  progressbar$tick(0)
  
  ### initializing list of home and away players
  home_list <- list()
  away_list <- list()
  output <- list()
  
  for (i in seq_along(dt_long)) {
    dt <- dt_long[[i]]
    
    progressbar$tick()
    
    if (!is.null(dt$homePlayers) && !is.null(dt$awayPlayers)) {
      ### create home player datatable and away datatable
      home_dt <- as.data.table(dt$homePlayers)[
        , `:=`(game_id= dt_long[[1]]$gameRefId, frame = dt$frameNum, home = 1, period=1)][
          , c("confidence", "visibility") := NULL]
      
      away_dt <- as.data.table(dt$awayPlayers)[
        , `:=`(game_id= dt_long[[1]]$gameRefId, frame = dt$frameNum, home = 0, period=1)][
          , c("confidence", "visibility") := NULL]
      
      
      ### Flip the sign of coordinates for second half
      if (dt$period == 2) {
        home_dt[, `:=`(x = -x, y = -y, period=2)]
        away_dt[, `:=`(x = -x, y = -y, period=2)]
      }
      home_list[[length(home_list) + 1]] <- home_dt
      away_list[[length(away_list) + 1]] <- away_dt
    }
  }
  
  ### Combine player data tables
  dt_home_players_long <- rbindlist(home_list)
  dt_away_players_long <- rbindlist(away_list)
  output[["dt_home_players_long"]] <- dt_home_players_long
  output[["dt_away_players_long"]] <- dt_away_players_long
  return(output) 
}

### creates a wide format of the data to include each players position within a row
reshape_players_wide_no_gk <- function(dt_long, team="home") {
  
  ### Ensure input is a data table
  dt_long <- as.data.table(dt_long)
  
  ### Enusre only unique rows
  dt_long <- unique(dt_long)
  
  
  ### Format the data into long format for x and y values
  dt_melt <- melt(dt_long,
                  id.vars = c("frame", "game_id", "period", "jerseyNum"),
                  measure.vars = c("x", "y"),
                  variable.name = "coord",
                  value.name = "value")
  
  ### Create a unique column name combining team, coord, and jersey number
  dt_melt[, colname := paste0(team, "_", coord, "_", jerseyNum)]
  
  ### Cast the melted data to wide format
  dt_wide <- dcast.data.table(dt_melt, game_id +frame ~ colname, value.var = "value")
  
  
  setnames(dt_wide, "frame", "frameNum")
  
  return(unique(dt_wide))
}

### creates information about the gks
detect_gks <- function(out, dt_home_wide, dt_away_wide){
  ## starting gk home
  out[[1]]$homePlayers
  start_gk_home <- out[[1]]$homePlayers[which.max(abs(out[[1]]$homePlayers$x)),]$jerseyNum
  
  ## starting gk away
  out[[1]]$awayPlayers
  start_gk_away <- out[[1]]$awayPlayers[which.max(abs(out[[1]]$awayPlayers$x)),]$jerseyNum
  counter<-0
  
  ## check for changes 
  for (num in c(start_gk_home,start_gk_away)){
    if (counter == 0){
      col <-paste0("home_x_", start_gk_home)
      print(col)
      if(anyNA(dt_home_wide[,..col])){
        
        print("got chnaged")
        #print(which(is.na(dt_home_wide[,..col]))[1])
        ind_sub <- which(is.na(dt_home_wide[,..col]))[1]-1
        gk_num <- strsplit(col, "_")[[1]][3]
        dt_home_wide[0:ind_sub, gk_home:=gk_num]
        #print(dt_home_wide[ind_sub]$frameNum)
        ###getting all colnames
        
        col_x <- grep("_x_", names(dt_home_wide), value=TRUE)
        
        col_x <- drop(col_x[which(!is.na(dt_home_wide[ind_sub,..col_x]))])
        if (length(col_x)>11){
          print("12 players")
          col_x <- drop(col)
          x_coord <- dt_home_wide[ind_sub, ..col]
          for (column in col_x){
            if (as.numeric(dt_home_wide[ind_sub, ..column])==x_coord){
              print(column)
              gk_num <- strsplit(column, "_")[[1]][3]
              dt_home_wide[(ind_sub+1):nrow(dt_home_wide), gk_home:=gk_num]
            }
          }
        }
      }
      else{
        gk_num <- strsplit(col, "_")[[1]][3]
        print("played whole game")
        
        dt_home_wide[, gk_home:=gk_num]
      }
    }
    else{
      col <- paste0("away_x_", start_gk_away)
      print(col)
      if(anyNA(dt_away_wide[,..col])){
        print("got chnaged")
        print(which(is.na(dt_away_wide[,..col]))[1])
        ind_sub <- which(is.na(dt_away_wide[,..col]))[1]-1
        gk_num <- strsplit(col, "_")[[1]][3]
        dt_away_wide[0:ind_sub, gk_away:=gk_num]
        print(dt_away_wide[ind_sub]$frameNum)
        ###getting all colnames
        
        col_x <- grep("_x_", names(dt_away_wide), value=TRUE)
        
        col_x <- drop(col_x[which(!is.na(dt_away_wide[ind_sub,..col_x]))])
        if (length(col_x)>11){
          print("12 players")
          
          x_coord <- dt_away_wide[ind_sub, ..col]
          for (column in col_x){
            if (column != col){
              if (as.numeric(dt_away_wide[ind_sub, ..column])==x_coord){
                print(column)
                gk_num <- strsplit(column, "_")[[1]][3]
                #print(gk_num)
                dt_away_wide[(ind_sub+1):nrow(dt_away_wide), gk_away:=gk_num]
              } 
            }
            
          }
        }
      }
      else{
        gk_num <- strsplit(col, "_")[[1]][3]
        print("played whole game")
        dt_away_wide[, gk_away:=gk_num]
      }
    }
    counter <- counter + 1
  }
  return(c(dt_away_wide, dt_home_wide))
}

### creates information about goals
detect_goals_from_tracking <- function(df_tracking, df_shots) {
  
  ### make sure df_tracking is a datatable
  goal_results_dt <- as.data.table(df_tracking)
  
  goal_results_dt[, "goal_likely":= FALSE]
  goal_results_dt[, "goal_reason":= "no_data"]
  
  ### loop through all frames declared as shots
  for (shot_frame in df_shots$frameNum) {
    ind_shot <- which(df_tracking$frameNum == shot_frame)[1]
    
    next_event_index_relative <- which(!is.na(df_tracking$possession_event_type[(ind_shot + 1):nrow(df_tracking)]))[1]
    
    ind_next_event <- if (is.na(next_event_index_relative)) {
      nrow(df_tracking)
    } else {
      ind_shot + next_event_index_relative
    }
    
    
    
    frame_range <- df_tracking[(ind_shot + 1):ind_next_event, ]
    frame_range <- frame_range[!is.na(ball_x) & !is.na(ball_y), ]
    
    goal_reason <- "none"
    ball_in_goal <- FALSE
    
    ### check if ball was in goal or the ball happened tobe in the center circle for kick off
    if (nrow(frame_range) > 0) {
      goal_frame_match <- with(frame_range,
                               !is.na(ball_x) & !is.na(ball_y) &
                                 (ball_x <= -52.5 | ball_x >= 52.5) &
                                 abs(ball_y) <= 3.66
      )
      
      last_row <- nrow(frame_range)
      if (!ball_in_goal && nrow(frame_range) > 0) {
        
        x_val <- as.numeric(frame_range$ball_x_smoothed[last_row])
        y_val <- as.numeric(frame_range$ball_y_smoothed[last_row])
        
        center_kickoff_hint <- isTRUE(
          !is.na(x_val) &&
            !is.na(y_val) &&
            abs(x_val) <= 9.15 &&
            abs(y_val) <= 9.15
        )
      }
      
      ### write the reason into a column
      if (any(goal_frame_match)) {
        ball_in_goal <- TRUE
        goal_reason <- "goal_detected"
        goal_results_dt[ind_shot, "goal_reason":= "goal_detected"]
      } else if (center_kickoff_hint) {
        ball_in_goal <- TRUE
        goal_reason <- "kickoff_center"
        goal_results_dt[ind_shot, "goal_reason":= "kickoff_center"]
      }
    } else {
      ball_in_goal <- NA
      goal_reason <- "no_data"
    }
    
    goal_results_dt[ind_shot, "goal_likely":= ball_in_goal]
  }
  
  return(goal_results_dt)
}

### merges tracking and event data
merge_infos <- function(tracking_data=goal_results_dt, event_data=filtered) {
  
  ### filter data for likely goals
  likely_goals_dt <- as.data.table(tracking_data[tracking_data$goal_likely==TRUE])
  
  ### make sure event data is a datatable
  event_data <- as.data.table(event_data)
  
  tracking_data <- as.data.table(tracking_data)[tracking_data$possession_event_type=="SH"]
  
  ### make sure that there are more likely goals or equal to actual goals
  if (nrow(likely_goals_dt)>=nrow(event_data)) {
    #merged_dt_new <- likely_goals_dt[event_data, roll = "nearest", on = .(period, gametime), nomatch = 0]
    merged_dt_new <- likely_goals_dt[event_data, on = .(game_id, event_id)]
    if (nrow(merged_dt_new)==nrow(event_data)){
      merged_dt<-merged_dt_new
      return(merged_dt)
    }
    else{
      print("Not all goals were recognized")
    }
  }
  else {
    #merged_dt_new <- tracking_data[event_data, roll = "nearest", on = .(period, gametime), nomatch = 0]
    merged_dt_new <- tracking_data[event_data, on = .(game_id, event_id)]
    #print(head(merged_dt_new))
    if (nrow(merged_dt_new)==nrow(event_data)){
      merged_dt<-merged_dt_new
      return(merged_dt)
    }
    else{
      print("Not all goals were recognized")
      return(merged_dt_new)
    } 
  }
  
}

### creates some new features for training the xg models
feature_engineering <- function(df) {
  ### making sure the input is formatted as a data table
  df <- as.data.table(df)
  
  ### feature engineering
  shooter_list_x <- list()
  shooter_list_y <- list()
  gk_list_x <- list()
  gk_list_y <- list()
  gk_list_number <- list()
  goal_dist_list <- list()
  goal_angle_list <- list()
  defender_view_list <-list()
  defender_around_list <- list()
  fill_gk_id_list <- list()
  
  for (row in seq_len(nrow(df))){
    #print(row)
    
    ### .. makes it seen as a variable
    ### get shooter x and y
    if (!is.na(df[row]$active_home_team)){
      side <- if(df[row, "active_home_team"]==1) "home" else "away"
      #print(side)
      shooter_x <- paste0(side,  "_x_", df[row]$active_jerseyNum)
      shooter_list_x[row] <- df[row, ..shooter_x]
      shooter_y <- paste0(side,  "_y_", df[row]$active_jerseyNum)
      shooter_list_y[row] <- df[row, ..shooter_y]
    }
    else {
      if (df[row]$penalty == TRUE){
        shooter_list_x[row] <- 41.5
        shooter_list_y[row] <- 0
      }
    }
    
    ### get gk x and y
    if (!is.na(df[row]$active_home_team)){
      side <- if(df[row, "active_home_team"]==1) "home" else "away"
      if(if(side == "home") !is.na(df[row]$gk_home) else !is.na(df[row]$gk_away)) {
        side_gk <- if(df[row, "active_home_team"]==0) "home" else "away"
        col <- paste0("gk_", side_gk)
        gk_x <- paste0(side_gk,  "_x_", df[row, ..col])
        gk_y <- paste0(side_gk,  "_y_", df[row, ..col])
        if (!is.na(df[row, ..gk_x])&!is.na(df[row, ..gk_y])){
          gk_list_x[row] <- df[row, ..gk_x]
          gk_list_y[row] <- df[row, ..gk_y]
        }
      }
      else {
        if (df[row]$penalty == TRUE){
          gk_list_x[row] <- 52.5
          gk_list_y[row] <- 0
        }
      }
    }
    ### we work with expected values we do not have data for the gk 
    else {
      if (df[row]$penalty == TRUE){
        gk_list_x[row] <- 52.5
        gk_list_y[row] <- 0
      }
    }
    
    ### gk number 
    if(if(side == "home") !is.na(df[row]$gk_home) else !is.na(df[row]$gk_away)) {
      if (!is.na(side_gk)){
        gk_list_number[row]<- paste0(side_gk, "_", df[row, ..col])
      }
      else {
        gk_list_number[row]<- NA
      }
    }
    else{
      gk_list_number[row]<- NA
    }
    
    ### get distance to the middle of the goal the euclidean distance formula is used
    
    if (df[row]$penalty == FALSE) {
      print(gk_list_x[row])
      print(row)
      ### to find out where the goal is located if they are shooting to the left or right
      value <- unlist(gk_list_x[row])

      if (!is.null(value)) {
      ###if (length(gk_list_x[row]) > 0 && !is.na(unlist(gk_list_x[row]))){
        if(abs(unlist(gk_list_x[row])-52.5)>abs(unlist(gk_list_x[row])+52.5)){
          goal_dist_list[row] <- sqrt((unlist(shooter_list_x[row])+52.5)^2+(unlist(shooter_list_y[row])-0)^2)
        }
        else {
          goal_dist_list[row] <- sqrt((unlist(shooter_list_x[row])-52.5)^2+(unlist(shooter_list_y[row])-0)^2)
        }
      }
    }
    else {
      goal_dist_list[row] <- 11
      
    }
    
    ### get angle of shooter
    if (df[row]$penalty == FALSE) {
      if (!is.null(value)) {
        ### to find out where the goal is located if they are shooting to the left or right
        if(abs(unlist(gk_list_x[row])-52.5)>abs(unlist(gk_list_x[row])+52.5)){
          post1 <- c(-52.5, -3.66)
          post2 <- c(-52.5, 3.66)
          shooter <- c(unlist(shooter_list_x[row]), unlist(shooter_list_y[row]))
          vector1 <- post1 - shooter
          vector2 <- post2 - shooter
          angle <- acos(dot(vector1,vector2)/(sqrt(vector1[1]^2+vector1[2]^2)*sqrt(vector2[1]^2+vector2[2]^2)))*180/pi
          
          goal_angle_list[row] <- angle
        }
        else {
          post1 <- c(+52.5, -3.66)
          post2 <- c(+52.5, 3.66)
          shooter <- c(unlist(shooter_list_x[row]), unlist(shooter_list_y[row]))
          vector1 <- post1 - shooter
          vector2 <- post2 - shooter
          angle <- acos(dot(vector1,vector2)/(sqrt(vector1[1]^2+vector1[2]^2)*sqrt(vector2[1]^2+vector2[2]^2)))*180/pi
          
          goal_angle_list[row] <- angle
        }
      }
    }
    else {
      goal_angle_list[row] <- 90
    }
    
    ### players in view or not 
    if (df[row]$penalty == FALSE) {
      ### to find out how many players in shot triangle A=goal_len*tri_height/2
      goal_len <- abs(post1[2]-post2[2])
      tri_height <- abs(post1[1]-shooter[1])
      shot_tri <- goal_len*tri_height/2
      ### get only columns containing x coordinates
      if (df[row]$active_home_team == 0){
        df2 <- df[, grep("home_x_",names(df), value=TRUE)]
      }
      else {
        df2 <- df[, grep("away_x_",names(df), value=TRUE)]
      }
      
      
      ### counting how many
      counter<-0
      for (col in df2){
        ### getting also y coordinates
        col2 <- gsub("x","y",col)
        P <- as.vector(c(unlist(df[row, ..col]),unlist(df[row, ..col2])))
        if (!is.na(P[1]) | !is.na(P[2])){
          A1 <- goal_len*abs(post1[1]-P[1])/2
          #A2 <- (abs(((shooter[2]-post1[2])*P[1])-((shooter[1]-post1[1])*P[2])+(shooter[1]*post1[2])-(shooter[2]*post1[1]))/sqrt(vector1[1]^2+vector1[2]^2)) ### len is ok
          #A3 <- (abs(((shooter[2]-post2[2])*P[1])-((shooter[1]-post2[1])*P[2])+(shooter[1]*post2[2])-(shooter[2]*post2[1]))/sqrt(vector2[1]^2+vector2[2]^2)) ### len is ok
          ### abs(vector1[2]*P[1]-vector1[1]*P[2]+post1[1]*shooter[2]-post1[2]*shooter[1])/sqrt(vector1[1]^2+vector1[2]^2) all this just to get h
          A2 <- (abs(vector1[2]*P[1]-vector1[1]*P[2]+post1[1]*shooter[2]-post1[2]*shooter[1])/sqrt(vector1[1]^2+vector1[2]^2))*sqrt(vector1[1]^2+vector1[2]^2)/2
          A3 <- abs(vector2[2]*P[1]-vector2[1]*P[2]+post2[1]*shooter[2]-post2[2]*shooter[1])/sqrt(vector2[1]^2+vector2[2]^2)*sqrt(vector2[1]^2+vector2[2]^2)/2
          A_total <- A1+A2+A3
          print(A_total)
          print(shot_tri)
          if(!is.na(A_total) || !is.na(shot_tri)){
            if (round(as.numeric(A_total), 2) == round(as.numeric(shot_tri), 2)){
              counter <- counter + 1
            }
          }
        }
        
      }
      defender_view_list[row] <- counter
    }
    else {
      defender_view_list[row] <- 1
    }
    
    
    ### players around shooter
    if (df[row]$penalty == FALSE) {
      ### to find out how many players are around shooter
      radius <- 4.5
      ### get only columns containing x coordinates
      'if (df[row]$active_home_team == 0){
        df2 <- df[, grep("home_x_",names(df), value=TRUE)]
      }
      else {
        df2 <- df[, grep("away_x_",names(df), value=TRUE)]
      }'
      
      
      ### counting how many
      counter2<-0
      for (col in df2){
        ### getting also y coordinates
        col2 <- gsub("x","y",col)
        P <- as.vector(c(unlist(df[row, ..col]),unlist(df[row, ..col2])))
        if (!is.na(P[1]) | !is.na(P[2])){
          vector3 <- P - shooter
          vector3_len <- sqrt(vector3[1]^2+vector3[2]^2)
          print(vector3_len)
          print(radius)
          print(vector3_len<radius)
          print((!is.na(vector3_len) && !is.null(vector3_len) && !is.na(vector3_len<radius) && !is.null(vector3_len<radius)))
          if (!is.na(vector3_len) && !is.null(vector3_len) && !is.na(vector3_len<radius) && !is.null(vector3_len<radius)){
            if (vector3_len<radius){
              counter2 <- counter2 + 1
              #print(col)
            }
          }
        }
        
      }
      defender_around_list[row] <- counter2
    }
    else {
      defender_around_list[row] <- 0
    }
    
  }
  
  
  
  ### creating a new data table with the new features
  feature_dt <- data.table(
    game_id = df$game_id,
    gametime = df$i.gametime,
    penalty = df$penalty,
    freekick = df$free_kick,
    shooter_id = df$shooter_id,
    shooter_x = as.numeric(sapply(shooter_list_x, function(x) if (length(x)) x[1] else NA)),
    shooter_y = as.numeric(sapply(shooter_list_y, function(x) if (length(x)) x[1] else NA)),
    gk = as.character(sapply(gk_list_number, function(x) if (length(x)) x[1] else NA)),
    gk_id = df$gk_id,
    gk_x = as.numeric(sapply(gk_list_x, function(x) if (length(x)) x[1] else NA)),
    gk_y = as.numeric(sapply(gk_list_y, function(x) if (length(x)) x[1] else NA)),
    shot_dist_goal = as.numeric(sapply(goal_dist_list, function(x) if (length(x)) x[1] else NA)),
    angle_to_goal = as.numeric(sapply(goal_angle_list, function(x) if (length(x)) x[1] else NA)),
    defender_infront = as.numeric(sapply(defender_view_list, function(x) if (length(x)) x[1] else NA)),
    defender_around = as.numeric(sapply(defender_around_list, function(x) if (length(x)) x[1] else NA)),
    goal = df$goal
  )
  
  ### fill gk_id NAs
  #print(unique(gk_list_number))
  for (i in unique(gk_list_number)){
    if (!is.na(i)){
      #print(i)   
      fill_id <- unique(feature_dt[!is.na(feature_dt$gk_id)&feature_dt$gk==i]$gk_id)
      if (length(fill_id)==1){
        #print(fill_id)
        fill_gk_id_list[i]<- fill_id
        feature_dt[is.na(feature_dt$gk_id)&feature_dt$gk==i]$gk_id <- fill_id
      }
      if (length(fill_id)==0){
        #print(fill_id)
        fill_gk_id_list[i]<- NA
      }
    }
    else {
      #print("shit")
    }
  }
  #print(fill_gk_id_list)
  
  return(feature_dt)
}

### function that calls all preprocessing functions
combine_all_data <- function(num_files = NULL, file_path = paste0(getwd(), "/data"), file_path2 = paste0(getwd(), "/EventData")) {
  start_time_run <- Sys.time()
  ### create a list that store all data tables as output later
  output <- list()
  
  all_merged_dt <- NULL
  
  ### only files that end in "\\.jsonl.bz2" will be taken into account
  file_list <- list.files(path = file_path, pattern = "\\.jsonl.bz2$", full.names = TRUE)
  
  ### for checking how many files
  cat("Found", length(file_list), "files\n")
  
  if (!is.null(num_files)){
    ###for testing or only certain files 
    file_list <- file_list[37:(37+num_files)]
    cat("Using", length(file_list), "files\n")
  }

  
  ### for checking how many files
  cat("Found", length(file_list), "files\n")
  
  ### Initialize progress bar
  progressbar <- progress_bar$new(total = length(file_list), format = "Processing Files [:bar] :percent | ETA: :eta")
  progressbar$tick(0)
  
  
  for (file in file_list) {
    cat("Started processing the following file: ", basename(file), " at ",format(Sys.time(), "%d.%m.%Y %X"))
    start_time <- Sys.time()
    ### get game id for event data file
    game_id <- sub("\\.jsonl.bz2", "", basename(file))

    
    ######
    ### Event Data
    #####
    
    ### read event data into out_event variable
    out_event<-lapply(readLines(paste0(file_path2,"/", game_id, ".json")), fromJSON)
    
    ### creating an event data datatable out of the the out_event list
    dt_out_event <- as.data.table(extract_tracking_info_event(out_event))
    filtered <- dt_out_event[!is.na(dt_out_event$goal)]
    dt_out_event_subs <- dt_out_event[dt_out_event$event_type == "SUB"]
    output[["dt_out_event_subs"]] <- dt_out_event_subs
    output[["dt_out_event_shots"]] <- filtered
    output[["dt_out_event"]] <- dt_out_event
    
    
    ### get only the data of the goals from event data for merge
    filtered_goals <- dt_out_event[dt_out_event$goal==TRUE]
    output[["dt_out_event_goals"]] <- filtered_goals
    
    cat("Finished processing event data of: ", basename(file), " at ",format(Sys.time(), "%d.%m.%Y %X"))

    
    
    ######
    ### Tracking Data
    ######
    
    ### read tracking data into out variable
    out <- lapply(readLines(file), fromJSON)
    
    ### create tracking  dt
    df_out <- as.data.table(extract_tracking_info(out))
    
    
    ### creating players and gk info
    ###old version

    ###new version
    ### get player position data longformat
    dt_players_positions <- get_players_positions(out)
    
    dt_home <- dt_players_positions$dt_home_players_long
    dt_away <- dt_players_positions$dt_away_players_long
    
    ### reshaping to wide format
    dt_home_wide <- reshape_players_wide_no_gk(dt_home)
    dt_away_wide <- reshape_players_wide_no_gk(dt_away, team = "away")
    
    ### get gk information
    df_out_players <- as.data.table(detect_gks(out, dt_home_wide, dt_away_wide))
    
    ### Remove the '.1' suffix from the remaining column names
    names(df_out_players) <- sub("\\.1$", "", names(df_out_players))
    df_out_players <- df_out_players[, !duplicated(colnames(df_out_players)), with= FALSE]
    #print(names(df_out_players))
    
    ### merge players positional info with metadata
    df_out <- merge(df_out, df_out_players, by="frameNum", all= TRUE)
    
    ### Remove columns with names ending in '.y'
    df_out <- df_out[, !grepl("\\.y$", names(df_out)), with = FALSE]
    #print(df_out)
    
    ### Remove the '.x' suffix from the remaining column names
    names(df_out) <- sub("\\.x$", "", names(df_out))
    #print(names(df_out))
    
    ### Make sure column names are unique 
    setnames(df_out, make.names(names(df_out), unique = TRUE))
    #print(names(df_out))
    
    ### Add result to the output list
    output[["df_out"]] <- df_out
    
    ### filter tracking dt only on shots
    df_shots <- df_out %>%
      filter(possession_event_type == "SH")
    
    
    ### get rid of duplicates
    df_shots <- df_shots[!duplicated(df_shots$frameNum), ]
    output[["shots"]] <- df_shots

    
    ### get goals from tracking data for merge
    goal_results_dt <- detect_goals_from_tracking(df_out, df_shots)
    output[["goals"]] <- goal_results_dt

    ### merge infos of event and tracking data together 
    merged_dt <- data.table()
    merged_dt <- merge_infos(tracking_data = goal_results_dt, event_data = filtered)
    
    output[["merged"]] <- merged_dt
    write_csv(merged_dt, paste0(getwd(),"/merged_data/", game_id, ".csv"))
    
    ### handling cases if all_merged_dt exists or not 
    if (!exists("all_merged_dt") || is.null(all_merged_dt)) {
      ### Initialize the data table
      all_merged_dt <- merged_dt
    } else if (nrow(all_merged_dt) == 0) {
      ### just overwrite
      all_merged_dt <- merged_dt
    } else {
      ### new info
      all_merged_dt <- rbindlist(list(all_merged_dt, merged_dt), use.names = TRUE, fill = TRUE)
    }
    
    output[["all_merged_dt"]] <- all_merged_dt
    
    progressbar$tick()
    end_time <- Sys.time()
    cat("Finished processing the following file: ", basename(file), " at ",format(Sys.time(), "%d.%m.%Y %X"))
    cat("Processing the file took the following amount of time:", round(difftime(end_time,start_time, units="mins")), " minutes.")
  }
  ### further processing steps removing duplicates
  all_merged_no_dups_dt <- all_merged_dt[!duplicated(all_merged_dt$event_id)]
  all_merged_no_dups_dt <-all_merged_no_dups_dt[!is.na(all_merged_no_dups_dt$frameNum),]
  
  ### feature engineering
  feature_engineered_shots_dt <- feature_engineering(all_merged_no_dups_dt)
  
  write_csv(feature_engineered_shots_dt, paste0(getwd(),"/feature_engineered/feature_engineered_", length(file_list), ".csv"))
  
  output[["feature_engineered_shots_dt"]] <- feature_engineered_shots_dt
  
  cat("The whole run took this long:", round(difftime(end_time,start_time, units="hours")), " hours")
  
  return(output)
}

'### function to add some missing gk ids if possible not anymore used
impute_gks <- function(test_feature_engineered_dt){
  test_list <- list()
  for (game_ids in unique(test_feature_engineered_dt$game_id)){
    #print(game_id)
    gk_list <- list()
    for (gks in unique(test_feature_engineered_dt[game_id==game_ids]$gk)){
      #print(gks)
      id_list <- list()
      gk_dt <- test_feature_engineered_dt[game_id==game_ids]
      for (id in unique(gk_dt[gk==gks]$gk_id)){
        #print(id)
        id_list <- c(id_list, id)
      }
      id_list <- id_list[!is.na(id_list)]
      gk_list[[gks]] <- id_list
    }
    test_list[[toString(game_ids)]]<- gk_list
  }
  for (row in 1:nrow(test_feature_engineered_dt)){
    #print(row)
    #print(test_feature_engineered_dt[row, "gk_id"])
    if (is.na(test_feature_engineered_dt[row]$gk_id)){
      gk_id <- toString(test_feature_engineered_dt[row]$game_id)
      gk_num <- test_feature_engineered_dt[row]$gk
      if (length(test_list[[gk_id]][[gk_num]])==1){
        test_feature_engineered_dt[row]$gk_id <- test_list[[gk_id]][[gk_num]]
      }
    }
  }
  return(test_feature_engineered_dt)
}'

### use this code to run all preprocessing steps, choose the number of files you want to work with
test_1_file <- combine_all_data(1)

'###imputing gk ids as last preprocessing step if necessary
test_1_file <- impute_gks(test_1_file)'

### only necessary for preloading data without running through all steps
'### merging files
merged_csv <- data.table()
path_csv<-paste0(getwd(),"/merged_data")
for (file in list.files(path=path_csv)){
  csv_data <- read_csv(paste0(path_csv,"/",file))
  merged_csv <- rbindlist(list(merged_csv, csv_data), use.names = TRUE, fill = TRUE)
  print(paste0(file, ": done"))
}
write_csv(merged_csv, "/Users/simonscheer/Documents/UNI/Bachelor_Thesis/merged_data/merged_files.csv")

### feature engineering from merged csv files
merged_csv <- merged_csv[!duplicated(merged_csv$event_id)]
merged_csv <-merged_csv[!is.na(merged_csv$frameNum),]
feature_engineered_dt <- feature_engineering(merged_csv)
write_csv(feature_engineered_dt, paste0(getwd(),"/feature_engineered/all_engineered_files.csv"))
feature_engineered_dt <- impute_gks(feature_engineered_dt)'
test10 <- read_csv(paste0(getwd(),"/feature_engineered/all_engineered_with_correct_gks.csv"))
which(is.na(test10$gk_id))
test10<-impute_gks(as.data.table(test10))

### get roster information per game
get_roster_info <- function(){
  file_path_rosters <- paste0(getwd(),"/Rosters")
  rosters <- data.table()
  for (file in list.files(path = file_path_rosters, pattern = "\\.json$", full.names = TRUE)){
    roster <- as.data.table(lapply(readLines(paste0(file_path_rosters,"/",basename(file))), fromJSON))
    roster$game.id <- sub("\\.json", "", basename(file))
    rosters <- rbindlist(list(rosters, roster), use.names = TRUE, fill = TRUE)
  }
  return(rosters)
}
rosters <- get_roster_info()
unique_rosters <- unique(rosters[,-5], by = "player.id")

### get additional player information for mapping of ids and names
player_dt <- read_csv("players.csv")
setnames(player_dt, "id", "player.id")
unique_player_dt <- unique(player_dt)
player_info <- merge(unique_player_dt, unique_rosters, by = "player.id")

### getting metadata
getting_metatdata <- function(){
  file_path_metadata <- paste0(getwd(),"/Metadata")
  metadatas <- data.table()
  for (file in list.files(path = file_path_metadata, pattern = "\\.json$", full.names = TRUE)){
    metadata <- as.data.table(lapply(readLines(paste0(file_path_metadata,"/",basename(file))), fromJSON))
    metadata$game.id <- sub("\\.json", "", basename(file))
    metadatas <- rbindlist(list(metadatas, metadata), use.names = TRUE, fill = TRUE)
  }
  return(metadatas)
}
metadatas <- getting_metatdata()

### further gk preprocessing
feature_engineered_dt <- combine_all_data()[["feature_engineered_shots_dt"]]
### test with all data
all_shots <- merge(feature_engineered_dt, player_info[, c(-2,-3, -4,-5, -8, -9,-10,-11,-12, -13,-15,-16)], by.x = "gk_id", by.y = "player.id", all.x=TRUE)
setnames(all_shots, c("nickname","positionGroupType.x"), c("gk_name", "gk_position_type"))
all_shots <- merge(all_shots, player_info[, c(-2,-3, -4,-5, -8, -9,-10,-11,-12, -13,-15,-16)], by.x = "shooter_id", by.y = "player.id", all.x=TRUE)
setnames(all_shots, c("nickname","positionGroupType.x"), c("shooter_name", "shooter_position_type"))
new_gks2 <- data.table()
change_gks <- function(dt_with_gks=all_shots, merged_dt=merged_csv, meta= metadatas, rosters_dt = rosters2, new_gks=new_gks2){
  
  #if (!all(dt_with_gks$gk_position_type == "GK", na.rm = TRUE)) {
  if (!all(dt_with_gks$gk_position_type == "GK", na.rm = FALSE)) {  
    print("wrong_gks")
    wrong_gks <- dt_with_gks[gk_position_type != "GK"|is.na(gk_position_type)]
    #print(wrong_gks)
    
    merged_no_dups_dt <- merged_dt[!duplicated(merged_dt$event_id)]
    merged_no_dups_dt <-merged_no_dups_dt[!is.na(merged_no_dups_dt$frameNum),]
    #print(names(wrong_gks))
    merged_dt_gks <- merged_no_dups_dt[wrong_gks, on = .(game_id, active_player_name==shooter_name,gametime), roll="nearest", keep=]
    print(names(merged_dt_gks))
    #print(which(merged_dt_gks$shooter_name!=merged_dt_gks$active_player_name))
    if(all(merged_dt_gks$shooter_name==merged_dt_gks$active_player_name, na.rm=TRUE)){
      print("yeah")
      for (game in merged_dt_gks$game_id){
        dt <- merged_dt_gks[game_id==game]
        
        for (row in seq_len(nrow(dt))){
          if (dt$home_team[row]==FALSE){
            team_id <-meta[game.id==game]$homeTeam.id
            gks <- rosters_dt[game.id==game&positionGroupType=="GK"&team.id==team_id]$shirtNumber
            for (gk in gks){
              col <- paste0("home_x_",gk)
              if (!is.na(dt[row, get(col)])){
                dt$correct_gk_col[row] <- col
                dt$correct_gk_id[row] <- rosters_dt[game.id==game&positionGroupType=="GK"&team.id==team_id&shirtNumber==gk]
              }
            }
          }
          else {
            team_id <-meta[game.id==game]$awayTeam.id
            gks <- rosters_dt[game.id==game&positionGroupType=="GK"&team.id==team_id]$shirtNumber
            for (gk in gks){
              col <- paste0("away_x_",gk)
              if (!is.na(dt[row, get(col)])){
                dt$correct_gk_col[row] <- col
                dt$correct_gk_id[row] <- rosters_dt[game.id==game&positionGroupType=="GK"&team.id==team_id&shirtNumber==gk]
              }
            }
          }
          new_gks<- rbindlist(list(new_gks, as.data.table(dt[row])), use.names = TRUE, fill = TRUE)
        }
      }
    }
  }  
  else{
    return(data.table())
  }
  return(new_gks)
}
merged_dt_gks<-change_gks(all_shots)
unique_merged_dt_gks<- unique(merged_dt_gks, by="event_id")

unique_merged_dt_gks$correct_gk_col<- as.character(unique_merged_dt_gks$correct_gk_col)
unique_merged_dt_gks$correct_gk_id<- as.numeric(unique_merged_dt_gks$correct_gk_id)
unique_merged_dt_gks <- merge(unique_merged_dt_gks, player_info[, c("player.id", "nickname", "positionGroupType.x")], by.x= "correct_gk_id", by.y="player.id", all.x=TRUE)
unique_merged_dt_gks$shooter_name <- unique_merged_dt_gks$active_player_name

copy_all_shots <- copy(all_shots)

change_old_to_new_gks <- function(dt_with_gks=copy_all_shots, new_gks=unique_merged_dt_gks){
  already_used <- data.table()
  for (row in seq_len(nrow(new_gks))){
    possible_shots <-dt_with_gks[game_id==new_gks$game_id[row]&shooter_name==new_gks$shooter_name[row]]
    #print(nrow(possible_shots))
    if (nrow(possible_shots)==1){
      dt_with_gks[game_id==new_gks$game_id[row]&shooter_name==new_gks$shooter_name[row], `:=`(gk_id=new_gks$correct_gk_id[row], gk_name=new_gks$nickname[row], gk_position_type=new_gks$positionGroupType.x[row])]
      #print("worked just 1 row")
      new_row <- as.data.table(list(game_id=new_gks$game_id[row], gk_id=new_gks$correct_gk_id[row]))
      already_used <- rbindlist(list(already_used, new_row))
      }
    else{
      #print(possible_shots[gk_position_type!="GK"|is.na(gk_position_type)])
      if (nrow(possible_shots[gk_position_type!="GK"|is.na(gk_position_type)])==1){
        dt_with_gks[game_id==new_gks$game_id[row]&shooter_name==new_gks$shooter_name[row]&gk_position_type!="GK"|game_id==new_gks$game_id[row]&shooter_name==new_gks$shooter_name[row]&is.na(gk_position_type), `:=`(gk_id=new_gks$correct_gk_id[row], gk_name=new_gks$nickname[row], gk_position_type=new_gks$positionGroupType.x[row])]
        print("worked")
        new_row <- as.data.table(list(game_id=new_gks$game_id[row], gk_id=new_gks$correct_gk_id[row]))
        already_used <- rbindlist(list(already_used, new_row))
      }
      else{
        if (nrow(possible_shots[gk_position_type!="GK"|is.na(gk_position_type)])>1){
          dt_with_gks[game_id==new_gks$game_id[row]&shooter_name==new_gks$shooter_name[row]&gk_position_type!="GK"|game_id==new_gks$game_id[row]&shooter_name==new_gks$shooter_name[row]&is.na(gk_position_type), `:=`(gk_id=new_gks$correct_gk_id[row], gk_name=new_gks$nickname[row], gk_position_type=new_gks$positionGroupType.x[row])]
        }
        else{
          
          #print(nrow(possible_shots[gk_position_type!="GK"|is.na(gk_position_type)]))
          #print(dt_with_gks[game_id==new_gks$game_id[row]&shooter_name==new_gks$shooter_name[row]&gk_position_type!="GK"])
        }
      }
    }
  }
  print(already_used)
  return(dt_with_gks)
}
dt_with_gks_all <- change_old_to_new_gks()





####################
##### Modeling #####
####################

#######
###first iteration of models for XG modeling
#######

set.seed(1234)

###preparing test and trainings dataset
feature_engineered_dt <- dt_with_gks_all
feature_engineered_dt$goal <- as.factor(feature_engineered_dt$goal)

### filtering out penalties
feature_engineered_dt <- feature_engineered_dt[penalty==FALSE]

### to write the completely preprocessed feature engineered file ready for modeling
'write_csv(feature_engineered_dt,paste0(getwd(),"/feature_engineered/all_engineered_with_correct_gks.csv"))'

### data preparation for modeling
train_inds <- sample(1:nrow(feature_engineered_dt),0.8*nrow(feature_engineered_dt))
train <- feature_engineered_dt[train_inds,]
test <- feature_engineered_dt[-train_inds,]
train_clean <- na.omit(train)
train_clean <- train_clean[, .( gametime, freekick, shooter_id, shooter_x, shooter_y,  gk_id, gk_x, gk_y, shot_dist_goal, angle_to_goal, defender_infront, defender_around, goal)]

cv <- trainControl(method = "cv", number=10)
test_clean <- na.omit(test)
test_clean <- test_clean[, .( gametime, freekick, shooter_id, shooter_x, shooter_y,  gk_id, gk_x, gk_y, shot_dist_goal, angle_to_goal, defender_infront, defender_around, goal)]


### logistic regression
model <- train(goal ~.,method="glm", family="binomial",data=train_clean, trControl=cv)
predictions <- predict(model, newdata = test_clean, type = "prob")


### random forrest
model_rf <- train(goal ~.,method="rf",data=train_clean, trControl=cv)
predictions_rf <- predict(model_rf, newdata = test_clean, type = "prob")


### xgboost
X_train <- sparse.model.matrix(~ . -1, data = as.data.frame(train_clean))
X_test <- sparse.model.matrix(~ . -1, data = as.data.frame(test_clean))
y_train <- as.numeric(train_clean$goal)-1
y_test <- as.numeric(test_clean$goal)-1
dtrain <- xgb.DMatrix(data = X_train, label=y_train)
dtest <- xgb.DMatrix(data = X_test, label=y_test)

params <- list(
  objective = "binary:logistic",
  eval_metric= "logloss",
  eta = eta,
  max_depth = max_depth,
  subsample = 0.8,
  colsample_bytree = 0.8
)
eta <- c(0.001,0.005,0.01,01,0.5,1)
max_depth <- c(1,3,4,5,7,9)

results <- data.frame()

eta <- c(0.001,0.005,0.01,0.1,0.5,1)
max_depth <- c(1,3,4,5,7,9)

for (e in eta){
  for (depth in max_depth){
    params <- list(
      objective = "binary:logistic",
      eval_metric= "logloss",
      eta = e,
      max_depth = depth,
      subsample = 0.8,
      colsample_bytree = 0.8
    )
    
    cv_results <- xgb.cv(
      params = params,
      data = dtrain,
      nfold=5,
      nrounds=1000,
      early_stopping_rounds = 50,
      verbose = FALSE
    )
    
    best_iter <- if (is.null(cv_results$best_iteration)) 0 else cv_results$best_iteration
    best_logloss <- if (is.null(cv_results$evaluation_log$test_logloss_mean[cv_results$best_iteration])) 0 else cv_results$evaluation_log$test_logloss_mean[cv_results$best_iteration]
    
    results <- rbind(results,
                     data.frame(eta=e, max_depth = depth, best_iter= best_iter, logloss= best_logloss))
  }
}

which.min(results$logloss)

best_params <- list(
  objective = "binary:logistic",
  eval_metric= "logloss",
  eta = results$eta[which.min(results$logloss)],
  max_depth = results$max_depth[which.min(results$logloss)],
  subsample = 0.8,
  colsample_bytree = 0.8
)

### Preparing features/ formatting the columns correctly for use in the model
feature_engineered_dt$shooter_x <- as.numeric(feature_engineered_dt$shooter_x)
feature_engineered_dt$shooter_y <- as.numeric(feature_engineered_dt$shooter_y)
feature_engineered_dt$gk <- as.character(feature_engineered_dt$gk)
feature_engineered_dt$gk_x <- as.numeric(feature_engineered_dt$gk_x)
feature_engineered_dt$gk_y <- as.numeric(feature_engineered_dt$gk_y)
feature_engineered_dt$shot_dist_goal <- as.numeric(feature_engineered_dt$shot_dist_goal)
feature_engineered_dt$angle_to_goal <- as.numeric(feature_engineered_dt$angle_to_goal)
feature_engineered_dt$defender_around <- as.numeric(feature_engineered_dt$defender_around)
feature_engineered_dt$defender_infront <- as.numeric(feature_engineered_dt$defender_infront)
feature_engineered_dt3<- feature_engineered_dt[, .( gametime, freekick, shooter_id, shooter_x, shooter_y,  gk_id, gk_x, gk_y, shot_dist_goal, angle_to_goal, defender_infront, defender_around, goal)]
feature_engineered_dt3<-na.omit(feature_engineered_dt3)
y_all <- as.numeric(feature_engineered_dt3$goal)-1
X_all <- sparse.model.matrix(~. -goal -predictions_xgb - 1,data=as.data.frame(feature_engineered_dt3))

### creating folds to predict all the values by using different models so we dont train and test with the same data
folds <- createFolds(y_all, k=5, list=TRUE, returnTrain = FALSE)

predictions_xgb_all <- rep(NA_real_, length(y_all))

eta <- c(0.001,0.005,0.01,0.1,0.5,1)
max_depth <- c(1,3,4,5,7,9)

results <- data.frame()


for (fold in seq_along(folds)){
  results <- data.frame()
  test_inds <- folds[[fold]]
  train_inds <- setdiff(seq_along(y_all), test_inds)
  dtrain <- xgb.DMatrix(data=X_all[train_inds,],label=y_all[train_inds])
  dtest <- xgb.DMatrix(data=X_all[test_inds,],label=y_all[test_inds])
  

  
  for (e in eta){
    for (depth in max_depth){
      params <- list(
        objective = "binary:logistic",
        eval_metric= "logloss",
        eta = e,
        max_depth = depth,
        subsample = 0.8,
        colsample_bytree = 0.8
      )
      
      cv_results <- xgb.cv(
        params = params,
        data = dtrain,
        nfold=5,
        nrounds=1000,
        early_stopping_rounds = 50,
        verbose = FALSE
      )
      
      best_iter <- if (is.null(cv_results$best_iteration)) 1 else cv_results$best_iteration
      best_logloss <- if (is.null(cv_results$evaluation_log$test_logloss_mean[cv_results$best_iteration])) 0 else cv_results$evaluation_log$test_logloss_mean[cv_results$best_iteration]
      
      results <- rbind(results,
                       data.frame(eta=e, max_depth = depth, best_iter= best_iter, logloss= best_logloss))
    }
  }
  
  which.min(results$logloss)
  
  best_params <- list(
    objective = "binary:logistic",
    eval_metric= "logloss",
    eta = results$eta[which.min(results$logloss)],
    max_depth = results$max_depth[which.min(results$logloss)],
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  model<- xgb.train(params= best_params, data=dtrain, nrounds = results$best_iter[which.min(results$logloss)])
  predictions_xgb_all[test_inds] <- predict(model, newdata = dtest)
}
feature_engineered_dt4 <- copy(feature_engineered_dt3)
feature_engineered_dt4$predictions_xgb <- NULL
feature_engineered_dt4$predictions_xgb <- predictions_xgb_all
feature_engineered_dt4$goal <- as.numeric(feature_engineered_dt4$goal)-1
### training and predicting the final xgboost model
#best_model_xgb <- xgb.train(params= best_params, data=dtrain, nrounds = results$best_iter[which.min(results$logloss)])
#pred_xgb_new2 <- predict(best_model_xgb, newdata = dtest, type = "prob")


### creating comparison matrix
pred_dt <- data.table()
pred_dt[, ':=' (logit=predictions$`TRUE`, rf=predictions_rf$`TRUE`, goal=test_clean$goal)]
pred_dt$goal_factor <- as.numeric(pred_dt$goal)
pred_dt$goal_factor<- pred_dt$goal_factor-1
pred_dt$xgb_best2 <- pred_xgb_new2

### evaluating the models
log_losses <- list(logLoss(pred_dt$goal_factor, pred_dt$logit),
logLoss(pred_dt$goal_factor, pred_dt$rf),
logLoss(pred_dt$goal_factor, pred_dt$xgb_best2))
print(log_losses)
log_losses[which.min(log_losses)]

### brier scores again xgb best model
brier_score_logit <- mean((pred_dt$logit-pred_dt$goal_factor)^2)
brier_score_rf <- mean((pred_dt$rf-pred_dt$goal_factor)^2)
brier_score_xgb_best2 <- mean((pred_dt$xgb_best2-pred_dt$goal_factor)^2)


### saving model for later usage xg model
xgb.save(best_model_xgb, "best_xgb_model.model")

###creating an output table
out_dt <- test_clean
out_dt$predictions <- pred_xgb_new2
out_dt2 <- merge(out_dt, player_info[, c(-2,-3, -4,-5, -8, -9,-10,-11,-12, -13,-15,-16)], by.x = "shooter_id", by.y = "player.id", all.x=TRUE)
setnames(out_dt2, c("nickname","positionGroupType.x"), c("shooter_name", "shooter_position_type"))
out_dt2 <- merge(out_dt2, player_info[, c(-2,-3, -4,-5, -8, -9,-10,-11,-12, -13,-15,-16)], by.x = "gk_id", by.y = "player.id", all.x=TRUE)
setnames(out_dt2, c("nickname","positionGroupType.x"), c("gk_name", "gk_position_type"))


### model whether a player would shoot from a certain position or not
feature_engineered_dt <- read_csv(paste0(getwd(), "/feature_engineered/all_engineered_with_correct_gks.csv"))
train_new <- copy(feature_engineered_dt4)
train_new <- na.omit(train_new)
train_new$predictions_xgb <- NULL
data_xg <- copy(train_new[,c("predictions_xgb", "goal")])
data_xg<- set_names(data_xg, c("preds", "y"))
data_xg<-data.frame(data_xg)
is.data.frame(data_xg)
str(data_xg)

write_csv(train_new,paste0(getwd(), "/github/Bachelor-Thesis/train_new.csv"))
train_new <- read_csv(paste0(getwd(), "/github/Bachelor-Thesis/train_new.csv"))

##########
#### new comet function creation:
##########
xG_reg <- function(y,x,xg_mod = NULL,...){
  out <- xg_mod
  class(out) <- c("xG",class(out))
  return(out)
}
predict.xG <- function(object,data = NULL,...){
  xg_mod$preds
}

residuals.xG <- function(object, response = NULL, data = NULL, ...) {
  xg_mod$y - xg_mod$preds
}


get_personalized_models <- function(input, xg_mod=data_xg){
  results <- data.table()
  input <- as.data.table(input)
  rgax_data <- data.table()
  
  
  for (shooter in unique(input$shooter_id)){
    if (as.numeric(nrow(df[df$shooter_id==shooter,]))>=5){
      input$chosen_shooter <- as.factor("0")
      input[shooter_id==shooter]$chosen_shooter <- as.factor("1")
      base_input <- copy(input)
      df <-as.data.frame(base_input)
      df$goal <- as.numeric(df$goal)#-1
    
    
      GCM <- comet(
        formula= goal ~ chosen_shooter | . - chosen_shooter, 
        data = df,
        test = "gcm",
        regYonZ="xG_reg",
        regXonZ="tuned_rf",
        args_XonZ = list(probability = TRUE),
        args_YonZ = list(xg_mod = xg_mod),
        type = "scalar",
        verbose = 0,
        coin = TRUE
      )
      #input$shot_expectations <- rf$predictions[,"1"]
      #results<- rbindlist(list(results, input))
      rGAX <- sum(GCM$rY * GCM$rX)
      tstat <- independence_test(GCM$rY ~ GCM$rX, teststat = "scalar")
      sd <- sqrt(variance(tstat))
      ci <- c(rGAX - 1.96 * sd, rGAX + 1.96 * sd)
      goals_scored <- sum(as.numeric(df[df$shooter_id==shooter, "goal"]))
      GAX <- goals_scored - sum(df[df$shooter_id==shooter, "predictions_xgb"])
      shots <- as.numeric(nrow(df[df$shooter_id==shooter,]))
      
      rgax_data <- data.table(
        shooter = shooter,
        number_of_shots= shots,
        non_penalty_goals_scored = goals_scored,
        GAX = GAX,
        rGAX = rGAX,
        tstat = statistic(tstat),
        sd = sd,
        ci_lower = ci[1],
        ci_upper = ci[2]
      )
      results <- rbindlist(list(results, rgax_data))
    }
  }
  return(results)
}

### preparations for rGAX/ creating residuals
dt_new5<-get_personalized_models(train_new)

dt_new5 <- merge(dt_new5, player_info[, c(-2,-3, -4,-5, -8, -9,-10,-11,-12, -13,-15,-16)], by.x = "shooter", by.y = "player.id", all.x=TRUE)
setnames(dt_new5, c("nickname","positionGroupType.x"), c("shooter_name", "shooter_position_type"))
rgax_data_final_with_shots <- dt_new


### saving the rgax model
write_csv(rgax_data_final_with_shots,paste0(getwd(), "/final_output/rgax_final_output.csv"))
#write_csv(dt_new, paste0(getwd(), "/final_output/prepared_for_rgax.csv"))

### correlation for gax and rgax
cor <- cor(rgax_data_final_with_shots$GAX, rgax_data_final_with_shots$rGAX, method = "spearman") #0.82
cor2 <- cor(dt_new5$GAX, dt_new5$rGAX)
