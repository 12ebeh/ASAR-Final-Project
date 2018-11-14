###############################################################################
# libraries functions
###############################################################################
#library(jsonlite)
library(tidyr)
library(dplyr)
library(lubridate)

###############################################################################
# debug functions
###############################################################################
use.dpc <- F
debug_load_test_dataset <- function() {
  json <- jsonlite::read_json("DPC.json")
  return(json)
}

###############################################################################
# additional api functions
#   Contains function that are not implemented in the ROpenDota package but
#   available in the web api
###############################################################################
get_url_internal <- function(url) {
  out <- tryCatch({
    jsonlite::fromJSON(url)
  }, error = function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    return(NA)
  }, warning = function(cond) {
    message(paste("URL caused a warning:", url))
    message("Here's the original warning message:")
    message(cond)
    return(NULL)
  })
  
  return(out)
}

get_schema <- function (table = NULL) {
  url <- "https://api.opendota.com/api/schema"
  
  out <- get_url_internal(url)
  
  if (!is.null(table)) {
    out <- tryCatch({
      filter(out, out$table_name == table)
    }, error = function (cond) {
      message(paste("Table not found:", table))
      message("Original error message:")
      message(cond)
      return(NA)
    }, warning = function(cond) {
      message(paste("Table filter caused a warning:", table))
      message("Here's the original warning message:")
      message(cond)
      return(NULL)
    })
  }
  
  return(out)
}

get_by_sql <- function (sql) {
  prefix <- "https://api.opendota.com/api/explorer"
  sql <- URLencode(sql)
  url <- paste(prefix, "?sql=", sql, sep = "")
  out <- get_url_internal(url)
  
  return(out)
}

get_pro_matches <- function (limit = 100, less_than_match_id = NULL) {
  prefix <- "https://api.opendota.com/api/proMatches"

  ret <- data.frame()
  while (nrow(ret) < limit) {
    url <- prefix
    if (!is.null(less_than_match_id )) {
      url <- paste(prefix, "?less_than_match_id=", less_than_match_id, sep = "")
    }
    
    out <- get_url_internal(url)
    
    less_than_match_id <- out$match_id[nrow(out)]
    ret <- rbind(ret, out)
  }
  
  ret <- ret[1:limit,]
  ret$start_time <- ymd_hms(as.POSIXct(ret$start_time, origin = "1970-01-01"))
  return(ret)
}

get_teams <- function(team_id = NULL) {
  prefix <- "https://api.opendota.com/api/teams"
  url <- prefix
  if (!is.null(team_id)) {
    url <- paste(prefix, "/", team_id, sep = "")
  }
  
  out <- get_url_internal(url)
  
  return(out)
}

get_team_matches <- function(team_id) {
  prefix <- "https://api.opendota.com/api/teams/"
  url <- paste(prefix, team_id, "/matches", sep = "")
  out <- get_url_internal(url)
  
  return(out)
}

get_team_players <- function(team_id) {
  prefix <- "https://api.opendota.com/api/teams/"
  url <- paste(prefix, team_id, "/players", sep = "")
  out <- get_url_internal(url)
  
  return(out)
}

###############################################################################
# wrapper functions
#   Contain functions that are implemented in the ROpenDota package.
#   Additional code is added to filter out unused data variables for cleaner
#   results.
###############################################################################
get_match_data <- function(match.id) {
  if (use.dpc) {
    #match.id = "3497210298"
    match <- json[[match.id]]
  } else {
    match <- ROpenDota::get_match_details(match.id)
  }
  
  match.list.names <- names(match)
  match.data <- c()

  match.data$match.id <- strtoi(match.id)
  match.data$start.time <- ymd_hms(as.POSIXct(match$start, origin = "1970-01-01"))
  match.data$duration <- match$duration
  match.data$region <- match$region
  match.data$patch <- match$patch
  match.data$league.id <- match$league$leagueid
  match.data$league.name <- match$league$name
  match.data$series.id <- match$series_id
  match.data$series.type <- match$series_type
  match.data$positive.votes <- match$positive_votes
  match.data$negetive.votes <- match$negative_votes
  match.data$radiant.win <- match$radiant_win
  match.data$radiant.team.id <- match$radiant_team$team_id
  match.data$radiant.team.name <- match$radiant_team$name
  match.data$dire.team.id <- match$dire_team$team_id
  match.data$dire.team.name <- match$dire_team$name
  match.data$first.blood.time <- match$first_blood_time
  match.data$barracks.status.radiant <- match$barracks_status_radiant
  match.data$barracks.status.dire <- match$barracks_status_dire
  match.data$tower.status.radiant <- match$tower_status_radiant
  match.data$tower.status.dire <- match$tower_status_dire
  match.data$throw <- ifelse("throw" %in% match.list.names, match$throw, NA)
  match.data$comeback <- ifelse("comeback" %in% match.list.names, match$comeback, NA)
  match.data$win <- ifelse("win" %in% match.list.names, match$win, NA)
  match.data$loss <- ifelse("loss" %in% match.list.names, match$loss, NA)
  match.data$gold.xp.adv <- data.frame(min = 1:length(match$radiant_gold_adv),
                                       radiant.gold.adv = unlist(match$radiant_gold_adv),
                                       radiant.xp.adv = unlist(match$radiant_xp_adv))
  match.data$draft.picks <- 
    if (class(match$draft_timings) == "data.frame") {
      match$draft_timings
    } else {
      data.table::rbindlist(
        lapply(match$draft_timings,
               function (x) lapply(x, function (y) ifelse(is.null(y), NA, y))
        )
      )
    }
  match.data$objectives <-
    if(class(match$objectives) == "data.frame") {
      match$objectives
    } else {
      data.table::rbindlist(match$objectives, fill = T)
    }
  
  return(match.data)
}

get_match_players <- function (match.id) {
  if (use.dpc) {
    #match.id = "3497210298"
    match <- json[[match.id]]
  } else {
    match <- ROpenDota::get_match_details(match.id)
  }
  
  match.players <- c()
  match.players.len <- length(match$players)
  for (i in 1:match.players.len) {
    player.data <- c()
    if (use.dpc) {
      player <- match$players[[i]]
    } else {
      player <- match$players[i,]
    }
    
    player.data$account.id <- player$account_id
    player.data$name <- player$name
    player.data$persona.name <- player$personaname
    player.data$region <- player$region
    player.data$is.radiant <- player$isRadiant
    player.data$player.slot <- player$player_slot
    player.data$hero.id <- player$hero_id
    player.data$level <- player$level
    player.data$lane <- player$lane
    player.data$lane.role <- player$lane_role
    player.data$item0 <- player$item_0
    player.data$item1 <- player$item_1
    player.data$item2 <- player$item_2
    player.data$item3 <- player$item_3
    player.data$item4 <- player$item_4
    player.data$item5 <- player$item_5
    player.data$backpack0 <- player$backpack_0
    player.data$backpack1 <- player$backpack_1
    player.data$backpack2 <- player$backpack_2
    player.data$firstblood.claimed <- player$firstblood_claimed
    player.data$kills <- player$kills
    player.data$deaths <- player$deaths
    player.data$assists <- player$assists
    player.data$kda <- player$kda
    player.data$total.gold <- player$total_gold
    player.data$gold <- player$gold
    player.data$gold.spent <- player$gold_spent
    player.data$gold.per.min <- player$gold_per_min
    player.data$total.xp <- player$total_xp
    player.data$xp_per_min <- player$xp_per_min
    player.data$actions_per_min <- player$actions_per_min
    player.data$last.hits <- player$last_hits
    player.data$denies <- player$denies
    player.data$lane.kills <- player$lane_kills
    player.data$hero.kills <- player$hero_kills
    player.data$neutral.kills <- player$neutral_kills
    player.data$tower.kills <- player$tower_kills
    player.data$courier.kills <- player$courier_kills
    player.data$necronomicon.kills <- player$necronomicon_kills
    player.data$ancient.kills <- player$ancient_kills
    player.data$sentry.kills <- player$sentry_kills
    player.data$observer.kills <- player$observer_kills
    player.data$roshan.kills <- player$roshan_kills
    player.data$stuns <- player$stuns
    player.data$purchase.log <- data.table::rbindlist(player$purchase_log, fill = T)
    player.data$runes.log <- data.table::rbindlist(player$runes_log, fill = T)
    player.data$metrics_t <- data.frame(min = 1:length(player$gold_t),
                                        gold_t = unlist(player$gold_t),
                                        xp_t = unlist(player$xp_t),
                                        lh_t = unlist(player$lh_t),
                                        dn_t = unlist(player$dn_t))
    player.data$abilities.upgrade <- data.frame(level = 1:length(player$ability_upgrades_arr),
                                                ability = unlist(player$ability_upgrades_arr))
    
    match.players[i] <- player.data
  }
  
  return(match.players)
}

if (use.dpc) {
  json <- debug_load_test_dataset()
}