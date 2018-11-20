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

LANE.CREEPS <- c("npc_dota_creep_badguys_melee",
                 "npc_dota_creep_badguys_melee_diretide",
                 "npc_dota_creep_badguys_melee_upgraded",
                 "npc_dota_creep_badguys_ranged",
                 "npc_dota_creep_badguys_ranged_diretide",
                 "npc_dota_creep_badguys_ranged_upgraded",
                 "npc_dota_creep_goodguys_melee",
                 "npc_dota_creep_goodguys_melee_diretide",
                 "npc_dota_creep_goodguys_melee_upgraded",
                 "npc_dota_creep_goodguys_ranged",
                 "npc_dota_creep_goodguys_ranged_diretide",
                 "npc_dota_creep_goodguys_ranged_upgraded")

NEUTRAL.CREEPS <-c("npc_dota_neutral_alpha_wolf",
                   "npc_dota_neutral_centaur_khan",
                   "npc_dota_neutral_centaur_outrunner",
                   "npc_dota_neutral_dark_troll_warlord",
                   "npc_dota_neutral_fel_beast",
                   "npc_dota_neutral_ghost",
                   "npc_dota_neutral_giant_wolf",
                   "npc_dota_neutral_harpy_scout",
                   "npc_dota_neutral_harpy_storm",
                   "npc_dota_neutral_polar_furbolg_champion",
                   "npc_dota_neutral_polar_furbolg_ursa_warrior",
                   "npc_dota_neutral_dark_troll",
                   "npc_dota_neutral_forest_troll_berserker",
                   "npc_dota_neutral_forest_troll_high_priest",
                   "npc_dota_neutral_kobold",
                   "npc_dota_neutral_kobold_tunneler",
                   "npc_dota_neutral_kobold_taskmaster",
                   "npc_dota_neutral_mud_golem",
                   "npc_dota_neutral_ogre_mauler",
                   "npc_dota_neutral_ogre_magi",
                   "npc_dota_neutral_satyr_trickster",
                   "npc_dota_neutral_satyr_soulstealer",
                   "npc_dota_neutral_satyr_hellcaller",
                   "npc_dota_neutral_gnoll_assassin",
                   "npc_dota_neutral_wildkin",
                   "npc_dota_neutral_enraged_wildkin")

NECRONOMICON.CREEPS <- c("npc_dota_necronomicon_archer_1",
                         "npc_dota_necronomicon_archer_2",
                         "npc_dota_necronomicon_archer_3",
                         "npc_dota_necronomicon_warrior_1",
                         "npc_dota_necronomicon_warrior_2",
                         "npc_dota_necronomicon_waarrior_3")

ANCIENT.CREEPS <- c("npc_dota_neutral_black_drake",
                    "npc_dota_neutral_black_dragon",
                    "npc_dota_neutral_blue_dragonspawn_sorcerer",
                    "npc_dota_neutral_blue_dragonspawn_overseer",
                    "npc_dota_neutral_granite_golem",
                    "npc_dota_neutral_elder_jungle_stalker",
                    "npc_dota_neutral_prowler_acolyte",
                    "npc_dota_neutral_prowler_shaman",
                    "npc_dota_neutral_rock_golem",
                    "npc_dota_neutral_small_thunder_lizard",
                    "npc_dota_neutral_jungle_stalker",
                    "npc_dota_neutral_big_thunder_lizard",
                    "npc_dota_roshan")

###############################################################################
# internal process functions
#   Contains function that are used by other functions in data_process.R
###############################################################################
process_kills <- function(killed, creeps = c()) {
  kills <- 0
  for (creep in creeps) {
    if (!is.null(killed[[creep]]) && !is.na(killed[[creep]])) {
      kills <- kills + as.integer(killed[[creep]])
    }
  }
  
  return(kills)
}

process_match_player_data <- function(player, match.duration) {
  player.data <- list()
  
  player.data$account.id <- player$account_id #y
  player.data$name <- player$name
  player.data$persona.name <- player$personaname
  player.data$region <- player$region
  player.data$player.slot <- player$player_slot #y
  player.data$is.radiant <- ifelse(!is.null(player$isRadiant),
                                   player$isRadiant, player$player_slot < 128)
  player.data$hero.id <- player$hero_id #y
  player.data$level <- player$level #y
  player.data$lane <- player$lane #y
  player.data$lane.role <- player$lane_role #y
  player.data$item0 <- player$item_0 #y
  player.data$item1 <- player$item_1 #y
  player.data$item2 <- player$item_2 #y
  player.data$item3 <- player$item_3 #y
  player.data$item4 <- player$item_4 #y
  player.data$item5 <- player$item_5 #y
  player.data$backpack0 <- player$backpack_0 #y
  player.data$backpack1 <- player$backpack_1 #y
  player.data$backpack2 <- player$backpack_2 #y
  player.data$firstblood.claimed <- player$firstblood_claimed #y
  player.data$kills <- player$kills #y
  player.data$deaths <- player$deaths #y
  player.data$assists <- player$assists #y
  player.data$kda <- ifelse(!is.null(player$kda), player$kda,
                            (player$kills + player$assists) / max(1, player$deaths))
  player.data$total.gold <- ifelse(!is.null(player$total_gold), player$total_gold,
                                   as.integer(player$gold_per_min * match.duration / 60))
  player.data$gold <- player$gold #y
  player.data$gold.spent <- player$gold_spent #y
  player.data$gold.per.min <- player$gold_per_min #y
  player.data$total.xp <- ifelse(!is.null(player$total_xp), player$total_xp,
                                 as.integer(player$xp_per_min * match.duration / 60))
  player.data$xp_per_min <- player$xp_per_min #y
  player.data$actions_per_min <- ifelse(!is.null(player$actions_per_min), player$actions_per_min,
                                        as.integer(rowSums(player$actions, na.rm = T) * 60 / match.duration))
  player.data$last.hits <- player$last_hits #y
  player.data$denies <- player$denies #y
  player.data$lane.kills <- ifelse(!is.null(player$lane_kills), player$lane_kills,
                                   process_kills(player$killed, LANE.CREEPS))
  player.data$hero.kills <- ifelse(!is.null(player$hero_kills), player$hero_kills, player$kills)
  player.data$neutral.kills <- ifelse(!is.null(player$neutral_kills), player$neutral_kills,
                                      process_kills(player$killed, NEUTRAL.CREEPS))
  player.data$tower.kills <- ifelse(!is.null(player$tower_kills), player$tower_kills, player$towers_killed)
  player.data$courier.kills <- ifelse(!is.null(player$courier_kills), player$courier_kills,
                                      ifelse(is.null(player$killed$npc_dota_courier) || is.na(player$killed$npc_dota_courier),
                                             0, player$killed$npc_dota_courier))
  player.data$necronomicon.kills <- ifelse(!is.null(player$necronomicon_kills), player$necronomicon_kills,
                                           process_kills(player$killed, NECRONOMICON.CREEPS)) 
  player.data$ancient.kills <- ifelse(!is.null(player$ancient_kills), player$ancient_kills,
                                      process_kills(player$killed, ANCIENT.CREEPS))
  player.data$sentry.kills <- ifelse(!is.null(player$sentry_kills), player$sentry_kills,
                                     ifelse(is.na(player$killed$npc_dota_sentry_wards) || is.na(player$killed$npc_dota_sentry_wards),
                                            0, player$killed$npc_dota_sentry_wards))
  player.data$observer.kills <- ifelse(!is.null(player$observer_kills), player$observer_kills,
                                       ifelse(is.null(player$killed$npc_dota_observer_wards) || is.na(player$killed$npc_dota_observer_wards),
                                              0, player$killed$npc_dota_observer_wards))
  player.data$roshan.kills <- ifelse(!is.null(player$roshan_kills), player$roshan_kills, player$roshans_killed)
  player.data$stuns <- player$stuns #y
  player.data$purchase.log <- data.table::rbindlist(player$purchase_log, fill = T) #y
  player.data$runes.log <- data.table::rbindlist(player$runes_log, fill = T) #y
  player.data$metrics_t <- data.frame(min = 1:length(player$gold_t),
                                      gold_t = unlist(player$gold_t), #y
                                      xp_t = unlist(player$xp_t), #y
                                      lh_t = unlist(player$lh_t), #y
                                      dn_t = unlist(player$dn_t)) #y
  player.data$abilities.upgrade <- data.frame(level = 1:length(player$ability_upgrades_arr), #y
                                              ability = unlist(player$ability_upgrades_arr))
  
  return(player.data)
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
    match <- json[[as.character(match.id)]]
  } else {
    match <- ROpenDota::get_match_details(match.id)
    if (is.null(match) || is.na(match)) {
      return(NULL)
    }
  }
  
  match.list.names <- names(match)
  match.data <- list()
  
  match.data$match.id <- as.character(match.id)
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
    match.id = "3497210298"
    match <- json[[as.character(match.id)]]
  } else {
    match <- ROpenDota::get_match_details(match.id)
    if (is.null(match) || is.na(match)) {
      return(NULL)
    }
  }
  
  match.players <- list()
  if (use.dpc) {
    match.players.len <- length(match$players)
  } else {
    match.players.len <- nrow(match$players)
  }
  for (i in 1:match.players.len) {
    if (use.dpc) {
      player <- match$players[[i]]
    } else {
      player <- match$players[i,]
    }
    
    player.data <- process_match_player_data(player, match$duration)
    match.players[[i]] <- player.data
  }
  
  return(match.players)
}

get_player_match_details <- function (account.id, limit = 100, earlier.than.date = NULL) {
  sql <- "SELECT player_matches.*, matches.start_time, matches.duration FROM player_matches LEFT JOIN matches ON player_matches.match_id = matches.match_id"
  sql <- paste(sql, "WHERE player_matches.account_id =", account.id)
  
  if (!is.null(earlier.than.date)) {
    earlier.than.date <- ymd_hms(earlier.than.date)
    if (is.na(earlier.than.date)) {
      print("earlier.than.date is not a valid date")
      return(NULL)
    }
    earlier.than.date <- as.numeric(earlier.than.date)
    sql <- paste(sql, "AND matches.start_time <=", earlier.than.date)
  }
  sql <- paste(sql, "ORDER BY start_time DESC")
  
  limit <- max(0, limit)
  if (limit > 0) {
    sql <- paste(sql, "LIMIT", limit)
  }
  
  print(sql)
  out <- get_by_sql(sql)
  
  player.match.details <- list()
  for(i in 1:nrow(out$rows)) {
    player.match.details[[i]] <- process_match_player_data(out$rows[1,], out$rows$duration)
  }
  
  return(player.match.details)
}

if (use.dpc) {
  json <- debug_load_test_dataset()
}