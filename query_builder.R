library(jsonlite)
library(tidyr)
library(dplyr)
library(ROpenDota)
library(lubridate)

ROpenDota::get_played_heroes(85008179)
player$personaname
played.heroes <- get_played_heroes(85008179)

ymd_hms(as.POSIXct(played.heroes$last_played[1], origin = "1970-01-01"))

heroes <- get_heroes()
filter(heroes, id==53)

query_opendota <- function (sql) {
  sql <- URLencode(sql)
  prefix <- "https://api.opendota.com/api/explorer?sql="
  url <- paste(prefix, sql, sep = "")
  out <- tryCatch({
    fromJSON(url)
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

query_opendota("select * from notable_players where team_name='Vici Gaming' limit 10")
