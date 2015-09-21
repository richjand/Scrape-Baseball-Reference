##Functions to scrape and munge box scores from Baseball Reference##

library(magrittr)
library(stringr)
library(dplyr)
library(rvest)
library(lubridate)

nl97 <- c('ARI', 'ATL', 'FLA', 'NYM', 'MON', 'WAS',
         'PHI','HOU','PIT','CIN','STL','CHC','SFG',
         'LAD','COL','SDP')

nl9812 <- c(nl97, 'MIL')

nl1314 <- nl9812[nl9812!='HOU']



replacename <- function(dat){ ##Replaces team names found in box scores with Lahman equivalents##
  dat = str_replace(dat,'AnaheimAngelsbatting', 'ANA')
  dat = str_replace(dat,'BaltimoreOriolesbatting','BAL')
  dat = str_replace(dat,'ArizonaDiamondbacksbatting','ARI')
  dat = str_replace(dat,'AtlantaBravesbatting','ATL')
  dat = str_replace(dat,'BostonRedSoxbatting','BOS')
  dat = str_replace(dat,'ChicagoCubsbatting','CHC')
  dat = str_replace(dat,'ChicagoWhiteSoxbatting', 'CHW')
  dat = str_replace(dat,'CincinnatiRedsbatting','CIN')
  dat = str_replace(dat,'ClevelandIndiansbatting','CLE')
  dat = str_replace(dat,'ColoradoRockiesbatting','COL')
  dat = str_replace(dat,'DetroitTigersbatting','DET')
  dat = str_replace(dat,'FloridaMarlinsbatting','FLA')
  dat = str_replace(dat,'MiamiMarlinsbatting','FLA')
  dat = str_replace(dat,'HoustonAstrosbatting','HOU')
  dat = str_replace(dat,'KansasCityRoyalsbatting','KCR')
  dat = str_replace(dat,'LosAngelesDodgersbatting','LAD')
  dat = str_replace(dat,'MilwaukeeBrewersbatting','MIL')
  dat = str_replace(dat,'MinnesotaTwinsbatting','MIN')
  dat = str_replace(dat,'MontrealExposbatting','MON')
  dat = str_replace(dat,'NewYorkMetsbatting','NYM')
  dat = str_replace(dat,'NewYorkYankeesbatting','NYY')
  dat = str_replace(dat,'OaklandAthleticsbatting','OAK')
  dat = str_replace(dat,'PhiladelphiaPhilliesbatting','PHI')
  dat = str_replace(dat,'PittsburghPiratesbatting','PIT')
  dat = str_replace(dat,'SanDiegoPadresbatting','SDP')
  dat = str_replace(dat,'SeattleMarinersbatting','SEA')
  dat = str_replace(dat,'SanFranciscoGiantsbatting','SFG')
  dat = str_replace(dat,'StLouisCardinalsbatting','STL')
  dat = str_replace(dat,'TampaBayDevilRaysbatting','TBD')
  dat = str_replace(dat,'TampaBayRaysbatting','TBD')
  dat = str_replace(dat,'TexasRangersbatting','TEX')
  dat = str_replace(dat,'TorontoBlueJaysbatting','TOR')
  dat = str_replace(dat,'WashingtonNationalsbatting','WAS')
  return(dat)
}


grabyear <- function(year){
  y <- paste('http://www.baseball-reference.com/boxes/',year,'.shtml', sep = '') 
  year_html <- html(y) ##Gets year page that contains links to dates
  year_links <- html_nodes(year_html, 'a')
  date_links <- html_attr(year_links, 'href')[grepl('play-index/st', html_attr(year_links, 'href'))] ##extract date links##
  yeargames <- list()
  for (date in 1:length(date_links)){
    datepage <- paste("http://www.baseball-reference.com",date_links[date], sep = '') 
    game_links <- html(datepage) ##Opens date page that contains links to box scores##
    print(datepage)
    game_links <- html_nodes(game_links, 'a')
    box_links <- html_attr(game_links, 'href')[grepl('boxes', html_attr(game_links, 'href'))] ##Extract box score links##
    boxes <- list()
    for (boxlink in 1:length(box_links)){
      url = paste('http://www.baseball-reference.com',box_links[boxlink], sep = '')
      print(url)
      bref = html(url) ##Opens box score
      tables <- html_nodes(bref, "table")
      batting_tables <- grep('batting', html_attr(tables, 'id')) ##Identifies batting box scores##
      keeptables <- tables[batting_tables]
      gameid <- str_sub(url, 45, -7)
      
      hometeam <- keeptables%>%
        extract2(2) %>%
        html_table() %>%
        rename(playnotes = ) %>%
        select(Batting, AB, R, H, RBI, BB, SO, PA, Pit, playnotes) %>%
        filter(!(Batting %in% c('','Team Totals'))) %>%
        mutate(teamID = replacename(html_attrs(tables[[batting_tables[2]]])[2]),
               links = unlist(html_attrs(html_nodes(tables[[batting_tables[2]]], 'a'))),
               game_id = gameid,
               playerID = str_sub(links,12,-7),
               home_team = 1,
               h1b = 0,
               h2b = 0,
               h3b = 0,
               hr = 0,
               hbp = 0,
               ibb = 0) %>%
        select(-links)
      
      awayteam <- keeptables %>%
        extract2(1) %>%
        html_table() %>%
        rename(playnotes = ) %>%
        select(Batting, AB, R, H, RBI, BB, SO, PA, Pit, playnotes) %>%
        filter(!(Batting %in% c('','Team Totals'))) %>%
        mutate(teamID = replacename(html_attrs(tables[[batting_tables[1]]])[2]),
               links = unlist(html_attrs(html_nodes(tables[[batting_tables[1]]], 'a'))),
               game_id = gameid,
               playerID = str_sub(links,12,-7),
               home_team = 0,
               h1b = 0,
               h2b = 0,
               h3b = 0,
               hr = 0,
               hbp = 0,
               ibb = 0) %>%
        select(-links)
      both <- rbind(hometeam, awayteam)
      both$playnotes[is.na(both$playnotes)] <- ''
      both_notes <- str_split(both$playnotes, ',') ##Split playnotes into component parts, allowing identification of hit type##
      
      
      for (i in 1:nrow(both)){
        #print(i)
        notes = both_notes[[i]]
        if (notes[1]!='') { ##No notes implies box score is fine##
          if (TRUE %in% grepl('2B', notes)){
            if ('2B' %in% notes) { ##Looks for doubles in notes
              both$h2b[i] = 1} else if ('2·2B' %in% notes){
                both$h2b[i] = 2} else if ('3·2B' %in% notes){
                  both$h2b[i] = 3} else if ('4·2B' %in% notes){
                    both$h2b[i] = 4}
          }
          if (TRUE %in% grepl('3B', notes)){
            if ('3B' %in% notes) { ##Triples in notes
              both$h3b[i] = 1} else if ('2·3B' %in% notes){
                both$h3b[i] = 2} else if ('3·3B' %in% notes){
                  both$h3b[i] = 3} else if ('4·3B' %in% notes){
                    both$h3b[i] = 4}
          }
          if (TRUE %in% grepl('HR', notes)){
            if ('HR' %in% notes) {
              both$hr[i] = 1} else if ('2·HR' %in% notes){
                both$hr[i] = 2} else if ('3·HR' %in% notes){
                  both$hr[i] = 3} else if ('4·HR' %in% notes){
                    both$hr[i] = 4}
          }
          if (TRUE %in% grepl('HBP', notes)){
            if ('HBP' %in% notes) {
              both$hbp[i] = 1} else if ('2·HBP' %in% notes){
                both$hbp[i] = 2} else if ('3·HBP' %in% notes){
                  both$hbp[i] = 3} else if ('4·HBP' %in% notes){
                    both$hbp[i] = 4}
          }
          if (TRUE %in% grepl('IBB', notes)){
            if ('IBB' %in% notes) {
              both$ibb[i] = 1} else if ('2·IBB' %in% notes){
                both$ibb[i] = 2} else if ('3·IBB' %in% notes){
                  both$ibb[i] = 3} else if ('4·IBB' %in% notes){
                    both$ibb[i] = 4}
          }
        }
      }
      
      both$h1b <- both$H - both$h2b - both$h3b - both$hr
      both$ubb <- both$BB - both$ibb
      both$position <- sapply(str_split(both$Batting,'  '),'[[',2) ##Extract position from batter info##
      
      boxes[[boxlink]] <- both 
      dayboxes <- rbind_all(boxes)#combine boxes from one day
    }
    yeargames[[date]] <- dayboxes 
    
  }
  yearboxes <- rbind_all(yeargames)
  yearboxes$year <- year
  yearboxes$team_league <- ifelse(yearboxes$year == 1997 & yearboxes$teamID %in% nl97, 'NL', 
                                 ifelse(yearboxes$year %in% 1998:2012 & yearboxes$teamID %in% nl9812, 'NL',
                                        ifelse(yearboxes$year %in% 2013:2014 & yearboxes$teamID %in% nl1314, 'NL', 'AL')))
  yearboxes$date <- dmy(
    str_c(str_sub(yearboxes$game_id, 10, 11),
          str_sub(yearboxes$game_id, 8, 9),
          str_sub(yearboxes$game_id, 4, 7),
          sep = '-'
    )
  )
  ##combine days from the year
  #ywoba <- filter(woba_weights, Season == year) ##Optional code to calculate game Woba using weights from Fangraphs
  #yearboxes <- yearboxes %>%
    #filter(PA != 0) %>%
    #mutate(woba = (ywoba$wBB*ubb + ywoba$w1B*h1b + ywoba$w2B*h2b + ywoba$w3B*h3b + ywoba$wHR*hr + ywoba$wHBP*hbp)/(PA-ibb))
  return(yearboxes)
}

z <- grabyear(1999)
write.csv(z, file = 'boxes1999.csv', row.names = F)
rm(z)