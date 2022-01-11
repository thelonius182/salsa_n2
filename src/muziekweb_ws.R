suppressWarnings(suppressPackageStartupMessages(library(httr)))
suppressWarnings(suppressPackageStartupMessages(library(xml2)))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))

muw_track_elm <- function(muw_req, muw_col_name, muw_xpath_var = NULL) {
  
  muw_xpath_base <- "//Result/Album/"
  muw_xpath_pfx <- case_when(muw_req == "t" ~ "Tracks/Track/",
                             muw_req == "p" ~ "Tracks/Track/Performers/Performer/",
                             muw_req == "a" ~ "AlbumTitle[@Language='nl']",
                             muw_req == "g" ~ "MusicStyles/MainCategory[1]/"
                   )
  result <- xml_text(xml_find_all(muw_xml, 
                                  xpath = paste0(muw_xpath_base,
                                                 muw_xpath_pfx,
                                                 muw_xpath_var))
                     ) %>% as_tibble()
  names(result) <- muw_col_name
  return(result)
}

muw_result <- GET("https://api.cdr.nl/ExtendedInfo/v3/albumInformation.xml?albumID=CEX1835",
authenticate("93f502b3-519e-4643-8bd0-8ffddb179781", "FSEB21P9")
)

muw_txt <- content(x = muw_result, as = "text", type = "text/xml", encoding = "UTF-8")
muw_xml <- as_xml_document(muw_txt)

muw_album <- muw_track_elm("a", muw_col_name = "album")
# muw_gen_a <- muw_track_elm("g", "Name[@Language='nl']")
# muw_gen_b <- muw_track_elm("g", "Category/Name[@Language='nl']")
# muw_gen_c <- muw_track_elm("g", "Category/Style/Name[@Language='nl']")

muw_tracks_id <- muw_track_elm("t", "AlbumTrackID", muw_col_name = "track_id")
muw_tracks_len <- muw_track_elm("t", "PlayTimeInSec", muw_col_name = "secs")
muw_tracks_tit <- muw_track_elm("t", "TrackTitle[@Language='nl']", muw_col_name = "titel")
# muw_tracks_tit_b <- muw_track_elm("t", "UniformTitle[@Language='nl']")
muw_tracks_tit_add <- muw_track_elm("t", "AddonForUniformTitle[@Language='nl']", muw_col_name = "titel_deel")

muw_prf_n <- xml_attr(xml_find_all(muw_xml, 
                                   "//Result/Album/Tracks/Track/Performers"), 
                      "Count") %>% as_tibble()
names(muw_prf_n) <- "n_uitvoerenden"
muw_prf_name <- muw_track_elm("p", "PresentationName", muw_col_name = "uitvoerende")
muw_prf_role <- muw_track_elm("p", "Role[@Language='nl']", muw_col_name = "rol")
