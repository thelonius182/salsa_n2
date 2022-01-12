suppressWarnings(suppressPackageStartupMessages(library(httr)))
suppressWarnings(suppressPackageStartupMessages(library(xml2)))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(keyring)))

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

# set_muw_result()
muw_album_id = "CLX1261"
muw_usr <- key_get(service = "muziekweb-ws-usr")
muw_pwd <- key_get(service = "muziekweb-ws-pwd")

muw_result <- GET(url = paste0("https://api.cdr.nl/ExtendedInfo/v3/albumInformation.xml?albumID=",
                               muw_album_id),
                  authenticate(muw_usr, muw_pwd)
)
                  

muw_txt <- content(x = muw_result, as = "text", type = "text/xml", encoding = "UTF-8")
muw_xml <- as_xml_document(muw_txt)

muw_album <- muw_track_elm("a", muw_col_name = "album")
# muw_gen_a <- muw_track_elm("g", "Name[@Language='nl']")
# muw_gen_b <- muw_track_elm("g", "Category/Name[@Language='nl']")
# muw_gen_c <- muw_track_elm("g", "Category/Style/Name[@Language='nl']")

muw_tracks_id <- muw_track_elm("t", "AlbumTrackID", muw_col_name = "track_id")
muw_tracks_len <- muw_track_elm("t", "PlayTimeInSec", muw_col_name = "secs")
muw_tracks_tit <- muw_track_elm("t", "TrackTitle[@Language='nl']", muw_col_name = "titel_en_deel")
muw_tracks_tit_b <- muw_track_elm("t", "UniformTitle[@Language='nl']", muw_col_name = "titel")
muw_tracks_tit_add <- muw_track_elm("t", "AddonForUniformTitle[@Language='nl']", muw_col_name = "deel_in_titel")

muw_prf_n <- xml_attr(xml_find_all(muw_xml, 
                                   "//Result/Album/Tracks/Track/Performers"), 
                      "Count") %>% as_tibble()
names(muw_prf_n) <- "n_uitvoerenden"
muw_prf_name <- muw_track_elm("p", "PresentationName", muw_col_name = "uitvoerende")
muw_prf_role <- muw_track_elm("p", "Role[@Language='nl']", muw_col_name = "rol")

# compile uitvoerenden
prf_n_idx <- 0L
muw_prf <- NULL

for (prf_n_chr in muw_prf_n$n_uitvoerenden) {
  # TEST
  # prf_n_chr = "3"
  # TEST
  
  prf_tib_by_track <- NULL
  prf_n <- as.integer(prf_n_chr)
  n1_start <- prf_n_idx + 1
  n1_stop <- prf_n_idx + prf_n
  
  for (n1 in n1_start:n1_stop) {
    
    # TEST
    # n1 <- 2L
    # TEST
    
    cur_prf_by_track <- tibble(prf_txt = paste0(muw_prf_name$uitvoerende[[n1]],
                                                " (",
                                                muw_prf_role$rol[[n1]],
                                                ")")
    )
    
    if (is.null(prf_tib_by_track)) {
      prf_tib_by_track <- cur_prf_by_track
    } else {
      prf_tib_by_track <- bind_rows(prf_tib_by_track, cur_prf_by_track)
    }
  }
  
  cur_prf_text <- prf_tib_by_track %>% unlist() %>% str_flatten(collapse = ", ")
  cur_prf_tib <- tibble(uitvoerenden = cur_prf_text)
  
  if (is.null(muw_prf)) {
    muw_prf <- cur_prf_tib
  } else{
    muw_prf <- bind_rows(muw_prf, cur_prf_tib)
  }
  
  prf_n_idx <- prf_n_idx + prf_n
}

# album-info compileren
album_info <- muw_tracks_id
album_info <- bind_cols(album_info, muw_album)
album_info <- bind_cols(album_info, muw_tracks_len)
album_info <- bind_cols(album_info, muw_tracks_tit_b)
album_info <- bind_cols(album_info, muw_tracks_tit_add)
album_info <- bind_cols(album_info, muw_prf)
