suppressWarnings(suppressPackageStartupMessages(library(googlesheets4)))
suppressWarnings(suppressPackageStartupMessages(library(yaml)))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(fs)))

config <- read_yaml("config_nip_nxt.yaml")

playlists_raw <- read_sheet(ss = config$url_nip_nxt, sheet = "playlists") %>% as_tibble()

# pick the open playlists
playlists.1 <- playlists_raw %>% filter(!is.na(playlist_id) & samengesteld_op == "NULL")

# collect muziekweb album-id's and tracks in nipperNxt-spreadsheet
muziekweb_raw <- read_sheet(ss = config$url_nip_nxt, sheet = "muziekweb")

# limit to selected album-id's and tracks of open playlists
muziekweb.1 <- muziekweb_raw %>% filter(!is.na(keuze) & playlist %in% playlists.1$playlist)

# separate the track-id's
muziekweb.2 <- muziekweb.1 %>% 
  mutate(track_list = strsplit(tracks, "(?i),", perl=TRUE),
         rrn = row_number())

# create order forms by playlist
for (cur_playlist_id in playlists.1$playlist_id) {
  
  cur_playlist <- playlists.1 %>% filter(playlist_id == cur_playlist_id)
  
  muw_aanvraag_file <- paste0("g:/salsa/muziekweb_aanvragen/", cur_playlist$playlist, ".txt")
  
  if (file_exists(muw_aanvraag_file)) {
    file_delete(muw_aanvraag_file)
  }
  
  cur_muziekweb <- muziekweb.2 %>% filter(playlist == cur_playlist$playlist)
  
  for (cur_rrn in cur_muziekweb$rrn) {
    # cur_rrn <- 2
    cur_row <- cur_muziekweb %>% filter(rrn == cur_rrn)
    
    for (track_item in cur_row$track_list[[1]]) {
      # track_item <- "5-11"
      
      if (str_detect(track_item, "-")) {
        track_seq <- strsplit(track_item, "(?i)-", perl = TRUE)
        track_start <- track_seq[[1]][1]
        track_stop <- track_seq[[1]][2]
        
        for (cur_track_id in track_start:track_stop) {
          write_lines(x = cur_row$`muziekweb-id`, file = muw_aanvraag_file, append = T)
          write_lines(x = cur_track_id, file = muw_aanvraag_file, append = T)
        }
        
      } else {
        write_lines(x = cur_row$`muziekweb-id`, file = muw_aanvraag_file, append = T)
        write_lines(x = track_item, file = muw_aanvraag_file, append = T)
      }
    }
  }
}
