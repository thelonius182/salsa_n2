suppressWarnings(suppressPackageStartupMessages(library(httr)))
suppressWarnings(suppressPackageStartupMessages(library(xml2)))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(keyring)))
suppressWarnings(suppressPackageStartupMessages(library(googlesheets4)))
suppressWarnings(suppressPackageStartupMessages(library(yaml)))
suppressWarnings(suppressPackageStartupMessages(library(fs)))

config <- read_yaml("config_nip_nxt.yaml")

source("src/nip_nxt_tools.R", encoding = "UTF-8")

# aanmelden bij Google
gs4_auth(email = "cz.teamservice@gmail.com")

# open playlists ophalen 
df_open_playlists.1 <- gd_open_playlists()

# per werk alle tracks ophalen
df_albums_and_tracks_file <- "C:/Users/gergiev/cz_rds_store/df_albums_and_tracks_all.RDS"
df_albums_and_tracks_all <- read_rds(df_albums_and_tracks_file)

df_albums_and_tracks.1 <- gd_albums_and_tracks(df_open_playlists.1) 

df_albums_and_tracks.2 <- df_albums_and_tracks.1 %>% 
  select(playlist, opnameNr) %>% inner_join(df_albums_and_tracks_all, 
                                  by = c("opnameNr" = "album_key")) %>% 
  select(playlist, muw_album_id, muw_track)

# create Muziekweb order forms for open playlists
for (cur_playlist_id in df_open_playlists.1$playlist_id) {
  
  # TEST
  # cur_playlist_id = "NN0003"
  # TEST
  
  cur_playlist <- df_open_playlists.1 %>% filter(playlist_id == cur_playlist_id)
  
  muw_aanvraag_file <- paste0("g:/salsa/muziekweb_aanvragen/", cur_playlist$playlist, ".txt")
  
  if (file_exists(muw_aanvraag_file)) {
    file_delete(muw_aanvraag_file)
  }
  
  cur_muziekweb <- df_albums_and_tracks.2 %>% 
    filter(playlist == cur_playlist$playlist) %>% 
    mutate(muw_track_chr = as.character(muw_track)) %>% 
    select(muw_album_id, muw_track_chr) %>% 
    pivot_longer(names_to = "df_name", values_to = "order_line", cols = c("muw_album_id", "muw_track_chr")) %>% 
    select(order_line)
  
  if (nrow(cur_muziekweb) > 0) {
    write_lines(x = cur_muziekweb$order_line, file = muw_aanvraag_file, append = F)
  }
}
