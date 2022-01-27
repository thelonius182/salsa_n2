suppressWarnings(suppressPackageStartupMessages(library(httr)))
suppressWarnings(suppressPackageStartupMessages(library(xml2)))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(keyring)))
suppressWarnings(suppressPackageStartupMessages(library(googlesheets4)))
suppressWarnings(suppressPackageStartupMessages(library(yaml)))
suppressWarnings(suppressPackageStartupMessages(library(fs)))

config <- read_yaml("config_nip_nxt.yaml")

source("src/nip_nxt_tools.R", encoding = "UTF-8")

df_open_playlists.1 <- gd_open_playlists()
df_albums_and_tracks <- gd_albums_and_tracks(df_open_playlists.1)

# only playlists for which albums and tracks are selected in Muziekweb
df_open_playlists <- df_open_playlists.1 %>% 
  filter(playlist %in% df_albums_and_tracks$playlist)

# create Muziekweb order forms for open playlists
for (cur_playlist_id in df_open_playlists$playlist_id) {
  
  # TEST
  # cur_playlist_id = "NN0005"
  # TEST
  
  cur_playlist <- df_open_playlists %>% filter(playlist_id == cur_playlist_id)
  
  muw_aanvraag_file <- paste0("g:/salsa/muziekweb_aanvragen/", cur_playlist$playlist, ".txt")
  
  if (file_exists(muw_aanvraag_file)) {
    file_delete(muw_aanvraag_file)
  }
  
  cur_muziekweb <- df_albums_and_tracks %>% 
    filter(playlist == cur_playlist$playlist) %>% 
    mutate(muw_track_chr = as.character(muw_track)) %>% 
    select(muw_album_id, muw_track_chr) %>% 
    pivot_longer(names_to = "df_name", values_to = "order_line", cols = c("muw_album_id", "muw_track_chr")) %>% 
    select(order_line)
  
    write_lines(x = cur_muziekweb$order_line, file = muw_aanvraag_file, append = F)
}
