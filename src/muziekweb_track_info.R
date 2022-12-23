suppressWarnings(suppressPackageStartupMessages(library(httr)))
suppressWarnings(suppressPackageStartupMessages(library(xml2)))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(keyring)))
suppressWarnings(suppressPackageStartupMessages(library(googlesheets4)))
suppressWarnings(suppressPackageStartupMessages(library(yaml)))
suppressWarnings(suppressPackageStartupMessages(library(fs)))
suppressWarnings(suppressPackageStartupMessages(library(magrittr)))
suppressWarnings(suppressPackageStartupMessages(library(hms)))

config <- read_yaml("config_nip_nxt.yaml")

source("src/nip_nxt_tools.R", encoding = "UTF-8")

# aanmelden bij Google
gs4_auth(email = "cz.teamservice@gmail.com")

df_open_playlists.1 <- gd_open_playlists()
df_albums_and_tracks.1 <- gd_albums_and_tracks_muw(df_open_playlists.1)
df_albums <- df_albums_and_tracks.1 %>% select(muw_album_id) %>% distinct()

# create Google Spreadsheet blocks for open playlists
muw_usr <- key_get(service = "muziekweb-ws-usr")
muw_pwd <- key_get(service = "muziekweb-ws-pwd")

all_album_info <- NULL

for (cur_album_id in df_albums$muw_album_id) {
  
  print(cur_album_id)
  
  # TEST
  # cur_album_id = "DBX11310"
  # cur_album_id = "CX1538"
  # TEST
 
  # cur_playlist <-
  #   df_open_playlists %>% filter(playlist_id == cur_playlist_id)
  
  muw_result <-
    GET(
      url = paste0(
        "https://api.cdr.nl/ExtendedInfo/v3/albumInformation.xml?albumID=",
        cur_album_id
      ),
      authenticate(muw_usr, muw_pwd)
    )
  
  muw_txt <-
    content(
      x = muw_result,
      as = "text",
      type = "text/xml",
      encoding = "UTF-8"
    )
  
  muw_xml <- as_xml_document(muw_txt)
  
  muw_album <- muw_track_elm("a", muw_col_name = "album")
  # muw_gen_a <- muw_track_elm("g", "Name[@Language='nl']")
  # muw_gen_b <- muw_track_elm("g", "Category/Name[@Language='nl']")
  # muw_gen_c <- muw_track_elm("g", "Category/Style/Name[@Language='nl']")
  
  stopifnot("Bovenstaande CD-code niet gevonden in Muziekweb-catalogus" = nrow(muw_album) > 0)
  
  muw_tracks_id <-
    muw_track_elm("t", "AlbumTrackID", muw_col_name = "track_id")
  muw_tracks_len <-
    muw_track_elm("t", "PlayTimeInSec", muw_col_name = "secs")
  muw_tracks_tit <-
    muw_track_elm("t", "TrackTitle[@Language='nl']", muw_col_name = "titel_en_deel")
  muw_tracks_tit_b <-
    muw_track_elm("t", "UniformTitle[@Language='nl']", muw_col_name = "titel")
  muw_tracks_tit_add <-
    muw_track_elm("t", "AddonForUniformTitle[@Language='nl']", muw_col_name = "deel_in_titel")
  
  muw_prf_n <- xml_attr(xml_find_all(muw_xml,
                                     "//Result/Album/Tracks/Track/Performers"),
                        "Count") %>% as_tibble()
  names(muw_prf_n) <- "n_uitvoerenden"
  
  muw_catalogue_type <- 
    muw_track_elm("p", "PrimaryCatalogueCode", muw_col_name = "cat_type")[[1,1]]
  muw_prf_name <-
    muw_track_elm("p", "PresentationName", muw_col_name = "uitvoerende")
  muw_prf_role <-
    muw_track_elm("p", "Role[@Language='nl']", muw_col_name = "rol")
  
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

      if (muw_catalogue_type == "POPULAR") {
        cur_prf_by_track <-
          tibble(prf_txt = muw_prf_name$uitvoerende[[n1]])
      } else {
        cur_prf_by_track <-
          tibble(prf_txt = paste0(muw_prf_name$uitvoerende[[n1]],
                                  " (",
                                  muw_prf_role$rol[[n1]],
                                  ")"))
      }
      
      if (is.null(prf_tib_by_track)) {
        prf_tib_by_track <- cur_prf_by_track
      } else {
        prf_tib_by_track %<>% add_row(cur_prf_by_track)
      }
    }
    
    cur_prf_text <-
      prf_tib_by_track %>% unlist() %>% str_flatten(collapse = ", ")
    cur_prf_tib <- tibble(uitvoerenden = cur_prf_text) %>% 
      mutate(uitvoerenden = str_replace_all(uitvoerenden, " \\(\\)", ""))
    
    if (is.null(muw_prf)) {
      muw_prf <- cur_prf_tib
    } else{
      muw_prf %<>% add_row(cur_prf_tib)
    }
    
    prf_n_idx <- prf_n_idx + prf_n
  }
  
  # album-info compileren
  album_info <- muw_tracks_id
  album_info %<>% add_column(muw_album)
  album_info %<>% add_column(muw_tracks_len)
  album_info %<>% add_column(muw_tracks_tit)
  album_info %<>% add_column(muw_tracks_tit_b)
  album_info %<>% add_column(muw_tracks_tit_add)
  album_info %<>% add_column(muw_prf)
  
  # zorg dat er altijd een titel is, zonodig door 'titel_en_deel' te kiezen
  album_info.1 <- album_info %>%
    mutate(tmp_titel = if_else(str_length(str_trim(titel, side = "both")) == 0, titel_en_deel, titel)) %>%
    rename(titel_w_nulls = titel, titel = tmp_titel) %>%
    select(-titel_w_nulls, -titel_en_deel)
  
  # verzamelen
  if (is.null(all_album_info)) {
    all_album_info <- album_info.1
  } else {
    all_album_info %<>% add_row(album_info.1)
  }
  
}


if (nrow(df_albums_and_tracks.1) > 0) {
  # uitdunnen: alleen de tracks die in de spreadsheet staan
  df_albums_and_tracks.2 <- df_albums_and_tracks.1 %>%
    inner_join(all_album_info, by = c("muw_track_id" = "track_id"))
  
  # groepeer op titel
  df_albums_and_tracks.3 <- df_albums_and_tracks.2 %>%
    group_by(titel) %>%
    mutate(werk = row_number(),
           werk_lengte = sum(as.integer(secs))) %>%
    ungroup() %>%
    mutate(album_key = if_else(werk == 1, muw_track_id, NA_character_)) %>%
    fill(album_key, .direction = "down")
  
  # bewaar koppeling album_key/tracks, voor de bestelling later (in muziekweb_audio.R)
  # NB - df_albums_and_tracks_all bewaart album_keys van ALLE selecties tot nu toe, omdat een playlist
  #      soms in delen ontstaat. Alleen het laatste deel bewaren is dan te weinig.
  df_albums_and_tracks_file <-
    "C:/Users/gergiev/cz_rds_store/df_albums_and_tracks_all.RDS"
  # bestaande set inlezen
  df_albums_and_tracks_all <- read_rds(df_albums_and_tracks_file)
  # aanvulling voorbereiden
  df_albums_and_tracks.3.1 <- df_albums_and_tracks.3 %>%
    select(album_key, muw_album_id, muw_track)
  # dubbele album_keys voorkomen (bv als zelfde df_3.1 per ongeluk nog een keer toegevoegd wordt)
  df_albums_and_tracks_all %<>% add_row(df_albums_and_tracks.3.1) %>% distinct()
  # nu pas opslaan
  saveRDS(object = df_albums_and_tracks_all, file = df_albums_and_tracks_file)
  
  # 1 track per werk (voor gids, RL, draaiboek)
  df_albums_and_tracks.3.2 <-
    df_albums_and_tracks.3 %>% filter(werk == 1)
  
  # prep het blok voor GD-sheet "nipper-select"
  df_albums_and_tracks.4 <- df_albums_and_tracks.3.2 %>%
    mutate(
      catalogue_type = muw_catalogue_type,
      componist = if_else(catalogue_type == "POPULAR", 
                          album, 
                          sub("^([^(]+) \\(componist\\), (.*)$",
                              "\\1",
                              uitvoerenden,
                              perl = TRUE,
                              ignore.case = TRUE)
      ),
      uitvoerenden = sub(
        "^([^(]+) \\(componist\\), (.*)$",
        "\\2",
        uitvoerenden,
        perl = TRUE,
        ignore.case = TRUE
      ),
      tot_time = NA_real_,
      detect = NA,
      keuze = T,
      lengte = as.character(hms(seconds = werk_lengte)),
      vt_blok_id = "Z99",
      opnameNr = muw_track_id
    ) %>%
    arrange(playlist, componist, titel) %>%
    # group_by(playlist, componist, titel) %>%
    # mutate(vt_blok = row_number()) %>%
    # ungroup() %>%
    # group_by(vt_blok) %>%
    # mutate(vt_blok_index = if_else(vt_blok == 1, row_number(), NA_integer_)) %>%
    # ungroup() %>%
    # select(componist, titel, tot_time, detect, keuze, lengte, playlist, vt_blok_index, vt_blok, uitvoerenden, album, opnameNr) %>%
    # arrange(componist, vt_blok_index, vt_blok) %>%
    # fill(vt_blok_index) %>%
    # mutate(vt_blok_id = paste0(LETTERS[vt_blok_index], str_pad(vt_blok, width = 2, side = "left", pad = "0"))) %>%
    select(
      componist,
      titel,
      tot_time,
      detect,
      keuze,
      lengte,
      playlist,
      vt_blok_id,
      uitvoerenden,
      album,
      opnameNr
    )
  
  # alleen als er iets te doen valt...
  if (nrow(df_albums_and_tracks.4) > 0) {
    # ... toevoegen aan de GD-sheet
    nn_ss <- config$url_nip_nxt
    
    # TEST
    # nn_ss <- "https://docs.google.com/spreadsheets/d/11i6tdUYZ8wTge97tTwEUXO_EE-6h9rOlMD4Co9m-qfk"
    # TEST
    
    rule_checkbox <- googlesheets4:::new(
      "DataValidationRule",
      condition = googlesheets4:::new_BooleanCondition(type = "BOOLEAN"),
      inputMessage = "Lorem ipsum dolor sit amet",
      strict = TRUE,
      showCustomUi = TRUE
    )
    
    # "selectievak"-validatie van kolom 'Keuze" verwijderen, zodat het vergroten vd sheet geen ongewilde cellen mee-kopieert
    # NB dit is een hack van Jenny Bryan: https://github.com/tidyverse/googlesheets4/issues/6
    sp <-
      sheet_properties(ss = nn_ss) %>% filter(name == "nipper-select")
    nn_bottom_col_E <-
      paste0("nipper-select!E", sp$grid_rows, ":E", sp$grid_rows)
    googlesheets4:::range_add_validation(nn_ss, range = nn_bottom_col_E, rule = NULL)
    
    # album-infoblok toevoegen
    # sheet_resize(ss = nn_ss, sheet = "nipper-select", nrow = sp$grid_rows + nrow(df_albums_and_tracks.4), exact = F)
    sheet_append(ss = nn_ss,
                 data = df_albums_and_tracks.4,
                 sheet = "nipper-select")
    
    # toon kolom 'Keuze' weer als selectievakjes
    googlesheets4:::range_add_validation(nn_ss, range = "nipper-select!E3:E", rule = rule_checkbox)
    
    # reset de selectievakjes in tabblad "muziekweb" (markeer als "data opgehaald")
    sp <- sheet_properties(ss = nn_ss) %>% filter(name == "muziekweb")
    nn_muw_keuze_col <- paste0("muziekweb!D5:D", sp$grid_rows)
    tb_reset_keuzes <-
      tibble(keuze = FALSE,
             cz_row_id = seq_len(length.out = sp$grid_rows - 4)) %>% select(keuze)
    range_write(
      ss = nn_ss,
      data = tb_reset_keuzes,
      range = nn_muw_keuze_col,
      col_names = F
    )
  }
}

