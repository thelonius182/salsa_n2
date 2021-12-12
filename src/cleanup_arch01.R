library(readr)
library(magrittr)
library(stringr)
library(tibble)
library(dplyr)
library(tidyr)

# list_arch03_raw <- read.delim("~/Downloads/list_arch03.txt", header=FALSE, sep = "/", quote = "", stringsAsFactors = F)
# list_arch03_raw <- read_file("~/Downloads/list_arch03.txt")

list_arch03_raw <-
  read_delim(
    "~/Downloads/list_arch03.txt",
    # col_types = cols(X1 = col_character()),
    trim_ws = FALSE,
    quote = "",
    delim = "¶",
    col_names = F
  ) %>% as_tibble() %>% 
  mutate(cz_id = row_number()) %>% 
  select(cz_id, cz_qfn = X1)

list_arch03.A <- list_arch03_raw %>%  
  separate(col = cz_qfn, into = paste0("cz_qfn_", 1:10), fill = "right", sep = "/")

list_arch03.B <- list_arch03.A %>%  
  filter(is.na(cz_qfn_2) | str_detect(cz_qfn_2, "(!!Texten ongesorteerd|SynologyDrive|[Mm]oved|MOVED|JAZZ CHRISTMAS)", negate = T))

list_arch03.C <- list_arch03.B %>% 
  filter(is.na(cz_qfn_2) | str_detect(cz_qfn_2, "^\\.", negate = T)) 

list_arch03.D <- list_arch03.C %>% 
  filter(is.na(cz_qfn_3) | str_detect(cz_qfn_3, "^\\.", negate = T)) 

list_arch03.E <- list_arch03.D %>% 
  filter(is.na(cz_qfn_3) | str_detect(cz_qfn_3, "([Aa]ank?\\.aif|[Aa]fk?\\.aif|\\s[Uu]it\\.aif)$", negate = T)) 

list_arch03.E2 <- list_arch03.E %>% 
  filter(is.na(cz_qfn_3) | str_detect(cz_qfn_3, "^(TXT|Txt|txt) ", negate = T)) 

list_arch03.F <- list_arch03.E2 %>% 
  filter(is.na(cz_qfn_4) | str_detect(cz_qfn_4, "^\\.", negate = T))

list_arch03.G <- list_arch03.F %>% 
  filter(is.na(cz_qfn_4) | str_detect(cz_qfn_4, "(Audio Files|Shortcut|Aanvulling)", negate = T))

list_arch03.H <- list_arch03.G %>% 
  select(cz_id, cz_qfn_2:cz_qfn_4)

# extract CZ-archief
cz_A <- list_arch03.H %>% 
  filter(cz_qfn_2 != "Jan Kruit-archief") %>% 
  select(cz_id, cz_qfn_2, cz_qfn_3)

# extract JK-archief
jk_A <- list_arch03.H %>% 
  filter(cz_qfn_2 == "Jan Kruit-archief")

jk_B <- jk_A %>% 
  select(cz_id, cz_qfn_2a = cz_qfn_3, cz_qfn_3a = cz_qfn_4) %>% 
  rename(cz_qfn_2 = cz_qfn_2a, cz_qfn_3 = cz_qfn_3a) 

cz_all <- cz_A %>% 
  bind_rows(jk_B) %>% 
  arrange(cz_id)
