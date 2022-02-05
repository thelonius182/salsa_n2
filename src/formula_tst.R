dat <- data.frame(x = c(1, 5, 3, 2, 4, 6))

ss <- gs4_create("gs4-formula-demo", sheets = dat)
ss

summaries <- tibble::tribble(
  ~desc, ~summaries,
  "max", "=max(A:A)",
  "sum", "=sum(A:A)",
  "min", "=min(A:A)",
  "sparkline", "=SPARKLINE(A:A; {\"color\"\\\"blue\"})"
)

# explicitly declare a column as `googlesheets4_formula`
summaries$summaries <- gs4_formula(summaries$summaries)
summaries

range_write(ss, data = summaries, range = "C1", reformat = FALSE)

miscellany <- tibble::tribble(
  ~example,
  "=HYPERLINK(\"http://www.google.com/\";\"Google\")",
  "=IMAGE(\"https://www.google.com/images/srpr/logo3w.png\")"
)
miscellany$example <- gs4_formula(miscellany$example)
miscellany

sheet_write(miscellany, ss = ss)

# clean up
gs4_find("gs4-formula-demo") %>%
  googledrive::drive_trash()
