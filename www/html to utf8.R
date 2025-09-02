library(readr)


txt_utf8 <- read_file(
  "www/dataDictionary.html",
  locale = locale(encoding = "WINDOWS-1252")  # or "GBK"
)

write_file(txt_utf8, "www/dataDictionary_utf8.html")


