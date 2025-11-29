carica_tutto <- function(path = "R") {
  files <- list.files(
    path,
    pattern   = "\\.R$",
    full.names = TRUE,
    recursive  = TRUE
  )
  
  # opzionale: ordino i file per avere un ordine stabile
  files <- sort(files)
  files <- files[!grepl("/(play_)", files)]
  
  for (f in files) {
    message("Sourcing: ", f)
    source(f, encoding = "UTF-8")
  }
  
  invisible(NULL)
}

