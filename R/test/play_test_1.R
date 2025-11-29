source("R/carica_tutto.R")
carica_tutto(path = "R")

gioco_test <- gioca_partita_test(
  max_turni = 10,
  seed      = 42,
  verbose   = TRUE
)

# Alla fine puoi ispezionare lo stato:
str(gioco_test)
