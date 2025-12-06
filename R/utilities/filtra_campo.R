filtra_campo_per_giocatore <- function(gioco,
                                       id_giocatore,
                                       per = c("controller", "owner"),
                                       zona = "campo",
                                       tipi = NULL,
                                       includi_phased_out = FALSE) {
  per <- match.arg(per)
  
  campo <- gioco$campo
  
  # prudenza: rimuovo eventuali NULL
  campo <- Filter(Negate(is.null), campo)
  
  # 1) filtro per giocatore (controller o owner)
  carte_giocatore <- Filter(function(carta) {
    !is.null(carta[[per]]) && identical(carta[[per]], id_giocatore)
  }, campo)
  
  # 2) filtro per zona (di default "campo", coerente col concetto di permanenti)
  if (!is.null(zona)) {
    carte_giocatore <- Filter(function(carta) {
      !is.null(carta$zona) && identical(carta$zona, zona)
    }, carte_giocatore)
  }
  
  # 3) filtro per tipo (attenzione: tipo può essere "Artefatto Creatura", ecc.)
  if (!is.null(tipi)) {
    tipi <- as.character(tipi)
    
    ha_tipo <- function(carta) {
      if (is.null(carta$tipo)) return(FALSE)
      # spezza eventuali tipi multipli tipo "Artefatto Creatura"
      tipi_carta <- strsplit(carta$tipo, "\\s+")[[1]]
      any(tipi_carta %in% tipi)
    }
    
    carte_giocatore <- Filter(ha_tipo, carte_giocatore)
  }
  
  # 4) gestisco il phasing (di default non includo i phased out)
  if (!includi_phased_out) {
    carte_giocatore <- Filter(function(carta) {
      !isTRUE(carta$phase_out)
    }, carte_giocatore)
  }
  
  return(carte_giocatore)
}

# # Tutti i permanenti sul campo controllati dal giocatore 1
# permanenti_p1 <- filtra_campo_per_giocatore(
#   gioco,
#   id_giocatore = 1
# )
# 
# # Tutte le creature sul campo controllate dal giocatore 2
# creature_p2 <- filtra_campo_per_giocatore(
#   gioco,
#   id_giocatore = 2,
#   tipi = "Creatura"
# )
# 
# # Tutte le terre possedute dal giocatore 1 sul campo (owner, non controller)
# terre_owned_p1 <- filtra_campo_per_giocatore(
#   gioco,
#   id_giocatore = 1,
#   per = "owner",
#   tipi = "Terra"
# )
# 
# get_permanenti_controllati <- function(gioco, id_giocatore, includi_phased_out = FALSE) {
#   filtra_campo_per_giocatore(
#     gioco          = gioco,
#     id_giocatore   = id_giocatore,
#     per            = "controller",
#     zona           = "campo",
#     tipi           = NULL,
#     includi_phased_out = includi_phased_out
#   )
# }
# 
# get_creature_controllate <- function(gioco, id_giocatore, includi_phased_out = FALSE) {
#   filtra_campo_per_giocatore(
#     gioco          = gioco,
#     id_giocatore   = id_giocatore,
#     per            = "controller",
#     zona           = "campo",
#     tipi           = "Creatura",
#     includi_phased_out = includi_phased_out
#   )
# }
# 
# get_terre_controllate <- function(gioco, id_giocatore, includi_phased_out = FALSE) {
#   filtra_campo_per_giocatore(
#     gioco          = gioco,
#     id_giocatore   = id_giocatore,
#     per            = "controller",
#     zona           = "campo",
#     tipi           = "Terra",
#     includi_phased_out = includi_phased_out
#   )
# }
