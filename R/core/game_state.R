controlla_azioni_stato <- function(gioco) {
  repeat {
    azioni_applicate <- FALSE
    
    ## 1. Giocatori perdono (vita, veleno, pesca da grimorio vuoto)
    for (g in seq_along(gioco$giocatori)) {
      gioc <- gioco$giocatori[[g]]
      
      # se ha già perso, salto
      if (isTRUE(gioc$ha_perso)) {
        next
      }
      
      # 1a. Vita <= 0
      if (gioc$punti_vita <= 0) {
        gioco <- aggiungi_log(
          gioco,
          paste0(gioc$nome, " ha 0 o meno punti vita e perde la partita.")
        )
        gioc$ha_perso <- TRUE
        azioni_applicate <- TRUE
      }
      
      # 1b. 10 o più segnalini veleno
      if (!isTRUE(gioc$ha_perso) && !is.null(gioc$veleno) && gioc$veleno >= 10L) {
        gioco <- aggiungi_log(
          gioco,
          paste0(gioc$nome, " ha 10 o più segnalini veleno e perde la partita.")
        )
        gioc$ha_perso <- TRUE
        azioni_applicate <- TRUE
      }
      
      # 1c. Ha tentato di pescare da grimorio vuoto (flag settato da pesca_carta)
      if (!isTRUE(gioc$ha_perso) && isTRUE(gioc$ha_tentato_pescata_grimorio_vuoto)) {
        gioco <- aggiungi_log(
          gioco,
          paste0(gioc$nome, " ha tentato di pescare da un grimorio vuoto e perde la partita.")
        )
        gioc$ha_perso <- TRUE
        azioni_applicate <- TRUE
      }
      
      # il flag va consumato ad ogni controllo SBA
      gioc$ha_tentato_pescata_grimorio_vuoto <- FALSE
      
      gioco$giocatori[[g]] <- gioc
    }
    
    ## 2. Pedine o copie fuori dal campo cessano di esistere
    # (tokens in qualsiasi zona diversa dal campo)
    
    # 2a. Zone personali (mano, cimitero, mazzo)
    for (g in seq_along(gioco$giocatori)) {
      for (zona in c("mano", "cimitero", "mazzo")) {
        lista <- get_zona(gioco, zona, g)
        if (!is.null(lista) && length(lista) > 0L) {
          idx_rm <- which(vapply(
            lista,
            function(carta) isTRUE(carta$token) || isTRUE(carta$copia),
            logical(1)
          ))
          
          if (length(idx_rm) > 0L) {
            lista[idx_rm] <- NULL
            gioco <- set_zona(gioco, zona, lista, g)
            azioni_applicate <- TRUE
            
            gioco <- aggiungi_log(
              gioco,
              paste0(
                "Pedine o copie in zona ", zona,
                " del giocatore ", gioco$giocatori[[g]]$nome,
                " cessano di esistere (azione basata sullo stato)."
              )
            )
          }
        }
      }
    }
    
    # 2b. Zone globali (esilio e pila)
    for (zona in c("esilio", "pila")) {
      lista <- get_zona(gioco, zona)
      if (!is.null(lista) && length(lista) > 0L) {
        idx_rm <- which(vapply(
          lista,
          function(carta) isTRUE(carta$token) || isTRUE(carta$copia),
          logical(1)
        ))
        
        if (length(idx_rm) > 0L) {
          lista[idx_rm] <- NULL
          gioco <- set_zona(gioco, zona, lista)
          azioni_applicate <- TRUE
          
          gioco <- aggiungi_log(
            gioco,
            paste0(
              "Pedine o copie in zona ", zona,
              " cessano di esistere (azione basata sullo stato)."
            )
          )
        }
      }
    }
    
    ## 3. Permanenti sul campo: creature e planeswalker morti
    da_spostare <- list()  # list(indice, owner_ref, motivo, descrizione)
    
    campo <- get_zona(gioco, "campo")
    
    if (!is.null(campo) && length(campo) > 0L) {
      for (i in seq_along(campo)) {
        carta <- campo[[i]]
        if (is.null(carta)) next
        
        tipo_line      <- tolower(carta$tipo %||% "")
        is_creatura    <- grepl("creatura", tipo_line)
        is_planeswalker<- grepl("planeswalker", tipo_line)
        
        # Creature: costituzione <= 0
        if (is_creatura && !is.null(carta$costituzione) && carta$costituzione <= 0) {
          owner_ref <- carta$owner %||% carta$proprietario
          da_spostare[[length(da_spostare) + 1L]] <- list(
            indice    = i,
            owner_ref = owner_ref,
            motivo    = "costituzione_0",
            descrizione = paste0(
              carta$nome,
              " ha costituzione <= 0 e viene messa nel cimitero di ",
              owner_ref, "."
            )
          )
          next
        }
        
        # Creature: danno letale
        if (is_creatura &&
            !is.null(carta$costituzione) &&
            !is.null(carta$danno) &&
            carta$danno >= carta$costituzione) {
          
          owner_ref <- carta$owner %||% carta$proprietario
          da_spostare[[length(da_spostare) + 1L]] <- list(
            indice    = i,
            owner_ref = owner_ref,
            motivo    = "danno_letale",
            descrizione = paste0(
              carta$nome,
              " ha subito danno letale ed è distrutta (messa nel cimitero di ",
              owner_ref, ")."
            )
          )
          next
        }
        
        # Planeswalker: fedeltà 0
        if (is_planeswalker && !is.null(carta$fedelta) && carta$fedelta <= 0) {
          owner_ref <- carta$owner %||% carta$proprietario
          da_spostare[[length(da_spostare) + 1L]] <- list(
            indice    = i,
            owner_ref = owner_ref,
            motivo    = "fedelta_0",
            descrizione = paste0(
              carta$nome,
              " ha fedeltà 0 e viene messa nel cimitero di ",
              owner_ref, "."
            )
          )
        }
      }
    }
    
    # Applico gli spostamenti dal campo al cimitero (indici in ordine decrescente)
    if (length(da_spostare) > 0L) {
      indici_unici <- unique(vapply(da_spostare, `[[`, integer(1), "indice"))
      indici_unici <- sort(indici_unici, decreasing = TRUE)
      
      campo <- get_zona(gioco, "campo")  # rileggo per sicurezza
      
      for (idx in indici_unici) {
        az <- da_spostare[[which(vapply(
          da_spostare,
          function(x) x$indice == idx,
          logical(1)
        ))[1L]]]
        
        gioco <- aggiungi_log(gioco, az$descrizione)
        
        gioco <- sposta_carta(
          gioco,
          da_zona      = "campo",
          a_zona       = "cimitero",
          indice_carta = idx,
          da_giocatore = NULL,           # campo è globale
          a_giocatore  = az$owner_ref    # id/nome del proprietario
        )
      }
      
      azioni_applicate <- TRUE
    }
    
    ## 4. Regola delle leggende
    campo <- get_zona(gioco, "campo")
    
    if (!is.null(campo) && length(campo) > 0L) {
      idx_leggendari <- which(vapply(
        campo,
        function(c) isTRUE(c$leggendario) || isTRUE(c$leggendaria),
        logical(1)
      ))
      
      if (length(idx_leggendari) > 0L) {
        chiave <- vapply(
          idx_leggendari,
          function(i) {
            c <- campo[[i]]
            ctrl <- c$controller %||% c$owner %||% c$proprietario
            paste(ctrl, c$nome, sep = "||")
          },
          character(1)
        )
        
        gruppi <- split(idx_leggendari, chiave)
        
        for (k in names(gruppi)) {
          indici <- gruppi[[k]]
          if (length(indici) > 1L) {
            # Mantengo la prima (in futuro: scelta del giocatore / AI)
            indici_da_sacrificare <- sort(indici, decreasing = TRUE)[-1L]
            
            for (idx in indici_da_sacrificare) {
              carta     <- campo[[idx]]
              owner_ref <- carta$owner %||% carta$proprietario
              ctrl_ref  <- carta$controller %||% owner_ref
              
              gioco <- aggiungi_log(
                gioco,
                paste0(
                  "Regola delle leggende: ",
                  carta$nome,
                  " in eccesso controllata da ",
                  ctrl_ref,
                  " viene messa nel cimitero di ",
                  owner_ref, "."
                )
              )
              
              gioco <- sposta_carta(
                gioco,
                da_zona      = "campo",
                a_zona       = "cimitero",
                indice_carta = idx,
                da_giocatore = NULL,
                a_giocatore  = owner_ref
              )
            }
            
            azioni_applicate <- TRUE
          }
        }
      }
    }
    
    ## Fine ciclo: se nessuna SBA è stata applicata, si termina
    if (!azioni_applicate) break
  }
  
  return(gioco)
}
