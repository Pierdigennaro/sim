fase_finale <- function(gioco) {
  gioco <- aggiungi_log(gioco, "=== Fase Finale ===", tipo = "fase")
  
  # 1. End Step
  gioco <- sottofase_end_step(gioco)
  
  # 2. Clean-up Step
  gioco <- sottofase_cleanup(gioco)
  
  return(gioco)
}

sottofase_end_step <- function(gioco) {
  gioco <- aggiungi_log(gioco, "Sottofase di Fine Turno (End Step)", tipo = "sottofase")
  
  # 1. Metti in pila i trigger "all’inizio dell’end step"
  gioco <- gestisci_inneschi_end_step(gioco)
  
  # 2. Giocatore attivo ottiene priorità
  gioco <- assegna_priorita_giocatore_attivo(gioco)
  
  # Quando la pila si svuota e tutti passano → vai alla cleanup
  return(gioco)
}


### 4.5.2 Pulizia (Cleanup) ----
sottofase_cleanup <- function(gioco) {
  gioco <- aggiungi_log(gioco, "Sottofase di Pulizia (Clean-Up)", tipo = "sottofase")
  
  repeat {
    trigger_durante_cleanup <- FALSE
    
    # 1. Scartare carte oltre 7
    gioco <- applica_regola_mano_massima(gioco)
    
    # 2. Rimuovere danno dalle creature
    gioco <- rimuovi_danni_tutti(gioco)
    
    # 3. Terminare tutti gli effetti "fino alla fine del turno"
    gioco <- termina_effetti_fine_turno(gioco)
    
    # 4. Controllare se si sono innescate abilità durante il cleanup
    nuovi_trigger <- trova_trigger_cleanup(gioco)
    
    if (length(nuovi_trigger) > 0) {
      trigger_durante_cleanup <- TRUE
      
      # metti in pila e risolvi
      gioco <- metti_in_pila_trigger(gioco, nuovi_trigger)
      gioco <- risolvi_pila(gioco)
    }
    
    # Se nessun trigger si è innescato → cleanup finito
    if (!trigger_durante_cleanup) break
  }
  
  return(gioco)
}


