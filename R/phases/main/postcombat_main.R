fase_principale2 <- function(gioco) {
  gioco <- aggiungi_log(gioco, "=== Seconda Fase Principale ===", tipo = "fase")
  
  # 1. Controllo stati immediato
  controllo_azioni_stato(gioco)
  
  # 2. Il giocatore attivo ottiene priorità
  gioco <- assegna_priorita_giocatore_attivo(gioco)
  
  # 3. (Futuro) Giocare terre, lanciare magie, attivare abilità
  # TODO: gestione di:
  # - gioca_terra
  # - lancia_magia
  # - attiva_abilita
  # - interazioni pila
  
  # 4. Quando il giocatore passa la priorità e nessuno risponde,
  #    si avanza alla fase finale
  gioco <- passa_alla_fase_successiva(gioco)
  
  return(gioco)
}
