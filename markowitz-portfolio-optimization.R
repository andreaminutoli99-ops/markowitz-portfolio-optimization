# ==============================================================================
# SCRIPT OTTIMIZZAZIONE PORTAFOGLIO MARKOWITZ - CASO STUDIO "VALORE INSIEME"
# ==============================================================================

# --- 1. CARICAMENTO LIBRERIE ---
library(dplyr)
library(ggplot2)

# --- 2. IMPORTAZIONE E PULIZIA DATI ---
# Caricamento del dataset 
dati <- read.csv("storico_eurizon_best.csv", 
                 sep = ";", dec = ".", header = TRUE)

# Formattazione date e ordinamento cronologico
dati$Data <- as.Date(dati$Data, format = "%d/%m/%Y")
dati <- dati[order(dati$Data), ]

# Isolamento dei prezzi e conversione in formato puramente numerico
prezzi <- dati %>% select(-Data)
prezzi <- data.frame(lapply(prezzi, 
                            function(x) as.numeric(as.character(x))))
prezzi_puliti <- na.omit(prezzi)

# Calcolo rendimenti logaritmici giornalieri
rendimenti <- as.data.frame(lapply(prezzi_puliti, 
                                   function(x) diff(log(x))))

# --- 3. STATISTICHE DESCRITTIVE (ANNUALIZZATE) ---
giorni_trading <- 252
rendimenti_attesi_annui <- colMeans(rendimenti) * giorni_trading
matrice_cov_annua <- cov(rendimenti) * giorni_trading

# --- 4. SIMULAZIONE MONTE CARLO (5.000 PORTAFOGLI) ---
num_port <- 5000
num_fondi <- ncol(rendimenti)
rf <- 0.02 # Tasso risk-free al 2%

port_ret <- numeric(num_port)
port_risk <- numeric(num_port)
port_sharpe <- numeric(num_port)
matrice_pesi <- matrix(nrow = num_port, ncol = num_fondi) 
colnames(matrice_pesi) <- colnames(rendimenti)

set.seed(123) # Replicabilità dei risultati
for (i in 1:num_port) {
  pesi <- runif(num_fondi)
  pesi <- pesi / sum(pesi) # Normalizzazione a 1
  
  matrice_pesi[i, ] <- pesi
  port_ret[i] <- sum(pesi * rendimenti_attesi_annui)
  port_risk[i] <- sqrt(t(pesi) %*% matrice_cov_annua %*% pesi)
  port_sharpe[i] <- (port_ret[i] - rf) / port_risk[i]
}

portafogli <- data.frame(Rendimento = port_ret, 
                         Rischio = port_risk, 
                         Sharpe = port_sharpe)

# --- 5. ESTRAZIONE DEL PORTAFOGLIO OTTIMALE E DEI 15 FONDI "BEST" ---
indice_max_sharpe <- which.max(portafogli$Sharpe)
pesi_ottimali <- matrice_pesi[indice_max_sharpe, ]

# Creazione tabella allocazione completa
allocazione <- data.frame(Fondo = names(pesi_ottimali), Peso_Simulazione = pesi_ottimali)
allocazione <- allocazione[order(allocazione$Peso_Simulazione, decreasing = TRUE), ]

# Estrazione dei Top 15 Fondi (approccio Top-Down)
top_15_fondi <- head(allocazione, 15)

# --- 6. APPLICAZIONE DEI CONTROVALORI AL CASO STUDIO (80.000 €) ---
capitale_totale_investito <- 80000

# Suddivisione macro-asset (Modello Valore Insieme)
peso_polizza <- 0.20      # 20% in Multiramo (16.000 €)
peso_certificati <- 0.15  # 15% in Certificati (12.000 €)
peso_fondi <- 0.65        # 65% in Fondi R (52.000 €)

capitale_fondi <- capitale_totale_investito * peso_fondi

# Ricalcolo dei pesi dei 15 fondi in modo che sommino a 100% all'interno del loro cassetto
top_15_fondi$Peso_Percentuale_Ricalcolato <- round((top_15_fondi$Peso_Simulazione / 
                                                      sum(top_15_fondi$Peso_Simulazione)) * 100, 2)

# Calcolo del controvalore esatto in Euro per ogni fondo
top_15_fondi$Controvalore_Euro <- round((top_15_fondi$Peso_Percentuale_Ricalcolato / 100) * 
                                          capitale_fondi, 2)

# --- 7. STAMPA DEI RISULTATI FINALI ---
cat("\n======================================================\n")
cat("   ARCHITETTURA PORTAFOGLIO (PATRIMONIO: 100.000 €)\n")
cat("======================================================\n")
cat("- Fondo Emergenza              : 20.000 € (Allocato su Eurizon Tesoreria)\n")
cat("------------------------------------------------------\n")
cat("   QUOTA INVESTITA CORE/SATELLITE (80.000 €)\n")
cat("- Polizza Multiramo (20%)      :", 
    capitale_totale_investito * peso_polizza, "€\n")
cat("- Certificati (15%)            :", 
    capitale_totale_investito * peso_certificati, "€\n")
cat("- Dossier Fondi Markowitz (65%):", capitale_fondi, "€\n")
cat("======================================================\n")
cat("\nSPACCATO DEI 15 FONDI CORE (Asset Allocation Markowitz):\n")
print(top_15_fondi[, c("Fondo", 
                       "Peso_Percentuale_Ricalcolato", 
                       "Controvalore_Euro")], 
      row.names = FALSE)

# --- 8. GENERAZIONE GRAFICO DELLA FRONTIERA ---
ggplot(portafogli, aes(x = Rischio, y = Rendimento, color = Sharpe)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_color_gradient(low = "darkblue", high = "red") +
  geom_point(data = portafogli[indice_max_sharpe, ], 
             aes(x = Rischio, y = Rendimento), 
             color = "gold", size = 4, shape = 18) + # Evidenzia portafoglio ottimale
  theme_minimal() +
  labs(title = "Frontiera Efficiente di Markowitz - Ottimizzazione Dossier Fondi",
       subtitle = "Il punto dorato indica il Portafoglio a Massimo Indice di Sharpe",
       x = "Rischio (Volatilità Annualizzata)",
       y = "Rendimento Atteso Annualizzato",
       color = "Sharpe\nRatio") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
write.csv2(top_15_fondi, "tabella_fondi.csv", row.names = FALSE)
