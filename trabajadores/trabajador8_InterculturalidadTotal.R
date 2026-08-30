# ====================================================================================
# TRABAJADOR 8: INTERCULTURALIDAD TOTAL (EMBUDO CONSTITUCIONAL)
# Arquitectura: Inclusión "No Aplica" -> Cascada Perfecta 1:1 -> Inyección por Bloques
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador8_InterculturalidadTotal <- function(tabla_consolidada, id_google_sheet, mes_objetivo, cubeta_completa = NULL) {
  cat("🌾 Trabajador 8 activado: Procesando Interculturalidad con Embudo Constitucional...\n")
  if(mes_objetivo > 12) return(invisible(NULL))
  
  col_destino_num <- mes_objetivo + 1
  letra_col <- LETTERS[col_destino_num]
  esp_validas <- "MEDICIN|MÉDIC|OBSTETR|PSICOLOG|ODONTO"
  
  nombres_cols <- names(tabla_consolidada)
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  base_clasificada <- tabla_consolidada %>%
    mutate(ESP = str_squish(str_remove_all(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE)), "[\r\n\"]"))) %>%
    filter(str_detect(ESP, esp_validas)) %>%
    mutate(
      ETN_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_AUTID_ETN), "", PCTE_AUTID_ETN)), "[\r\n\"]")),
      NAC_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_NAC_ETN), "", PCTE_NAC_ETN)), "[\r\n\"]")),
      PUE_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_PUEBLO), "", PCTE_PUEBLO)), "[\r\n\"]")),
      
      es_pueblo_huancavilca = str_detect(PUE_RAW, "HUANCAVILCA"),
      tiene_pueblo = str_detect(PUE_RAW, "CHIBULEO|HUANCAVILCA|KA.*ARI|CA.*ARI|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|KICHWA AMAZ.*|KISAPINCHA|QUISAPINCHA|KITUKARA|QUITUCARA|NATABUELA|OTAVALO|PALTAS|PANZALEO|PASTOS|PURUH.*|PURUHÁ|PURUHA|SALASA.*|SARAGURO|TOMABELA|WARAMKA|GUARANCA"),
      tiene_nac = str_detect(NAC_RAW, "ACHUAR|ANDOA|AWA|AWÁ|CHACHI|COF.*N|KOF.*N|EPERA|.*PERA|HUANCAVILCA|KICHWA|QUICHUA|SECOYA|SHIWIAR|SHUAR|TS.*CHILA|TSÁCHILA|WAORANI|HUAORANI|ZAPARA|ZÁPARA"),
      
      ETN_RESCATADA = case_when(tiene_pueblo | tiene_nac ~ "INDÍGENA", TRUE ~ ETN_RAW),
      NAC_RESCATADA = case_when(tiene_pueblo & es_pueblo_huancavilca ~ "HUANCAVILCA", tiene_pueblo & !es_pueblo_huancavilca ~ "KICHWA", tiene_nac ~ NAC_RAW, TRUE ~ ""),
      
      # 🚀 NIVEL 1: (10 espacios) Se habilita el "No Aplica" en la fila 13
      IDX_ETN = case_when(
        str_detect(ETN_RESCATADA, "AFRO.*") ~ 4, str_detect(ETN_RESCATADA, "BLANC.*") ~ 5,
        str_detect(ETN_RESCATADA, "IND.*GENA|INDÍGENA|INDIGENA") ~ 6, str_detect(ETN_RESCATADA, "MESTIZ.*") ~ 7,
        str_detect(ETN_RESCATADA, "MONTUB.*|MONTUV.*") ~ 8, str_detect(ETN_RESCATADA, "MULAT.*") ~ 9,
        str_detect(ETN_RESCATADA, "NEGR.*") ~ 10, str_detect(ETN_RESCATADA, "OTR.*") ~ 11,
        str_detect(ETN_RESCATADA, "APLICA") ~ 13, # Exclusivo para NO APLICA
        str_detect(ETN_RESCATADA, "SABE.*|RESPONDE") ~ 12,
        TRUE ~ 12 # Vacíos van a No sabe
      ),
      
      # 🚀 NIVEL 2: Solo Indígenas
      IDX_NAC = case_when(
        IDX_ETN == 6 ~ case_when(
          str_detect(NAC_RESCATADA, "ACHUAR") ~ 19, str_detect(NAC_RESCATADA, "ANDOA") ~ 20, str_detect(NAC_RESCATADA, "AWA|AWÁ") ~ 21,
          str_detect(NAC_RESCATADA, "CHACHI") ~ 22, str_detect(NAC_RESCATADA, "COF.*N|KOF.*N") ~ 23, str_detect(NAC_RESCATADA, "EPERA|.*PERA") ~ 24,
          str_detect(NAC_RESCATADA, "HUANCAVILCA") ~ 25, str_detect(NAC_RESCATADA, "KICHWA|QUICHUA") ~ 26, str_detect(NAC_RESCATADA, "SECOYA") ~ 27,
          str_detect(NAC_RESCATADA, "SHIWIAR") ~ 28, str_detect(NAC_RESCATADA, "SHUAR") ~ 29, str_detect(NAC_RESCATADA, "TS.*CHILA") ~ 30,
          str_detect(NAC_RESCATADA, "WAORANI|HUAORANI") ~ 31, str_detect(NAC_RESCATADA, "ZAPARA|ZÁPARA") ~ 32, TRUE ~ 33
        ), TRUE ~ -1
      ),
      
      # 🚀 NIVEL 3: Cascada
      IDX_PUE = case_when(
        IDX_ETN == 6 ~ case_when(
          IDX_NAC == 26 & tiene_pueblo & !es_pueblo_huancavilca ~ case_when(
            str_detect(PUE_RAW, "CHIBULEO") ~ 40, str_detect(PUE_RAW, "KA.*ARI|CA.*ARI") ~ 42, str_detect(PUE_RAW, "KARANKI|CARANQUI") ~ 43,
            str_detect(PUE_RAW, "KAYAMBI|CAYAMBI") ~ 44, str_detect(PUE_RAW, "KICHWA AMAZ.*") ~ 45, str_detect(PUE_RAW, "KISAPINCHA|QUISAPINCHA") ~ 46,
            str_detect(PUE_RAW, "KITUKARA|QUITUCARA") ~ 47, str_detect(PUE_RAW, "NATABUELA") ~ 48, str_detect(PUE_RAW, "OTAVALO") ~ 49,
            str_detect(PUE_RAW, "PALTAS") ~ 50, str_detect(PUE_RAW, "PANZALEO") ~ 51, str_detect(PUE_RAW, "PASTOS") ~ 52,
            str_detect(PUE_RAW, "PURUH.*|PURUHÁ|PURUHA") ~ 53, str_detect(PUE_RAW, "SALASA.*") ~ 54, str_detect(PUE_RAW, "SARAGURO") ~ 55,
            str_detect(PUE_RAW, "TOMABELA") ~ 56, str_detect(PUE_RAW, "WARAMKA|GUARANCA") ~ 57, TRUE ~ 58
          ),
          IDX_NAC == 25 & es_pueblo_huancavilca ~ 41, TRUE ~ 58
        ), TRUE ~ -1
      )
    )
  
  matEtn <- numeric(10) # 10 Filas de Etnia
  matNac <- numeric(15) 
  matPue <- numeric(19) 
  
  if(nrow(base_clasificada) > 0) {
    cEtn <- base_clasificada %>% filter(IDX_ETN != -1) %>% count(IDX_ETN)
    cNac <- base_clasificada %>% filter(IDX_NAC != -1) %>% count(IDX_NAC)
    cPue <- base_clasificada %>% filter(IDX_PUE != -1) %>% count(IDX_PUE)
    
    if(nrow(cEtn) > 0) { for(i in 1:nrow(cEtn)) matEtn[cEtn$IDX_ETN[i] - 3] <- cEtn$n[i] }
    if(nrow(cNac) > 0) { for(i in 1:nrow(cNac)) matNac[cNac$IDX_NAC[i] - 18] <- cNac$n[i] }
    if(nrow(cPue) > 0) { for(i in 1:nrow(cPue)) matPue[cPue$IDX_PUE[i] - 39] <- cPue$n[i] }
  }
  
  matEtn[matEtn == 0] <- NA_real_
  matNac[matNac == 0] <- NA_real_
  matPue[matPue == 0] <- NA_real_
  
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matEtn), range = paste0("ATEN POR AUTO-ETN!", letra_col, "4"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matNac), range = paste0("ATEN POR AUTO-ETN!", letra_col, "19"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matPue), range = paste0("ATEN POR AUTO-ETN!", letra_col, "40"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 8 finalizado con soporte explícito de 'No Aplica'.\n")
}
