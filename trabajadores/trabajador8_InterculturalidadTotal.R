# ====================================================================================
# TRABAJADOR 8: INTERCULTURALIDAD (ETNIA, NACIONALIDAD, PUEBLO)
# Arquitectura: Embudo Constitucional -> Amputación No-Indígenas -> API Batching
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador8_InterculturalidadTotal <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🌾 Trabajador 8 activado: Procesando Interculturalidad con Embudo Constitucional...\n")
  
  # ====================================================================
  # PASO 1. MAPA DE MESES Y COORDENADAS
  # ====================================================================
  mes_anterior <- mes_objetivo
  
  # Si es un mes de Trimestre (13, 14, 15, 16), el Trabajador se apaga
  if(mes_objetivo > 12) {
    cat("⏭️ Trabajador 8: Modo Trimestral detectado. Omitiendo proceso.\n")
    return(invisible(NULL))
  }
  
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  # ====================================================================
  # PASO 2. CATÁLOGOS DE ESPECIALIDADES Y BLINDAJE
  # ====================================================================
  # ⚠️ AVISO DE ARQUITECTURA: Radar homologado con el Trabajador 4 y 7. 
  # Inyección del comodín (.*) en M.*DIC para blindar al código de fallas de lectura en la nube.
  esp_validas <- "MEDICIN|M.*DIC|OBSTETR|PSICOLOG|ODONTO"
  
  # Escudo de Columnas: Protege si el archivo CSV del PRAS viene con columnas faltantes
  nombres_cols <- names(tabla_consolidada)
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  # ====================================================================
  # PASO 3. LIMPIEZA, EMBUDO Y AUTOCORRECCIÓN (Base Completa)
  # ====================================================================
  base_clasificada <- tabla_consolidada %>%
    mutate(
      # Escudo Anti-NA para purificar textos antes de las búsquedas regex
      ESP = str_squish(str_remove_all(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE)), "[\r\n\"]"))
    ) %>%
    filter(str_detect(ESP, esp_validas)) %>%
    mutate(
      ETN_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_AUTID_ETN), "", PCTE_AUTID_ETN)), "[\r\n\"]")),
      NAC_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_NAC_ETN), "", PCTE_NAC_ETN)), "[\r\n\"]")),
      PUE_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_PUEBLO), "", PCTE_PUEBLO)), "[\r\n\"]")),
      
      # Radares de rescate antropológico
      es_pueblo_huancavilca = str_detect(PUE_RAW, "HUANCAVILCA"),
      tiene_pueblo = str_detect(PUE_RAW, "CHIBULEO|HUANCAVILCA|KA.*ARI|CA.*ARI|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|KICHWA AMAZ.*|KISAPINCHA|QUISAPINCHA|KITUKARA|QUITUCARA|NATABUELA|OTAVALO|PALTAS|PANZALEO|PASTOS|PURUH.*|PURUHÁ|PURUHA|SALASA.*|SARAGURO|TOMABELA|WARAMKA|GUARANCA"),
      tiene_nac = str_detect(NAC_RAW, "ACHUAR|ANDOA|AWA|AWÁ|CHACHI|COF.*N|KOF.*N|EPERA|.*PERA|HUANCAVILCA|KICHWA|QUICHUA|SECOYA|SHIWIAR|SHUAR|TS.*CHILA|TSÁCHILA|WAORANI|HUAORANI|ZAPARA|ZÁPARA"),
      
      # 🚀 PASO 0: Rescate Étnico (El Falso Mestizo es corregido en RAM)
      ETN_RESCATADA = case_when(
        tiene_pueblo | tiene_nac ~ "INDÍGENA",
        TRUE ~ ETN_RAW
      ),
      NAC_RESCATADA = case_when(
        tiene_pueblo & es_pueblo_huancavilca ~ "HUANCAVILCA",
        tiene_pueblo & !es_pueblo_huancavilca ~ "KICHWA",
        tiene_nac ~ NAC_RAW,
        TRUE ~ ""
      ),
      
      # ====================================================================
      # PASO 4: CLASIFICADORES ESTRICTOS DE FILAS (CASCADA PERFECTA 1:1)
      # ====================================================================
      
      # 🚀 Nivel 1: Etnia (10 Filas posibles, de 4 a 13)
      Fila_ETN = case_when(
        str_detect(ETN_RESCATADA, "AFRO.*") ~ 4,
        str_detect(ETN_RESCATADA, "BLANC.*") ~ 5,
        str_detect(ETN_RESCATADA, "IND.*GENA|INDÍGENA|INDIGENA") ~ 6,
        str_detect(ETN_RESCATADA, "MESTIZ.*") ~ 7,
        str_detect(ETN_RESCATADA, "MONTUB.*|MONTUV.*") ~ 8,
        str_detect(ETN_RESCATADA, "MULAT.*") ~ 9,
        str_detect(ETN_RESCATADA, "NEGR.*") ~ 10,
        str_detect(ETN_RESCATADA, "OTR.*") ~ 11,
        str_detect(ETN_RESCATADA, "SABE.*|RESPONDE") ~ 12,
        TRUE ~ 13 # NA o No Aplica
      ),
      
      # 🚀 Nivel 2: Nacionalidad (15 Filas posibles, de 19 a 33)
      Fila_NAC = case_when(
        Fila_ETN == 6 ~ case_when( # SOLO LOS INDÍGENAS BAJAN A ESTE NIVEL
          str_detect(NAC_RESCATADA, "ACHUAR") ~ 19,
          str_detect(NAC_RESCATADA, "ANDOA") ~ 20,
          str_detect(NAC_RESCATADA, "AWA|AWÁ") ~ 21,
          str_detect(NAC_RESCATADA, "CHACHI") ~ 22,
          str_detect(NAC_RESCATADA, "COF.*N|KOF.*N") ~ 23,
          str_detect(NAC_RESCATADA, "EPERA|.*PERA") ~ 24,
          str_detect(NAC_RESCATADA, "HUANCAVILCA") ~ 25,
          str_detect(NAC_RESCATADA, "KICHWA|QUICHUA") ~ 26,
          str_detect(NAC_RESCATADA, "SECOYA") ~ 27,
          str_detect(NAC_RESCATADA, "SHIWIAR") ~ 28,
          str_detect(NAC_RESCATADA, "SHUAR") ~ 29,
          str_detect(NAC_RESCATADA, "TS.*CHILA") ~ 30,
          str_detect(NAC_RESCATADA, "WAORANI|HUAORANI") ~ 31,
          str_detect(NAC_RESCATADA, "ZAPARA|ZÁPARA") ~ 32,
          TRUE ~ 33 # Indígenas sin datos van a "No Sabemos"
        ),
        TRUE ~ -1 # Los Mestizos son ignorados y no ocupan espacio
      ),
      
      # 🚀 Nivel 3: Pueblo (19 Filas posibles, de 40 a 58)
      Fila_PUE = case_when(
        Fila_ETN == 6 ~ case_when( # SOLO LOS INDÍGENAS BAJAN A ESTE NIVEL
          Fila_NAC == 26 & tiene_pueblo & !es_pueblo_huancavilca ~ case_when( # Solo Kichwas con Pueblo
            str_detect(PUE_RAW, "CHIBULEO") ~ 40,
            str_detect(PUE_RAW, "KA.*ARI|CA.*ARI") ~ 42,
            str_detect(PUE_RAW, "KARANKI|CARANQUI") ~ 43,
            str_detect(PUE_RAW, "KAYAMBI|CAYAMBI") ~ 44,
            str_detect(PUE_RAW, "KICHWA AMAZ.*") ~ 45,
            str_detect(PUE_RAW, "KISAPINCHA|QUISAPINCHA") ~ 46,
            str_detect(PUE_RAW, "KITUKARA|QUITUCARA") ~ 47,
            str_detect(PUE_RAW, "NATABUELA") ~ 48,
            str_detect(PUE_RAW, "OTAVALO") ~ 49,
            str_detect(PUE_RAW, "PALTAS") ~ 50,
            str_detect(PUE_RAW, "PANZALEO") ~ 51,
            str_detect(PUE_RAW, "PASTOS") ~ 52,
            str_detect(PUE_RAW, "PURUH.*|PURUHÁ|PURUHA") ~ 53,
            str_detect(PUE_RAW, "SALASA.*") ~ 54,
            str_detect(PUE_RAW, "SARAGURO") ~ 55,
            str_detect(PUE_RAW, "TOMABELA") ~ 56,
            str_detect(PUE_RAW, "WARAMKA|GUARANCA") ~ 57,
            TRUE ~ 58
          ),
          Fila_NAC == 25 & es_pueblo_huancavilca ~ 41, # Huancavilca
          TRUE ~ 58 # Amazónicos y Kichwas sin pueblo van a "No Sabemos" (Fila 58)
        ),
        TRUE ~ -1 # Los Mestizos son ignorados y no ocupan espacio
      )
    )
  
  # ====================================================================
  # PASO 5. AGREGACIÓN VECTORIZADA (NUBE ESTÁNDAR)
  # ====================================================================
  # Construimos 3 vectores de longitud exacta para evitar bucles for lentos a la API
  matEtn <- numeric(10) # 10 espacios (Fila 4 a 13)
  matNac <- numeric(15) # 15 espacios (Fila 19 a 33)
  matPue <- numeric(19) # 19 espacios (Fila 40 a 58)
  
  if(nrow(base_clasificada) > 0) {
    # Agrupamos ignorando los valores -1 (No-indígenas)
    cEtn <- base_clasificada %>% filter(Fila_ETN != -1) %>% count(Fila_ETN)
    cNac <- base_clasificada %>% filter(Fila_NAC != -1) %>% count(Fila_NAC)
    cPue <- base_clasificada %>% filter(Fila_PUE != -1) %>% count(Fila_PUE)
    
    # Asignamos los totales usando las coordenadas matemáticas de desfase
    if(nrow(cEtn) > 0) {
      for(i in 1:nrow(cEtn)) matEtn[cEtn$Fila_ETN[i] - 3] <- cEtn$n[i]
    }
    if(nrow(cNac) > 0) {
      for(i in 1:nrow(cNac)) matNac[cNac$Fila_NAC[i] - 18] <- cNac$n[i]
    }
    if(nrow(cPue) > 0) {
      for(i in 1:nrow(cPue)) matPue[cPue$Fila_PUE[i] - 39] <- cPue$n[i]
    }
  }
  
  # ====================================================================
  # PASO 6. INYECCIÓN BLINDADA AL EXCEL (Solo 3 peticiones a la API)
  # ====================================================================
  # Convertimos ceros en NA para mantener la estética limpia del cuadro Excel
  matEtn[matEtn == 0] <- NA_real_
  matNac[matNac == 0] <- NA_real_
  matPue[matPue == 0] <- NA_real_
  
  # Remplazamos el for() de 40 iteraciones por 3 disparos matriciales masivos
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matEtn), range = paste0("ATEN POR AUTO-ETN!", letra_col, "4"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matNac), range = paste0("ATEN POR AUTO-ETN!", letra_col, "19"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matPue), range = paste0("ATEN POR AUTO-ETN!", letra_col, "40"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 8 finalizado. Integridad referencial étnica inyectada con éxito.\n")
}
