# ====================================================================================
# TRABAJADOR 3: OBSTETRICIA INTERCULTURAL (PRIMEROS CONTROLES)
# Arquitectura: Embudo Constitucional -> Amputación No-Indígenas -> API Batching Doble
# Coordenadas Adaptadas a Plantilla: Etnia(65), Nacionalidad(79), Pueblo(99)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador3_Edades <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🌍 Trabajador 3 activado: Clasificando Censo Obstétrico Intercultural...\n")
  
  # ====================================================================
  # 1. MAPA DE MESES Y COORDENADAS
  # ====================================================================
  mes_anterior <- mes_objetivo
  
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q")
  letra_col <- letras_columnas[col_destino_num]
  letra_resumen <- "Q" # Mantenemos la doble inyección en la columna de Resumen
  
  # ====================================================================
  # 2. DEFINICIÓN DE CATÁLOGOS ORIGINALES Y BLINDAJE
  # ====================================================================
  esp_validas <- "MEDICINA FAMILIAR Y COMUNITARIA|MEDICINA RURAL|OBSTETRICIA|PSICOLOGÍA CLÍNICA|PSICOLOGIA CLINICA|ODONTOLOGÍA|ODONTOLOGIA|ENFERMERÍA|ENFERMERIA"
  
  # Escudo de Columnas (Previene Object Not Found)
  nombres_cols <- names(tabla_consolidada)
  if (!"ATEMED_TIP_DIAG" %in% nombres_cols) tabla_consolidada$ATEMED_TIP_DIAG <- NA
  if (!"ATEMED_TIPO_DIAG" %in% nombres_cols) tabla_consolidada$ATEMED_TIPO_DIAG <- NA
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  # ====================================================================
  # PASO 1: LIMPIEZA, FILTRADO PREVENTIVO/OBSTÉTRICO
  # ====================================================================
  base_intercultural <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    mutate(
      # Unificación segura del nombre de columna de diagnóstico
      RAW_TIP = coalesce(as.character(ATEMED_TIP_DIAG), as.character(ATEMED_TIPO_DIAG), ""),
      
      TP = str_squish(str_remove_all(toupper(RAW_TIP), "[\r\n\"]")),
      CR = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG)), "[\r\n\"]")),
      ES = str_squish(str_remove_all(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE)), "[\r\n\"]")),
      CI = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10)), "[\r\n\"\\.]")),
      ET = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_AUTID_ETN), "", PCTE_AUTID_ETN)), "[\r\n\"]")),
      NA_col = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_NAC_ETN), "", PCTE_NAC_ETN)), "[\r\n\"]")),
      PU = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_PUEBLO), "", PCTE_PUEBLO)), "[\r\n\"]"))
    ) %>%
    # Filtros Clínicos Estrictos
    filter(str_detect(TP, "PREV")) %>%
    filter(CI == "Z321" | str_starts(CI, "Z34") | str_starts(CI, "Z35")) %>%
    filter(str_detect(CR, "PRIMER|^P$|^1$|^N$")) %>%
    filter(str_detect(ES, esp_validas))
  
  # ====================================================================
  # PASO 2: EMBUDO CONSTITUCIONAL Y CLASIFICADORES ESTRICTOS
  # ====================================================================
  base_clasificada <- base_intercultural %>%
    mutate(
      # Radares de rescate
      es_pueblo_huancavilca = str_detect(PU, "HUANCAVILCA"),
      tiene_pueblo = str_detect(PU, "CHIBULEO|HUANCAVILCA|KA.*ARI|CA.*ARI|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|KICHWA AMAZ.*|KISAPINCHA|QUISAPINCHA|KITUKARA|QUITUCARA|NATABUELA|OTAVALO|PALTAS|PANZALEO|PASTOS|PURUH.*|PURUHÁ|PURUHA|SALASA.*|SARAGURO|TOMABELA|WARAMKA|GUARANCA"),
      tiene_nac = str_detect(NA_col, "ACHUAR|ANDOA|AWA|AWÁ|CHACHI|COF.*N|KOF.*N|EPERA|.*PERA|HUANCAVILCA|KICHWA|QUICHUA|SECOYA|SHIWIAR|SHUAR|TS.*CHILA|TSÁCHILA|WAORANI|HUAORANI|ZAPARA|ZÁPARA"),
      
      # 🚀 PASO 0: Rescate Étnico (Corrección del Falso Mestizo)
      ETN_RESCATADA = case_when(
        tiene_pueblo | tiene_nac ~ "INDÍGENA",
        TRUE ~ ET
      ),
      NAC_RESCATADA = case_when(
        tiene_pueblo & es_pueblo_huancavilca ~ "HUANCAVILCA",
        tiene_pueblo & !es_pueblo_huancavilca ~ "KICHWA",
        tiene_nac ~ NA_col,
        TRUE ~ ""
      ),
      
      # 🚀 Nivel 1: Asignación de Etnia (Destrucción total del "No Aplica")
      Fila_ETN = case_when(
        str_detect(ETN_RESCATADA, "AFRO.*") ~ 65,
        str_detect(ETN_RESCATADA, "BLANC.*") ~ 66,
        str_detect(ETN_RESCATADA, "IND.*GENA|INDÍGENA|INDIGENA") ~ 67,
        str_detect(ETN_RESCATADA, "MESTIZ.*") ~ 68,
        str_detect(ETN_RESCATADA, "MONTUB.*|MONTUV.*") ~ 69,
        str_detect(ETN_RESCATADA, "MULAT.*") ~ 70,
        str_detect(ETN_RESCATADA, "NEGR.*") ~ 71,
        str_detect(ETN_RESCATADA, "OTR.*") ~ 72,
        # Absorbe Vacíos, Nulos, y la palabra "APLICA" directo a la fila 73
        TRUE ~ 73 
      ),
      
      # 🚀 Nivel 2: Asignación de Nacionalidad (Amputamos No-Indígenas)
      Fila_NAC = case_when(
        Fila_ETN == 67 ~ case_when( # Solo bajan los Indígenas
          str_detect(NAC_RESCATADA, "ACHUAR") ~ 79,
          str_detect(NAC_RESCATADA, "ANDOA") ~ 80,
          str_detect(NAC_RESCATADA, "AWA|AWÁ") ~ 81,
          str_detect(NAC_RESCATADA, "CHACHI") ~ 82,
          str_detect(NAC_RESCATADA, "COF.*N|KOF.*N") ~ 83,
          str_detect(NAC_RESCATADA, "EPERA|.*PERA") ~ 84,
          str_detect(NAC_RESCATADA, "HUANCAVILCA") ~ 85,
          str_detect(NAC_RESCATADA, "KICHWA|QUICHUA") ~ 86,
          str_detect(NAC_RESCATADA, "SECOYA") ~ 87,
          str_detect(NAC_RESCATADA, "SHIWIAR") ~ 88,
          str_detect(NAC_RESCATADA, "SHUAR") ~ 89,
          str_detect(NAC_RESCATADA, "TS.*CHILA") ~ 90,
          str_detect(NAC_RESCATADA, "WAORANI|HUAORANI") ~ 91,
          str_detect(NAC_RESCATADA, "ZAPARA|ZÁPARA") ~ 92,
          TRUE ~ 93 # Indígenas sin datos van a "No Sabemos/No responde"
        ),
        TRUE ~ -1 # Los Mestizos son ignorados y no ocupan espacio
      ),
      
      # 🚀 Nivel 3: Asignación de Pueblo (Cascada Forzada 1:1)
      Fila_PUE = case_when(
        Fila_ETN == 67 ~ case_when( # Solo bajan los Indígenas
          Fila_NAC == 86 & tiene_pueblo & !es_pueblo_huancavilca ~ case_when( # Solo Kichwas con Pueblo
            str_detect(PU, "CHIBULEO") ~ 99,
            str_detect(PU, "KA.*ARI|CA.*ARI") ~ 101,
            str_detect(PU, "KARANKI|CARANQUI") ~ 102,
            str_detect(PU, "KAYAMBI|CAYAMBI") ~ 103,
            str_detect(PU, "KICHWA AMAZ.*") ~ 104,
            str_detect(PU, "KISAPINCHA|QUISAPINCHA") ~ 105,
            str_detect(PU, "KITUKARA|QUITUCARA") ~ 106,
            str_detect(PU, "NATABUELA") ~ 107,
            str_detect(PU, "OTAVALO") ~ 108,
            str_detect(PU, "PALTAS") ~ 109,
            str_detect(PU, "PANZALEO") ~ 110,
            str_detect(PU, "PASTOS") ~ 111,
            str_detect(PU, "PURUH.*|PURUHÁ|PURUHA") ~ 112,
            str_detect(PU, "SALASA.*") ~ 113,
            str_detect(PU, "SARAGURO") ~ 114,
            str_detect(PU, "TOMABELA") ~ 115,
            str_detect(PU, "WARAMKA|GUARANCA") ~ 116,
            TRUE ~ 117
          ),
          Fila_NAC == 85 & es_pueblo_huancavilca ~ 100, # Huancavilca directo
          TRUE ~ 117 # Amazónicos y Kichwas sin pueblo aterrizan en "No Sabemos/No responde"
        ),
        TRUE ~ -1 # Los Mestizos son ignorados y no ocupan espacio
      )
    )
  
  # ====================================================================
  # PASO 3. AGREGACIÓN VECTORIZADA Y CONTEO
  # ====================================================================
  # Construimos vectores de longitud exacta adaptados a la plantilla
  matEtn <- numeric(9)  # 9 espacios (Fila 65 a 73)
  matNac <- numeric(15) # 15 espacios (Fila 79 a 93)
  matPue <- numeric(19) # 19 espacios (Fila 99 a 117)
  
  if(nrow(base_clasificada) > 0) {
    # Agrupamos ignorando los valores -1 (No-indígenas)
    cEtn <- base_clasificada %>% filter(Fila_ETN != -1) %>% count(Fila_ETN)
    cNac <- base_clasificada %>% filter(Fila_NAC != -1) %>% count(Fila_NAC)
    cPue <- base_clasificada %>% filter(Fila_PUE != -1) %>% count(Fila_PUE)
    
    # Rellenamos los vectores usando la resta del índice inicial para posicionar
    if(nrow(cEtn) > 0) { for(i in 1:nrow(cEtn)) matEtn[cEtn$Fila_ETN[i] - 64] <- cEtn$n[i] }
    if(nrow(cNac) > 0) { for(i in 1:nrow(cNac)) matNac[cNac$Fila_NAC[i] - 78] <- cNac$n[i] }
    if(nrow(cPue) > 0) { for(i in 1:nrow(cPue)) matPue[cPue$Fila_PUE[i] - 98] <- cPue$n[i] }
  }
  
  # Estética visual para Google Sheets (Convertimos ceros a NA_real_)
  matEtn[matEtn == 0] <- NA_real_
  matNac[matNac == 0] <- NA_real_
  matPue[matPue == 0] <- NA_real_
  
  # ====================================================================
  # PASO 4. INYECCIÓN BLINDADA AL EXCEL (Doble Inyección Batch)
  # ====================================================================
  
  # A) Inyección del Mes Analizado
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matEtn), range = paste0("Censo_Obstetrico!", letra_col, "65"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matNac), range = paste0("Censo_Obstetrico!", letra_col, "79"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matPue), range = paste0("Censo_Obstetrico!", letra_col, "99"), col_names = FALSE, reformat = FALSE)
  
  # B) Inyección Espejo en el Resumen Final (Columna Q)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matEtn), range = paste0("Censo_Obstetrico!", letra_resumen, "65"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matNac), range = paste0("Censo_Obstetrico!", letra_resumen, "79"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matPue), range = paste0("Censo_Obstetrico!", letra_resumen, "99"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 3 finalizado. Matriz obstétrica intercultural inyectada con perfección.\n")
}
