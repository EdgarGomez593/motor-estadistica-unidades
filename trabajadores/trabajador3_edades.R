# ====================================================================================
# TRABAJADOR 3: ETNIA, NACIONALIDAD Y PUEBLO (ESTÁNDAR OFICIAL MSP)
# Arquitectura: Purga por Ticket -> Respeto a Autoidentificación -> Cascada Exclusiva Indígena
# Coordenadas Originales: Etnia(24), Nacionalidad(38), Pueblo(58)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador3_Edades <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🌍 Trabajador 3 activado: Clasificando Etnia, Nacionalidad y Pueblo Exclusivo Indígena...\n")
  if(mes_objetivo > 12) return(invisible(NULL))
  
  # ====================================================================
  # 1. MAPA DE MESES Y COORDENADAS ORIGINALES
  # ====================================================================
  mes_anterior <- mes_objetivo
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q")
  letra_col <- letras_columnas[col_destino_num]
  letra_resumen <- "Q" 
  
  # ====================================================================
  # 2. CATÁLOGOS OFICIALES SEGÚN FICHA METODOLÓGICA MSP
  # ====================================================================
  especialidades_validas <- "MEDICINA GENERAL INTEGRAL|MEDICINA FAMILIAR Y COMUNITARIA|MEDICINA RURAL|MEDICO RURAL|MÉDICO RURAL|GINECOLOGÍA Y OBSTETRICIA|GINECOLOGIA Y OBSTETRICIA|ATENCIÓN PRIMARIA EN SALUD|ATENCION PRIMARIA EN SALUD|OBSTETRICIA|OBSTETRICIA RURAL|MEDICINA GENERAL"
  
  codigos_emb <- c("Z340", "Z348", "Z349", "Z350", "Z351", "Z352", "Z353", "Z354", "Z355", "Z356", "Z357", "Z358", "Z359", "Z321")

  nombres_cols <- names(tabla_consolidada)
  if (!"ATEMED_ID" %in% nombres_cols) tabla_consolidada$ATEMED_ID <- paste0("FILA_", row_number())
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  if (!"PROF_ESP_ATE" %in% nombres_cols) tabla_consolidada$PROF_ESP_ATE <- ""
  if (!"ATEMED_CRON_DIAG" %in% nombres_cols) tabla_consolidada$ATEMED_CRON_DIAG <- ""
  
  col_tipo_diag <- "ATEMED_TIP_DIAG"
  if (!("ATEMED_TIP_DIAG" %in% nombres_cols) && ("ATEMED_TIPO_DIAG" %in% nombres_cols)) {
    col_tipo_diag <- "ATEMED_TIPO_DIAG"
  } else if (!"ATEMED_TIP_DIAG" %in% nombres_cols && !"ATEMED_TIPO_DIAG" %in% nombres_cols) {
    tabla_consolidada$ATEMED_TIP_DIAG <- ""
    col_tipo_diag <- "ATEMED_TIP_DIAG"
  }
  
  # ====================================================================
  # PASO 1: LA PURGA INICIAL Y PREPARACIÓN DE COLUMNAS
  # ====================================================================
  base_limpia <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    mutate(
      TICKET = str_trim(ATEMED_ID),
      CR = str_remove_all(str_trim(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG))), "[\r\n\"]"),
      TP = str_remove_all(str_trim(toupper(ifelse(is.na(!!sym(col_tipo_diag)), "", !!sym(col_tipo_diag)))), "[\r\n\"]"),
      ES = str_remove_all(str_trim(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE))), "[\r\n\"]"),
      CI = str_remove_all(str_trim(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10))), "[\r\n\"\\.]"),
      ET = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_AUTID_ETN), "", PCTE_AUTID_ETN))), "[\r\n\"]"),
      NA_col = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_NAC_ETN), "", PCTE_NAC_ETN))), "[\r\n\"\\.]"),
      PU = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_PUEBLO), "", PCTE_PUEBLO))), "[\r\n\"\\.]")
    )
  
  # ====================================================================
  # PASO 2: FILTRADO ESTRICTO CLÍNICO Y PURGA POR TICKET
  # ====================================================================
  base_intercultural <- base_limpia %>%
    filter(CI %in% codigos_emb) %>%
    filter(str_detect(CR, "PRIMER|^P$|^1$|^N$")) %>%
    filter(str_detect(ES, especialidades_validas)) %>%
    distinct(TICKET, .keep_all = TRUE)
  
  # ====================================================================
  # PASO 3: EMBUDO CONSTITUCIONAL Y CASCADA EXCLUSIVA INDÍGENA
  # ====================================================================
  base_clasificada <- base_intercultural %>%
    mutate(
      es_pueblo_huancavilca = str_detect(PU, "HUANCAVILCA"),
      tiene_pueblo = str_detect(PU, "CHIBULEO|HUANCAVILCA|KA.*ARI|CA.*ARI|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|KICHWA AMAZ.*|KISAPINCHA|QUISAPINCHA|KITUKARA|QUITUCARA|NATABUELA|OTAVALO|PALTAS|PANZALEO|PASTOS|PURUH.*|PURUHÁ|PURUHA|SALASA.*|SARAGURO|TOMABELA|WARAMKA|GUARANCA"),
      
      # 🚀 REGLA 1: Respeto Absoluto a la Autoidentificación (Cero Rescate Étnico)
      ETN_RESC = ET,
      NAC_RESC = NA_col,
      
      # 🚀 NIVEL 1: ETNIA (10 espacios: 0 al 9)
      iE = case_when(
        str_detect(ETN_RESC, 'AFRO.*') ~ 0,
        str_detect(ETN_RESC, 'BLANC.*') ~ 1,
        str_detect(ETN_RESC, 'IND.*GENA|INDÍGENA') ~ 2,
        str_detect(ETN_RESC, 'MESTIZ.*') ~ 3,
        str_detect(ETN_RESC, 'MONTUBI.*|MONTUVI.*') ~ 4,
        str_detect(ETN_RESC, 'MULAT.*') ~ 5,
        str_detect(ETN_RESC, 'NEGR.*') ~ 6,
        str_detect(ETN_RESC, 'OTR.*') ~ 7,
        str_detect(ETN_RESC, 'APLICA') ~ 9,
        str_detect(ETN_RESC, 'SABE.*|RESPONDE') | ETN_RESC == "" | is.na(ETN_RESC) ~ 8,
        TRUE ~ 8
      ),
      
      # 🚀 REGLA 2 y 4: CASCADA NACIONALIDAD (15 espacios: 0 al 14, Amputación total para no indígenas)
      iN = case_when(
        iE == 2 ~ case_when(
          str_detect(NAC_RESC, 'ACHUAR') ~ 0, str_detect(NAC_RESC, 'ANDOA') ~ 1, str_detect(NAC_RESC, 'AWA') ~ 2,
          str_detect(NAC_RESC, 'CHACHI') ~ 3, str_detect(NAC_RESC, 'COFAN') ~ 4, str_detect(NAC_RESC, 'EPERA') ~ 5,
          str_detect(NAC_RESC, 'HUANCAVILCA') ~ 6, str_detect(NAC_RESC, 'KICHWA') ~ 7, str_detect(NAC_RESC, 'SECOYA') ~ 8,
          str_detect(NAC_RESC, 'SHIWIAR') ~ 9, str_detect(NAC_RESC, 'SHUAR') ~ 10, str_detect(NAC_RESC, 'TS.*CHILA') ~ 11,
          str_detect(NAC_RESC, 'WAORANI|HUAORANI') ~ 12, str_detect(NAC_RESC, 'ZAPARA') ~ 13,
          TRUE ~ 14 # Vacíos y "No Sabe/Responde" exclusivos de indígenas
        ),
        TRUE ~ -1 # Amputación de Mestizos, Blancos, etc.
      ),
      
      # 🚀 REGLA 2, 3 y 5: CASCADA PUEBLO (19 espacios: 0 al 18, Amputación total para no indígenas)
      iP = case_when(
        iE == 2 ~ case_when(
          tiene_pueblo & !es_pueblo_huancavilca ~ case_when(
            str_detect(PU, 'CHIBULEO') ~ 0, str_detect(PU, 'KA.*ARI|CA.*ARI') ~ 2, str_detect(PU, 'KARANKI|CARANQUI') ~ 3,
            str_detect(PU, 'KAYAMBI|CAYAMBI') ~ 4, str_detect(PU, 'KICHWA AMAZ.*') ~ 5, str_detect(PU, 'KISAPINCHA|QUISAPINCHA') ~ 6,
            str_detect(PU, 'KITUKARA|QUITUCARA') ~ 7, str_detect(PU, 'NATABUELA') ~ 8, str_detect(PU, 'OTAVALO') ~ 9,
            str_detect(PU, 'PALTAS') ~ 10, str_detect(PU, 'PANZALEO') ~ 11, str_detect(PU, 'PASTOS') ~ 12,
            str_detect(PU, 'PURUH.*') ~ 13, str_detect(PU, 'SALASA.*') ~ 14, str_detect(PU, 'SARAGURO') ~ 15,
            str_detect(PU, 'TOMABELA') ~ 16, str_detect(PU, 'WARAMKA|GUARANCA') ~ 17,
            TRUE ~ 18 # Vacíos exclusivos de indígenas
          ),
          es_pueblo_huancavilca ~ 1,
          TRUE ~ 18 
        ),
        TRUE ~ -1 # Amputación de Mestizos, Blancos, etc.
      )
    )
  
  # ====================================================================
  # PASO 4: CONTEO ESTRICTO DE MATRICES
  # ====================================================================
  cE <- numeric(10) 
  cN <- numeric(15) 
  cP <- numeric(19) 
  
  for(i in 0:9)  { cE[i+1] <- sum(base_clasificada$iE == i, na.rm = TRUE) }
  for(i in 0:14) { cN[i+1] <- sum(base_clasificada$iN == i, na.rm = TRUE) }
  for(i in 0:18) { cP[i+1] <- sum(base_clasificada$iP == i, na.rm = TRUE) }
  
  formatear_ceros_vector <- function(v) { 
    v <- as.numeric(v)
    v[v == 0 | is.na(v)] <- NA_real_ 
    return(v)
  }
  
  v_Etnia <- formatear_ceros_vector(cE)
  v_Nac   <- formatear_ceros_vector(cN)
  v_Pueb  <- formatear_ceros_vector(cP)
  
  # ====================================================================
  # PASO 5: INYECCIÓN EXACTA EN COORDENADAS ORIGINALES (GPS SAGRADO)
  # ====================================================================
  range_write(ss = id_google_sheet, data = data.frame(v_Etnia), range = paste0("Censo_Obstetrico!", letra_col, "24"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_Nac),   range = paste0("Censo_Obstetrico!", letra_col, "38"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_Pueb),  range = paste0("Censo_Obstetrico!", letra_col, "58"), col_names = FALSE, reformat = FALSE)
  
  tryCatch({
    range_write(ss = id_google_sheet, data = data.frame(v_Etnia), range = paste0("Censo_Obstetrico!", letra_resumen, "24"), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = data.frame(v_Nac),   range = paste0("Censo_Obstetrico!", letra_resumen, "38"), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = data.frame(v_Pueb),  range = paste0("Censo_Obstetrico!", letra_resumen, "58"), col_names = FALSE, reformat = FALSE)
  }, error = function(e) { cat("⚠️ Resumen omitido (columna Q inexistente).\n") })
  
  cat("✅ Trabajador 3 finalizado. Matriz obstétrica intercultural actualizada bajo estándar de sub-universo indígena en coordenadas originales (24, 38, 58).\n")
}
