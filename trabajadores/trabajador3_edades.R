# ====================================================================================
# TRABAJADOR 3: ETNIA, NACIONALIDAD Y PUEBLO (ESTÁNDAR PROVINCIAL)
# Arquitectura: Embudo Constitucional -> Cascada Perfecta 1:1 -> Coordenadas Originales
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador3_Edades <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🌍 Trabajador 3 activado: Clasificando Censo Obstétrico Intercultural...\n")
  
  if(mes_objetivo > 12) {
    cat("⏭️ Trabajador 3: Modo Trimestral detectado. Omitiendo proceso.\n")
    return(invisible(NULL))
  }
  
  # ====================================================================
  # 1. MAPA DE MESES Y COORDENADAS (Filas Originales: 24, 38, 58)
  # ====================================================================
  mes_anterior <- mes_objetivo
  
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q")
  letra_col <- letras_columnas[col_destino_num]
  letra_resumen <- "Q" # La columna 17 
  
  # ====================================================================
  # 2. DEFINICIÓN DE CATÁLOGOS ORIGINALES Y BLINDAJE
  # ====================================================================
  especialidades_validas <- "MEDICINA FAMILIAR Y COMUNITARIA|MEDICINA RURAL|OBSTETRICIA|PSICOLOGÍA CLÍNICA|PSICOLOGIA CLINICA|ODONTOLOGÍA|ODONTOLOGIA|ENFERMERÍA|ENFERMERIA"
  
  nombres_cols <- names(tabla_consolidada)
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  col_tipo_diag <- "ATEMED_TIP_DIAG"
  if (!("ATEMED_TIP_DIAG" %in% nombres_cols) && ("ATEMED_TIPO_DIAG" %in% nombres_cols)) {
    col_tipo_diag <- "ATEMED_TIPO_DIAG"
  }
  
  # ====================================================================
  # PASO 1: LA PURGA INICIAL Y PREPARACIÓN DE COLUMNAS
  # ====================================================================
  base_limpia <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    mutate(
      CR = str_remove_all(str_trim(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG))), "[\r\n\"]"),
      TP = str_remove_all(str_trim(toupper(ifelse(is.na(!!sym(col_tipo_diag)), "", !!sym(col_tipo_diag)))), "[\r\n\"]"),
      ES = str_remove_all(str_trim(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE))), "[\r\n\"]"),
      CI = str_remove_all(str_trim(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10))), "[\r\n\"\\.]"),
      ET = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_AUTID_ETN), "", PCTE_AUTID_ETN))), "[\r\n\"]"),
      NA_col = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_NAC_ETN), "", PCTE_NAC_ETN))), "[\r\n\"]"),
      PU = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_PUEBLO), "", PCTE_PUEBLO))), "[\r\n\"]")
    )
  
  # ====================================================================
  # PASO 2: FILTRADO ESTRICTO (Primera Atención, Preventiva y Embarazo)
  # ====================================================================
  base_intercultural <- base_limpia %>%
    filter(str_detect(TP, "PREV")) %>%
    filter(CI == "Z321" | str_starts(CI, "Z34") | str_starts(CI, "Z35")) %>%
    filter(str_detect(CR, "PRIMER|^P$|^1$|^N$")) %>%
    filter(str_detect(ES, especialidades_validas))
  
  # ====================================================================
  # PASO 3: EMBUDO CONSTITUCIONAL Y CASCADA PERFECTA 1:1
  # ====================================================================
  base_clasificada <- base_intercultural %>%
    mutate(
      es_pueblo_huancavilca = str_detect(PU, "HUANCAVILCA"),
      tiene_pueblo = str_detect(PU, "CHIBULEO|HUANCAVILCA|KA.*ARI|CA.*ARI|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|KICHWA AMAZ.*|KISAPINCHA|QUISAPINCHA|KITUKARA|QUITUCARA|NATABUELA|OTAVALO|PALTAS|PANZALEO|PASTOS|PURUH.*|PURUHÁ|PURUHA|SALASA.*|SARAGURO|TOMABELA|WARAMKA|GUARANCA"),
      tiene_nac = str_detect(NA_col, "ACHUAR|ANDOA|AWA|AWÁ|CHACHI|COF.*N|KOF.*N|EPERA|.*PERA|HUANCAVILCA|KICHWA|QUICHUA|SECOYA|SHIWIAR|SHUAR|TS.*CHILA|TSÁCHILA|WAORANI|HUAORANI|ZAPARA|ZÁPARA"),
      
      # Rescate Étnico
      ETN_RESC = case_when(
        tiene_pueblo | tiene_nac ~ "INDÍGENA",
        TRUE ~ ET
      ),
      NAC_RESC = case_when(
        tiene_pueblo & es_pueblo_huancavilca ~ "HUANCAVILCA",
        tiene_pueblo & !es_pueblo_huancavilca ~ "KICHWA",
        tiene_nac ~ NA_col,
        TRUE ~ ""
      ),
      
      # Nivel 1: Etnia (Destruye "No Aplica")
      iE = case_when(
        str_detect(ETN_RESC, 'AFRO.*') ~ 0,
        str_detect(ETN_RESC, 'BLANC.*') ~ 1,
        str_detect(ETN_RESC, 'IND.*GENA|INDÍGENA') ~ 2,
        str_detect(ETN_RESC, 'MESTIZ.*') ~ 3,
        str_detect(ETN_RESC, 'MONTUBI.*|MONTUVI.*') ~ 4,
        str_detect(ETN_RESC, 'MULAT.*') ~ 5,
        str_detect(ETN_RESC, 'NEGR.*') ~ 6,
        str_detect(ETN_RESC, 'OTR.*') ~ 7,
        TRUE ~ 8 # Vacíos, "Aplica", "No Sabe" caen aquí
      ),
      
      # Nivel 2: Nacionalidad (Solo Indígenas)
      iN = case_when(
        iE == 2 ~ case_when(
          str_detect(NAC_RESC, 'ACHUAR') ~ 0,
          str_detect(NAC_RESC, 'ANDOA') ~ 1,
          str_detect(NAC_RESC, 'AWA') ~ 2,
          str_detect(NAC_RESC, 'CHACHI') ~ 3,
          str_detect(NAC_RESC, 'COFAN') ~ 4,
          str_detect(NAC_RESC, 'EPERA') ~ 5,
          str_detect(NAC_RESC, 'HUANCAVILCA') ~ 6,
          str_detect(NAC_RESC, 'KICHWA') ~ 7,
          str_detect(NAC_RESC, 'SECOYA') ~ 8,
          str_detect(NAC_RESC, 'SHIWIAR') ~ 9,
          str_detect(NAC_RESC, 'SHUAR') ~ 10,
          str_detect(NAC_RESC, 'TS.*CHILA') ~ 11,
          str_detect(NAC_RESC, 'WAORANI|HUAORANI') ~ 12,
          str_detect(NAC_RESC, 'ZAPARA') ~ 13,
          TRUE ~ 14 # "No Sabe/No Responde"
        ),
        TRUE ~ -1 # Mestizos amputados
      ),
      
      # Nivel 3: Pueblo (Cascada 1:1)
      iP = case_when(
        iE == 2 ~ case_when(
          iN == 7 & tiene_pueblo & !es_pueblo_huancavilca ~ case_when(
            str_detect(PU, 'CHIBULEO') ~ 0,
            str_detect(PU, 'KA.*ARI|CA.*ARI') ~ 2,
            str_detect(PU, 'KARANKI|CARANQUI') ~ 3,
            str_detect(PU, 'KAYAMBI|CAYAMBI') ~ 4,
            str_detect(PU, 'KICHWA AMAZ.*') ~ 5,
            str_detect(PU, 'KISAPINCHA|QUISAPINCHA') ~ 6,
            str_detect(PU, 'KITUKARA|QUITUCARA') ~ 7,
            str_detect(PU, 'NATABUELA') ~ 8,
            str_detect(PU, 'OTAVALO') ~ 9,
            str_detect(PU, 'PALTAS') ~ 10,
            str_detect(PU, 'PANZALEO') ~ 11,
            str_detect(PU, 'PASTOS') ~ 12,
            str_detect(PU, 'PURUH.*') ~ 13,
            str_detect(PU, 'SALASA.*') ~ 14,
            str_detect(PU, 'SARAGURO') ~ 15,
            str_detect(PU, 'TOMABELA') ~ 16,
            str_detect(PU, 'WARAMKA|GUARANCA') ~ 17,
            TRUE ~ 18
          ),
          iN == 6 & es_pueblo_huancavilca ~ 1,
          TRUE ~ 18 # "No Sabe/No Responde"
        ),
        TRUE ~ -1 # Mestizos amputados
      )
    )
  
  # ====================================================================
  # PASO 4: CONTEO Y PREPARACIÓN DE MATRICES (VECTORES ACTUALIZADOS)
  # ====================================================================
  cE <- numeric(9)
  cN <- numeric(15)
  cP <- numeric(19)
  
  for(i in 0:8)  { cE[i+1] <- sum(base_clasificada$iE == i, na.rm = TRUE) }
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
  # PASO 5: ESCRITURA MÚLTIPLE EN GOOGLE SHEETS (Coordenadas 24, 38, 58)
  # ====================================================================
  
  # Escritura para el Mes Actual 
  range_write(ss = id_google_sheet, data = data.frame(v_Etnia), range = paste0("Censo_Obstetrico!", letra_col, "24"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_Nac),   range = paste0("Censo_Obstetrico!", letra_col, "38"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_Pueb),  range = paste0("Censo_Obstetrico!", letra_col, "58"), col_names = FALSE, reformat = FALSE)
  
  # Escritura del Resumen Final (Columna Q)
  range_write(ss = id_google_sheet, data = data.frame(v_Etnia), range = paste0("Censo_Obstetrico!", letra_resumen, "24"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_Nac),   range = paste0("Censo_Obstetrico!", letra_resumen, "38"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_Pueb),  range = paste0("Censo_Obstetrico!", letra_resumen, "58"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 3 finalizado. Matriz obstétrica intercultural actualizada en coordenadas originales.\n")
}
