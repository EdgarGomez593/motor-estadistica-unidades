# ====================================================================================
# TRABAJADOR 8: ATENCIONES GLOBALES POR AUTOIDENTIFICACIÓN ÉTNICA (NAVY SEAL 8)
# Arquitectura: Cero Filtros + Rescate Étnico + Cascada Perfecta 1:1
# Coordenadas Exactas: Etnia(4), Nacionalidad(19), Pueblo(40)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador8_InterculturalidadTotal <- function(tabla_consolidada, id_google_sheet, mes_objetivo, cubeta_completa = NULL) {
  cat("🌾 Trabajador 8 activado: Procesando Interculturalidad Global (Auditoría Profunda)...\n")
  if(mes_objetivo > 12) return(invisible(NULL))
  
  # ====================================================================
  # 1. MAPA DE COORDENADAS
  # ====================================================================
  col_destino_num <- mes_objetivo + 1
  letra_col <- LETTERS[col_destino_num]
  
  nombres_cols <- names(tabla_consolidada)
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  # ====================================================================
  # PASO 1: BASE PURA (EXTRACCIÓN SIN FILTROS RESTRICTIVOS)
  # ====================================================================
  base_limpia <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    mutate(
      ETN_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_AUTID_ETN), "", PCTE_AUTID_ETN)), "[\r\n\"]")),
      NAC_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_NAC_ETN), "", PCTE_NAC_ETN)), "[\r\n\"]")),
      PUE_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_PUEBLO), "", PCTE_PUEBLO)), "[\r\n\"]"))
    )
  
  # ====================================================================
  # PASO 2: EMBUDO CONSTITUCIONAL Y CASCADA MILIMÉTRICA
  # ====================================================================
  base_clasificada <- base_limpia %>%
    mutate(
      # Detectores Antropológicos
      es_pueblo_huancavilca = str_detect(PUE_RAW, "HUANCAVILCA"),
      tiene_pueblo = str_detect(PUE_RAW, "CHIBULEO|HUANCAVILCA|KA.*ARI|CA.*ARI|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|KICHWA AMAZ.*|KISAPINCHA|QUISAPINCHA|KITUKARA|QUITUCARA|NATABUELA|OTAVALO|PALTAS|PANZALEO|PASTOS|PURUH.*|PURUHÁ|PURUHA|SALASA.*|SARAGURO|TOMABELA|WARAMKA|GUARANCA"),
      tiene_nac = str_detect(NAC_RAW, "ACHUAR|ANDOA|AWA|AWÁ|CHACHI|COF.*N|KOF.*N|EPERA|.*PERA|HUANCAVILCA|KICHWA|QUICHUA|SECOYA|SHIWIAR|SHUAR|TS.*CHILA|TSÁCHILA|WAORANI|HUAORANI|ZAPARA|ZÁPARA"),
      
      # 🚀 RESCATE ÉTNICO (Convierte Falsos Mestizos a Indígenas)
      ETN_RESC = case_when(tiene_pueblo | tiene_nac ~ "INDÍGENA", TRUE ~ ETN_RAW),
      NAC_RESC = case_when(tiene_pueblo & es_pueblo_huancavilca ~ "HUANCAVILCA", tiene_pueblo & !es_pueblo_huancavilca ~ "KICHWA", tiene_nac ~ NAC_RAW, TRUE ~ ""),
      
      # 🚀 NIVEL 1: ETNIA (10 Opciones exactas)
      iE = case_when(
        str_detect(ETN_RESC, 'AFRO.*') ~ 0,
        str_detect(ETN_RESC, 'BLANC.*') ~ 1,
        str_detect(ETN_RESC, 'IND.*GENA|INDÍGENA') ~ 2,
        str_detect(ETN_RESC, 'MESTIZ.*') ~ 3,
        str_detect(ETN_RESC, 'MONTUBI.*|MONTUVI.*') ~ 4,
        str_detect(ETN_RESC, 'MULAT.*') ~ 5,
        str_detect(ETN_RESC, 'NEGR.*') ~ 6,
        str_detect(ETN_RESC, 'OTR.*') ~ 7,
        str_detect(ETN_RESC, 'APLICA') ~ 9, # Mantiene su propia Fila (13)
        str_detect(ETN_RESC, 'SABE.*|RESPONDE') | ETN_RESC == "" | is.na(ETN_RESC) ~ 8, # Vacíos a "No Sabe" (Fila 12)
        TRUE ~ 8 
      ),
      
      # 🚀 NIVEL 2: NACIONALIDAD (Cascada 1:1, excluye "No Aplica")
      iN = case_when(
        iE == 2 ~ case_when( # Solo bajan los Indígenas
          str_detect(NAC_RESC, 'ACHUAR') ~ 0, str_detect(NAC_RESC, 'ANDOA') ~ 1, str_detect(NAC_RESC, 'AWA') ~ 2,
          str_detect(NAC_RESC, 'CHACHI') ~ 3, str_detect(NAC_RESC, 'COFAN') ~ 4, str_detect(NAC_RESC, 'EPERA') ~ 5,
          str_detect(NAC_RESC, 'HUANCAVILCA') ~ 6, str_detect(NAC_RESC, 'KICHWA') ~ 7, str_detect(NAC_RESC, 'SECOYA') ~ 8,
          str_detect(NAC_RESC, 'SHIWIAR') ~ 9, str_detect(NAC_RESC, 'SHUAR') ~ 10, str_detect(NAC_RESC, 'TS.*CHILA') ~ 11,
          str_detect(NAC_RESC, 'WAORANI|HUAORANI') ~ 12, str_detect(NAC_RESC, 'ZAPARA') ~ 13, 
          TRUE ~ 14 # Vacíos, "APLICA" y "SABE" se fusionan en "No sabe/No responde" (Fila 33)
        ),
        TRUE ~ -1 # Amputación Estricta de Mestizos
      ),
      
      # 🚀 NIVEL 3: PUEBLO (Cascada 1:1, excluye "No Aplica")
      iP = case_when(
        iE == 2 ~ case_when( # Solo bajan los Indígenas
          iN == 7 & tiene_pueblo & !es_pueblo_huancavilca ~ case_when(
            str_detect(PUE_RAW, 'CHIBULEO') ~ 0, str_detect(PUE_RAW, 'KA.*ARI|CA.*ARI') ~ 2, str_detect(PUE_RAW, 'KARANKI|CARANQUI') ~ 3,
            str_detect(PUE_RAW, 'KAYAMBI|CAYAMBI') ~ 4, str_detect(PUE_RAW, 'KICHWA AMAZ.*') ~ 5, str_detect(PUE_RAW, 'KISAPINCHA|QUISAPINCHA') ~ 6,
            str_detect(PUE_RAW, 'KITUKARA|QUITUCARA') ~ 7, str_detect(PUE_RAW, 'NATABUELA') ~ 8, str_detect(PUE_RAW, 'OTAVALO') ~ 9,
            str_detect(PUE_RAW, 'PALTAS') ~ 10, str_detect(PUE_RAW, 'PANZALEO') ~ 11, str_detect(PUE_RAW, 'PASTOS') ~ 12,
            str_detect(PUE_RAW, 'PURUH.*') ~ 13, str_detect(PUE_RAW, 'SALASA.*') ~ 14, str_detect(PUE_RAW, 'SARAGURO') ~ 15,
            str_detect(PUE_RAW, 'TOMABELA') ~ 16, str_detect(PUE_RAW, 'WARAMKA|GUARANCA') ~ 17, 
            TRUE ~ 18 # Vacíos, "APLICA" y "SABE" se fusionan en "No sabe/No responde" (Fila 58)
          ),
          iN == 6 & es_pueblo_huancavilca ~ 1,
          TRUE ~ 18 # Vacíos a "No sabe/No responde"
        ),
        TRUE ~ -1 # Amputación Estricta de Mestizos
      )
    )
  
  # ====================================================================
  # PASO 3: CONTEO VECTORIZADO Y MATRICES (10, 15, 19)
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
  # PASO 4: ESCRITURA EN COORDENADAS EXACTAS (4, 19, 40)
  # ====================================================================
  range_write(ss = id_google_sheet, data = data.frame(VALOR = v_Etnia), range = paste0("ATEN POR AUTO-ETN!", letra_col, "4"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = v_Nac),   range = paste0("ATEN POR AUTO-ETN!", letra_col, "19"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = v_Pueb),  range = paste0("ATEN POR AUTO-ETN!", letra_col, "40"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 8 finalizado. Matriz global intercultural inyectada sin filtros restrictivos.\n")
}
