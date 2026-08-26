# ====================================================================================
# TRABAJADOR 15: INTELIGENCIA OPERATIVA - SALUD MENTAL
# Arquitectura: Matriz Horizontal Expandida (138 Columnas) -> Inyección Vertical Apilada
# Clon Estructural T12 (Adaptado para 4 diagnósticos + 5 filas de autolimpieza = 9 Filas)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador15_SaludMental <- function(tabla_consolidada, id_google_sheet, mes_objetivo, cubeta_completa = NULL) {
  
  cat(paste0("🧠 Trabajador 15 activado: Calculando Salud Mental (Matriz Expandida) para el mes ", mes_objetivo, "...\n"))
  
  if(mes_objetivo > 12) {
    cat("⏭️ Trabajador 15: Modo Trimestral detectado. Omitiendo proceso.\n")
    return(invisible(NULL))
  }
  
  # ============================================================================
  # 1. RADARES TOPOGRÁFICOS MAESTROS (Filas)
  # ============================================================================
  # Enero inicia en la fila 42. Salto exacto de 17 filas por mes.
  fila_inicio_mes <- 42 + (mes_objetivo - 1) * 17
  
  # ============================================================================
  # 2. ESCUDO PROTECTOR ANTI-NA (Arquitectura Nativa)
  # ============================================================================
  columnas_requeridas <- c("ATEMED_CIE10", "ATEMED_TIP_DIAG", "ATEMED_TIPO_DIAG", "ATEMED_CRON_DIAG",
                           "PCTE_SEXO", "PCTE_SEXO_BIOLOGICO", "PCTE_ORI_SEX", "ORIENTACION",
                           "PCTE_AUTID_ETN", "PCTE_AUTIDENTIFICACION_ETNICA", "AUTOIDENTIFICACION",
                           "PCTE_NAC_ETN", "NACIONALIDAD", "PCTE_PUEBLO", "PUEBLO")
  
  for(col in columnas_requeridas) {
    if(!col %in% names(tabla_consolidada)) tabla_consolidada[[col]] <- NA
  }
  
  nombres_cols <- names(tabla_consolidada)
  col_cie <- nombres_cols[str_detect(nombres_cols, "ATEMED_CIE10")][1]
  col_tip <- nombres_cols[str_detect(nombres_cols, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG")][1]
  col_cro <- nombres_cols[str_detect(nombres_cols, "ATEMED_CRON_DIAG")][1]
  col_sex <- nombres_cols[str_detect(nombres_cols, "PCTE_SEXO")][1]
  col_ori <- nombres_cols[str_detect(nombres_cols, "PCTE_ORI_SEX|ORIENTACION")][1]
  
  col_etn <- nombres_cols[str_detect(nombres_cols, "PCTE_AUT")][1] 
  if(is.na(col_etn)) col_etn <- nombres_cols[str_detect(nombres_cols, "ETNICA|AUTOIDENTIFICACION")][1]
  col_nac <- nombres_cols[str_detect(nombres_cols, "PCTE_NAC_ETN|NACIONALIDAD")][1]
  col_pue <- nombres_cols[str_detect(nombres_cols, "PCTE_PUEBLO|PUEBLO")][1]
  
  # ============================================================================
  # 3. BASE MAESTRA: EXTRACCIÓN, FILTRADO Y MOTOR DEMOGRÁFICO
  # ============================================================================
  base_filtrada <- tabla_consolidada %>%
    mutate(
      CIE = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_cie)), "", !!sym(col_cie))), "[\r\n\"\\.]")),
      TIP = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_tip)), "", !!sym(col_tip))), "[\r\n\"]")),
      CRO = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_cro)), "", !!sym(col_cro))), "[\r\n\"]"))
    ) %>%
    # 🚀 Acelerador RAM: Filtramos prematuramente para no sobrecargar los diccionarios
    filter(
      str_detect(CIE, "^(F412|F938|F321|F320)$"),
      str_detect(TIP, "MORB"),
      str_detect(CRO, "PRIMER|^P$|^1$|^N$")
    ) %>%
    mutate(
      SEXO = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_sex)), "", !!sym(col_sex))), "[\r\n\"]")),
      ORI = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_ori)), "", !!sym(col_ori))), "[\r\n\"]")),
      ETNIA = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_etn)), "NO APLICA", !!sym(col_etn))), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_nac)), "NO APLICA", !!sym(col_nac))), "[\r\n\"]")),
      PUEB = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_pue)), "NO APLICA", !!sym(col_pue))), "[\r\n\"]")),
      
      # Lógica de Género (Prioridad LGBTI)
      esLGBTI = str_detect(ORI, "GAY|LESBIANA|BISEXUAL|TRANS"),
      esHombre = !esLGBTI & str_detect(SEXO, "HOMBRE|^M$"),
      esMujer = !esLGBTI & str_detect(SEXO, "MUJER|^F$"),
      GENERO_OFFSET = case_when(esHombre ~ 0, esMujer ~ 1, esLGBTI ~ 2, TRUE ~ -1)
    ) %>%
    filter(GENERO_OFFSET != -1) %>%
    mutate(
      # IDENTIFICACIÓN DE FILA OBJETIVO (CIE-10) -> Fila en la matriz de R (1 a 4)
      DIAG_INDEX = case_when(
        CIE == "F412" ~ 1,
        CIE == "F938" ~ 2,
        CIE == "F321" ~ 3,
        CIE == "F320" ~ 4,
        TRUE ~ 0
      ),
      
      # DICCIONARIOS INTERCULTURALES
      iE = case_when(str_detect(ETNIA, 'AFRO.*') ~ 1, str_detect(ETNIA, 'BLANC.*') ~ 2, str_detect(ETNIA, 'IND.*GENA|INDÍGENA|INDIGENA') ~ 3, str_detect(ETNIA, 'MESTIZ.*') ~ 4, str_detect(ETNIA, 'MONTUB.*|MONTUV.*') ~ 5, str_detect(ETNIA, 'MULAT.*') ~ 6, str_detect(ETNIA, 'NEGR.*') ~ 7, str_detect(ETNIA, 'OTR.*') ~ 8, str_detect(ETNIA, 'SABE.*|RESPONDE') ~ 9, str_detect(ETNIA, 'APLICA|^$') ~ 10, TRUE ~ 10),
      iN = case_when(str_detect(NAC, 'ACHUAR') ~ 1, str_detect(NAC, 'ANDOA') ~ 2, str_detect(NAC, 'AWA|AWÁ') ~ 3, str_detect(NAC, 'CHACHI') ~ 4, str_detect(NAC, 'COF.*N|KOF.*N|COFÁN|COFAN') ~ 5, str_detect(NAC, 'EPERA|.*PERA|ÉPERA') ~ 6, str_detect(NAC, 'HUANCAVILCA') ~ 7, str_detect(NAC, 'KICH.*|QUICH.*') | str_detect(PUEB, 'CHIBULEO|SALASA|TOMABELA|KISAPINCHA|QUISAPINCHA|NATABUELA|OTAVALO|PURUH|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|PANZALEO|PASTOS|WARAMKA|GUARANCA|KA.*ARI|CA.*ARI|KAÑARI|CAÑARI') ~ 8, str_detect(NAC, 'SECOYA') ~ 9, str_detect(NAC, 'SHIWIAR') ~ 10, str_detect(NAC, 'SHUAR') ~ 11, str_detect(NAC, 'TS.*CHILA|TSÁCHILA') ~ 12, str_detect(NAC, 'WAORANI|HUAORANI') ~ 13, str_detect(NAC, 'Z.*PARA|ZÁPARA') ~ 14, str_detect(NAC, 'SABE.*|RESPONDE') ~ 15, str_detect(NAC, 'APLICA|^$') ~ 16, TRUE ~ 16),
      iP = case_when(str_detect(PUEB, 'CHIBULEO') ~ 1, str_detect(PUEB, 'HUANCAVILCA') ~ 2, str_detect(PUEB, 'KA.*ARI|CA.*ARI|KAÑARI|CAÑARI') ~ 3, str_detect(PUEB, 'KARANKI|CARANQUI') ~ 4, str_detect(PUEB, 'KAYAMBI|CAYAMBI') ~ 5, str_detect(PUEB, 'KICHWA AMAZ.*') ~ 6, str_detect(PUEB, 'KISAPINCHA|QUISAPINCHA') ~ 7, str_detect(PUEB, 'KITUKARA|QUITUCARA') ~ 8, str_detect(PUEB, 'NATABUELA') ~ 9, str_detect(PUEB, 'OTAVALO') ~ 10, str_detect(PUEB, 'PALTAS') ~ 11, str_detect(PUEB, 'PANZALEO') ~ 12, str_detect(PUEB, 'PASTOS') ~ 13, str_detect(PUEB, 'PURUH.*|PURUHÁ|PURUHA') ~ 14, str_detect(PUEB, 'SALASA.*') ~ 15, str_detect(PUEB, 'SARAGURO') ~ 16, str_detect(PUEB, 'TOMABELA') ~ 17, str_detect(PUEB, 'WARAMKA|GUARANCA') ~ 18, str_detect(PUEB, 'SABE.*|RESPONDE') ~ 19, str_detect(PUEB, 'APLICA|^$') ~ 20, TRUE ~ 20),
      
      # 🚀 ECUACIÓN DE COORDENADAS EXACTAS PARA LA MATRIZ (Escalado en Base-1 para R)
      COL_ETN = (iE - 1) * 3 + GENERO_OFFSET + 1,
      COL_NAC = 30 + (iN - 1) * 3 + GENERO_OFFSET + 1,
      COL_PUE = 78 + (iP - 1) * 3 + GENERO_OFFSET + 1
    ) %>%
    filter(DIAG_INDEX != 0)
  
  # ============================================================================
  # 4. CONSTRUCCIÓN Y AUTO-LIMPIEZA DE LA MATRIZ GIGANTE (9 Filas x 138 Columnas)
  # ============================================================================
  # Creamos la matriz llena de NA para que las celdas de Excel queden limpias
  matriz <- matrix(NA_real_, nrow = 9, ncol = 138)
  
  if(nrow(base_filtrada) > 0) {
    # Vectorización rápida de conteos sin bucles pesados
    c_etn <- base_filtrada %>% count(row = DIAG_INDEX, col = COL_ETN)
    c_nac <- base_filtrada %>% count(row = DIAG_INDEX, col = COL_NAC)
    c_pue <- base_filtrada %>% count(row = DIAG_INDEX, col = COL_PUE)
    
    all_counts <- bind_rows(c_etn, c_nac, c_pue) %>%
      group_by(row, col) %>% summarise(n = sum(n), .groups="drop")
    
    # Ensamblaje en la Matriz
    for(i in seq_len(nrow(all_counts))) {
      matriz[all_counts$row[i], all_counts$col[i]] <- all_counts$n[i]
    }
  }
  
  df_final <- as.data.frame(matriz)
  
  # ============================================================================
  # 5. INYECCIÓN BLINDADA (Un solo hit a la API de Sheets)
  # ============================================================================
  rwrite_blindado(ss = id_google_sheet, data = df_final, range = paste0("SALUD_MENTAL!C", fila_inicio_mes), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 15 finalizado. Matriz Salud Mental de 9x138 inyectada en la nube.\n")
}
