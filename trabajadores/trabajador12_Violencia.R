# ====================================================================================
# TRABAJADOR 12: INTELIGENCIA OPERATIVA - VIOLENCIA FAMILIAR (MATRIZ ESPEJO)
# Arquitectura: Visibilidad Total (T74 Global) -> Matriz Horizontal Expandida (138 Cols)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador12_Violencia <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🛡️ Trabajador 12 activado: Procesando Inteligencia Operativa - Violencia y Etnias...\n")
  
  # ====================================================================
  # 1. ARQUITECTURA DE COORDENADAS (Salto Vertical Mensual)
  # ====================================================================
  # Enero(1) = 42 | Febrero(2) = 59 | Marzo(3) = 76...
  fila_inicio_mes <- 42 + (mes_objetivo - 1) * 17
  
  # Función auxiliar para convertir número de columna a Letra (Soporte hasta ZZ)
  num_a_letra <- function(n) {
    res <- ""
    while(n > 0) { rem <- (n - 1) %% 26; res <- paste0(LETTERS[rem + 1], res); n <- (n - 1) %/% 26 }
    return(res)
  }
  
  # La inyección horizontal siempre empieza en la columna C (Número 3)
  letra_inicio_matriz <- num_a_letra(3)
  
  # ====================================================================
  # 2. RADAR DE CABECERAS (Tolerancia a variaciones y Escudos Anti-NA)
  # ====================================================================
  nombres_cols <- names(tabla_consolidada)
  
  col_ori <- if(any(str_detect(nombres_cols, "ORI_SEX|ORIENTACION"))) nombres_cols[str_detect(nombres_cols, "ORI_SEX|ORIENTACION")][1] else { tabla_consolidada$PCTE_ORI_SEX <- ""; "PCTE_ORI_SEX" }
  col_etn <- if(any(str_detect(nombres_cols, "AUTOIDENTIFICACION|ETNICA|ETN"))) nombres_cols[str_detect(nombres_cols, "AUTOIDENTIFICACION|ETNICA|ETN")][1] else { tabla_consolidada$PCTE_AUTID_ETN <- ""; "PCTE_AUTID_ETN" }
  col_nac <- if(any(str_detect(nombres_cols, "NACIONALIDAD|NAC_ETN"))) nombres_cols[str_detect(nombres_cols, "NACIONALIDAD|NAC_ETN")][1] else { tabla_consolidada$PCTE_NAC_ETN <- ""; "PCTE_NAC_ETN" }
  col_pue <- if(any(str_detect(nombres_cols, "PUEBLO"))) nombres_cols[str_detect(nombres_cols, "PUEBLO")][1] else { tabla_consolidada$PCTE_PUEBLO <- ""; "PCTE_PUEBLO" }
  col_sex <- if(any(str_detect(nombres_cols, "^PCTE_SEXO$|PCTE_SEXO_BIOLOGICO"))) nombres_cols[str_detect(nombres_cols, "^PCTE_SEXO$|PCTE_SEXO_BIOLOGICO")][1] else { tabla_consolidada$PCTE_SEXO <- ""; "PCTE_SEXO" }
  col_tip <- if(any(str_detect(nombres_cols, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG"))) nombres_cols[str_detect(nombres_cols, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG")][1] else { tabla_consolidada$ATEMED_TIP_DIAG <- ""; "ATEMED_TIP_DIAG" }
  
  # ====================================================================
  # 3. PURGA Y MATRIZ ESPEJO (Sin Guillotina)
  # ====================================================================
  base_filtrada <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    
    # distinct(ATEMED_ID, .keep_all = TRUE) %>% 
    
    mutate(
      CIE = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10)), "[\r\n\"\\.]")),
      CRON = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG)), "[\r\n\"]")),
      TIP = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_tip)), "", !!sym(col_tip))), "[\r\n\"]")),
      SEXO = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_sex)), "", !!sym(col_sex))), "[\r\n\"]")),
      ORI = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_ori)), "", !!sym(col_ori))), "[\r\n\"]")),
      ETNIA = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_etn)), "", !!sym(col_etn))), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_nac)), "", !!sym(col_nac))), "[\r\n\"]")),
      PUEB = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_pue)), "", !!sym(col_pue))), "[\r\n\"]"))
    ) %>%
    filter(str_starts(CIE, "T74") & str_detect(TIP, "MORB") & str_detect(CRON, "PRIMER|^P$|^1$|^N$")) %>%
    mutate(
      esLGBTI = str_detect(ORI, "GAY|LESBIANA|BISEXUAL|TRANS"),
      esHombre = !esLGBTI & str_detect(SEXO, "HOMBRE|^M$"),
      esMujer = !esLGBTI & str_detect(SEXO, "MUJER|^F$"),
      
      idx_gen = case_when(esHombre ~ 1, esMujer ~ 2, esLGBTI ~ 3, TRUE ~ 0),
      fCie = case_when(CIE=="T740" ~ 1, CIE=="T741" ~ 2, CIE=="T742" ~ 3, CIE=="T743" ~ 4, CIE=="T748" ~ 5, CIE=="T749" ~ 6, TRUE ~ 0)
    )
  
  # ====================================================================
  # 4. MAPEO DE CLASIFICADORES INTERCULTURALES (Regex Curso Selva)
  # ====================================================================
  base_clasificada <- base_filtrada %>%
    filter(idx_gen > 0 & fCie > 0) %>%
    mutate(
      iE = case_when(str_detect(ETNIA, 'AFRO.*') ~ 1, str_detect(ETNIA, 'BLANC.*') ~ 2, str_detect(ETNIA, 'IND.*GENA') ~ 3, str_detect(ETNIA, 'MESTIZ.*') ~ 4, str_detect(ETNIA, 'MONTUBI.*|MONTUVI.*') ~ 5, str_detect(ETNIA, 'MULAT.*') ~ 6, str_detect(ETNIA, 'NEGR.*') ~ 7, str_detect(ETNIA, 'OTR.*') ~ 8, str_detect(ETNIA, 'SABE.*|RESPONDE') ~ 9, str_detect(ETNIA, 'APLICA') | ETNIA == "" ~ 10, TRUE ~ 10),
      iN = case_when(str_detect(NAC, 'ACHUAR') ~ 1, str_detect(NAC, 'ANDOA') ~ 2, str_detect(NAC, 'AWA') ~ 3, str_detect(NAC, 'CHACHI') ~ 4, str_detect(NAC, 'COFAN') ~ 5, str_detect(NAC, 'EPERA') ~ 6, str_detect(NAC, 'HUANCAVILCA') ~ 7, str_detect(NAC, 'KICHWA') ~ 8, str_detect(NAC, 'SECOYA') ~ 9, str_detect(NAC, 'SHIWIAR') ~ 10, str_detect(NAC, 'SHUAR') ~ 11, str_detect(NAC, 'TS.*CHILA') ~ 12, str_detect(NAC, 'WAORANI|HUAORANI') ~ 13, str_detect(NAC, 'ZAPARA') ~ 14, str_detect(NAC, 'SABE.*|RESPONDE') ~ 15, str_detect(NAC, 'APLICA') | NAC == "" ~ 16, TRUE ~ 16),
      iP = case_when(str_detect(PUEB, 'CHIBULEO') ~ 1, str_detect(PUEB, 'HUANCAVILCA') ~ 2, str_detect(PUEB, 'KA.*ARI|CA.*ARI') ~ 3, str_detect(PUEB, 'KARANKI|CARANQUI') ~ 4, str_detect(PUEB, 'KAYAMBI|CAYAMBI') ~ 5, str_detect(PUEB, 'KICHWA AMAZ.*') ~ 6, str_detect(PUEB, 'KISAPINCHA|QUISAPINCHA') ~ 7, str_detect(PUEB, 'KITUKARA|QUITUCARA') ~ 8, str_detect(PUEB, 'NATABUELA') ~ 9, str_detect(PUEB, 'OTAVALO') ~ 10, str_detect(PUEB, 'PALTAS') ~ 11, str_detect(PUEB, 'PANZALEO') ~ 12, str_detect(PUEB, 'PASTOS') ~ 13, str_detect(PUEB, 'PURUH.*') ~ 14, str_detect(PUEB, 'SALASA.*') ~ 15, str_detect(PUEB, 'SARAGURO') ~ 16, str_detect(PUEB, 'TOMABELA') ~ 17, str_detect(PUEB, 'WARAMKA|GUARANCA') ~ 18, str_detect(PUEB, 'SABE.*|RESPONDE') ~ 19, str_detect(PUEB, 'APLICA') | PUEB == "" ~ 20, TRUE ~ 20)
    )
  
  # ====================================================================
  # 5. CONSTRUCCIÓN DE LA SUPERMATRIZ (6 Filas x 138 Columnas)
  # ====================================================================
  matriz_gigante <- matrix(0, nrow = 6, ncol = 138)
  
  if (nrow(base_clasificada) > 0) {
    for (r in 1:nrow(base_clasificada)) {
      fila <- base_clasificada[r, ]
      
      fC <- fila$fCie
      gI <- fila$idx_gen - 1
      
      # Matemática de punteros
      cE <- (fila$iE - 1) * 3 + gI + 1
      cN <- 30 + (fila$iN - 1) * 3 + gI + 1
      cP <- 78 + (fila$iP - 1) * 3 + gI + 1
      
      # Sumatoria
      matriz_gigante[fC, cE] <- matriz_gigante[fC, cE] + 1
      matriz_gigante[fC, cN] <- matriz_gigante[fC, cN] + 1
      matriz_gigante[fC, cP] <- matriz_gigante[fC, cP] + 1
    }
  }
  
  # ====================================================================
  # 6. SANITIZACIÓN A NA_REAL_ (Misma lógica original de GitHub)
  # ====================================================================
  matriz_gigante[matriz_gigante == 0] <- NA_real_
  df_inyeccion <- as.data.frame(matriz_gigante)
  
  # ====================================================================
  # 7. INYECCIÓN BLINDADA POR ZONAS (Nube Nativa)
  # ====================================================================
  coordenada_objetivo <- paste0("VIOLENCIA!", letra_inicio_matriz, fila_inicio_mes)
  
  # Manteniendo tu función range_write intacta y blindada
  range_write(
    ss = id_google_sheet, 
    data = df_inyeccion, 
    range = coordenada_objetivo, 
    col_names = FALSE, 
    reformat = FALSE
  )
  
  cat("✅ Trabajador 12 finalizado. Matriz Horizontal de Violencia registrada en la nube.\n")
}
