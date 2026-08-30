# ====================================================================================
# TRABAJADOR 12: INTELIGENCIA OPERATIVA - VIOLENCIA FAMILIAR (MATRIZ ESPEJO)
# Arquitectura: Cascada Étnica Inmaculada + Escudo SOGIESC (Detección Trans & Misclicks)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador12_Violencia <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🛡️ Trabajador 12 activado: Procesando Violencia Familiar (Lógica SOGIESC y Étnica)...\n")
  
  # ====================================================================
  # 1. ARQUITECTURA DE COORDENADAS (GPS INTACTO)
  # ====================================================================
  fila_inicio_mes <- 42 + (mes_objetivo - 1) * 17
  
  num_a_letra <- function(n) {
    res <- ""
    while(n > 0) { rem <- (n - 1) %% 26; res <- paste0(LETTERS[rem + 1], res); n <- (n - 1) %/% 26 }
    return(res)
  }
  
  letra_inicio_matriz <- num_a_letra(3) # Columna C
  
  # ====================================================================
  # 2. RADAR DE CABECERAS FORENSES (Taxonomías Gemelas)
  # ====================================================================
  nombres_cols <- names(tabla_consolidada)
  nombres_upper <- toupper(nombres_cols)
  
  col_sex <- if(any(str_detect(nombres_upper, "^PCTE_SEXO$|PCTE_SEXO_BIOLOGICO"))) nombres_cols[str_detect(nombres_upper, "^PCTE_SEXO$|PCTE_SEXO_BIOLOGICO")][1] else { tabla_consolidada$PCTE_SEXO <- ""; "PCTE_SEXO" }
  col_ori <- if(any(str_detect(nombres_upper, "ORI_SEX|ORIENTACION"))) nombres_cols[str_detect(nombres_upper, "ORI_SEX|ORIENTACION")][1] else { tabla_consolidada$PCTE_ORI_SEX <- ""; "PCTE_ORI_SEX" }
  col_ide <- if(any(str_detect(nombres_upper, "IDE_GEN|IDENTIDAD_GENERO"))) nombres_cols[str_detect(nombres_upper, "IDE_GEN|IDENTIDAD_GENERO")][1] else { tabla_consolidada$PCTE_IDE_GEN <- ""; "PCTE_IDE_GEN" }
  
  col_etn <- if(any(str_detect(nombres_upper, "AUTOIDENTIFICACION|ETNICA|AUTID_ETN"))) nombres_cols[str_detect(nombres_upper, "AUTOIDENTIFICACION|ETNICA|AUTID_ETN")][1] else { tabla_consolidada$PCTE_AUTID_ETN <- ""; "PCTE_AUTID_ETN" }
  col_nac <- if("PCTE_NAC_ETN" %in% nombres_upper) nombres_cols[nombres_upper == "PCTE_NAC_ETN"][1] else if(any(str_detect(nombres_upper, "NAC_ETN|NACIONALIDAD_ETN"))) nombres_cols[str_detect(nombres_upper, "NAC_ETN|NACIONALIDAD_ETN")][1] else { tabla_consolidada$PCTE_NAC_ETN <- ""; "PCTE_NAC_ETN" }
  col_pue <- if(any(str_detect(nombres_upper, "PUEBLO"))) nombres_cols[str_detect(nombres_upper, "PUEBLO")][1] else { tabla_consolidada$PCTE_PUEBLO <- ""; "PCTE_PUEBLO" }
  col_tip <- if(any(str_detect(nombres_upper, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG"))) nombres_cols[str_detect(nombres_upper, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG")][1] else { tabla_consolidada$ATEMED_TIP_DIAG <- ""; "ATEMED_TIP_DIAG" }
  
  # ====================================================================
  # 3. EXTRACCIÓN Y FILTRADO CLÍNICO
  # ====================================================================
  base_filtrada <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    mutate(
      CIE = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10)), "[\r\n\"\\.]")),
      CRON = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG)), "[\r\n\"]")),
      TIP = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_tip)), "", !!sym(col_tip))), "[\r\n\"]")),
      
      SEXO_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_sex)), "", !!sym(col_sex))), "[\r\n\"]")),
      ORI_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_ori)), "", !!sym(col_ori))), "[\r\n\"]")),
      IDE_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_ide)), "", !!sym(col_ide))), "[\r\n\"]")),
      
      ETN_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_etn)), "", !!sym(col_etn))), "[\r\n\"]")),
      NAC_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_nac)), "", !!sym(col_nac))), "[\r\n\"]")),
      PUE_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_pue)), "", !!sym(col_pue))), "[\r\n\"]"))
    ) %>%
    filter(str_starts(CIE, "T74") & str_detect(TIP, "MORB") & str_detect(CRON, "PRIMER|^P$|^1$|^N$"))
  
  cat(paste0("🔍 AUDITORÍA PREVIA: Se encontraron ", nrow(base_filtrada), " casos base de Violencia (T74) de Primera Vez.\n"))
  
  if (nrow(base_filtrada) == 0) {
    cat("⚠️ Trabajador 12 se detiene de forma segura. No hay datos.\n")
    return(invisible(NULL))
  }

  # ====================================================================
  # 4. LOGICA SOGIESC (EL ESCUDO LGBTI Y CORRECCIÓN DE MISCLICKS)
  # ====================================================================
  base_clasificada <- base_filtrada %>%
    mutate(
      # El paciente es LGBTI SÓLO si su orientación lo dicta o si su identidad es Trans.
      esLGBTI = str_detect(ORI_RAW, "GAY|LESBIANA|BISEXUAL") | str_detect(IDE_RAW, "TRANS"),
      
      # Si no es LGBTI confirmado, recae automáticamente en su sexo biológico puro
      # Esto corrige los misclicks y asume "cisgénero" ante los vacíos
      esHombre = !esLGBTI & str_detect(SEXO_RAW, "HOMBRE|^M$"),
      esMujer = !esLGBTI & str_detect(SEXO_RAW, "MUJER|^F$"),
      
      idx_gen = case_when(esHombre ~ 1, esMujer ~ 2, esLGBTI ~ 3, TRUE ~ 0),
      
      fCie = case_when(
        str_starts(CIE, "T740") ~ 1, 
        str_starts(CIE, "T741") ~ 2, 
        str_starts(CIE, "T742") ~ 3, 
        str_starts(CIE, "T743") ~ 4, 
        str_starts(CIE, "T748") ~ 5, 
        str_starts(CIE, "T749") ~ 6, 
        TRUE ~ 0
      ),
      
      # ====================================================================
      # 5. CASCADA ÉTNICA INMACULADA (REGLAS 1 A 5)
      # ====================================================================
      es_pueblo_huancavilca = str_detect(PUE_RAW, "HUANCAVILCA"),
      tiene_pueblo = str_detect(PUE_RAW, "CHIBULEO|HUANCAVILCA|KA.*ARI|CA.*ARI|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|KICHWA AMAZ.*|KISAPINCHA|QUISAPINCHA|KITUKARA|QUITUCARA|NATABUELA|OTAVALO|PALTAS|PANZALEO|PASTOS|PURUH.*|PURUHÁ|PURUHA|SALASA.*|SARAGURO|TOMABELA|WARAMKA|GUARANCA"),
      tiene_nac = str_detect(NAC_RAW, "ACHUAR|ANDOA|AWA|AWÁ|CHACHI|COF.*N|KOF.*N|EPERA|.*PERA|HUANCAVILCA|KICHWA|QUICHUA|SECOYA|SHIWIAR|SHUAR|TS.*CHILA|TSÁCHILA|WAORANI|HUAORANI|ZAPARA|ZÁPARA"),
      
      ETNIA_RESC = case_when(tiene_pueblo | tiene_nac ~ "INDÍGENA", TRUE ~ ETN_RAW),
      NAC_RESC = NAC_RAW, 
      
      iE = case_when(
        str_detect(ETNIA_RESC, 'AFRO.*') ~ 1, str_detect(ETNIA_RESC, 'BLANC.*') ~ 2,
        str_detect(ETNIA_RESC, 'IND.*GENA|INDÍGENA') ~ 3, str_detect(ETNIA_RESC, 'MESTIZ.*') ~ 4,
        str_detect(ETNIA_RESC, 'MONTUBI.*|MONTUVI.*') ~ 5, str_detect(ETNIA_RESC, 'MULAT.*') ~ 6,
        str_detect(ETNIA_RESC, 'NEGR.*') ~ 7, str_detect(ETNIA_RESC, 'OTR.*') ~ 8,
        str_detect(ETNIA_RESC, 'APLICA') ~ 10,
        str_detect(ETNIA_RESC, 'SABE.*|RESPONDE') | ETNIA_RESC == "" | is.na(ETNIA_RESC) ~ 9, 
        TRUE ~ 9
      ),
      
      iN = case_when(
        iE == 3 ~ case_when(
          str_detect(NAC_RESC, 'ACHUAR') ~ 1, str_detect(NAC_RESC, 'ANDOA') ~ 2, str_detect(NAC_RESC, 'AWA') ~ 3,
          str_detect(NAC_RESC, 'CHACHI') ~ 4, str_detect(NAC_RESC, 'COF.*N|KOF.*N') ~ 5, str_detect(NAC_RESC, 'EPERA|.*PERA') ~ 6,
          str_detect(NAC_RESC, 'HUANCAVILCA') ~ 7, str_detect(NAC_RESC, 'KICHWA|QUICHUA') ~ 8, str_detect(NAC_RESC, 'SECOYA') ~ 9,
          str_detect(NAC_RESC, 'SHIWIAR') ~ 10, str_detect(NAC_RESC, 'SHUAR') ~ 11, str_detect(NAC_RESC, 'TS.*CHILA') ~ 12,
          str_detect(NAC_RESC, 'WAORANI|HUAORANI') ~ 13, str_detect(NAC_RESC, 'ZAPARA') ~ 14, 
          TRUE ~ 15
        ), TRUE ~ -1 
      ),
      
      iP = case_when(
        iE == 3 ~ case_when(
          tiene_pueblo & !es_pueblo_huancavilca ~ case_when(
            str_detect(PUE_RAW, 'CHIBULEO') ~ 1, str_detect(PUE_RAW, 'KA.*ARI|CA.*ARI') ~ 3, str_detect(PUE_RAW, 'KARANKI|CARANQUI') ~ 4,
            str_detect(PUE_RAW, 'KAYAMBI|CAYAMBI') ~ 5, str_detect(PUE_RAW, 'KICHWA AMAZ.*') ~ 6, str_detect(PUE_RAW, 'KISAPINCHA|QUISAPINCHA') ~ 7,
            str_detect(PUE_RAW, 'KITUKARA|QUITUCARA') ~ 8, str_detect(PUE_RAW, 'NATABUELA') ~ 9, str_detect(PUE_RAW, 'OTAVALO') ~ 10,
            str_detect(PUE_RAW, 'PALTAS') ~ 11, str_detect(PUE_RAW, 'PANZALEO') ~ 12, str_detect(PUE_RAW, 'PASTOS') ~ 13,
            str_detect(PUE_RAW, 'PURUH.*') ~ 14, str_detect(PUE_RAW, 'SALASA.*') ~ 15, str_detect(PUE_RAW, 'SARAGURO') ~ 16,
            str_detect(PUE_RAW, 'TOMABELA') ~ 17, str_detect(PUE_RAW, 'WARAMKA|GUARANCA') ~ 18, TRUE ~ 19
          ),
          es_pueblo_huancavilca ~ 2,
          TRUE ~ 19
        ), TRUE ~ -1 
      )
    ) %>%
    filter(idx_gen > 0 & fCie > 0)
  
  cat(paste0("🔍 AUDITORÍA POST-FILTRO: ", nrow(base_clasificada), " casos listos para inyección.\n"))

  # ====================================================================
  # 6. CONSTRUCCIÓN DE LA SUPERMATRIZ MILIMÉTRICA (6 Filas x 132 Columnas)
  # ====================================================================
  matriz_gigante <- matrix(0, nrow = 6, ncol = 132)
  
  if (nrow(base_clasificada) > 0) {
    for (r in 1:nrow(base_clasificada)) {
      fila <- base_clasificada[r, ]
      
      fC <- fila$fCie
      gI <- fila$idx_gen - 1 # 0: H, 1: M, 2: LGBTI
      
      cE <- (fila$iE - 1) * 3 + gI + 1
      matriz_gigante[fC, cE] <- matriz_gigante[fC, cE] + 1
      
      if (fila$iN != -1) {
        cN <- 30 + (fila$iN - 1) * 3 + gI + 1
        matriz_gigante[fC, cN] <- matriz_gigante[fC, cN] + 1
      }
      
      if (fila$iP != -1) {
        cP <- 75 + (fila$iP - 1) * 3 + gI + 1
        matriz_gigante[fC, cP] <- matriz_gigante[fC, cP] + 1
      }
    }
  }
  
  # ====================================================================
  # 7. INYECCIÓN BLINDADA (Zero-Ghost Data)
  # ====================================================================
  matriz_gigante[matriz_gigante == 0] <- NA_real_
  df_inyeccion <- as.data.frame(matriz_gigante)
  
  coordenada_objetivo <- paste0("VIOLENCIA!", letra_inicio_matriz, fila_inicio_mes)
  
  range_write(
    ss = id_google_sheet, 
    data = df_inyeccion, 
    range = coordenada_objetivo, 
    col_names = FALSE, 
    reformat = FALSE
  )
  
  cat("✅ Trabajador 12 finalizado. Matriz Inmaculada (132 Cols, Escudo SOGIESC) registrada en la nube.\n")
}
