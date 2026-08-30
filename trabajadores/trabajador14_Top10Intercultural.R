# ====================================================================================
# TRABAJADOR 14: MOTOR TOP 10 INTERCULTURAL POR CICLO DE VIDA Y VULNERABILIDAD
# Arquitectura: Zero-Ghost Data -> Salto Trimestral Algorítmico -> Cascada Inmaculada
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador14_Top10Intercultural <- function(tabla_consolidada, id_google_sheet, mes_objetivo, cubeta_completa = NULL) {
  
  cat("🏆 Trabajador 14 activado: Iniciando supercomputadora de Top 10 Intercultural...\n")
  
  # ====================================================================
  # 1. ARQUITECTURA DE SALTO ALGORÍTMICO Y TRIMESTRES (INTACTA)
  # ====================================================================
  if (mes_objetivo == 13) {
    indice_bloque <- 4   
    cat("📊 MODO TRIMESTRE 1 DETECTADO: Apuntando a la bóveda Q1...\n")
  } else if (mes_objetivo == 14) {
    indice_bloque <- 8   
    cat("📊 MODO TRIMESTRE 2 DETECTADO: Apuntando a la bóveda Q2...\n")
  } else if (mes_objetivo == 15) {
    indice_bloque <- 12  
    cat("📊 MODO TRIMESTRE 3 DETECTADO: Apuntando a la bóveda Q3...\n")
  } else if (mes_objetivo == 16) {
    indice_bloque <- 16  
    cat("📊 MODO TRIMESTRE 4 DETECTADO: Apuntando a la bóveda Q4...\n")
  } else {
    indice_bloque <- mes_objetivo + floor((mes_objetivo - 1) / 3)
  }
  
  col_inicio_num <- 104 + (indice_bloque - 1) * 101
  
  num_a_letra <- function(n) {
    res <- ""
    while(n > 0) {
      rem <- (n - 1) %% 26
      res <- paste0(LETTERS[rem + 1], res)
      n <- (n - 1) %/% 26
    }
    return(res)
  }
  
  letra_inicio <- num_a_letra(col_inicio_num)
  
  # ====================================================================
  # 2. RADAR DE CABECERAS Y ESCUDOS ANTI-NA
  # ====================================================================
  nombres_cols <- names(tabla_consolidada)
  col_desc <- if(any(str_detect(nombres_cols, "ATEMED_DES_CIE10"))) "ATEMED_DES_CIE10" else { tabla_consolidada$ATEMED_DES_CIE10 <- ""; "ATEMED_DES_CIE10" }
  col_edad <- if(any(str_detect(nombres_cols, "ANIOS|AÑOS|EDAD"))) nombres_cols[str_detect(nombres_cols, "ANIOS|AÑOS|EDAD")][1] else { tabla_consolidada$PCTE_ANIOS <- -1; "PCTE_ANIOS" }
  col_disc <- if(any(str_detect(nombres_cols, "DISC"))) nombres_cols[str_detect(nombres_cols, "DISC")][1] else { tabla_consolidada$PCTE_DISC <- "NO"; "PCTE_DISC" }
  col_ori <- if(any(str_detect(nombres_cols, "ORI_SEX|ORIENTACION"))) nombres_cols[str_detect(nombres_cols, "ORI_SEX|ORIENTACION")][1] else { tabla_consolidada$PCTE_ORI_SEX <- ""; "PCTE_ORI_SEX" }
  
  col_etn <- if(any(str_detect(nombres_cols, "PCTE_AUT"))) nombres_cols[str_detect(nombres_cols, "PCTE_AUT")][1] else if(any(str_detect(nombres_cols, "ETNICA|AUTOIDENTIFICACION"))) nombres_cols[str_detect(nombres_cols, "ETNICA|AUTOIDENTIFICACION")][1] else { tabla_consolidada$PCTE_AUTID_ETN <- ""; "PCTE_AUTID_ETN" }
  col_nac <- if(any(str_detect(nombres_cols, "PCTE_NAC_ETN"))) "PCTE_NAC_ETN" else if(any(str_detect(nombres_cols, "NACIONALIDAD"))) nombres_cols[str_detect(nombres_cols, "NACIONALIDAD")][1] else { tabla_consolidada$PCTE_NAC_ETN <- ""; "PCTE_NAC_ETN" }
  col_pue <- if(any(str_detect(nombres_cols, "PCTE_PUEBLO"))) "PCTE_PUEBLO" else if(any(str_detect(nombres_cols, "PUEBLO"))) nombres_cols[str_detect(nombres_cols, "PUEBLO")][1] else { tabla_consolidada$PCTE_PUEBLO <- ""; "PCTE_PUEBLO" }
  col_sex <- if(any(str_detect(nombres_cols, "^PCTE_SEXO$|PCTE_SEXO_BIOLOGICO"))) nombres_cols[str_detect(nombres_cols, "^PCTE_SEXO$|PCTE_SEXO_BIOLOGICO")][1] else { tabla_consolidada$PCTE_SEXO <- ""; "PCTE_SEXO" }
  col_tip <- if(any(str_detect(nombres_cols, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG"))) nombres_cols[str_detect(nombres_cols, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG")][1] else { tabla_consolidada$ATEMED_TIP_DIAG <- ""; "ATEMED_TIP_DIAG" }
  
  # ====================================================================
  # 3. BASE MAESTRA PURIFICADA (Con Lógica de Cascada Inmaculada)
  # ====================================================================
  base_maestra <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    mutate(
      CIE = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10)), "[\r\n\"\\.]")),
      DESC = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_desc)), "", !!sym(col_desc))), "[\r\n\"]")),
      EDAD = as.numeric(ifelse(is.na(!!sym(col_edad)), -1, !!sym(col_edad))),
      DISC = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_disc)), "NO", !!sym(col_disc))), "[\r\n\"]")),
      CRON = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG)), "[\r\n\"]")),
      TIP = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_tip)), "", !!sym(col_tip))), "[\r\n\"]")),
      SEXO = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_sex)), "", !!sym(col_sex))), "[\r\n\"]")),
      ORI = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_ori)), "", !!sym(col_ori))), "[\r\n\"]")),
      ETNIA_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_etn)), "", !!sym(col_etn))), "[\r\n\"]")),
      NAC_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_nac)), "", !!sym(col_nac))), "[\r\n\"]")),
      PUEB_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_pue)), "", !!sym(col_pue))), "[\r\n\"]"))
    ) %>%
    # Filtros Clínicos de Base Intocables
    filter(str_detect(TIP, "MORB") & str_detect(CRON, "PRIMER|^P$|^1$|^N$")) %>%
    mutate(
      esLGBTI = str_detect(ORI, "GAY|LESBIANA|BISEXUAL|TRANS"),
      esHombre = !esLGBTI & str_detect(SEXO, "HOMBRE|^M$"),
      esMujer = !esLGBTI & str_detect(SEXO, "MUJER|^F$"),
      
      # 🚀 RADARES Y RESCATE ÉTNICO (Regla 1 y 2)
      es_pueblo_huancavilca = str_detect(PUEB_RAW, "HUANCAVILCA"),
      tiene_pueblo = str_detect(PUEB_RAW, "CHIBULEO|HUANCAVILCA|KA.*ARI|CA.*ARI|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|KICHWA AMAZ.*|KISAPINCHA|QUISAPINCHA|KITUKARA|QUITUCARA|NATABUELA|OTAVALO|PALTAS|PANZALEO|PASTOS|PURUH.*|PURUHÁ|PURUHA|SALASA.*|SARAGURO|TOMABELA|WARAMKA|GUARANCA"),
      tiene_nac = str_detect(NAC_RAW, "ACHUAR|ANDOA|AWA|AWÁ|CHACHI|COF.*N|KOF.*N|EPERA|.*PERA|HUANCAVILCA|KICHWA|QUICHUA|SECOYA|SHIWIAR|SHUAR|TS.*CHILA|TSÁCHILA|WAORANI|HUAORANI|ZAPARA|ZÁPARA"),
      
      ETNIA_RESC = case_when(tiene_pueblo | tiene_nac ~ "INDÍGENA", TRUE ~ ETNIA_RAW),
      NAC_RESC = NAC_RAW, # Mantiene independencia, no infla el Kichwa
      
      # 🚀 CASCADA NIVEL 1: Etnia (1 a 10)
      iE = case_when(
        str_detect(ETNIA_RESC, 'AFRO.*') ~ 1, str_detect(ETNIA_RESC, 'BLANC.*') ~ 2,
        str_detect(ETNIA_RESC, 'IND.*GENA|INDÍGENA') ~ 3, str_detect(ETNIA_RESC, 'MESTIZ.*') ~ 4,
        str_detect(ETNIA_RESC, 'MONTUBI.*|MONTUVI.*') ~ 5, str_detect(ETNIA_RESC, 'MULAT.*') ~ 6,
        str_detect(ETNIA_RESC, 'NEGR.*') ~ 7, str_detect(ETNIA_RESC, 'OTR.*') ~ 8,
        str_detect(ETNIA_RESC, 'APLICA') ~ 10,
        str_detect(ETNIA_RESC, 'SABE.*|RESPONDE') | ETNIA_RESC == "" | is.na(ETNIA_RESC) ~ 9, 
        TRUE ~ 9
      ),
      
      # 🚀 CASCADA NIVEL 2: Nacionalidad (1 a 15) - Solo entran Indígenas
      iN = case_when(
        iE == 3 ~ case_when(
          str_detect(NAC_RESC, 'ACHUAR') ~ 1, str_detect(NAC_RESC, 'ANDOA') ~ 2, str_detect(NAC_RESC, 'AWA') ~ 3,
          str_detect(NAC_RESC, 'CHACHI') ~ 4, str_detect(NAC_RESC, 'COF.*N|KOF.*N') ~ 5, str_detect(NAC_RESC, 'EPERA|.*PERA') ~ 6,
          str_detect(NAC_RESC, 'HUANCAVILCA') ~ 7, str_detect(NAC_RESC, 'KICHWA|QUICHUA') ~ 8, str_detect(NAC_RESC, 'SECOYA') ~ 9,
          str_detect(NAC_RESC, 'SHIWIAR') ~ 10, str_detect(NAC_RESC, 'SHUAR') ~ 11, str_detect(NAC_RESC, 'TS.*CHILA') ~ 12,
          str_detect(NAC_RESC, 'WAORANI|HUAORANI') ~ 13, str_detect(NAC_RESC, 'ZAPARA') ~ 14, 
          TRUE ~ 15 # Vacíos, "Aplica" y "No Sabe" caen aquí
        ), TRUE ~ -1 # Amputación de otras etnias
      ),
      
      # 🚀 CASCADA NIVEL 3: Pueblo (1 a 19) - Independiente de Nacionalidad
      iP = case_when(
        iE == 3 ~ case_when(
          tiene_pueblo & !es_pueblo_huancavilca ~ case_when(
            str_detect(PUEB_RAW, 'CHIBULEO') ~ 1, str_detect(PUEB_RAW, 'KA.*ARI|CA.*ARI') ~ 3, str_detect(PUEB_RAW, 'KARANKI|CARANQUI') ~ 4,
            str_detect(PUEB_RAW, 'KAYAMBI|CAYAMBI') ~ 5, str_detect(PUEB_RAW, 'KICHWA AMAZ.*') ~ 6, str_detect(PUEB_RAW, 'KISAPINCHA|QUISAPINCHA') ~ 7,
            str_detect(PUEB_RAW, 'KITUKARA|QUITUCARA') ~ 8, str_detect(PUEB_RAW, 'NATABUELA') ~ 9, str_detect(PUEB_RAW, 'OTAVALO') ~ 10,
            str_detect(PUEB_RAW, 'PALTAS') ~ 11, str_detect(PUEB_RAW, 'PANZALEO') ~ 12, str_detect(PUEB_RAW, 'PASTOS') ~ 13,
            str_detect(PUEB_RAW, 'PURUH.*') ~ 14, str_detect(PUEB_RAW, 'SALASA.*') ~ 15, str_detect(PUEB_RAW, 'SARAGURO') ~ 16,
            str_detect(PUEB_RAW, 'TOMABELA') ~ 17, str_detect(PUEB_RAW, 'WARAMKA|GUARANCA') ~ 18, TRUE ~ 19
          ),
          es_pueblo_huancavilca ~ 2,
          TRUE ~ 19 # Vacíos, "Aplica" y "No Sabe" caen aquí
        ), TRUE ~ -1 # Amputación de otras etnias
      )
    )
  
  # ====================================================================
  # 4. EL MOTOR DE CONSTRUCCIÓN (Data Reshaping)
  # ====================================================================
  construir_matriz_top10 <- function(df_subgrupo) {
    ranking <- df_subgrupo %>%
      group_by(CIE) %>%
      summarise(DESC = first(DESC), TOTAL = n(), .groups = "drop") %>%
      arrange(desc(TOTAL)) %>%
      slice(1:10)
    
    matriz_final <- as.data.frame(matrix(NA_real_, nrow = 10, ncol = 101))
    
    if(nrow(ranking) == 0) return(matriz_final)
    
    for(r in 1:nrow(ranking)) {
      c_cie <- ranking$CIE[r]
      c_desc <- ranking$DESC[r]
      sub <- df_subgrupo %>% filter(CIE == c_cie)
      
      # 🚀 REGLA 3: Dimensiones Matemáticas Exactas
      vE <- numeric(20); vN <- numeric(30); vP <- numeric(38)
      
      idx <- 1; for(i in 1:10) { vE[idx] <- sum(sub$iE == i & sub$esHombre, na.rm=T); vE[idx+1] <- sum(sub$iE == i & sub$esMujer, na.rm=T); idx <- idx + 2 }
      idx <- 1; for(i in 1:15) { vN[idx] <- sum(sub$iN == i & sub$esHombre, na.rm=T); vN[idx+1] <- sum(sub$iN == i & sub$esMujer, na.rm=T); idx <- idx + 2 }
      idx <- 1; for(i in 1:19) { vP[idx] <- sum(sub$iP == i & sub$esHombre, na.rm=T); vP[idx+1] <- sum(sub$iP == i & sub$esMujer, na.rm=T); idx <- idx + 2 }
      
      vE[vE == 0] <- NA_real_; vN[vN == 0] <- NA_real_; vP[vP == 0] <- NA_real_
      tE <- sum(vE, na.rm=T); if(tE == 0) tE <- NA_real_
      tN <- sum(vN, na.rm=T); if(tN == 0) tN <- NA_real_
      tP <- sum(vP, na.rm=T); if(tP == 0) tP <- NA_real_
      
      # 🚀 GPS DE INYECCIÓN INTOCABLE (Totales en 24, 58 y 100)
      matriz_final[r, 1] <- as.numeric(r)
      matriz_final[r, 2] <- as.character(c_cie)
      matriz_final[r, 3] <- as.character(c_desc)
      
      matriz_final[r, 4:23] <- vE                  # Etnia (10 pares)
      matriz_final[r, 24] <- tE                    # Total Etnia
      matriz_final[r, 25] <- NA_real_              # %
      
      matriz_final[r, 26:55] <- vN                 # Nac (15 pares)
      matriz_final[r, 58] <- tN                    # Total Nac (GPS Mantenido)
      matriz_final[r, 59] <- NA_real_              # %
      
      matriz_final[r, 60:97] <- vP                 # Pueblo (19 pares)
      matriz_final[r, 100] <- tP                   # Total Pueblo (GPS Mantenido)
      matriz_final[r, 101] <- NA_real_             # %
    }
    return(matriz_final)
  }
  
  # ====================================================================
  # 5. GENERACIÓN POR CICLOS DE VIDA
  # ====================================================================
  m_gen <- construir_matriz_top10(base_maestra)
  m_men <- construir_matriz_top10(base_maestra %>% filter(EDAD >= 0 & EDAD < 5))
  m_emb <- construir_matriz_top10(base_maestra %>% filter(str_starts(CIE, "O|Z321|Z34|Z35")))
  m_ado <- construir_matriz_top10(base_maestra %>% filter(EDAD >= 10 & EDAD <= 19))
  m_may <- construir_matriz_top10(base_maestra %>% filter(EDAD >= 65))
  m_dis <- construir_matriz_top10(base_maestra %>% filter(str_detect(DISC, "SI|^SÍ")))
  
  # ====================================================================
  # 6. INYECCIÓN BLINDADA A GOOGLE SHEETS
  # ====================================================================
  range_write(ss = id_google_sheet, data = as.data.frame(m_gen), range = paste0("TOP10_INTERCULTURAL!", letra_inicio, "4"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(m_men), range = paste0("TOP10_INTERCULTURAL!", letra_inicio, "17"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(m_emb), range = paste0("TOP10_INTERCULTURAL!", letra_inicio, "30"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(m_ado), range = paste0("TOP10_INTERCULTURAL!", letra_inicio, "43"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(m_may), range = paste0("TOP10_INTERCULTURAL!", letra_inicio, "56"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(m_dis), range = paste0("TOP10_INTERCULTURAL!", letra_inicio, "69"), col_names = FALSE, reformat = FALSE)
  
  cat(paste0("✅ Trabajador 14 Finalizado. Matriz intercultural inyectada desde la columna ", letra_inicio, ".\n"))

  # ==================================================================================
  # 🤖 SUB-RUTINA AUTÓNOMA: DETECCIÓN Y DISPARO TRIMESTRAL
  # ==================================================================================
  mes_num <- as.numeric(mes_objetivo) 
  meses_trimestre <- c(3, 6, 9, 12)
  
  if (!is.na(mes_num) && mes_num %in% meses_trimestre && !is.null(cubeta_completa)) {
    cat(paste0("\n⏱️ TRABAJADOR 14: Mes ", mes_num, " detectado como cierre de trimestre...\n"))
    
    if (is.data.frame(cubeta_completa)) {
      cat("🔴 ALERTA DE ARQUITECTURA: 'cubeta_completa' recibió una Tabla en lugar de una Lista Histórica.\n")
    } else if (is.list(cubeta_completa)) {
      
      if (length(cubeta_completa) >= mes_num) {
        mes_inicio <- mes_num - 2
        cat(paste0("📦 FUSIONANDO pacientes históricos (Meses ", mes_inicio, " al ", mes_num, ")...\n"))
        
        lista_temporal <- lapply(cubeta_completa[mes_inicio:mes_num], function(df) {
          df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
        })
        
        base_trimestral <- dplyr::bind_rows(lista_temporal)
        
        if(nrow(base_trimestral) > 0 && "ATEMED_ID" %in% names(base_trimestral)) {
          codigo_trimestre <- 13 + (mes_num / 3) - 1
          cat(paste0("🚀 DISPARANDO matriz pura a la bóveda del Trimestre ", (mes_num/3), "...\n"))
          
          trabajador14_Top10Intercultural(tabla_consolidada = base_trimestral, 
                                          id_google_sheet = id_google_sheet, 
                                          mes_objetivo = codigo_trimestre, 
                                          cubeta_completa = NULL)
          
          cat(paste0("✅ TRABAJADOR 14: Trimestre ", (mes_num/3), " consolidado y blindado.\n"))
        } else {
          cat("⚠️ TRABAJADOR 14: Fusión vacía. Se aborta la inyección.\n")
        }
      } else {
        cat(paste0("⚠️ TRABAJADOR 14: Memoria histórica insuficiente (", length(cubeta_completa), ").\n"))
      }
    }
  }
}
