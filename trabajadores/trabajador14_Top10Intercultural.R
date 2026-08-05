# ====================================================================================
# TRABAJADOR 14: MOTOR TOP 10 INTERCULTURAL POR CICLO DE VIDA Y VULNERABILIDAD
# Arquitectura: Zero-Ghost Data -> Salto Trimestral Algorítmico -> Type-Safe Injection
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador14_Top10Intercultural <- function(tabla_consolidada, id_google_sheet, mes_objetivo, cubeta_completa = NULL) {
  
  cat("🏆 Trabajador 14 activado: Iniciando supercomputadora de Top 10 Intercultural...\n")
  
  # ====================================================================
  # 1. ARQUITECTURA DE SALTO ALGORÍTMICO Y TRIMESTRES
  # ====================================================================
  # Detectamos si es un mes normal (1-12) o un Trimestre (13, 14, 15, 16)
  if (mes_objetivo == 13) {
    indice_bloque <- 4   # Trimestre 1 -> Va a la columna OQ
    cat("📊 MODO TRIMESTRE 1 DETECTADO: Apuntando a la bóveda Q1...\n")
  } else if (mes_objetivo == 14) {
    indice_bloque <- 8   # Trimestre 2 -> Va a la columna AEE
    cat("📊 MODO TRIMESTRE 2 DETECTADO: Apuntando a la bóveda Q2...\n")
  } else if (mes_objetivo == 15) {
    indice_bloque <- 12  # Trimestre 3 -> Va a la columna ATS
    cat("📊 MODO TRIMESTRE 3 DETECTADO: Apuntando a la bóveda Q3...\n")
  } else if (mes_objetivo == 16) {
    indice_bloque <- 16  # Trimestre 4 -> Va a la columna BJG
    cat("📊 MODO TRIMESTRE 4 DETECTADO: Apuntando a la bóveda Q4...\n")
  } else {
    # Meses normales: Ecuación con salto automático para no pisar los trimestres
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
 
  # 🛡️ RADARES INTERCULTURALES BLINDADOS (Prioridad Exacta)
  col_etn <- if(any(str_detect(nombres_cols, "PCTE_AUT"))) nombres_cols[str_detect(nombres_cols, "PCTE_AUT")][1] else if(any(str_detect(nombres_cols, "ETNICA|AUTOIDENTIFICACION"))) nombres_cols[str_detect(nombres_cols, "ETNICA|AUTOIDENTIFICACION")][1] else { tabla_consolidada$PCTE_AUTID_ETN <- ""; "PCTE_AUTID_ETN" }
  col_nac <- if(any(str_detect(nombres_cols, "PCTE_NAC_ETN"))) "PCTE_NAC_ETN" else if(any(str_detect(nombres_cols, "NACIONALIDAD"))) nombres_cols[str_detect(nombres_cols, "NACIONALIDAD")][1] else { tabla_consolidada$PCTE_NAC_ETN <- ""; "PCTE_NAC_ETN" }
  col_pue <- if(any(str_detect(nombres_cols, "PCTE_PUEBLO"))) "PCTE_PUEBLO" else if(any(str_detect(nombres_cols, "PUEBLO"))) nombres_cols[str_detect(nombres_cols, "PUEBLO")][1] else { tabla_consolidada$PCTE_PUEBLO <- ""; "PCTE_PUEBLO" }
  col_sex <- if(any(str_detect(nombres_cols, "^PCTE_SEXO$|PCTE_SEXO_BIOLOGICO"))) nombres_cols[str_detect(nombres_cols, "^PCTE_SEXO$|PCTE_SEXO_BIOLOGICO")][1] else { tabla_consolidada$PCTE_SEXO <- ""; "PCTE_SEXO" }
  col_tip <- if(any(str_detect(nombres_cols, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG"))) nombres_cols[str_detect(nombres_cols, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG")][1] else { tabla_consolidada$ATEMED_TIP_DIAG <- ""; "ATEMED_TIP_DIAG" }
  
  # ====================================================================
  # 3. BASE MAESTRA PURIFICADA (Sin Guillotina)
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
      ETNIA = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_etn)), "", !!sym(col_etn))), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_nac)), "", !!sym(col_nac))), "[\r\n\"]")),
      PUEB = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_pue)), "", !!sym(col_pue))), "[\r\n\"]"))
    ) %>%
    filter(str_detect(TIP, "MORB") & str_detect(CRON, "PRIMER|^P$|^1$|^N$")) %>%
    mutate(
      esLGBTI = str_detect(ORI, "GAY|LESBIANA|BISEXUAL|TRANS"),
      esHombre = !esLGBTI & str_detect(SEXO, "HOMBRE|^M$"),
      esMujer = !esLGBTI & str_detect(SEXO, "MUJER|^F$"),
      
      # ⚠️ RADARES DE SELVA ACTIVADOS (Sincronía total .*)
      iE = case_when(str_detect(ETNIA, 'AFRO.*') ~ 1, str_detect(ETNIA, 'BLANC.*') ~ 2, str_detect(ETNIA, 'IND.*GENA') ~ 3, str_detect(ETNIA, 'MESTIZ.*') ~ 4, str_detect(ETNIA, 'MONTUB.*|MONTUV.*') ~ 5, str_detect(ETNIA, 'MULAT.*') ~ 6, str_detect(ETNIA, 'NEGR.*') ~ 7, str_detect(ETNIA, 'OTR.*') ~ 8, str_detect(ETNIA, 'SABE.*|RESPONDE') ~ 9, str_detect(ETNIA, 'APLICA|^$') ~ 10, TRUE ~ 10),
      iN = case_when(
        str_detect(NAC, 'ACHUAR.*') ~ 1, 
        str_detect(NAC, 'ANDOA.*') ~ 2, 
        str_detect(NAC, 'AWA.*|AWÁ.*') ~ 3, 
        str_detect(NAC, 'CHACHI.*') ~ 4, 
        str_detect(NAC, 'COF.*N|KOF.*N') ~ 5, 
        str_detect(NAC, 'EPERA|.*PERA') ~ 6, 
        str_detect(NAC, 'HUANCAVILCA.*') ~ 7, 
        
        # 🚀 MOTOR KICHWA: Atrapa TODA variante ortográfica Y auto-deduce si el Pueblo es Kichwa
        str_detect(NAC, 'KICH.*|QUICH.*') | str_detect(PUEB, 'CHIBULEO|SALASA|TOMABELA|KISAPINCHA|QUISAPINCHA|NATABUELA|OTAVALO|PURUH|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|PANZALEO|PASTOS|WARAMKA|GUARANCA|KA.*ARI|CA.*ARI') ~ 8, 
        
        str_detect(NAC, 'SECOYA.*') ~ 9, 
        str_detect(NAC, 'SHIWIAR.*') ~ 10, 
        str_detect(NAC, 'SHUAR.*') ~ 11, 
        str_detect(NAC, 'TS.*CHILA') ~ 12, 
        str_detect(NAC, 'WAORANI|HUAORANI') ~ 13, 
        str_detect(NAC, 'Z.*PARA|ZAPARA.*') ~ 14, 
        str_detect(NAC, 'SABE.*|RESPONDE') ~ 15, 
        str_detect(NAC, 'APLICA|^$') ~ 16, 
        TRUE ~ 16
      ),
      iP = case_when(str_detect(PUEB, 'CHIBULEO') ~ 1, str_detect(PUEB, 'HUANCAVILCA') ~ 2, str_detect(PUEB, 'KA.*ARI|CA.*ARI') ~ 3, str_detect(PUEB, 'KARANKI|CARANQUI') ~ 4, str_detect(PUEB, 'KAYAMBI|CAYAMBI') ~ 5, str_detect(PUEB, 'KICHWA AMAZ.*') ~ 6, str_detect(PUEB, 'KISAPINCHA|QUISAPINCHA') ~ 7, str_detect(PUEB, 'KITUKARA|QUITUCARA') ~ 8, str_detect(PUEB, 'NATABUELA') ~ 9, str_detect(PUEB, 'OTAVALO') ~ 10, str_detect(PUEB, 'PALTAS') ~ 11, str_detect(PUEB, 'PANZALEO') ~ 12, str_detect(PUEB, 'PASTOS') ~ 13, str_detect(PUEB, 'PURUH.*') ~ 14, str_detect(PUEB, 'SALASA.*') ~ 15, str_detect(PUEB, 'SARAGURO') ~ 16, str_detect(PUEB, 'TOMABELA') ~ 17, str_detect(PUEB, 'WARAMKA|GUARANCA') ~ 18, str_detect(PUEB, 'SABE.*|RESPONDE') ~ 19, str_detect(PUEB, 'APLICA|^$') ~ 20, TRUE ~ 20)
    )
  
  # ====================================================================
  # 4. EL MOTOR DE CONSTRUCCIÓN (Data Reshaping)
  # ====================================================================
  construir_matriz_top10 <- function(df_subgrupo) {
    # Paso A: Aislamiento del Top 10
    ranking <- df_subgrupo %>%
      group_by(CIE) %>%
      summarise(DESC = first(DESC), TOTAL = n(), .groups = "drop") %>%
      arrange(desc(TOTAL)) %>%
      slice(1:10)
    
    # Matriz de protección (Evita datos fantasma)
    matriz_final <- as.data.frame(matrix(NA_real_, nrow = 10, ncol = 101))
    
    if(nrow(ranking) == 0) return(matriz_final)
    
    # Paso B: Aritmética Transversal
    for(r in 1:nrow(ranking)) {
      c_cie <- ranking$CIE[r]
      c_desc <- ranking$DESC[r]
      sub <- df_subgrupo %>% filter(CIE == c_cie)
      
      vE <- numeric(20); vN <- numeric(32); vP <- numeric(40)
      
      idx <- 1; for(i in 1:10) { vE[idx] <- sum(sub$iE == i & sub$esHombre, na.rm=T); vE[idx+1] <- sum(sub$iE == i & sub$esMujer, na.rm=T); idx <- idx + 2 }
      idx <- 1; for(i in 1:16) { vN[idx] <- sum(sub$iN == i & sub$esHombre, na.rm=T); vN[idx+1] <- sum(sub$iN == i & sub$esMujer, na.rm=T); idx <- idx + 2 }
      idx <- 1; for(i in 1:20) { vP[idx] <- sum(sub$iP == i & sub$esHombre, na.rm=T); vP[idx+1] <- sum(sub$iP == i & sub$esMujer, na.rm=T); idx <- idx + 2 }
      
      # Coerción y Sanitización de 0 a NA (Protege las fórmulas de Excel)
      vE[vE == 0] <- NA_real_; vN[vN == 0] <- NA_real_; vP[vP == 0] <- NA_real_
      tE <- sum(vE, na.rm=T); if(tE == 0) tE <- NA_real_
      tN <- sum(vN, na.rm=T); if(tN == 0) tN <- NA_real_
      tP <- sum(vP, na.rm=T); if(tP == 0) tP <- NA_real_
      
      # Inyección Type-Safe (Protege el texto y los números por separado)
      matriz_final[r, 1] <- as.numeric(r)          # Ranking
      matriz_final[r, 2] <- as.character(c_cie)    # CIE-10
      matriz_final[r, 3] <- as.character(c_desc)   # Enfermedad
      matriz_final[r, 4:23] <- vE                  # Etnia (H/M)
      matriz_final[r, 24] <- tE                    # Total Etnia
      matriz_final[r, 25] <- NA_real_              # % (Para fórmula de Sheets)
      matriz_final[r, 26:57] <- vN                 # Nac (H/M)
      matriz_final[r, 58] <- tN                    # Total Nac
      matriz_final[r, 59] <- NA_real_              # %
      matriz_final[r, 60:99] <- vP                 # Pueblo (H/M)
      matriz_final[r, 100] <- tP                   # Total Pueblo
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
  m_dis <- construir_matriz_top10(base_maestra %>% filter(str_detect(DISC, "SI")))
  
  # ====================================================================
  # 6. INYECCIÓN BLINDADA A GOOGLE SHEETS (NUBE NATIVA)
  # ====================================================================
  # ⚠️ Se usa range_write y se envuelven las matrices puras en DataFrames
  range_write(ss = id_google_sheet, data = as.data.frame(m_gen), range = paste0("TOP10_INTERCULTURAL!", letra_inicio, "4"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(m_men), range = paste0("TOP10_INTERCULTURAL!", letra_inicio, "17"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(m_emb), range = paste0("TOP10_INTERCULTURAL!", letra_inicio, "30"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(m_ado), range = paste0("TOP10_INTERCULTURAL!", letra_inicio, "43"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(m_may), range = paste0("TOP10_INTERCULTURAL!", letra_inicio, "56"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(m_dis), range = paste0("TOP10_INTERCULTURAL!", letra_inicio, "69"), col_names = FALSE, reformat = FALSE)
  
  cat(paste0("✅ Trabajador 14 Finalizado. Matriz intercultural inyectada desde la columna ", letra_inicio, ".\n"))

  # ==================================================================================
  # 🤖 SUB-RUTINA AUTÓNOMA: DETECCIÓN Y DISPARO TRIMESTRAL (VERSIÓN SENIOR)
  # ==================================================================================
  mes_num <- as.numeric(mes_objetivo) 
  meses_trimestre <- c(3, 6, 9, 12)
  
  if (!is.na(mes_num) && mes_num %in% meses_trimestre && !is.null(cubeta_completa)) {
    cat(paste0("\n⏱️ TRABAJADOR 14: Mes ", mes_num, " detectado como cierre de trimestre...\n"))
    
    # 🛡️ ESCUDO 1: Auditoría de Estructura de Datos
    if (is.data.frame(cubeta_completa)) {
      cat("🔴 ALERTA DE ARQUITECTURA: 'cubeta_completa' recibió una Tabla en lugar de una Lista Histórica.\n")
      cat("   Corrija su script principal.R para que acumule los meses en una list().\n")
      
    } else if (is.list(cubeta_completa)) {
      
      # 🛡️ ESCUDO 2: Validación de memoria histórica
      if (length(cubeta_completa) >= mes_num) {
        mes_inicio <- mes_num - 2
        
        cat(paste0("📦 FUSIONANDO pacientes históricos (Meses ", mes_inicio, " al ", mes_num, ")...\n"))
        
        # 🛡️ FUSIÓN HISTÓRICA AISLADA 
        lista_temporal <- lapply(cubeta_completa[mes_inicio:mes_num], function(df) {
          df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
        })
        
        base_trimestral <- dplyr::bind_rows(lista_temporal)
        
        # 🛡️ ESCUDO 3: Validación anti-vacíos (Asegura que la fusión fue exitosa)
        if(nrow(base_trimestral) > 0 && "ATEMED_ID" %in% names(base_trimestral)) {
          codigo_trimestre <- 13 + (mes_num / 3) - 1
          
          cat(paste0("🚀 DISPARANDO matriz pura a la bóveda del Trimestre ", (mes_num/3), "...\n"))
          
          # Auto-ejecución recursiva segura
          trabajador14_Top10Intercultural(tabla_consolidada = base_trimestral, 
                                          id_google_sheet = id_google_sheet, 
                                          mes_objetivo = codigo_trimestre, 
                                          cubeta_completa = NULL)
          
          cat(paste0("✅ TRABAJADOR 14: Trimestre ", (mes_num/3), " consolidado y blindado.\n"))
        } else {
          cat("⚠️ TRABAJADOR 14: La fusión histórica resultó vacía. Se aborta la inyección para proteger Google Sheets.\n")
        }
      } else {
        cat(paste0("⚠️ TRABAJADOR 14: Memoria histórica insuficiente (", length(cubeta_completa), " detectados). Faltan meses para el trimestre.\n"))
      }
    }
  }
}
