# ====================================================================================
# TRABAJADOR 11: MOTOR DE ESTADO NUTRICIONAL E INTERCULTURALIDAD EXPANDIDA
# Arquitectura: Matriz Vertical de 4 Filas (Aguda, Crónica, Sobrepeso, Anemia)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador11_EstadoNutricional <- function(tabla_consolidada, id_google_sheet, mes_objetivo, cubeta_completa = NULL) {
  
  cat(paste0("🍼 Trabajador 11 activado: Calculando Inteligencia Nutricional para el mes ", mes_objetivo, "...\n"))
  
  # Si es un mes de Trimestre (13, 14, 15, 16), el Trabajador 11 se apaga, porque él opera mes a mes.
  if(mes_objetivo > 12) {
    cat("⏭️ Trabajador 11: Modo Trimestral detectado. Omitiendo proceso para proteger los históricos.\n")
    return(invisible(NULL))
  }
  
  # ============================================================================
  # 1. RADARES TOPOGRÁFICOS MAESTROS (Columnas)
  # ============================================================================
  col_destino_num <- mes_objetivo + 1
  letra_col_conteo <- LETTERS[col_destino_num] # Columna B a M (Para los totales superiores)
  
  # ============================================================================
  # 2. RADARES DE CABECERAS Y PURIFICADORES ANTI-NA
  # ============================================================================
  nombres_cols <- names(tabla_consolidada)
  
  # Definición estricta de columnas para evitar fugas
  col_edad_meses <- if(any(str_detect(nombres_cols, "PCTE_ANIOS_EN_MESES"))) nombres_cols[str_detect(nombres_cols, "PCTE_ANIOS_EN_MESES")][1] else { tabla_consolidada$PCTE_ANIOS_EN_MESES <- NA; "PCTE_ANIOS_EN_MESES" }
  col_grp_pri <- if(any(str_detect(nombres_cols, "PCTE_GRP_PRI"))) nombres_cols[str_detect(nombres_cols, "PCTE_GRP_PRI")][1] else { tabla_consolidada$PCTE_GRP_PRI <- ""; "PCTE_GRP_PRI" }
  col_hemo <- if(any(str_detect(nombres_cols, "PCTE_IND_ANEMIA_HEMOGLOBINA"))) nombres_cols[str_detect(nombres_cols, "PCTE_IND_ANEMIA_HEMOGLOBINA")][1] else { tabla_consolidada$PCTE_IND_ANEMIA_HEMOGLOBINA <- ""; "PCTE_IND_ANEMIA_HEMOGLOBINA" }
  col_imc <- if(any(str_detect(nombres_cols, "PCTE_ULT_IMC$"))) nombres_cols[str_detect(nombres_cols, "PCTE_ULT_IMC$")][1] else { tabla_consolidada$PCTE_ULT_IMC <- NA; "PCTE_ULT_IMC" }
  col_tip_diag <- if(any(str_detect(nombres_cols, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG"))) nombres_cols[str_detect(nombres_cols, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG")][1] else { tabla_consolidada$ATEMED_TIP_DIAG <- ""; "ATEMED_TIP_DIAG" }
  
  # Radares Interculturales
  col_etn <- if(any(str_detect(nombres_cols, "PCTE_AUT"))) nombres_cols[str_detect(nombres_cols, "PCTE_AUT")][1] else if(any(str_detect(nombres_cols, "ETNICA|AUTOIDENTIFICACION"))) nombres_cols[str_detect(nombres_cols, "ETNICA|AUTOIDENTIFICACION")][1] else { tabla_consolidada$PCTE_AUTID_ETN <- ""; "PCTE_AUTID_ETN" }
  col_nac <- if(any(str_detect(nombres_cols, "PCTE_NAC_ETN"))) "PCTE_NAC_ETN" else if(any(str_detect(nombres_cols, "NACIONALIDAD"))) nombres_cols[str_detect(nombres_cols, "NACIONALIDAD")][1] else { tabla_consolidada$PCTE_NAC_ETN <- ""; "PCTE_NAC_ETN" }
  col_pue <- if(any(str_detect(nombres_cols, "PCTE_PUEBLO"))) "PCTE_PUEBLO" else if(any(str_detect(nombres_cols, "PUEBLO"))) nombres_cols[str_detect(nombres_cols, "PUEBLO")][1] else { tabla_consolidada$PCTE_PUEBLO <- ""; "PCTE_PUEBLO" }
  
  # ============================================================================
  # 3. BASE MAESTRA Y DICCIONARIOS DE ALTA EFICIENCIA
  # ============================================================================
  base_maestra <- tabla_consolidada %>%
    mutate(
      EDAD_MESES = as.numeric(ifelse(is.na(!!sym(col_edad_meses)), -1, !!sym(col_edad_meses))),
      IMC = as.numeric(str_replace_all(ifelse(is.na(!!sym(col_imc)), -1, !!sym(col_imc)), ",", ".")),
      CIE = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10)), "[\r\n\"\\.]")),
      GRP = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_grp_pri)), "", !!sym(col_grp_pri))), "[\r\n\"]")),
      HEMO = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_hemo)), "", !!sym(col_hemo))), "[\r\n\"]")),
      TIP = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_tip_diag)), "", !!sym(col_tip_diag))), "[\r\n\"]")),
      CRON = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG)), "[\r\n\"]")),
      ETNIA = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_etn)), "NO APLICA", !!sym(col_etn))), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_nac)), "NO APLICA", !!sym(col_nac))), "[\r\n\"]")),
      PUEB = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_pue)), "NO APLICA", !!sym(col_pue))), "[\r\n\"]"))
    ) %>%
    mutate(
      # 🚀 DICCIONARIOS INTERCULTURALES (Blindaje Ortográfico Total .*)
      iE = case_when(str_detect(ETNIA, 'AFRO.*') ~ 1, str_detect(ETNIA, 'BLANC.*') ~ 2, str_detect(ETNIA, 'IND.*GENA') ~ 3, str_detect(ETNIA, 'MESTIZ.*') ~ 4, str_detect(ETNIA, 'MONTUB.*|MONTUV.*') ~ 5, str_detect(ETNIA, 'MULAT.*') ~ 6, str_detect(ETNIA, 'NEGR.*') ~ 7, str_detect(ETNIA, 'OTR.*') ~ 8, str_detect(ETNIA, 'SABE.*|RESPONDE') ~ 9, str_detect(ETNIA, 'APLICA|^$') ~ 10, TRUE ~ 10),
      iN = case_when(str_detect(NAC, 'ACHUAR') ~ 1, str_detect(NAC, 'ANDOA') ~ 2, str_detect(NAC, 'AWA|AWÁ') ~ 3, str_detect(NAC, 'CHACHI') ~ 4, str_detect(NAC, 'COF.*N|KOF.*N') ~ 5, str_detect(NAC, 'EPERA|.*PERA') ~ 6, str_detect(NAC, 'HUANCAVILCA') ~ 7, str_detect(NAC, 'KICH.*|QUICH.*') | str_detect(PUEB, 'CHIBULEO|SALASA|TOMABELA|KISAPINCHA|QUISAPINCHA|NATABUELA|OTAVALO|PURUH|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|PANZALEO|PASTOS|WARAMKA|GUARANCA|KA.*ARI|CA.*ARI') ~ 8, str_detect(NAC, 'SECOYA') ~ 9, str_detect(NAC, 'SHIWIAR') ~ 10, str_detect(NAC, 'SHUAR') ~ 11, str_detect(NAC, 'TS.*CHILA') ~ 12, str_detect(NAC, 'WAORANI|HUAORANI') ~ 13, str_detect(NAC, 'Z.*PARA') ~ 14, str_detect(NAC, 'SABE.*|RESPONDE') ~ 15, str_detect(NAC, 'APLICA|^$') ~ 16, TRUE ~ 16),
      iP = case_when(str_detect(PUEB, 'CHIBULEO') ~ 1, str_detect(PUEB, 'HUANCAVILCA') ~ 2, str_detect(PUEB, 'KA.*ARI|CA.*ARI') ~ 3, str_detect(PUEB, 'KARANKI|CARANQUI') ~ 4, str_detect(PUEB, 'KAYAMBI|CAYAMBI') ~ 5, str_detect(PUEB, 'KICHWA AMAZ.*') ~ 6, str_detect(PUEB, 'KISAPINCHA|QUISAPINCHA') ~ 7, str_detect(PUEB, 'KITUKARA|QUITUCARA') ~ 8, str_detect(PUEB, 'NATABUELA') ~ 9, str_detect(PUEB, 'OTAVALO') ~ 10, str_detect(PUEB, 'PALTAS') ~ 11, str_detect(PUEB, 'PANZALEO') ~ 12, str_detect(PUEB, 'PASTOS') ~ 13, str_detect(PUEB, 'PURUH.*') ~ 14, str_detect(PUEB, 'SALASA.*') ~ 15, str_detect(PUEB, 'SARAGURO') ~ 16, str_detect(PUEB, 'TOMABELA') ~ 17, str_detect(PUEB, 'WARAMKA|GUARANCA') ~ 18, str_detect(PUEB, 'SABE.*|RESPONDE') ~ 19, str_detect(PUEB, 'APLICA|^$') ~ 20, TRUE ~ 20),
      
      # Reglas Clínicas Nutricionales (Se reintegra la Anemia para Niños)
      esPrimeraGeneral = str_detect(CRON, "PRIMER|^P$|^1$|^N$"),
      esAguda = (CIE == "E441"),
      esCronica = (CIE %in% c("E43X", "E43")),
      esSobrepeso = (CIE %in% c("E660", "E669")),
      esAnemiaNino = (CIE %in% c("D46X", "D46"))
    )
  
  # ============================================================================
  # 4. EXTRACCIÓN Y SUMATORIA DE POBLACIONES
  # ============================================================================
  # Filtramos a los niños (INTOCABLE - Lógica de Pediatría preservada)
  b_24 <- base_maestra %>% filter(EDAD_MESES >= 0 & EDAD_MESES < 24)
  b_59 <- base_maestra %>% filter(EDAD_MESES >= 0 & EDAD_MESES < 60)
  
  # 🏆 FILTRO DE EMBARAZADAS (Preservando el motor original y aplicando nuevos umbrales)
  b_emb <- base_maestra %>% filter(str_detect(GRP, "EMBARAZADAS")) %>%
    mutate(
      # 1. Tu motor original de Primeras Consultas (Intocable)
      esPrevEmbPrimera = (str_detect(TIP, "PREV") & str_detect(CIE, "^Z321|^Z34|^Z35") & esPrimeraGeneral),
      
      # 2. Motor Matemático de IMC (Nuevos Umbrales Clínicos amarrados a Primera Consulta)
      esBajoPesoEmb = (IMC > 0 & IMC < 18 & esPrevEmbPrimera == TRUE),
      esSobrepesoEmb = (IMC >= 25 & IMC <= 29.9 & esPrevEmbPrimera == TRUE),
      esObesidadEmb = (IMC >= 30 & esPrevEmbPrimera == TRUE),
      
      # 3. Motor de Anemia
      esAnemiaHemoEmb = str_detect(HEMO, "ANEMIA LEVE|ANEMIA MODERADA")
    )
  
  # Totales Superiores (Para el cuadro inicial de la hoja)
  t24_pri <- sum(b_24$esPrimeraGeneral, na.rm=T)
  t24_cro <- sum(b_24$esCronica, na.rm=T)
  t24_agu <- sum(b_24$esAguda, na.rm=T)
  t24_sob <- sum(b_24$esSobrepeso, na.rm=T)
  t24_ane <- sum(b_24$esAnemiaNino, na.rm=T) # 🚀 AÑADIDO: Anemia < 24m
  
  t59_pri <- sum(b_59$esPrimeraGeneral, na.rm=T)
  t59_cro <- sum(b_59$esCronica, na.rm=T)
  t59_agu <- sum(b_59$esAguda, na.rm=T)
  t59_sob <- sum(b_59$esSobrepeso, na.rm=T)
  
  tem_pri <- sum(b_emb$esPrevEmbPrimera, na.rm=T)
  tem_obe <- sum(b_emb$esObesidadEmb, na.rm=T)
  tem_baj <- sum(b_emb$esBajoPesoEmb, na.rm=T) # 🚀 AÑADIDO: Bajo Peso Emb
  tem_sob <- sum(b_emb$esSobrepesoEmb, na.rm=T) # 🚀 AÑADIDO: Sobrepeso Emb
  
  # ============================================================================
  # 5. INYECCIÓN DEL CUADRO RESUMEN SUPERIOR (Nube Nativa)
  # ============================================================================
  fc <- function(x) { if (x > 0) return(x) else return(NA_real_) }
  
  # 🎯 Cuadro Niños < 24 meses
  range_write(ss = id_google_sheet, data = data.frame(fc(t24_pri)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "3"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t24_cro)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "4"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t24_agu)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "5"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t24_sob)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "6"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t24_ane)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "7"), col_names = FALSE, reformat = FALSE)
  
  # 🎯 Cuadro Niños < 59 meses
  range_write(ss = id_google_sheet, data = data.frame(fc(t59_pri)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "12"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t59_cro)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "13"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t59_agu)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "14"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t59_sob)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "15"), col_names = FALSE, reformat = FALSE)
  
  # 🎯 Cuadro Embarazadas
  range_write(ss = id_google_sheet, data = data.frame(fc(tem_pri)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "23"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(tem_obe)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "25"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(tem_baj)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "26"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(tem_sob)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "27"), col_names = FALSE, reformat = FALSE)
  
  # ============================================================================
  # 6. MOTOR DE MATRIZ VERTICAL INTERCULTURAL (Bloques de 4 Filas)
  # ============================================================================
  # Ecuaciones Topográficas Maestras
  salto_filas <- (mes_objetivo - 1) * 8
  
  f_inicio_24 <- 35 + salto_filas
  f_inicio_59 <- 132 + salto_filas
  f_inicio_emb <- 229 + salto_filas
  
  # 🛡️ FUNCIÓN TOPOGRÁFICA REDISEÑADA (Añade "huecos" para no pisar las columnas de Totales)
  construir_fila_plana <- function(base_filtro, indicador_logico) {
    vE <- numeric(10); vN <- numeric(16); vP <- numeric(20)
    
    sub <- base_filtro %>% filter(!!sym(indicador_logico) == TRUE)
    
    for(i in 1:10) vE[i] <- sum(sub$iE == i, na.rm=T)
    for(i in 1:16) vN[i] <- sum(sub$iN == i, na.rm=T)
    for(i in 1:20) vP[i] <- sum(sub$iP == i, na.rm=T)
    
    # 🚀 LA TUERCA CLAVE: 
    # vE (10 columnas) + 2 NA (Columnas L y M) + vN (16 columnas) + 2 NA (Columnas AD y AE) + vP (20 columnas).
    # Total de la matriz: 50 Columnas exactas.
    fila_completa <- c(vE, NA_real_, NA_real_, vN, NA_real_, NA_real_, vP)
    fila_completa[fila_completa == 0] <- NA_real_
    
    return(fila_completa)
  }
  
  # 🏆 MATRIZ < 24 MESES (4 Filas exactas)
  matriz_24 <- as.data.frame(matrix(NA_real_, nrow = 4, ncol = 50))
  matriz_24[1, ] <- construir_fila_plana(b_24, "esAguda")
  matriz_24[2, ] <- construir_fila_plana(b_24, "esCronica")
  matriz_24[3, ] <- construir_fila_plana(b_24, "esSobrepeso")
  matriz_24[4, ] <- construir_fila_plana(b_24, "esAnemiaNino")
  
  # 🏆 MATRIZ < 59 MESES (3 Filas exactas)
  matriz_59 <- as.data.frame(matrix(NA_real_, nrow = 3, ncol = 50))
  matriz_59[1, ] <- construir_fila_plana(b_59, "esAguda")
  matriz_59[2, ] <- construir_fila_plana(b_59, "esCronica")
  matriz_59[3, ] <- construir_fila_plana(b_59, "esSobrepeso")
  
  # 🏆 MATRIZ EMBARAZADAS (3 Filas exactas: Obesidad, Bajo Peso, Sobrepeso)
  matriz_emb <- as.data.frame(matrix(NA_real_, nrow = 3, ncol = 50))
  matriz_emb[1, ] <- construir_fila_plana(b_emb, "esObesidadEmb")
  matriz_emb[2, ] <- construir_fila_plana(b_emb, "esBajoPesoEmb")
  matriz_emb[3, ] <- construir_fila_plana(b_emb, "esSobrepesoEmb")
  
  # Purificador de coerción numérica para evitar "Fantasmas NA" textuales en la API
  matriz_24 <- matriz_24 %>% mutate(across(everything(), as.numeric))
  matriz_59 <- matriz_59 %>% mutate(across(everything(), as.numeric))
  matriz_emb <- matriz_emb %>% mutate(across(everything(), as.numeric))
  
  # ============================================================================
  # 7. INYECCIÓN BLINDADA (Una sola llamada a la API por matriz de 4x50)
  # ============================================================================
  
  # ⚠️ AVISO ARQUITECTURA: Se utiliza range_write estándar para plena compatibilidad 
  range_write(ss = id_google_sheet, data = matriz_24, range = paste0("ESTADO_NUTRI!B", f_inicio_24), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = matriz_59, range = paste0("ESTADO_NUTRI!B", f_inicio_59), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = matriz_emb, range = paste0("ESTADO_NUTRI!B", f_inicio_emb), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 11 finalizado. Arquitectura topográfica desplegada con éxito en la nube.\n")
}
