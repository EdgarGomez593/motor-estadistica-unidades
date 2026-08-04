# ====================================================================================
# TRABAJADOR 12: INTELIGENCIA OPERATIVA - VIOLENCIA FAMILIAR (MATRIZ ESPEJO)
# Arquitectura: Visibilidad Total (T74 Global) -> Clasificación Intercultural -> Inyección Doble
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador12_Violencia <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🛡️ Trabajador 12 activado: Procesando Inteligencia Operativa - Violencia y Etnias...\n")
  
  # ====================================================================
  # 1. ARQUITECTURA DE COORDENADAS (Triple Columna Dinámica)
  # ====================================================================
  # Enero(1) = B(2), C(3), D(4) | Febrero(2) = E(5), F(6), G(7)
  col_h_num <- (mes_objetivo - 1) * 3 + 2
  col_m_num <- col_h_num + 1
  col_l_num <- col_h_num + 2
  
  letras <- c(LETTERS, paste0("A", LETTERS)) # Soporte hasta la columna ZZ
  letra_h <- letras[col_h_num]
  letra_m <- letras[col_m_num]
  letra_l <- letras[col_l_num]
  
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
  # 3. PURGA Y MATRIZ ESPEJO (1 Diagnóstico = 1 Demografía)
  # ====================================================================
  base_filtrada <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    
    # ====================================================================================
    # ⚠️ TÍTULO DE CONTROL: APAGADO TEMPORAL DE GUILLOTINA (VISIBILIDAD TOTAL DE DIAGNÓSTICOS)
    # Nota de Arquitectura: Actualmente el hashtag '#' está activo. Esto permite que si un 
    # paciente tiene T741 y T742 el mismo día, AMBOS diagnósticos se cuenten arriba y abajo.
    # Para cambiar a "1 Paciente = 1 Evento", simplemente borre el '#' de la línea inferior.
    # ====================================================================================
    
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
    # Filtro Estricto: Toda la Violencia (T74), Morbilidad y Primera Atención
    filter(str_starts(CIE, "T74") & str_detect(TIP, "MORB") & str_detect(CRON, "PRIMER|^P$|^1$|^N$")) %>%
    # Lógica de Género con Prioridad LGBTI
    mutate(
      esLGBTI = str_detect(ORI, "GAY|LESBIANA|BISEXUAL|TRANS"),
      esHombre = !esLGBTI & str_detect(SEXO, "HOMBRE|^M$"),
      esMujer = !esLGBTI & str_detect(SEXO, "MUJER|^F$")
    )
  
  # ====================================================================
  # 4. EXTRACCIÓN DE DIAGNÓSTICOS (Para las Filas 3 al 8)
  # ====================================================================
  resumen_cie <- base_filtrada %>%
    group_by(CIE) %>%
    summarise(
      h = sum(esHombre, na.rm = TRUE),
      m = sum(esMujer, na.rm = TRUE),
      l = sum(esLGBTI, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ====================================================================
  # 5. MAPEO DE CLASIFICADORES INTERCULTURALES (Regex Provincial Curso Selva)
  # ====================================================================
  base_clasificada <- base_filtrada %>%
    mutate(
      iE = case_when(str_detect(ETNIA, 'AFRO.*') ~ 1, str_detect(ETNIA, 'BLANC.*') ~ 2, str_detect(ETNIA, 'IND.*GENA') ~ 3, str_detect(ETNIA, 'MESTIZ.*') ~ 4, str_detect(ETNIA, 'MONTUBI.*|MONTUVI.*') ~ 5, str_detect(ETNIA, 'MULAT.*') ~ 6, str_detect(ETNIA, 'NEGR.*') ~ 7, str_detect(ETNIA, 'OTR.*') ~ 8, str_detect(ETNIA, 'SABE.*|RESPONDE') ~ 9, str_detect(ETNIA, 'APLICA') | ETNIA == "" ~ 10, TRUE ~ 10),
      iN = case_when(str_detect(NAC, 'ACHUAR') ~ 1, str_detect(NAC, 'ANDOA') ~ 2, str_detect(NAC, 'AWA') ~ 3, str_detect(NAC, 'CHACHI') ~ 4, str_detect(NAC, 'COFAN') ~ 5, str_detect(NAC, 'EPERA') ~ 6, str_detect(NAC, 'HUANCAVILCA') ~ 7, str_detect(NAC, 'KICHWA') ~ 8, str_detect(NAC, 'SECOYA') ~ 9, str_detect(NAC, 'SHIWIAR') ~ 10, str_detect(NAC, 'SHUAR') ~ 11, str_detect(NAC, 'TS.*CHILA') ~ 12, str_detect(NAC, 'WAORANI|HUAORANI') ~ 13, str_detect(NAC, 'ZAPARA') ~ 14, str_detect(NAC, 'SABE.*|RESPONDE') ~ 15, str_detect(NAC, 'APLICA') | NAC == "" ~ 16, TRUE ~ 16),
      iP = case_when(str_detect(PUEB, 'CHIBULEO') ~ 1, str_detect(PUEB, 'HUANCAVILCA') ~ 2, str_detect(PUEB, 'KA.*ARI|CA.*ARI') ~ 3, str_detect(PUEB, 'KARANKI|CARANQUI') ~ 4, str_detect(PUEB, 'KAYAMBI|CAYAMBI') ~ 5, str_detect(PUEB, 'KICHWA AMAZ.*') ~ 6, str_detect(PUEB, 'KISAPINCHA|QUISAPINCHA') ~ 7, str_detect(PUEB, 'KITUKARA|QUITUCARA') ~ 8, str_detect(PUEB, 'NATABUELA') ~ 9, str_detect(PUEB, 'OTAVALO') ~ 10, str_detect(PUEB, 'PALTAS') ~ 11, str_detect(PUEB, 'PANZALEO') ~ 12, str_detect(PUEB, 'PASTOS') ~ 13, str_detect(PUEB, 'PURUH.*') ~ 14, str_detect(PUEB, 'SALASA.*') ~ 15, str_detect(PUEB, 'SARAGURO') ~ 16, str_detect(PUEB, 'TOMABELA') ~ 17, str_detect(PUEB, 'WARAMKA|GUARANCA') ~ 18, str_detect(PUEB, 'SABE.*|RESPONDE') ~ 19, str_detect(PUEB, 'APLICA') | PUEB == "" ~ 20, TRUE ~ 20)
    )
  
  # ====================================================================
  # 6. MATRIZ DE VECTORES EN BLOQUE (Aritmética Booleana)
  # ====================================================================
  hE <- numeric(10); mE <- numeric(10); lE <- numeric(10)
  hN <- numeric(16); mN <- numeric(16); lN <- numeric(16)
  hP <- numeric(20); mP <- numeric(20); lP <- numeric(20)
  
  for(i in 1:10) { hE[i] <- sum(base_clasificada$iE == i & base_clasificada$esHombre); mE[i] <- sum(base_clasificada$iE == i & base_clasificada$esMujer); lE[i] <- sum(base_clasificada$iE == i & base_clasificada$esLGBTI) }
  for(i in 1:16) { hN[i] <- sum(base_clasificada$iN == i & base_clasificada$esHombre); mN[i] <- sum(base_clasificada$iN == i & base_clasificada$esMujer); lN[i] <- sum(base_clasificada$iN == i & base_clasificada$esLGBTI) }
  for(i in 1:20) { hP[i] <- sum(base_clasificada$iP == i & base_clasificada$esHombre); mP[i] <- sum(base_clasificada$iP == i & base_clasificada$esMujer); lP[i] <- sum(base_clasificada$iP == i & base_clasificada$esLGBTI) }
  
  # Sanitización a NA_real_ (Protección de tableros)
  fc <- function(x) { if (is.na(x) || x == 0) return(NA_real_) else return(x) }
  formatear_bloque <- function(v) { v <- as.numeric(v); v[v == 0 | is.na(v)] <- NA_real_; return(v) }
  
  hE <- formatear_bloque(hE); mE <- formatear_bloque(mE); lE <- formatear_bloque(lE)
  hN <- formatear_bloque(hN); mN <- formatear_bloque(mN); lN <- formatear_bloque(lN)
  hP <- formatear_bloque(hP); mP <- formatear_bloque(mP); lP <- formatear_bloque(lP)
  
  # ====================================================================
  # 7. INYECCIÓN BLINDADA POR ZONAS (Nube Nativa)
  # ====================================================================
  
  # ZONA SUPERIOR: Diagnósticos (Filas 3 a 8)
  mapa_filas <- c("T740" = 3, "T741" = 4, "T742" = 5, "T743" = 6, "T748" = 7, "T749" = 8)
  
  if (nrow(resumen_cie) > 0) {
    for (i in 1:nrow(resumen_cie)) {
      row <- resumen_cie[i, ]
      fila <- mapa_filas[[row$CIE]]
      
      if (!is.null(fila)) {
        range_write(ss = id_google_sheet, data = data.frame(fc(row$h)), range = paste0("VIOLENCIA!", letra_h, fila), col_names = FALSE, reformat = FALSE)
        range_write(ss = id_google_sheet, data = data.frame(fc(row$m)), range = paste0("VIOLENCIA!", letra_m, fila), col_names = FALSE, reformat = FALSE)
        range_write(ss = id_google_sheet, data = data.frame(fc(row$l)), range = paste0("VIOLENCIA!", letra_l, fila), col_names = FALSE, reformat = FALSE)
      }
    }
  }
  
  # ZONA INFERIOR: Demografía Intercultural (Filas 13, 28 y 50)
  range_write(ss = id_google_sheet, data = data.frame(hE), range = paste0("VIOLENCIA!", letra_h, "13"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(mE), range = paste0("VIOLENCIA!", letra_m, "13"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(lE), range = paste0("VIOLENCIA!", letra_l, "13"), col_names = FALSE, reformat = FALSE)
  
  range_write(ss = id_google_sheet, data = data.frame(hN), range = paste0("VIOLENCIA!", letra_h, "28"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(mN), range = paste0("VIOLENCIA!", letra_m, "28"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(lN), range = paste0("VIOLENCIA!", letra_l, "28"), col_names = FALSE, reformat = FALSE)
  
  range_write(ss = id_google_sheet, data = data.frame(hP), range = paste0("VIOLENCIA!", letra_h, "50"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(mP), range = paste0("VIOLENCIA!", letra_m, "50"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(lP), range = paste0("VIOLENCIA!", letra_l, "50"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 12 finalizado. Matriz Espejo de violencia registrada en la nube.\n")
}
