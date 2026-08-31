# ====================================================================================
# TRABAJADOR 9: MOTOR EPIDEMIOLÓGICO TOP 50 (PARADIGMA ELT)
# Arquitectura: Zero-Ghost Data -> Respeto Autoidentidad -> Cascada Exclusiva Indígena
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador9_PerfilEpidemiologico <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🦠 Trabajador 9 activado: Procesando Perfil Epidemiológico Top 50 Exclusivo Indígena...\n")
  if(mes_objetivo > 12) return(invisible(NULL))
  
  mes_actual_num <- mes_objetivo
  
  # ====================================================================
  # 1. RADAR DE CABECERAS Y ESCUDOS ANTI-NA (Intocable)
  # ====================================================================
  nombres_cols <- names(tabla_consolidada)
  if (!"ATEMED_DES_CIE10" %in% nombres_cols) tabla_consolidada$ATEMED_DES_CIE10 <- ""
  if (!"PCTE_SEXO" %in% nombres_cols) tabla_consolidada$PCTE_SEXO <- ""
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  col_tipo_diag <- "ATEMED_TIP_DIAG"
  if (!("ATEMED_TIP_DIAG" %in% nombres_cols) && ("ATEMED_TIPO_DIAG" %in% nombres_cols)) {
    col_tipo_diag <- "ATEMED_TIPO_DIAG"
  } else if (!"ATEMED_TIP_DIAG" %in% nombres_cols && !"ATEMED_TIPO_DIAG" %in% nombres_cols) {
    tabla_consolidada$ATEMED_TIP_DIAG <- ""
  }
  
  # ====================================================================
  # 2. LIMPIEZA Y FILTRADO ESTRICTO (El Escudo Clínico)
  # ====================================================================
  base_limpia <- tabla_consolidada %>%
    mutate(
      ESP = str_squish(str_remove_all(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE)), "[\r\n\"]")),
      TIP = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_tipo_diag)), "", !!sym(col_tipo_diag))), "[\r\n\"]")),
      CR  = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG)), "[\r\n\"]")),
      CON = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CON_DIAG), "", ATEMED_CON_DIAG)), "[\r\n\"]")),
      CIE = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10)), "[\r\n\"\\.]")),
      DES = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_DES_CIE10), "", ATEMED_DES_CIE10)), "[\r\n\"]")),
      SEX = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_SEXO), "", PCTE_SEXO)), "[\r\n\"]")),
      ETN = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_AUTID_ETN), "", PCTE_AUTID_ETN)), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_NAC_ETN), "", PCTE_NAC_ETN)), "[\r\n\"]")),
      PUE = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_PUEBLO), "", PCTE_PUEBLO)), "[\r\n\"]"))
    ) %>%
    # Filtros Clínicos Intocables
    filter(
      str_detect(TIP, "MORBILIDAD") & 
        str_detect(CR, "PRIMER|^P$|^1$|^N$") & 
        str_detect(CON, "DEFINITIVO INICIAL")
    ) %>%
    mutate(
      esMed = str_detect(ESP, "MEDICIN|M.*DIC|OBSTETR|PSICOLOG"),
      esOdo = str_detect(ESP, "ODONTO"),
      esMujer = str_detect(SEX, "MUJER|^F$"),
      esHombre = !str_detect(SEX, "MUJER|^F$") & SEX != "" 
    ) %>%
    # Aislamos a la población objetivo
    filter((esMed | esOdo) & (esMujer | esHombre)) %>%
    
    # ====================================================================
    # 3. LÓGICA DE CASCADA EXCLUSIVA INDÍGENA Y AUTOIDENTIDAD (REGLAS 1 A 5)
    # ====================================================================
    mutate(
      # Detectores Antropológicos
      es_pueblo_huancavilca = str_detect(PUE, "HUANCAVILCA"),
      tiene_pueblo = str_detect(PUE, "CHIBULEO|HUANCAVILCA|KA.*ARI|CA.*ARI|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|KICHWA AMAZ.*|KISAPINCHA|QUISAPINCHA|KITUKARA|QUITUCARA|NATABUELA|OTAVALO|PALTAS|PANZALEO|PASTOS|PURUH.*|PURUHÁ|PURUHA|SALASA.*|SARAGURO|TOMABELA|WARAMKA|GUARANCA"),
      
      # 🚀 REGLA 1: Respeto Absoluto a la Autoidentificación (Cero Rescate Étnico)
      ETNIA_RESC = ETN,
      NAC_RESC = NAC,
      
      # 🚀 REGLA 4: Etnia (1 a 10 espacios mapeados de 1 a 10 en R)
      iE = case_when(
        str_detect(ETNIA_RESC, 'AFRO.*') ~ 1, 
        str_detect(ETNIA_RESC, 'BLANC.*') ~ 2,
        str_detect(ETNIA_RESC, 'IND.*GENA|INDÍGENA') ~ 3, 
        str_detect(ETNIA_RESC, 'MESTIZ.*') ~ 4,
        str_detect(ETNIA_RESC, 'MONTUBI.*|MONTUVI.*') ~ 5, 
        str_detect(ETNIA_RESC, 'MULAT.*') ~ 6,
        str_detect(ETNIA_RESC, 'NEGR.*') ~ 7, 
        str_detect(ETNIA_RESC, 'OTR.*') ~ 8,
        str_detect(ETNIA_RESC, 'APLICA') ~ 10,
        str_detect(ETNIA_RESC, 'SABE.*|RESPONDE') | ETNIA_RESC == "" | is.na(ETNIA_RESC) ~ 9, 
        TRUE ~ 9
      ),
      
      # 🚀 REGLA 2 y 4: Cascada Nacionalidad (Solo Indígenas, Amputación No Indígenas)
      iN = case_when(
        iE == 3 ~ case_when(
          str_detect(NAC_RESC, 'ACHUAR') ~ 1, str_detect(NAC_RESC, 'ANDOA') ~ 2, str_detect(NAC_RESC, 'AWA') ~ 3,
          str_detect(NAC_RESC, 'CHACHI') ~ 4, str_detect(NAC_RESC, 'COF.*N|KOF.*N') ~ 5, str_detect(NAC_RESC, 'EPERA|.*PERA') ~ 6,
          str_detect(NAC_RESC, 'HUANCAVILCA') ~ 7, str_detect(NAC_RESC, 'KICHWA|QUICHUA') ~ 8, str_detect(NAC_RESC, 'SECOYA') ~ 9,
          str_detect(NAC_RESC, 'SHIWIAR') ~ 10, str_detect(NAC_RESC, 'SHUAR') ~ 11, str_detect(NAC_RESC, 'TS.*CHILA') ~ 12,
          str_detect(NAC_RESC, 'WAORANI|HUAORANI') ~ 13, str_detect(NAC_RESC, 'ZAPARA') ~ 14, 
          TRUE ~ 15 # Vacíos y "No Sabe/Responde" exclusivos de indígenas
        ), 
        TRUE ~ -1 # Amputación total
      ),
      
      # 🚀 REGLA 2, 3 y 5: Cascada Pueblo (Solo Indígenas, Independiente)
      iP = case_when(
        iE == 3 ~ case_when(
          tiene_pueblo & !es_pueblo_huancavilca ~ case_when(
            str_detect(PUE, 'CHIBULEO') ~ 1, str_detect(PUE, 'KA.*ARI|CA.*ARI') ~ 3, str_detect(PUE, 'KARANKI|CARANQUI') ~ 4,
            str_detect(PUE, 'KAYAMBI|CAYAMBI') ~ 5, str_detect(PUE, 'KICHWA AMAZ.*') ~ 6, str_detect(PUE, 'KISAPINCHA|QUISAPINCHA') ~ 7,
            str_detect(PUE, 'KITUKARA|QUITUCARA') ~ 8, str_detect(PUE, 'NATABUELA') ~ 9, str_detect(PUE, 'OTAVALO') ~ 10,
            str_detect(PUE, 'PALTAS') ~ 11, str_detect(PUE, 'PANZALEO') ~ 12, str_detect(PUE, 'PASTOS') ~ 13,
            str_detect(PUE, 'PURUH.*') ~ 14, str_detect(PUE, 'SALASA.*') ~ 15, str_detect(PUE, 'SARAGURO') ~ 16,
            str_detect(PUE, 'TOMABELA') ~ 17, str_detect(PUE, 'WARAMKA|GUARANCA') ~ 18, 
            TRUE ~ 19 # Vacíos exclusivos de indígenas
          ),
          es_pueblo_huancavilca ~ 2,
          TRUE ~ 19 # Vacíos exclusivos de indígenas
        ), 
        TRUE ~ -1 # Amputación total
      )
    )
  
  # ====================================================================
  # 4. EL MOTOR DE CONSTRUCCIÓN Y ZERO-GHOST DATA
  # ====================================================================
  procesar_motor <- function(base_datos, col_cie, col_etn, col_nac, col_pue) {
    if(nrow(base_datos) == 0) return(NULL)
    
    # Extraemos el Top 50 real
    ranking <- base_datos %>% 
      group_by(CIE) %>% 
      summarise(DES = first(DES), TOTAL = n(), .groups = 'drop') %>%
      arrange(desc(TOTAL)) %>%
      head(50)
    
    if(nrow(ranking) == 0) return(NULL)
    
    # Matrices pre-formateadas a 50 filas con NA para evitar Ghost Data (Regla 4 aplicada)
    df_cie <- data.frame(CIE = rep(NA_character_, 50), DES = rep(NA_character_, 50), H = rep(NA_real_, 50), M = rep(NA_real_, 50), stringsAsFactors = FALSE)
    df_etn <- as.data.frame(matrix(NA_real_, 50, 20)) # 10x2 = 20
    df_nac <- as.data.frame(matrix(NA_real_, 50, 30)) # 15x2 = 30
    df_pue <- as.data.frame(matrix(NA_real_, 50, 38)) # 19x2 = 38
    
    for(r in 1:nrow(ranking)) {
      c_cie <- ranking$CIE[r]
      sub <- base_datos %>% filter(CIE == c_cie)
      
      df_cie$CIE[r] <- c_cie
      df_cie$DES[r] <- ranking$DES[r]
      
      h_val <- sum(sub$esHombre, na.rm=T); if(h_val > 0) df_cie$H[r] <- h_val
      m_val <- sum(sub$esMujer, na.rm=T); if(m_val > 0) df_cie$M[r] <- m_val
      
      for(i in 1:10) { 
        vH <- sum(sub$iE == i & sub$esHombre, na.rm=T); if(vH > 0) df_etn[r, (i*2)-1] <- vH
        vM <- sum(sub$iE == i & sub$esMujer, na.rm=T); if(vM > 0) df_etn[r, i*2] <- vM
      }
      for(i in 1:15) { 
        vH <- sum(sub$iN == i & sub$esHombre, na.rm=T); if(vH > 0) df_nac[r, (i*2)-1] <- vH
        vM <- sum(sub$iN == i & sub$esMujer, na.rm=T); if(vM > 0) df_nac[r, i*2] <- vM
      }
      for(i in 1:19) { 
        vH <- sum(sub$iP == i & sub$esHombre, na.rm=T); if(vH > 0) df_pue[r, (i*2)-1] <- vH
        vM <- sum(sub$iP == i & sub$esMujer, na.rm=T); if(vM > 0) df_pue[r, i*2] <- vM
      }
    }
    
    # ====================================================================
    # 5. INYECCIÓN BLINDADA (GPS INTOCABLE)
    # ====================================================================
    fila_objetivo <- 64 + (mes_actual_num - 1) * 58
    
    range_write(ss = id_google_sheet, data = df_cie, range = paste0("PRINCI-MORBI!", col_cie, fila_objetivo), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = df_etn, range = paste0("PRINCI-MORBI!", col_etn, fila_objetivo), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = df_nac, range = paste0("PRINCI-MORBI!", col_nac, fila_objetivo), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = df_pue, range = paste0("PRINCI-MORBI!", col_pue, fila_objetivo), col_names = FALSE, reformat = FALSE)
  }
  
  # ====================================================================
  # PASO 6. EJECUTAR LOS MOTORES (Inyección Dinámica de Coordenadas X)
  # ====================================================================
  base_med <- base_limpia %>% filter(esMed)
  base_odo <- base_limpia %>% filter(esOdo)
  
  procesar_motor(base_med, "B", "F", "AB", "BJ")    # Coordenadas Medicina
  procesar_motor(base_odo, "DB", "DF", "EB", "FJ")  # Coordenadas Odontología
  
  cat("✅ Trabajador 9 finalizado. Perfiles Epidemiológicos (ELT) con Cascada Exclusiva Indígena actualizados en cajones mensuales.\n")
}
