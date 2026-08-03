# ====================================================================================
# TRABAJADOR 9: MOTOR EPIDEMIOLÓGICO (LÓGICA INTERCULTURAL ESTRICTA Y ORTOGRÁFICA)
# Arquitectura: Paradigma ELT -> Top 50 Dinámico Mensual -> Vectorización Horizontal
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador9_PerfilEpidemiologico <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🦠 Trabajador 9 activado: Procesando Perfil Epidemiológico Top 50 (Paradigma ELT)...\n")
  
  # ====================================================================
  # PASO 1. DETERMINAR EL MES ACTUAL DE PROCESAMIENTO
  # ====================================================================
  mes_actual_num <- mes_objetivo
  
  # Blindaje de columnas
  nombres_cols <- names(tabla_consolidada)
  if (!"ATEMED_DES_CIE10" %in% nombres_cols) tabla_consolidada$ATEMED_DES_CIE10 <- ""
  if (!"PCTE_SEXO" %in% nombres_cols) tabla_consolidada$PCTE_SEXO <- ""
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  # ====================================================================
  # PASO 2. LIMPIEZA Y FILTRADO ESTRICTO (Escudo NA Activo)
  # ====================================================================
  base_limpia <- tabla_consolidada %>%
    mutate(
      ESP = str_squish(str_remove_all(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE)), "[\r\n\"]")),
      TIP = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_TIP_DIAG), "", ATEMED_TIP_DIAG)), "[\r\n\"]")),
      CR  = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG)), "[\r\n\"]")),
      CON = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CON_DIAG), "", ATEMED_CON_DIAG)), "[\r\n\"]")),
      CIE = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10)), "[\r\n\"\\.]")),
      DES = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_DES_CIE10), "", ATEMED_DES_CIE10)), "[\r\n\"]")),
      SEX = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_SEXO), "", PCTE_SEXO)), "[\r\n\"]")),
      ETN = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_AUTID_ETN), "", PCTE_AUTID_ETN)), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_NAC_ETN), "", PCTE_NAC_ETN)), "[\r\n\"]")),
      PUE = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_PUEBLO), "", PCTE_PUEBLO)), "[\r\n\"]"))
    ) %>%
    filter(
      str_detect(TIP, "MORBILIDAD") & 
        str_detect(CR, "PRIMER|^P$|^1$|^N$") & 
        str_detect(CON, "DEFINITIVO INICIAL")
    ) %>%
    mutate(
      # ⚠️ AVISO ARQUITECTURA: Blindaje ortográfico provincial aplicado (M.*DIC)
      esMed = str_detect(ESP, "MEDICIN|M.*DIC|OBSTETR|PSICOLOG"),
      esOdo = str_detect(ESP, "ODONTO"),
      esMujer = str_detect(SEX, "MUJER|^F$"),
      esHombre = !str_detect(SEX, "MUJER|^F$") 
    ) %>%
    filter(esMed | esOdo) 
  
  # ====================================================================
  # FUNCIÓN MOTOR: Procesa datos y ejecuta inyección ELT
  # ====================================================================
  procesar_motor <- function(base_datos, col_cie, col_etn, col_nac, col_pue) {
    if(nrow(base_datos) == 0) return(NULL)
    
    # --- AGRUPACIÓN Y CREACIÓN DE MATRIZ TOP 50 ---
    resumen_enfermedades <- base_datos %>%
      group_by(CIE, DES) %>%
      summarise(
        TOTAL = n(),
        Hombres_Mes = sum(esHombre, na.rm = TRUE),
        Mujeres_Mes = sum(esMujer, na.rm = TRUE),
        
        # ETNIA (10 ítems x 2 géneros = 20 variables) ----
        ETN_Afro_H     = sum(str_detect(ETN, "AFRO.*") & esHombre, na.rm=T),
        ETN_Afro_M     = sum(str_detect(ETN, "AFRO.*") & esMujer, na.rm=T),
        ETN_Blanco_H   = sum(str_detect(ETN, "BLANC.*") & esHombre, na.rm=T),
        ETN_Blanco_M   = sum(str_detect(ETN, "BLANC.*") & esMujer, na.rm=T),
        ETN_Indigena_H = sum(str_detect(ETN, "IND.*GENA") & esHombre, na.rm=T),
        ETN_Indigena_M = sum(str_detect(ETN, "IND.*GENA") & esMujer, na.rm=T),
        ETN_Mestizo_H  = sum(str_detect(ETN, "MESTIZ.*") & esHombre, na.rm=T),
        ETN_Mestizo_M  = sum(str_detect(ETN, "MESTIZ.*") & esMujer, na.rm=T),
        ETN_Montubio_H = sum(str_detect(ETN, "MONTUB.*|MONTUV.*") & esHombre, na.rm=T),
        ETN_Montubio_M = sum(str_detect(ETN, "MONTUB.*|MONTUV.*") & esMujer, na.rm=T),
        ETN_Mulato_H   = sum(str_detect(ETN, "MULAT.*") & esHombre, na.rm=T),
        ETN_Mulato_M   = sum(str_detect(ETN, "MULAT.*") & esMujer, na.rm=T),
        ETN_Negro_H    = sum(str_detect(ETN, "NEGR.*") & esHombre, na.rm=T),
        ETN_Negro_M    = sum(str_detect(ETN, "NEGR.*") & esMujer, na.rm=T),
        ETN_Otros_H    = sum(str_detect(ETN, "OTR.*") & esHombre, na.rm=T),
        ETN_Otros_M    = sum(str_detect(ETN, "OTR.*") & esMujer, na.rm=T),
        ETN_NoSabe_H   = sum(str_detect(ETN, "SABE.*|RESPONDE") & esHombre, na.rm=T),
        ETN_NoSabe_M   = sum(str_detect(ETN, "SABE.*|RESPONDE") & esMujer, na.rm=T),
        ETN_Aplica_H   = sum((str_detect(ETN, "APLICA") | ETN == "") & esHombre, na.rm=T),
        ETN_Aplica_M   = sum((str_detect(ETN, "APLICA") | ETN == "") & esMujer, na.rm=T),
        
        # NACIONALIDAD (16 ítems x 2 géneros = 32 variables) ----
        NAC_Achuar_H      = sum(str_detect(NAC, "ACHUAR") & esHombre, na.rm=T),
        NAC_Achuar_M      = sum(str_detect(NAC, "ACHUAR") & esMujer, na.rm=T),
        NAC_Andoa_H       = sum(str_detect(NAC, "ANDOA") & esHombre, na.rm=T),
        NAC_Andoa_M       = sum(str_detect(NAC, "ANDOA") & esMujer, na.rm=T),
        NAC_Awa_H         = sum(str_detect(NAC, "AWA|AWÁ") & esHombre, na.rm=T),
        NAC_Awa_M         = sum(str_detect(NAC, "AWA|AWÁ") & esMujer, na.rm=T),
        NAC_Chachi_H      = sum(str_detect(NAC, "CHACHI") & esHombre, na.rm=T),
        NAC_Chachi_M      = sum(str_detect(NAC, "CHACHI") & esMujer, na.rm=T),
        NAC_Cofan_H       = sum(str_detect(NAC, "COFAN|COFÁN") & esHombre, na.rm=T),
        NAC_Cofan_M       = sum(str_detect(NAC, "COFAN|COFÁN") & esMujer, na.rm=T),
        NAC_Epera_H       = sum(str_detect(NAC, "EPERA") & esHombre, na.rm=T),
        NAC_Epera_M       = sum(str_detect(NAC, "EPERA") & esMujer, na.rm=T),
        NAC_Huancavilca_H = sum(str_detect(NAC, "HUANCAVILCA") & esHombre, na.rm=T),
        NAC_Huancavilca_M = sum(str_detect(NAC, "HUANCAVILCA") & esMujer, na.rm=T),
        NAC_Kichwa_H      = sum(str_detect(NAC, "KICHWA|QUICHUA") & esHombre, na.rm=T),
        NAC_Kichwa_M      = sum(str_detect(NAC, "KICHWA|QUICHUA") & esMujer, na.rm=T),
        NAC_Secoya_H      = sum(str_detect(NAC, "SECOYA") & esHombre, na.rm=T),
        NAC_Secoya_M      = sum(str_detect(NAC, "SECOYA") & esMujer, na.rm=T),
        NAC_Shiwiar_H     = sum(str_detect(NAC, "SHIWIAR") & esHombre, na.rm=T),
        NAC_Shiwiar_M     = sum(str_detect(NAC, "SHIWIAR") & esMujer, na.rm=T),
        NAC_Shuar_H       = sum(str_detect(NAC, "SHUAR") & esHombre, na.rm=T),
        NAC_Shuar_M       = sum(str_detect(NAC, "SHUAR") & esMujer, na.rm=T),
        NAC_Tsachila_H    = sum(str_detect(NAC, "TS.*CHILA") & esHombre, na.rm=T),
        NAC_Tsachila_M    = sum(str_detect(NAC, "TS.*CHILA") & esMujer, na.rm=T),
        NAC_Waorani_H     = sum(str_detect(NAC, "WAORANI|HUAORANI") & esHombre, na.rm=T),
        NAC_Waorani_M     = sum(str_detect(NAC, "WAORANI|HUAORANI") & esMujer, na.rm=T),
        NAC_Zapara_H      = sum(str_detect(NAC, "ZAPARA") & esHombre, na.rm=T),
        NAC_Zapara_M      = sum(str_detect(NAC, "ZAPARA") & esMujer, na.rm=T),
        NAC_NoSabe_H      = sum(str_detect(NAC, "SABE.*|RESPONDE") & esHombre, na.rm=T),
        NAC_NoSabe_M      = sum(str_detect(NAC, "SABE.*|RESPONDE") & esMujer, na.rm=T),
        NAC_NoAplica_H    = sum((str_detect(NAC, "APLICA") | NAC == "") & esHombre, na.rm=T),
        NAC_NoAplica_M    = sum((str_detect(NAC, "APLICA") | NAC == "") & esMujer, na.rm=T),
        
        # PUEBLO (20 ítems x 2 géneros = 40 variables) ----
        PUE_Chibuleo_H       = sum(str_detect(PUE, "CHIBULEO") & esHombre, na.rm=T),
        PUE_Chibuleo_M       = sum(str_detect(PUE, "CHIBULEO") & esMujer, na.rm=T),
        PUE_Huancavilca_H    = sum(str_detect(PUE, "HUANCAVILCA") & esHombre, na.rm=T),
        PUE_Huancavilca_M    = sum(str_detect(PUE, "HUANCAVILCA") & esMujer, na.rm=T),
        PUE_Kanari_H         = sum(str_detect(PUE, "KA.*ARI|CA.*ARI") & esHombre, na.rm=T),
        PUE_Kanari_M         = sum(str_detect(PUE, "KA.*ARI|CA.*ARI") & esMujer, na.rm=T),
        PUE_Karanki_H        = sum(str_detect(PUE, "KARANKI") & esHombre, na.rm=T),
        PUE_Karanki_M        = sum(str_detect(PUE, "KARANKI") & esMujer, na.rm=T),
        PUE_Kayambi_H        = sum(str_detect(PUE, "KAYAMBI") & esHombre, na.rm=T),
        PUE_Kayambi_M        = sum(str_detect(PUE, "KAYAMBI") & esMujer, na.rm=T),
        PUE_KichwaAmaz_H     = sum(str_detect(PUE, "KICHWA AMAZ.*") & esHombre, na.rm=T),
        PUE_KichwaAmaz_M     = sum(str_detect(PUE, "KICHWA AMAZ.*") & esMujer, na.rm=T),
        PUE_Kisapincha_H     = sum(str_detect(PUE, "KISAPINCHA|QUISAPINCHA") & esHombre, na.rm=T),
        PUE_Kisapincha_M     = sum(str_detect(PUE, "KISAPINCHA|QUISAPINCHA") & esMujer, na.rm=T),
        PUE_Kitukara_H       = sum(str_detect(PUE, "KITUKARA") & esHombre, na.rm=T),
        PUE_Kitukara_M       = sum(str_detect(PUE, "KITUKARA") & esMujer, na.rm=T),
        PUE_Natabuela_H      = sum(str_detect(PUE, "NATABUELA") & esHombre, na.rm=T),
        PUE_Natabuela_M      = sum(str_detect(PUE, "NATABUELA") & esMujer, na.rm=T),
        PUE_Otavalo_H        = sum(str_detect(PUE, "OTAVALO") & esHombre, na.rm=T),
        PUE_Otavalo_M        = sum(str_detect(PUE, "OTAVALO") & esMujer, na.rm=T),
        PUE_Paltas_H         = sum(str_detect(PUE, "PALTAS") & esHombre, na.rm=T),
        PUE_Paltas_M         = sum(str_detect(PUE, "PALTAS") & esMujer, na.rm=T),
        PUE_Panzaleo_H       = sum(str_detect(PUE, "PANZALEO") & esHombre, na.rm=T),
        PUE_Panzaleo_M       = sum(str_detect(PUE, "PANZALEO") & esMujer, na.rm=T),
        PUE_Pastos_H         = sum(str_detect(PUE, "PASTOS") & esHombre, na.rm=T),
        PUE_Pastos_M         = sum(str_detect(PUE, "PASTOS") & esMujer, na.rm=T),
        PUE_Puruha_H         = sum(str_detect(PUE, "PURUH.*") & esHombre, na.rm=T),
        PUE_Puruha_M         = sum(str_detect(PUE, "PURUH.*") & esMujer, na.rm=T),
        PUE_Salasaca_H       = sum(str_detect(PUE, "SALASA.*") & esHombre, na.rm=T),
        PUE_Salasaca_M       = sum(str_detect(PUE, "SALASA.*") & esMujer, na.rm=T),
        PUE_Saraguro_H       = sum(str_detect(PUE, "SARAGURO") & esHombre, na.rm=T),
        PUE_Saraguro_M       = sum(str_detect(PUE, "SARAGURO") & esMujer, na.rm=T),
        PUE_Tomabela_H       = sum(str_detect(PUE, "TOMABELA") & esHombre, na.rm=T),
        PUE_Tomabela_M       = sum(str_detect(PUE, "TOMABELA") & esMujer, na.rm=T),
        PUE_Waramka_H        = sum(str_detect(PUE, "WARAMKA|GUARANCA") & esHombre, na.rm=T),
        PUE_Waramka_M        = sum(str_detect(PUE, "WARAMKA|GUARANCA") & esMujer, na.rm=T),
        PUE_NoSabe_H         = sum(str_detect(PUE, "SABE.*|RESPONDE") & esHombre, na.rm=T),
        PUE_NoSabe_M         = sum(str_detect(PUE, "SABE.*|RESPONDE") & esMujer, na.rm=T),
        PUE_NoAplica_H       = sum((str_detect(PUE, "APLICA") | PUE == "") & esHombre, na.rm=T),
        PUE_NoAplica_M       = sum((str_detect(PUE, "APLICA") | PUE == "") & esMujer, na.rm=T),
        
        .groups = 'drop'
      ) %>%
      arrange(desc(TOTAL)) %>%
      head(50) # Corte exacto al Top 50 para optimización
    
    # 1. VECTORIZACIÓN Y COERCIÓN NUMÉRICA (na_if previene errores de texto en Sheets) ----
    df_cie_des_hm <- resumen_enfermedades %>% 
      select(CIE, DES, Hombres_Mes, Mujeres_Mes) %>% 
      mutate(across(c(Hombres_Mes, Mujeres_Mes), ~na_if(., 0)))
    
    df_etn <- resumen_enfermedades %>% 
      select(starts_with("ETN_")) %>% 
      mutate(across(everything(), ~na_if(., 0)))
    
    df_nac <- resumen_enfermedades %>% 
      select(starts_with("NAC_")) %>% 
      mutate(across(everything(), ~na_if(., 0)))
    
    df_pue <- resumen_enfermedades %>% 
      select(starts_with("PUE_")) %>% 
      mutate(across(everything(), ~na_if(., 0)))
    
    # 2. ESCRITURA MASIVA Y QUIRÚRGICA (Nube Nativa) ----
    fila_objetivo <- 64 + (mes_actual_num - 1) * 58
    
    # ⚠️ AVISO ARQUITECTURA: Se utiliza range_write estándar para plena compatibilidad 
    # con GitHub Actions, garantizando la inyección masiva en la API de Google.
    range_write(ss = id_google_sheet, data = as.data.frame(df_cie_des_hm), range = paste0("PRINCI-MORBI!", col_cie, fila_objetivo), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = as.data.frame(df_etn), range = paste0("PRINCI-MORBI!", col_etn, fila_objetivo), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = as.data.frame(df_nac), range = paste0("PRINCI-MORBI!", col_nac, fila_objetivo), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = as.data.frame(df_pue), range = paste0("PRINCI-MORBI!", col_pue, fila_objetivo), col_names = FALSE, reformat = FALSE)
  }
  
  # ====================================================================
  # PASO 3. EJECUTAR LOS MOTORES (Inyección Dinámica de Coordenadas X)
  # ====================================================================
  base_med <- base_limpia %>% filter(esMed)
  base_odo <- base_limpia %>% filter(esOdo)
  
  procesar_motor(base_med, "B", "F", "AB", "BJ")    # Coordenadas Medicina
  procesar_motor(base_odo, "DB", "DF", "EB", "FJ")  # Coordenadas Odontología
  
  cat("✅ Trabajador 9 finalizado. Perfiles Epidemiológicos Top 50 (ELT) actualizados en cajones mensuales.\n")
}
