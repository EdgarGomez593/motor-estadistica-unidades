# ====================================================================================
# TRABAJADOR 9: MOTOR EPIDEMIOLÓGICO (LÓGICA INTERCULTURAL ESTRICTA Y ORTOGRÁFICA)
# Arquitectura: Filtro Morbilidad Inicial -> Top 100 (Mensual/Etnia) y Top 10 (Intercultural)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador9_PerfilEpidemiologico <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🦠 Trabajador 9 activado: Procesando Perfil Epidemiológico (Top 100 y Top 10)...\n")
  
  # 1. DETERMINAR EL MES ACTUAL DE PROCESAMIENTO
  mes_actual_num <- mes_objetivo
  
  # Blindaje de columnas
  nombres_cols <- names(tabla_consolidada)
  if (!"ATEMED_DES_CIE10" %in% nombres_cols) tabla_consolidada$ATEMED_DES_CIE10 <- ""
  if (!"PCTE_SEXO" %in% nombres_cols) tabla_consolidada$PCTE_SEXO <- ""
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  # 2. LIMPIEZA Y FILTRADO ESTRICTO (Solo Morbilidad, Primera, Definitivo Inicial)
  base_limpia <- tabla_consolidada %>%
    mutate(
      ESP = str_squish(str_remove_all(toupper(PROF_ESP_ATE), "[\r\n\"]")),
      TIP = str_squish(str_remove_all(toupper(ATEMED_TIP_DIAG), "[\r\n\"]")),
      CR  = str_squish(str_remove_all(toupper(ATEMED_CRON_DIAG), "[\r\n\"]")),
      CON = str_squish(str_remove_all(toupper(ATEMED_CON_DIAG), "[\r\n\"]")),
      CIE = str_squish(str_remove_all(toupper(ATEMED_CIE10), "[\r\n\"\\.]")),
      DES = str_squish(str_remove_all(toupper(ATEMED_DES_CIE10), "[\r\n\"]")),
      SEX = str_squish(str_remove_all(toupper(PCTE_SEXO), "[\r\n\"]")),
      ETN = str_squish(str_remove_all(toupper(PCTE_AUTID_ETN), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(PCTE_NAC_ETN), "[\r\n\"]")),
      PUE = str_squish(str_remove_all(toupper(PCTE_PUEBLO), "[\r\n\"]"))
    ) %>%
    filter(
      str_detect(TIP, "MORBILIDAD") & 
        str_detect(CR, "PRIMER|^P$|^1$|^N$") & 
        str_detect(CON, "DEFINITIVO INICIAL")
    ) %>%
    mutate(
      esMed = str_detect(ESP, "MEDICIN|OBSTETR|PSICOLOG"),
      esOdo = str_detect(ESP, "ODONTO"),
      esMujer = str_detect(SEX, "MUJER|^F$"),
      esHombre = !str_detect(SEX, "MUJER|^F$") # Asumimos hombre si no es mujer para el conteo binario
    ) %>%
    filter(esMed | esOdo) # Solo dejamos pasar las ramas autorizadas
  
  # Función interna para procesar cada bloque (Médicos u Odontólogos)
  procesar_motor <- function(base_datos, fila_global_100, fila_global_10, fila_mes_10) {
    if(nrow(base_datos) == 0) return(NULL)
    
    # --- AGRUPACIÓN TOP 100 ---
    resumen_enfermedades <- base_datos %>%
      group_by(CIE, DES) %>%
      summarise(
        TOTAL = n(),
        
        # Conteo de Género para el mes actual procesado
        Hombres_Mes = sum(esHombre, na.rm = TRUE),
        Mujeres_Mes = sum(esMujer, na.rm = TRUE),
        
        # Conteo Ortográfico de Etnias (Columnas 30 a 39 mapeadas a índices 1 a 10)
        E_Indigena = sum(str_detect(ETN, "INDIGENA|INDÍGENA"), na.rm = TRUE),
        E_Mestizo  = sum(str_detect(ETN, "MESTIZ"), na.rm = TRUE),
        E_Afro     = sum(str_detect(ETN, "AFRO"), na.rm = TRUE),
        E_Montubio = sum(str_detect(ETN, "MONTUB|MONTUV"), na.rm = TRUE),
        E_Mulato   = sum(str_detect(ETN, "MULAT"), na.rm = TRUE),
        E_Negro    = sum(str_detect(ETN, "NEGR"), na.rm = TRUE),
        E_Blanco   = sum(str_detect(ETN, "BLANC"), na.rm = TRUE),
        E_NoAplica = sum(str_detect(ETN, "APLICA"), na.rm = TRUE),
        E_NoSabe   = sum(str_detect(ETN, "SABE|RESPONDE"), na.rm = TRUE),
        
        # Clasificadores Top 10 (Lógica Estricta)
        T10_E_Indigena_H = sum(str_detect(ETN, "INDIGENA|INDÍGENA") & esHombre, na.rm=T),
        T10_E_Indigena_M = sum(str_detect(ETN, "INDIGENA|INDÍGENA") & esMujer, na.rm=T),
        T10_E_Mestizo_H  = sum(str_detect(ETN, "MESTIZ") & esHombre, na.rm=T),
        T10_E_Mestizo_M  = sum(str_detect(ETN, "MESTIZ") & esMujer, na.rm=T),
        T10_E_Blanco_H   = sum(str_detect(ETN, "BLANC") & esHombre, na.rm=T),
        T10_E_Blanco_M   = sum(str_detect(ETN, "BLANC") & esMujer, na.rm=T),
        T10_E_Aplica_H   = sum(str_detect(ETN, "APLICA") & esHombre, na.rm=T),
        T10_E_Aplica_M   = sum(str_detect(ETN, "APLICA") & esMujer, na.rm=T),
        T10_E_Montubio_H = sum(str_detect(ETN, "MONTUB|MONTUV") & esHombre, na.rm=T),
        T10_E_Montubio_M = sum(str_detect(ETN, "MONTUB|MONTUV") & esMujer, na.rm=T),
        T10_E_Sabe_H     = sum(str_detect(ETN, "SABE|RESPONDE") & esHombre, na.rm=T),
        T10_E_Sabe_M     = sum(str_detect(ETN, "SABE|RESPONDE") & esMujer, na.rm=T),
        T10_E_Afro_H     = sum(str_detect(ETN, "AFRO|MULAT|NEGR") & esHombre, na.rm=T),
        T10_E_Afro_M     = sum(str_detect(ETN, "AFRO|MULAT|NEGR") & esMujer, na.rm=T),
        
        T10_N_Kichwa_H   = sum(str_detect(NAC, "KICHWA|QUICHUA") & esHombre, na.rm=T),
        T10_N_Kichwa_M   = sum(str_detect(NAC, "KICHWA|QUICHUA") & esMujer, na.rm=T),
        T10_N_Sabe_H     = sum(str_detect(NAC, "SABE|RESPONDE") & esHombre, na.rm=T),
        T10_N_Sabe_M     = sum(str_detect(NAC, "SABE|RESPONDE") & esMujer, na.rm=T),
        T10_N_Shuar_H    = sum(str_detect(NAC, "SHUAR") & esHombre, na.rm=T),
        T10_N_Shuar_M    = sum(str_detect(NAC, "SHUAR") & esMujer, na.rm=T),
        
        T10_P_Tomabela_H = sum(str_detect(PUE, "TOMABELA") & esHombre, na.rm=T),
        T10_P_Tomabela_M = sum(str_detect(PUE, "TOMABELA") & esMujer, na.rm=T),
        T10_P_Chibuleo_H = sum(str_detect(PUE, "CHIBULEO") & esHombre, na.rm=T),
        T10_P_Chibuleo_M = sum(str_detect(PUE, "CHIBULEO") & esMujer, na.rm=T),
        T10_P_Otavalo_H  = sum(str_detect(PUE, "OTAVALO") & esHombre, na.rm=T),
        T10_P_Otavalo_M  = sum(str_detect(PUE, "OTAVALO") & esMujer, na.rm=T),
        T10_P_Puruha_H   = sum(str_detect(PUE, "PURUHA|PURUHÁ") & esHombre, na.rm=T),
        T10_P_Puruha_M   = sum(str_detect(PUE, "PURUHA|PURUHÁ") & esMujer, na.rm=T),
        T10_P_Salasaca_H = sum(str_detect(PUE, "SALASAKA|SALASACA") & esHombre, na.rm=T),
        T10_P_Salasaca_M = sum(str_detect(PUE, "SALASAKA|SALASACA") & esMujer, na.rm=T),
        T10_P_Guaranca_H = sum(str_detect(PUE, "WARANKA|GUARANCA") & esHombre, na.rm=T),
        T10_P_Guaranca_M = sum(str_detect(PUE, "WARANKA|GUARANCA") & esMujer, na.rm=T),
        T10_P_Karanki_H  = sum(str_detect(PUE, "KARANKI") & esHombre, na.rm=T),
        T10_P_Karanki_M  = sum(str_detect(PUE, "KARANKI") & esMujer, na.rm=T),
        T10_P_Sabe_H     = sum(str_detect(PUE, "SABE|RESPONDE") & esHombre, na.rm=T),
        T10_P_Sabe_M     = sum(str_detect(PUE, "SABE|RESPONDE") & esMujer, na.rm=T),
        T10_P_VacioInd_H = sum((PUE == "" | is.na(PUE)) & str_detect(ETN, "INDIGENA|INDÍGENA") & esHombre, na.rm=T),
        T10_P_VacioInd_M = sum((PUE == "" | is.na(PUE)) & str_detect(ETN, "INDIGENA|INDÍGENA") & esMujer, na.rm=T),
        
        .groups = 'drop'
      ) %>%
      arrange(desc(TOTAL)) %>%
      head(100)
    
    # -------------------------------------------------------------
    # CONSTRUCCIÓN MATRIZ TOP 100 (100 filas x 26 cols) (100 filas x 10 cols)
    # -------------------------------------------------------------
    matriz_100_demografica <- matrix("", nrow = 100, ncol = 26)
    matriz_100_etnias <- matrix("", nrow = 100, ncol = 10)
    
    for(i in 1:min(100, nrow(resumen_enfermedades))) {
      row <- resumen_enfermedades[i, ]
      matriz_100_demografica[i, 1] <- row$CIE
      matriz_100_demografica[i, 2] <- row$DES
      
      # Ubicar los datos de H/M en la columna del mes correspondiente
      col_h <- 2 + (mes_actual_num - 1) * 2 + 1
      col_m <- 2 + (mes_actual_num - 1) * 2 + 2
      
      if(row$Hombres_Mes > 0) matriz_100_demografica[i, col_h] <- row$Hombres_Mes
      if(row$Mujeres_Mes > 0) matriz_100_demografica[i, col_m] <- row$Mujeres_Mes
      
      # Etnias (Mapeo a las columnas 30-39 de tu código original)
      if(row$E_Indigena > 0) matriz_100_etnias[i, 1] <- row$E_Indigena
      if(row$E_Mestizo > 0)  matriz_100_etnias[i, 2] <- row$E_Mestizo
      if(row$E_Afro > 0)     matriz_100_etnias[i, 3] <- row$E_Afro
      if(row$E_Montubio > 0) matriz_100_etnias[i, 4] <- row$E_Montubio
      if(row$E_Mulato > 0)   matriz_100_etnias[i, 5] <- row$E_Mulato
      if(row$E_Negro > 0)    matriz_100_etnias[i, 6] <- row$E_Negro
      if(row$E_Blanco > 0)   matriz_100_etnias[i, 7] <- row$E_Blanco
      if(row$E_NoAplica > 0) matriz_100_etnias[i, 8] <- row$E_NoAplica
      if(row$E_NoSabe > 0)   matriz_100_etnias[i, 9] <- row$E_NoSabe
      
      # El resto ("OTR") va a la columna 10 (colE = 39)
      total_otros <- row$TOTAL - sum(c(row$E_Indigena, row$E_Mestizo, row$E_Afro, row$E_Montubio, row$E_Mulato, row$E_Negro, row$E_Blanco, row$E_NoAplica, row$E_NoSabe))
      if(total_otros > 0) matriz_100_etnias[i, 10] <- total_otros
    }
    
    # -------------------------------------------------------------
    # CONSTRUCCIÓN MATRICES TOP 10 (10 filas)
    # -------------------------------------------------------------
    matriz_10_E <- matrix("", nrow = 10, ncol = 16)
    matriz_10_N <- matrix("", nrow = 10, ncol = 8)
    matriz_10_P <- matrix("", nrow = 10, ncol = 20)
    
    for(i in 1:min(10, nrow(resumen_enfermedades))) {
      row <- resumen_enfermedades[i, ]
      
      matriz_10_E[i, 1] <- row$CIE; matriz_10_E[i, 2] <- row$DES
      matriz_10_N[i, 1] <- row$CIE; matriz_10_N[i, 2] <- row$DES
      matriz_10_P[i, 1] <- row$CIE; matriz_10_P[i, 2] <- row$DES
      
      # Llenado de H y M si son mayores a 0
      fc <- function(val) { if(val > 0) return(val) else return("") }
      
      matriz_10_E[i, 3] <- fc(row$T10_E_Indigena_H); matriz_10_E[i, 4] <- fc(row$T10_E_Indigena_M)
      matriz_10_E[i, 5] <- fc(row$T10_E_Mestizo_H);  matriz_10_E[i, 6] <- fc(row$T10_E_Mestizo_M)
      matriz_10_E[i, 7] <- fc(row$T10_E_Blanco_H);   matriz_10_E[i, 8] <- fc(row$T10_E_Blanco_M)
      matriz_10_E[i, 9] <- fc(row$T10_E_Aplica_H);   matriz_10_E[i, 10] <- fc(row$T10_E_Aplica_M)
      matriz_10_E[i, 11] <- fc(row$T10_E_Montubio_H); matriz_10_E[i, 12] <- fc(row$T10_E_Montubio_M)
      matriz_10_E[i, 13] <- fc(row$T10_E_Sabe_H);    matriz_10_E[i, 14] <- fc(row$T10_E_Sabe_M)
      matriz_10_E[i, 15] <- fc(row$T10_E_Afro_H);    matriz_10_E[i, 16] <- fc(row$T10_E_Afro_M)
      
      matriz_10_N[i, 3] <- fc(row$T10_N_Kichwa_H);   matriz_10_N[i, 4] <- fc(row$T10_N_Kichwa_M)
      matriz_10_N[i, 5] <- fc(row$T10_N_Sabe_H);     matriz_10_N[i, 6] <- fc(row$T10_N_Sabe_M)
      matriz_10_N[i, 7] <- fc(row$T10_N_Shuar_H);    matriz_10_N[i, 8] <- fc(row$T10_N_Shuar_M)
      
      matriz_10_P[i, 3] <- fc(row$T10_P_Tomabela_H); matriz_10_P[i, 4] <- fc(row$T10_P_Tomabela_M)
      matriz_10_P[i, 5] <- fc(row$T10_P_Chibuleo_H); matriz_10_P[i, 6] <- fc(row$T10_P_Chibuleo_M)
      matriz_10_P[i, 7] <- fc(row$T10_P_Otavalo_H);  matriz_10_P[i, 8] <- fc(row$T10_P_Otavalo_M)
      matriz_10_P[i, 9] <- fc(row$T10_P_Puruha_H);   matriz_10_P[i, 10] <- fc(row$T10_P_Puruha_M)
      matriz_10_P[i, 11] <- fc(row$T10_P_Salasaca_H); matriz_10_P[i, 12] <- fc(row$T10_P_Salasaca_M)
      matriz_10_P[i, 13] <- fc(row$T10_P_Guaranca_H); matriz_10_P[i, 14] <- fc(row$T10_P_Guaranca_M)
      matriz_10_P[i, 15] <- fc(row$T10_P_Karanki_H);  matriz_10_P[i, 16] <- fc(row$T10_P_Karanki_M)
      matriz_10_P[i, 17] <- fc(row$T10_P_Sabe_H);     matriz_10_P[i, 18] <- fc(row$T10_P_Sabe_M)
      matriz_10_P[i, 19] <- fc(row$T10_P_VacioInd_H); matriz_10_P[i, 20] <- fc(row$T10_P_VacioInd_M)
    }
    
# ====================================================================
    # FILTRO PURIFICADOR QUIRÚRGICO (Respeta las letras de CIE10 y Diagnóstico)
    # ====================================================================
    
    # 1. Purificar Top 100
    df_100_demografica <- as.data.frame(matriz_100_demografica)
    # El secreto: [, 3:ncol(...)] aplica la limpieza solo desde la 3ra columna (Enero Hombre)
    df_100_demografica[, 3:ncol(df_100_demografica)] <- lapply(df_100_demografica[, 3:ncol(df_100_demografica)], as.numeric)
    
    df_100_etnias <- as.data.frame(matriz_100_etnias)
    df_100_etnias[] <- lapply(df_100_etnias, as.numeric) # (Etnia son puros números, se limpia completo)
    
    # 2. Purificar Top 10
    df_10_E <- as.data.frame(matriz_10_E)
    df_10_E[, 3:ncol(df_10_E)] <- lapply(df_10_E[, 3:ncol(df_10_E)], as.numeric)
    
    df_10_N <- as.data.frame(matriz_10_N)
    df_10_N[, 3:ncol(df_10_N)] <- lapply(df_10_N[, 3:ncol(df_10_N)], as.numeric)
    
    df_10_P <- as.data.frame(matriz_10_P)
    df_10_P[, 3:ncol(df_10_P)] <- lapply(df_10_P[, 3:ncol(df_10_P)], as.numeric)


    # ====================================================================
    # ESCRITURA MASIVA (Inyectando solo números puros)
    # ====================================================================
    
    # Top 100
    range_write(ss = id_google_sheet, data = df_100_demografica, range = paste0("PRINCI-MORBI!B", fila_global_100), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = df_100_etnias, range = paste0("PRINCI-MORBI!AF", fila_global_100), col_names = FALSE, reformat = FALSE)
    
    # Top 10 (Global)
    range_write(ss = id_google_sheet, data = df_10_E, range = paste0("PRINCI-MORBI!BK", fila_global_10), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = df_10_N, range = paste0("PRINCI-MORBI!CE", fila_global_10), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = df_10_P, range = paste0("PRINCI-MORBI!CQ", fila_global_10), col_names = FALSE, reformat = FALSE)
    
    # Top 10 (Mes Actual)
    range_write(ss = id_google_sheet, data = df_10_E, range = paste0("PRINCI-MORBI!BK", fila_mes_10), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = df_10_N, range = paste0("PRINCI-MORBI!CE", fila_mes_10), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = df_10_P, range = paste0("PRINCI-MORBI!CQ", fila_mes_10), col_names = FALSE, reformat = FALSE)
  }
  
  # 3. EJECUTAR LOS MOTORES
  base_med <- base_limpia %>% filter(esMed)
  base_odo <- base_limpia %>% filter(esOdo)
  
  procesar_motor(base_med, 6, 6, 22)    # Médicos
  procesar_motor(base_odo, 112, 112, 128) # Odontología
  
  cat("✅ Trabajador 9 finalizado. Perfiles Epidemiológicos Top 100 y Top 10 actualizados.\n")
}
