# ====================================================================================
# TRABAJADOR 9: MOTOR EPIDEMIOLÓGICO (VISIÓN ANUAL + MEDICINA Y ODONTOLOGÍA)
# Arquitectura: Base Anual -> Ranking Global Inamovible -> Escritura en Matriz 100x26
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador9_PerfilEpidemiologico <- function(tabla_consolidada_anual, id_google_sheet, mes_maximo) {
  cat("🦠 Trabajador 9 activado: Procesando Perfil Epidemiológico Anual (Medicina y Odontología)...\n")
  
  # Blindaje de columnas
  nombres_cols <- names(tabla_consolidada_anual)
  if (!"ATEMED_DES_CIE10" %in% nombres_cols) tabla_consolidada_anual$ATEMED_DES_CIE10 <- ""
  if (!"PCTE_SEXO" %in% nombres_cols) tabla_consolidada_anual$PCTE_SEXO <- ""
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada_anual$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada_anual$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada_anual$PCTE_PUEBLO <- ""
  
  # 1. LIMPIEZA Y FILTRADO ESTRICTO (Respetando Tildes)
  base_limpia <- tabla_consolidada_anual %>%
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
      esMed = str_detect(ESP, "MEDICIN|OBSTETR"), 
      esOdo = str_detect(ESP, "ODONTO"), # ¡Odontología restaurada!
      esMujer = str_detect(SEX, "MUJER|^F$"),
      esHombre = !str_detect(SEX, "MUJER|^F$")
    ) %>%
    filter(esMed | esOdo) # Dejamos pasar a ambas ramas
  
  # Función interna para procesar el bloque anual (Se ejecutará 2 veces: Med y Odo)
  procesar_motor_anual <- function(base_datos, fila_100, nombre_rama) {
    if(nrow(base_datos) == 0) return(NULL)
    cat("   -> Procesando Top 100 Anual para:", nombre_rama, "...\n")
    
    # --- MEGA-AGRUPACIÓN ANUAL ---
    resumen_enfermedades <- base_datos %>%
      group_by(CIE, DES) %>%
      summarise(
        TOTAL_ANUAL = n(),
        
        # Distribución exacta de Hombres y Mujeres por los 12 meses
        H_1 = sum(MES_ORIGEN == 1 & esHombre, na.rm=T), M_1 = sum(MES_ORIGEN == 1 & esMujer, na.rm=T),
        H_2 = sum(MES_ORIGEN == 2 & esHombre, na.rm=T), M_2 = sum(MES_ORIGEN == 2 & esMujer, na.rm=T),
        H_3 = sum(MES_ORIGEN == 3 & esHombre, na.rm=T), M_3 = sum(MES_ORIGEN == 3 & esMujer, na.rm=T),
        H_4 = sum(MES_ORIGEN == 4 & esHombre, na.rm=T), M_4 = sum(MES_ORIGEN == 4 & esMujer, na.rm=T),
        H_5 = sum(MES_ORIGEN == 5 & esHombre, na.rm=T), M_5 = sum(MES_ORIGEN == 5 & esMujer, na.rm=T),
        H_6 = sum(MES_ORIGEN == 6 & esHombre, na.rm=T), M_6 = sum(MES_ORIGEN == 6 & esMujer, na.rm=T),
        H_7 = sum(MES_ORIGEN == 7 & esHombre, na.rm=T), M_7 = sum(MES_ORIGEN == 7 & esMujer, na.rm=T),
        H_8 = sum(MES_ORIGEN == 8 & esHombre, na.rm=T), M_8 = sum(MES_ORIGEN == 8 & esMujer, na.rm=T),
        H_9 = sum(MES_ORIGEN == 9 & esHombre, na.rm=T), M_9 = sum(MES_ORIGEN == 9 & esMujer, na.rm=T),
        H_10 = sum(MES_ORIGEN == 10 & esHombre, na.rm=T), M_10 = sum(MES_ORIGEN == 10 & esMujer, na.rm=T),
        H_11 = sum(MES_ORIGEN == 11 & esHombre, na.rm=T), M_11 = sum(MES_ORIGEN == 11 & esMujer, na.rm=T),
        H_12 = sum(MES_ORIGEN == 12 & esHombre, na.rm=T), M_12 = sum(MES_ORIGEN == 12 & esMujer, na.rm=T),
        
        # Etnias Anuales (Búsqueda Exacta)
        E_Indigena = sum(ETN == "INDÍGENA", na.rm = TRUE),
        E_Mestizo  = sum(ETN == "MESTIZO/A", na.rm = TRUE),
        E_Afro     = sum(ETN == "AFROECUATORIANO/AFRODESCENDIENTE", na.rm = TRUE),
        E_Blanco   = sum(ETN == "BLANCO/A", na.rm = TRUE),
        E_Mulato   = sum(ETN == "MULATO/A", na.rm = TRUE),
        E_Montubio = sum(ETN == "MONTUBIO/A", na.rm = TRUE),
        E_NoSabe   = sum(ETN == "NO SABE/NO RESPONDE", na.rm = TRUE),
        E_NoAplica = sum(ETN == "NO APLICA", na.rm = TRUE),
        E_Otro     = sum(ETN == "OTRO/A", na.rm = TRUE),
        
        .groups = 'drop'
      ) %>%
      arrange(desc(TOTAL_ANUAL)) %>%
      head(100)
    
    # --- CONSTRUCCIÓN DE LA MATRIZ 100x26 ---
    matriz_100_demografica <- matrix("", nrow = 100, ncol = 26)
    matriz_100_etnias <- matrix("", nrow = 100, ncol = 10)
    
    for(i in 1:min(100, nrow(resumen_enfermedades))) {
      row <- resumen_enfermedades[i, ]
      matriz_100_demografica[i, 1] <- row$CIE
      matriz_100_demografica[i, 2] <- row$DES
      
      # Inyectamos solo si es mayor a 0 para no llenar de ceros
      if(row$H_1 > 0) matriz_100_demografica[i, 3] <- row$H_1;   if(row$M_1 > 0) matriz_100_demografica[i, 4] <- row$M_1
      if(row$H_2 > 0) matriz_100_demografica[i, 5] <- row$H_2;   if(row$M_2 > 0) matriz_100_demografica[i, 6] <- row$M_2
      if(row$H_3 > 0) matriz_100_demografica[i, 7] <- row$H_3;   if(row$M_3 > 0) matriz_100_demografica[i, 8] <- row$M_3
      if(row$H_4 > 0) matriz_100_demografica[i, 9] <- row$H_4;   if(row$M_4 > 0) matriz_100_demografica[i, 10] <- row$M_4
      if(row$H_5 > 0) matriz_100_demografica[i, 11] <- row$H_5;  if(row$M_5 > 0) matriz_100_demografica[i, 12] <- row$M_5
      if(row$H_6 > 0) matriz_100_demografica[i, 13] <- row$H_6;  if(row$M_6 > 0) matriz_100_demografica[i, 14] <- row$M_6
      if(row$H_7 > 0) matriz_100_demografica[i, 15] <- row$H_7;  if(row$M_7 > 0) matriz_100_demografica[i, 16] <- row$M_7
      if(row$H_8 > 0) matriz_100_demografica[i, 17] <- row$H_8;  if(row$M_8 > 0) matriz_100_demografica[i, 18] <- row$M_8
      if(row$H_9 > 0) matriz_100_demografica[i, 19] <- row$H_9;  if(row$M_9 > 0) matriz_100_demografica[i, 20] <- row$M_9
      if(row$H_10 > 0) matriz_100_demografica[i, 21] <- row$H_10; if(row$M_10 > 0) matriz_100_demografica[i, 22] <- row$M_10
      if(row$H_11 > 0) matriz_100_demografica[i, 23] <- row$H_11; if(row$M_11 > 0) matriz_100_demografica[i, 24] <- row$M_11
      if(row$H_12 > 0) matriz_100_demografica[i, 25] <- row$H_12; if(row$M_12 > 0) matriz_100_demografica[i, 26] <- row$M_12
      
      if(row$E_Indigena > 0) matriz_100_etnias[i, 1] <- row$E_Indigena
      if(row$E_Mestizo > 0)  matriz_100_etnias[i, 2] <- row$E_Mestizo
      if(row$E_Afro > 0)     matriz_100_etnias[i, 3] <- row$E_Afro
      if(row$E_Montubio > 0) matriz_100_etnias[i, 4] <- row$E_Montubio
      if(row$E_Mulato > 0)   matriz_100_etnias[i, 5] <- row$E_Mulato
      if(row$E_Blanco > 0)   matriz_100_etnias[i, 7] <- row$E_Blanco
      if(row$E_NoAplica > 0) matriz_100_etnias[i, 8] <- row$E_NoAplica
      if(row$E_NoSabe > 0)   matriz_100_etnias[i, 9] <- row$E_NoSabe
      if(row$E_Otro > 0)     matriz_100_etnias[i, 10] <- row$E_Otro
    }
    
    # Conversión a números puros
    df_100_demografica <- as.data.frame(matriz_100_demografica)
    df_100_demografica[, 3:ncol(df_100_demografica)] <- lapply(df_100_demografica[, 3:ncol(df_100_demografica)], as.numeric)
    
    df_100_etnias <- as.data.frame(matriz_100_etnias)
    df_100_etnias[] <- lapply(df_100_etnias, as.numeric)
    
    # --- ESCRITURA EN BLOQUE GIGANTE ---
    range_write(ss = id_google_sheet, data = df_100_demografica, range = paste0("PRINCI-MORBI!B", fila_100), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = df_100_etnias, range = paste0("PRINCI-MORBI!AF", fila_100), col_names = FALSE, reformat = FALSE)
  }
  
  # 4. EJECUTAMOS LOS DOS MOTORES (Medicina y Odontología)
  base_med <- base_limpia %>% filter(esMed)
  base_odo <- base_limpia %>% filter(esOdo)
  
  procesar_motor_anual(base_med, 6, "Medicina")
  procesar_motor_anual(base_odo, 112, "Odontología")
  
  cat("✅ Trabajador 9 finalizado. Perfil Epidemiológico Anual (Med y Odo) cuadrado a la perfección.\n")
}
