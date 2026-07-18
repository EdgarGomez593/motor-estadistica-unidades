# ====================================================================================
# TRABAJADOR 10: DISCAPACIDADES (MOTOR BLINDADO Y DEDUPLICACIÓN INTELIGENTE)
# Arquitectura: Filtro 'SI' -> Deduplicación por (ID + Tipo) -> Matrices H/M y Etnia
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador10_Discapacidades <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("♿ Trabajador 10 activado: Evaluando Discapacidades y Deduplicación Inteligente...\n")
  
  # 1. DETERMINAR MES Y COLUMNAS DESTINO
  mes_actual_num <- mes_objetivo
  
  # Lógica del offset: Enero es H=3(C), M=4(D). Febrero es H=5(E), M=6(F).
  col_h_num <- (mes_actual_num * 2) + 1
  col_m_num <- (mes_actual_num * 2) + 2
  
  letras <- c(LETTERS, paste0("A", LETTERS)) # Soporta hasta la columna AZ
  letra_h <- letras[col_h_num]
  
  # 2. BLINDAJE DE COLUMNAS
  nombres_cols <- names(tabla_consolidada)
  if (!"PCTE_DISC" %in% nombres_cols) tabla_consolidada$PCTE_DISC <- ""
  if (!"PCTE_TIP_DISC" %in% nombres_cols) tabla_consolidada$PCTE_TIP_DISC <- ""
  if (!"PCTE_SEXO" %in% nombres_cols) tabla_consolidada$PCTE_SEXO <- ""
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  
  # 3. LIMPIEZA, FILTRADO Y DEDUPLICACIÓN INTELIGENTE
  base_limpia <- tabla_consolidada %>%
    mutate(
      ID = str_squish(str_remove_all(ATEMED_ID, "[\r\n\"]")),
      CRON = str_squish(str_remove_all(toupper(ATEMED_CRON_DIAG), "[\r\n\"]")),
      DISC = str_squish(str_remove_all(toupper(PCTE_DISC), "[\r\n\"]")),
      TIP_DISC = str_squish(str_remove_all(toupper(PCTE_TIP_DISC), "[\r\n\"]")),
      SEXO = str_squish(str_remove_all(toupper(PCTE_SEXO), "[\r\n\"]")),
      ETN = str_squish(str_remove_all(toupper(PCTE_AUTID_ETN), "[\r\n\"]"))
    ) %>%
    # Filtros iniciales
    filter(ID != "" & !is.na(ID)) %>%
    filter(str_detect(DISC, "^SI|^SÍ")) %>%
    filter(str_detect(CRON, "PRIMER|^P$|^1$")) %>%
    # Clasificación del Tipo de Discapacidad (Filas 1 a 6)
    mutate(
      rD = case_when(
        str_detect(TIP_DISC, "AUDITIV") ~ 1,
        str_detect(TIP_DISC, "FISIC|FÍSIC") ~ 2,
        str_detect(TIP_DISC, "INTELECTUAL") ~ 3,
        str_detect(TIP_DISC, "LENGUAJE") ~ 4,
        str_detect(TIP_DISC, "PSICOSOCIAL") ~ 5,
        str_detect(TIP_DISC, "VISUAL") ~ 6,
        TRUE ~ -1
      )
    ) %>%
    filter(rD != -1) %>%
    # DEDUPLICACIÓN INTELIGENTE: (Equivalente a tu llaveUnica = idAte + "_" + rD)
    distinct(ID, rD, .keep_all = TRUE) %>%
    # Clasificadores de Género y Etnia
    mutate(
      esMujer = str_detect(SEXO, "MUJER|FEMENINO|^F$"),
      cE = case_when(
        str_detect(ETN, "AFRO") ~ 1, str_detect(ETN, "BLANC") ~ 2, str_detect(ETN, "INDIGENA|INDÍGENA") ~ 3,
        str_detect(ETN, "MESTIZ") ~ 4, str_detect(ETN, "MONTUB|MONTUV") ~ 5, str_detect(ETN, "MULAT") ~ 6,
        str_detect(ETN, "NEGR") ~ 7, str_detect(ETN, "APLICA") ~ 8, str_detect(ETN, "SABE|RESPONDE") ~ 9,
        str_detect(ETN, "OTR") ~ 10, TRUE ~ -1
      )
    )
  
  # 4. CONSTRUCCIÓN DE MATRICES (6x2 para Mes/Género, 6x10 para Etnias)
  mat_mes <- matrix("", nrow = 6, ncol = 2)
  mat_etnia <- matrix("", nrow = 6, ncol = 10)
  
  for(i in 1:6) {
    # Filtramos las filas que corresponden a este tipo de discapacidad
    sub_df <- base_limpia %>% filter(rD == i)
    
    h_count <- nrow(sub_df %>% filter(!esMujer))
    m_count <- nrow(sub_df %>% filter(esMujer))
    
    if(h_count > 0) mat_mes[i, 1] <- h_count
    if(m_count > 0) mat_mes[i, 2] <- m_count
    
    for(j in 1:10) {
      e_count <- nrow(sub_df %>% filter(cE == j))
      if(e_count > 0) mat_etnia[i, j] <- e_count
    }
  }
  
# 5. ESCRITURA EN EXCEL
  
  # 5.1. Filtro purificador: Convertir a Data Frame y forzar todo a formato numérico
  df_mes <- as.data.frame(mat_mes)
  df_mes[] <- lapply(df_mes, as.numeric)
  
  df_etnia <- as.data.frame(mat_etnia)
  df_etnia[] <- lapply(df_etnia, as.numeric)

  # 5.2. Escribe el bloque de Mes (Ej: Fila 16, Columna de Hombres del mes actual)
  range_write(ss = id_google_sheet, data = df_mes, range = paste0("DISCAPACIDADES!", letra_h, "16"), col_names = FALSE, reformat = FALSE)
  
  # 5.3. Escribe el bloque de Etnia (Fila 16, Columna AE)
  range_write(ss = id_google_sheet, data = df_etnia, range = "DISCAPACIDADES!AE16", col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 10 finalizado. Matrices de discapacidades actualizadas.\n")
