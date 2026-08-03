# ====================================================================================
# TRABAJADOR 10: DISCAPACIDADES (MOTOR BLINDADO Y DEDUPLICACIÓN INTELIGENTE)
# Arquitectura: Filtro 'SI' -> Deduplicación por (ID + Tipo) -> Matrices H/M y Etnia
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador10_Discapacidades <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("♿ Trabajador 10 activado: Evaluando Discapacidades y Deduplicación Inteligente...\n")
  
  # ====================================================================
  # 1. DETERMINAR MES Y COLUMNAS DESTINO
  # ====================================================================
  mes_actual_num <- mes_objetivo
  
  # Lógica del offset: Enero es H=3(C), M=4(D). Febrero es H=5(E), M=6(F).
  col_h_num <- (mes_actual_num * 2) + 1
  col_m_num <- (mes_actual_num * 2) + 2
  
  letras <- c(LETTERS, paste0("A", LETTERS)) # Soporta hasta la columna AZ
  letra_h <- letras[col_h_num]
  
  # ====================================================================
  # 2. BLINDAJE DE COLUMNAS
  # ====================================================================
  nombres_cols <- names(tabla_consolidada)
  if (!"PCTE_DISC" %in% nombres_cols) tabla_consolidada$PCTE_DISC <- ""
  if (!"PCTE_TIP_DISC" %in% nombres_cols) tabla_consolidada$PCTE_TIP_DISC <- ""
  if (!"PCTE_SEXO" %in% nombres_cols) tabla_consolidada$PCTE_SEXO <- ""
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  # ====================================================================
  # 3. LIMPIEZA, FILTRADO Y CLASIFICACIÓN (Escudo Anti-NA y Curso de Selva)
  # ====================================================================
  
  base_clasificada <- tabla_consolidada %>%
    mutate(
      ID = str_squish(str_remove_all(ifelse(is.na(ATEMED_ID), "", ATEMED_ID), "[\r\n\"]")),
      CRON = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG)), "[\r\n\"]")),
      DISC = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_DISC), "", PCTE_DISC)), "[\r\n\"]")),
      TIP_DISC = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_TIP_DISC), "", PCTE_TIP_DISC)), "[\r\n\"]")),
      SEXO = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_SEXO), "", PCTE_SEXO)), "[\r\n\"]")),
      ETN = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_AUTID_ETN), "", PCTE_AUTID_ETN)), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_NAC_ETN), "", PCTE_NAC_ETN)), "[\r\n\"]")),
      PUE = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_PUEBLO), "", PCTE_PUEBLO)), "[\r\n\"]"))
    ) %>%
    filter(ID != "" & !is.na(ID)) %>%
    filter(str_detect(DISC, "^SI|^SÍ")) %>%
    filter(str_detect(CRON, "PRIMER|^P$|^1$")) %>%
    mutate(
      # Género Binario
      esMujer = str_detect(SEXO, "MUJER|FEMENINO|^F$"),
      
      # EJE Y: Discapacidades (rD: 1 al 6) - Blindaje Ortográfico Incluido
      rD = case_when(
        str_detect(TIP_DISC, "AUDITIV") ~ 1, str_detect(TIP_DISC, "FISIC|F.*SIC") ~ 2,
        str_detect(TIP_DISC, "INTELECTUAL") ~ 3, str_detect(TIP_DISC, "LENGUAJE") ~ 4,
        str_detect(TIP_DISC, "PSICOSOCIAL") ~ 5, str_detect(TIP_DISC, "VISUAL") ~ 6,
        TRUE ~ -1
      ),
      
      # EJE X: Etnia (cE: 1 al 10) - Radares de Selva Activos
      cE = case_when(
        str_detect(ETN, "AFRO.*") ~ 1, str_detect(ETN, "BLANC.*") ~ 2, 
        str_detect(ETN, "IND.*GENA") ~ 3, str_detect(ETN, "MESTIZ.*") ~ 4, 
        str_detect(ETN, "MONTUB.*|MONTUV.*") ~ 5, str_detect(ETN, "MULAT.*") ~ 6,
        str_detect(ETN, "NEGR.*") ~ 7, str_detect(ETN, "OTR.*") ~ 8,
        str_detect(ETN, "SABE.*|RESPONDE") ~ 9, 
        str_detect(ETN, "APLICA") | ETN == "" | is.na(ETN) ~ 10,
        TRUE ~ -1
      ),
      
      # EJE X: Nacionalidad (cN: 1 al 16) - Radares de Selva Activos
      cN = case_when(
        str_detect(NAC, "ACHUAR.*") ~ 1, str_detect(NAC, "ANDOA.*") ~ 2,
        str_detect(NAC, "AWA.*|AW.*") ~ 3, str_detect(NAC, "CHACHI.*") ~ 4,
        str_detect(NAC, "COFAN.*|COF.*N.*") ~ 5, str_detect(NAC, "EPERA.*") ~ 6,
        str_detect(NAC, "HUANCAVILCA.*") ~ 7, str_detect(NAC, "KICHWA.*|QUICHUA.*") ~ 8,
        str_detect(NAC, "SECOYA.*") ~ 9, str_detect(NAC, "SHIWIAR.*") ~ 10,
        str_detect(NAC, "SHUAR.*") ~ 11, str_detect(NAC, "TS.*CHILA.*") ~ 12,
        str_detect(NAC, "WAORANI.*|HUAORANI.*") ~ 13, str_detect(NAC, "ZAPARA.*") ~ 14,
        str_detect(NAC, "SABE.*|RESPONDE") ~ 15,
        str_detect(NAC, "APLICA") | NAC == "" | is.na(NAC) ~ 16,
        TRUE ~ -1
      ),
      
      # EJE X: Pueblo (cP: 1 al 20) - Radares de Selva Activos
      cP = case_when(
        str_detect(PUE, "CHIBULEO.*") ~ 1, str_detect(PUE, "HUANCAVILCA.*") ~ 2,
        str_detect(PUE, "KA.*ARI.*|CA.*ARI.*") ~ 3, str_detect(PUE, "KARANKI.*") ~ 4,
        str_detect(PUE, "KAYAMBI.*") ~ 5, str_detect(PUE, "KICHWA AMAZ.*") ~ 6,
        str_detect(PUE, "KISAPINCHA.*|QUISAPINCHA.*") ~ 7, str_detect(PUE, "KITUKARA.*") ~ 8,
        str_detect(PUE, "NATABUELA.*") ~ 9, str_detect(PUE, "OTAVALO.*") ~ 10,
        str_detect(PUE, "PALTAS.*") ~ 11, str_detect(PUE, "PANZALEO.*") ~ 12,
        str_detect(PUE, "PASTOS.*") ~ 13, str_detect(PUE, "PURUH.*") ~ 14,
        str_detect(PUE, "SALASA.*") ~ 15, str_detect(PUE, "SARAGURO.*") ~ 16,
        str_detect(PUE, "TOMABELA.*") ~ 17, str_detect(PUE, "WARAMKA.*|GUARANCA.*") ~ 18,
        str_detect(PUE, "SABE.*|RESPONDE") ~ 19,
        str_detect(PUE, "APLICA") | PUE == "" | is.na(PUE) ~ 20,
        TRUE ~ -1
      )
    ) %>%
    filter(rD != -1)
  
  # ====================================================================
  # BIFURCACIÓN DE DEDUPLICACIÓN (La clave del rigor estadístico)
  # ====================================================================
  # ⚠️ AVISO ARQUITECTURA: Mantiene conteo de discapacidades separadas de personas.
  base_tipos     <- base_clasificada %>% distinct(ID, rD, .keep_all = TRUE)
  base_pacientes <- base_clasificada %>% distinct(ID, .keep_all = TRUE)
  
  # ====================================================================
  # 4. CONSTRUCCIÓN DE MATRICES (Valores nulos puros para UI de Sheets)
  # ====================================================================
  mat_tipo <- matrix(NA_real_, nrow = 6, ncol = 2)
  mat_etn  <- matrix(NA_real_, nrow = 10, ncol = 2)
  mat_nac  <- matrix(NA_real_, nrow = 16, ncol = 2)
  mat_pue  <- matrix(NA_real_, nrow = 20, ncol = 2)
  
  # Llenado Matriz Tipos (6) - Usa base_tipos
  for(i in 1:6) {
    h_c <- sum(base_tipos$rD == i & !base_tipos$esMujer, na.rm = TRUE)
    m_c <- sum(base_tipos$rD == i & base_tipos$esMujer, na.rm = TRUE)
    if(h_c > 0) mat_tipo[i, 1] <- h_c
    if(m_c > 0) mat_tipo[i, 2] <- m_c
  }
  
  # Llenado Matriz Etnia (10) - Usa base_pacientes
  for(i in 1:10) {
    h_c <- sum(base_pacientes$cE == i & !base_pacientes$esMujer, na.rm = TRUE)
    m_c <- sum(base_pacientes$cE == i & base_pacientes$esMujer, na.rm = TRUE)
    if(h_c > 0) mat_etn[i, 1] <- h_c
    if(m_c > 0) mat_etn[i, 2] <- m_c
  }
  
  # Llenado Matriz Nacionalidad (16) - Usa base_pacientes
  for(i in 1:16) {
    h_c <- sum(base_pacientes$cN == i & !base_pacientes$esMujer, na.rm = TRUE)
    m_c <- sum(base_pacientes$cN == i & base_pacientes$esMujer, na.rm = TRUE)
    if(h_c > 0) mat_nac[i, 1] <- h_c
    if(m_c > 0) mat_nac[i, 2] <- m_c
  }
  
  # Llenado Matriz Pueblo (20) - Usa base_pacientes
  for(i in 1:20) {
    h_c <- sum(base_pacientes$cP == i & !base_pacientes$esMujer, na.rm = TRUE)
    m_c <- sum(base_pacientes$cP == i & base_pacientes$esMujer, na.rm = TRUE)
    if(h_c > 0) mat_pue[i, 1] <- h_c
    if(m_c > 0) mat_pue[i, 2] <- m_c
  }
  
  # ====================================================================
  # 5. ESCRITURA EN EXCEL (Data Warehouse Apilado - Nube Nativa)
  # ====================================================================
  
  col_destino <- paste0("DISCAPACIDADES!", letra_h)
  
  # ⚠️ AVISO ARQUITECTURA: Homologación a range_write para inyección segura en la API
  range_write(ss = id_google_sheet, data = as.data.frame(mat_tipo), range = paste0(col_destino, "16"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(mat_etn),  range = paste0(col_destino, "26"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(mat_nac),  range = paste0(col_destino, "40"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = as.data.frame(mat_pue),  range = paste0(col_destino, "61"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 10 finalizado. Matrices apiladas con deduplicación epidemiológica pura.\n")
}
