# ====================================================================================
# TRABAJADOR 1: CENSO OBSTÉTRICO (CONTROL PRENATAL E INTERCULTURALIDAD)
# Arquitectura: Búsqueda Exacta con Comodín Regex (.) Anti-Corrupción
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador1_Embarazadas <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🤰 Trabajador 1 activado: Calculando Censo Obstétrico y Demografía...\n")
  
  # 1. MAPA DE COLUMNAS FRANCOTIRADOR
  col_destino_num <- mes_objetivo + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  # 2. BLINDAJE DE CABECERAS
  nombres_cols <- names(tabla_consolidada)
  col_tipo_diag <- "ATEMED_TIP_DIAG"
  if (!("ATEMED_TIP_DIAG" %in% nombres_cols) && ("ATEMED_TIPO_DIAG" %in% nombres_cols)) {
    col_tipo_diag <- "ATEMED_TIPO_DIAG"
  }
  
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  # 3. FILTRADO QUIRÚRGICO DE EMBARAZADAS (Z321, Z34, Z35)
  datos_filtrados <- tabla_consolidada %>%
    select(CIE = ATEMED_CIE10, TIP = all_of(col_tipo_diag), CRO = ATEMED_CRON_DIAG, ETN = PCTE_AUTID_ETN, NAC = PCTE_NAC_ETN, PUE = PCTE_PUEBLO) %>%
    mutate(
      CIE = str_remove_all(str_trim(toupper(CIE)), "[\r\n\"\\.]"),
      TIP = str_remove_all(str_trim(toupper(TIP)), "[\r\n\"]"),
      CRO = str_remove_all(str_trim(toupper(CRO)), "[\r\n\"]"),
      ETN = str_squish(str_remove_all(toupper(ETN), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(NAC), "[\r\n\"]")),
      PUE = str_squish(str_remove_all(toupper(PUE), "[\r\n\"]"))
    ) %>%
    filter(str_detect(TIP, "PREV")) %>%
    filter(CIE == "Z321" | str_starts(CIE, "Z34") | str_starts(CIE, "Z35")) %>%
    mutate(
      esPrimera = if_else(str_detect(CRO, "PRIMER") | CRO == "P" | CRO == "1" | CRO == "N", 1, 0),
      esSubsecuente = if_else(str_detect(CRO, "SUBSEC") | CRO == "S" | CRO == "2", 1, 0)
    )
  
  # 4. CONTEOS CLÁSICOS (Filas 2 y 3)
  totalPrimeras <- sum(datos_filtrados$esPrimera, na.rm = TRUE)
  totalSubsecuentes <- sum(datos_filtrados$esSubsecuente, na.rm = TRUE)
  
  df_primeras <- data.frame(VALOR = ifelse(totalPrimeras > 0, totalPrimeras, ""))
  df_subsecuentes <- data.frame(VALOR = ifelse(totalSubsecuentes > 0, totalSubsecuentes, ""))
  
  # ====================================================================
  # 5. CONTEO DEMOGRÁFICO ROBUSTO (Con Comodín '.' para esquivar errores)
  # ====================================================================
  
  # --- Bloque 1: Etnias (Filas 24-33) ---
  etn_embarazadas <- c(
    sum(str_detect(datos_filtrados$ETN, "AFRO"), na.rm=TRUE),                   # 24
    sum(str_detect(datos_filtrados$ETN, "BLANC"), na.rm=TRUE),                  # 25
    sum(str_detect(datos_filtrados$ETN, "IND.GENA"), na.rm=TRUE),               # 26 (¡MAGIA! Atrapa con o sin tilde)
    sum(str_detect(datos_filtrados$ETN, "MESTIZ"), na.rm=TRUE),                 # 27
    sum(str_detect(datos_filtrados$ETN, "MONTUB|MONTUV"), na.rm=TRUE),          # 28
    sum(str_detect(datos_filtrados$ETN, "MULAT"), na.rm=TRUE),                  # 29
    sum(str_detect(datos_filtrados$ETN, "NEGR"), na.rm=TRUE),                   # 30
    sum(str_detect(datos_filtrados$ETN, "APLICA"), na.rm=TRUE),                 # 31
    sum(str_detect(datos_filtrados$ETN, "SABE|RESPONDE"), na.rm=TRUE),          # 32
    sum(str_detect(datos_filtrados$ETN, "OTR"), na.rm=TRUE)                     # 33
  )
  
  # --- Bloque 2: Nacionalidad (Filas 37-43) ---
  nac_embarazadas <- c(
    sum(str_detect(datos_filtrados$NAC, "ACHUAR"), na.rm=TRUE),                 # 37
    sum(str_detect(datos_filtrados$NAC, "ANDOA"), na.rm=TRUE),                  # 38
    sum(str_detect(datos_filtrados$NAC, "KICHWA|QUICHUA"), na.rm=TRUE),         # 39
    sum(str_detect(datos_filtrados$NAC, "APLICA"), na.rm=TRUE),                 # 40
    sum(str_detect(datos_filtrados$NAC, "SABE|RESPONDE"), na.rm=TRUE),          # 41
    sum(str_detect(datos_filtrados$NAC, "SHUAR"), na.rm=TRUE),                  # 42
    sum(str_detect(datos_filtrados$NAC, "WAORANI|HUAORANI"), na.rm=TRUE)        # 43
  )
  
  # --- Bloque 3: Pueblo (Filas 47-56) ---
  pue_embarazadas <- c(
    sum(str_detect(datos_filtrados$PUE, "CHIBULEO"), na.rm=TRUE),               # 47
    sum(str_detect(datos_filtrados$PUE, "TOMABELA"), na.rm=TRUE),               # 48
    sum(str_detect(datos_filtrados$PUE, "KA.ARI|KANARI"), na.rm=TRUE),          # 49 (Comodín en la Ñ)
    sum(str_detect(datos_filtrados$PUE, "KARANKI"), na.rm=TRUE),                # 50
    sum(str_detect(datos_filtrados$PUE, "KAYAMBI"), na.rm=TRUE),                # 51
    sum(str_detect(datos_filtrados$PUE, "AMAZ.NIC"), na.rm=TRUE),               # 52 (Comodín en Amazónico)
    sum(str_detect(datos_filtrados$PUE, "KISAPINCHA|QUISAPINCHA"), na.rm=TRUE), # 53
    sum(str_detect(datos_filtrados$PUE, "NATABUELA"), na.rm=TRUE),              # 54
    sum(str_detect(datos_filtrados$PUE, "SABE|RESPONDE"), na.rm=TRUE),          # 55
    sum(str_detect(datos_filtrados$PUE, "OTAVALO"), na.rm=TRUE)                 # 56
  )
  
  # Transformación limpia (Oculta los ceros para mantener la hoja impecable)
  df_etn <- data.frame(VALOR = ifelse(etn_embarazadas > 0, etn_embarazadas, ""))
  df_nac <- data.frame(VALOR = ifelse(nac_embarazadas > 0, nac_embarazadas, ""))
  df_pue <- data.frame(VALOR = ifelse(pue_embarazadas > 0, pue_embarazadas, ""))
  
  # ====================================================================
  # 6. ESCRITURA EN GOOGLE SHEETS (Disparos por Bloques)
  # ====================================================================
  rango_primeras <- paste0("Censo_Obstetrico!", letra_col, "2")
  rango_subsecuentes <- paste0("Censo_Obstetrico!", letra_col, "3")
  
  rango_etn <- paste0("Censo_Obstetrico!", letra_col, "24")
  rango_nac <- paste0("Censo_Obstetrico!", letra_col, "37")
  rango_pue <- paste0("Censo_Obstetrico!", letra_col, "47")
  
  range_write(ss = id_google_sheet, data = df_primeras, range = rango_primeras, col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = df_subsecuentes, range = rango_subsecuentes, col_names = FALSE, reformat = FALSE)
  
  range_write(ss = id_google_sheet, data = df_etn, range = rango_etn, col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = df_nac, range = rango_nac, col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = df_pue, range = rango_pue, col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 1 finalizado. Censo y Demografía intercultural guardados.\n")
}
