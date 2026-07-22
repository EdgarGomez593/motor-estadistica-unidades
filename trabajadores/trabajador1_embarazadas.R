# ====================================================================================
# TRABAJADOR 1: CENSO OBSTÉTRICO (SOLO TOTALES: PRIMERAS Y SUBSECUENTES)
# Arquitectura: Base Completa -> Conteo Censo -> Filas 2 y 3
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador1_Embarazadas <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🤰 Trabajador 1 activado: Calculando Censo Obstétrico (Totales)...\n")
  
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
  
  # 3. FILTRADO QUIRÚRGICO DE EMBARAZADAS (Z321, Z34, Z35)
  datos_filtrados <- tabla_consolidada %>%
    select(CIE = ATEMED_CIE10, TIP = all_of(col_tipo_diag), CRO = ATEMED_CRON_DIAG) %>%
    mutate(
      CIE = str_remove_all(str_trim(toupper(CIE)), "[\r\n\"\\.]"),
      TIP = str_remove_all(str_trim(toupper(TIP)), "[\r\n\"]"),
      CRO = str_remove_all(str_trim(toupper(CRO)), "[\r\n\"]")
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
  # 5. ESCRITURA EN GOOGLE SHEETS (Solo dispara a filas 2 y 3)
  # ====================================================================
  rango_primeras <- paste0("Censo_Obstetrico!", letra_col, "2")
  rango_subsecuentes <- paste0("Censo_Obstetrico!", letra_col, "3")
  
  range_write(ss = id_google_sheet, data = df_primeras, range = rango_primeras, col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = df_subsecuentes, range = rango_subsecuentes, col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 1 finalizado. Solo Totales (Filas 2 y 3) guardados.\n")
}
