# ====================================================================================
# TRABAJADOR 1: CENSO OBSTÉTRICO (CONTROL PRENATAL)
# Arquitectura: Base Completa -> Sin Guillotina (Suma de Filas) -> Prevención -> Z321/Z34/Z35
# ====================================================================================

library(dplyr)
library(stringr)
library(googlesheets4)

trabajador1_Embarazadas <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🤰 Trabajador 1 activado: Calculando Censo Obstétrico...\n")
  
  # 1. MAPA DE MESES (R deduce la columna destino de forma automática)
  mes_anterior <- mes_objetivo
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  # 2. TOLERANCIA A CABECERAS (Blindaje contra variaciones del sistema matriz)
  nombres_cols <- names(tabla_consolidada)
  col_tipo_diag <- "ATEMED_TIP_DIAG"
  if (!("ATEMED_TIP_DIAG" %in% nombres_cols) && ("ATEMED_TIPO_DIAG" %in% nombres_cols)) {
    col_tipo_diag <- "ATEMED_TIPO_DIAG"
  }
  
  # 3. FILTRADO QUIRÚRGICO DE LA BASE COMPLETA (Con Escudo Anti-NA)
  datos_filtrados <- tabla_consolidada %>%
    select(CIE = ATEMED_CIE10, TIP = all_of(col_tipo_diag), CRO = ATEMED_CRON_DIAG) %>%
    
    # Limpieza profunda con escudo contra nulos absolutos antes de limpiar texto
    mutate(
      CIE = str_remove_all(str_trim(toupper(ifelse(is.na(CIE), "", CIE))), "[\r\n\"\\.]"),
      TIP = str_remove_all(str_trim(toupper(ifelse(is.na(TIP), "", TIP))), "[\r\n\"]"),
      CRO = str_remove_all(str_trim(toupper(ifelse(is.na(CRO), "", CRO))), "[\r\n\"]")
    ) %>%
    
    # FILTRO 1: TIPO DE DIAGNÓSTICO PREVENCIÓN
    filter(str_detect(TIP, "PREV")) %>%
    
    # FILTRO 2: CIE-10 Obstétrico (Z321, Z34, Z35)
    filter(CIE == "Z321" | str_starts(CIE, "Z34") | str_starts(CIE, "Z35")) %>%
    
    # FILTRO 3: MAPA CRONOLÓGICO RESTAURADO
    mutate(
      esPrimera = if_else(str_detect(CRO, "PRIMER") | CRO == "P" | CRO == "1" | CRO == "N", 1, 0),
      esSubsecuente = if_else(str_detect(CRO, "SUBSEC") | CRO == "S" | CRO == "2", 1, 0)
    )
  
  # 4. CONTEO DIRECTO (Con protección de coerción numérica pura)
  totalPrimeras <- sum(datos_filtrados$esPrimera, na.rm = TRUE)
  totalSubsecuentes <- sum(datos_filtrados$esSubsecuente, na.rm = TRUE)
  
  # Validación de escritura para evitar el Fantasma Borrador en la nube
  valor_primeras <- if (totalPrimeras > 0) totalPrimeras else NA_real_
  valor_subsecuentes <- if (totalSubsecuentes > 0) totalSubsecuentes else NA_real_
  
  cat("   -> Total Primeras encontradas:", totalPrimeras, "\n")
  cat("   -> Total Subsecuentes encontradas:", totalSubsecuentes, "\n")
  
  # ====================================================================
  # 5. ESCRITURA EN EL EXCEL (Estándar Nativo de Nube)
  # ====================================================================
  rango_primeras <- paste0("Censo_Obstetrico!", letra_col, "2")
  rango_subsecuentes <- paste0("Censo_Obstetrico!", letra_col, "3")
  
  # Usamos range_write con reformat = FALSE para proteger el diseño visual de la matriz
  range_write(ss = id_google_sheet, data = data.frame(valor_primeras), range = rango_primeras, col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(valor_subsecuentes), range = rango_subsecuentes, col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 1 finalizado. Datos guardados en la columna:", letra_col, "\n")
}
