# ====================================================================================
# TRABAJADOR 1: CENSO OBSTÉTRICO (CONTROL PRENATAL)
# Arquitectura: Base Completa -> Sin Guillotina (Suma de Filas) -> Prevención -> Z321/Z34/Z35
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador1_Embarazadas <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🤰 Trabajador 1 activado: Calculando Censo Obstétrico...\n")
  
  # 1. MAPA DE MESES (Equivalente a tu MAPA_MESES_COL)
  # R deduce el mes actual de forma automática para saber en qué columna escribir.
  mes_anterior <- mes_objetivo
  
  # Si Enero es la columna 2, Febrero la 3... La columna destino siempre es el número del mes + 1
  col_destino_num <- mes_anterior + 1
  
  # Convertimos el número a la letra de la columna de Excel (2 = B, 3 = C, etc.)
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  # 2. TOLERANCIA A CABECERAS (El mismo blindaje que tenías en Apps Script)
  nombres_cols <- names(tabla_consolidada)
  
  col_tipo_diag <- "ATEMED_TIP_DIAG"
  if (!("ATEMED_TIP_DIAG" %in% nombres_cols) && ("ATEMED_TIPO_DIAG" %in% nombres_cols)) {
    col_tipo_diag <- "ATEMED_TIPO_DIAG"
  }
  
  # 3. FILTRADO QUIRÚRGICO DE LA BASE COMPLETA
  datos_filtrados <- tabla_consolidada %>%
    # Seleccionamos solo las 3 columnas necesarias
    select(CIE = ATEMED_CIE10, TIP = all_of(col_tipo_diag), CRO = ATEMED_CRON_DIAG) %>%
    
    # Limpieza profunda (Elimina \r, \n, comillas dobles y espacios en blanco, igual que en tu código)
    mutate(
      CIE = str_remove_all(str_trim(toupper(CIE)), "[\r\n\"\\.]"),
      TIP = str_remove_all(str_trim(toupper(TIP)), "[\r\n\"]"),
      CRO = str_remove_all(str_trim(toupper(CRO)), "[\r\n\"]")
    ) %>%
    
    # FILTRO 1: TIPO DE DIAGNÓSTICO PREVENCIÓN (Equivalente a tip.indexOf("PREV") !== -1)
    filter(str_detect(TIP, "PREV")) %>%
    
    # FILTRO 2: CIE-10 Obstétrico (Z321, Z34, Z35)
    filter(CIE == "Z321" | str_starts(CIE, "Z34") | str_starts(CIE, "Z35")) %>%
    
    # FILTRO 3: MAPA CRONOLÓGICO RESTAURADO
    mutate(
      esPrimera = if_else(str_detect(CRO, "PRIMER") | CRO == "P" | CRO == "1" | CRO == "N", 1, 0),
      esSubsecuente = if_else(str_detect(CRO, "SUBSEC") | CRO == "S" | CRO == "2", 1, 0)
    )
  
  # 4. CONTEO DIRECTO
  totalPrimeras <- sum(datos_filtrados$esPrimera, na.rm = TRUE)
  totalSubsecuentes <- sum(datos_filtrados$esSubsecuente, na.rm = TRUE)
  
  # Validación de escritura: si es mayor a 0, escribe el número; si no, deja la celda en blanco ("")
  valor_primeras <- if (totalPrimeras > 0) totalPrimeras else ""
  valor_subsecuentes <- if (totalSubsecuentes > 0) totalSubsecuentes else ""
  
  cat("   -> Total Primeras encontradas:", totalPrimeras, "\n")
  cat("   -> Total Subsecuentes encontradas:", totalSubsecuentes, "\n")
  
  # 5. ESCRITURA EN EL EXCEL
  # Armamos el rango (Ej: "Censo_Obstetrico!H2")
  rango_primeras <- paste0("Censo_Obstetrico!", letra_col, "2")
  rango_subsecuentes <- paste0("Censo_Obstetrico!", letra_col, "3")
  
  # Inyectamos los datos. 'reformat = FALSE' protege los colores y bordes de tu matriz visual
  range_write(ss = id_google_sheet, data = data.frame(valor_primeras), range = rango_primeras, col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(valor_subsecuentes), range = rango_subsecuentes, col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 1 finalizado. Datos guardados en la columna:", letra_col, "\n")
}