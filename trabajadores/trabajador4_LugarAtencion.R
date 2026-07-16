# ====================================================================================
# TRABAJADOR 4: LUGAR DE ATENCIÓN (INTRAMURAL / EXTRAMURAL)
# Arquitectura: Deduplicación exacta por ATEMED_ID -> Filtro Especialidad -> Conteo
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador4_LugarAtencion <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🏥 Trabajador 4 activado: Calculando Atenciones Intramurales y Extramurales...\n")
  
  # 1. MAPA DE MESES
  mes_anterior <- mes_objetivo
  
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  # 2. DEFINICIÓN DE CATÁLOGOS (Tus raíces para atrapar variaciones)
  especialidades_validas <- "MEDICIN|MÉDIC|OBSTETR|PSICOLOG|ODONTO"
  
  # Blindaje de columnas por si vienen vacías en algún archivo
  nombres_cols <- names(tabla_consolidada)
  if (!"ATEMED_TIP_ATE" %in% nombres_cols) tabla_consolidada$ATEMED_TIP_ATE <- ""
  if (!"PROF_ESP_ATE" %in% nombres_cols) tabla_consolidada$PROF_ESP_ATE <- ""
  
  # ====================================================================
  # PASO 1: LA PURGA INICIAL Y DEDUPLICACIÓN ESTRICTA
  # ====================================================================
  base_limpia <- tabla_consolidada %>%
    # Filtramos filas vacías
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    # DEDUPLICACIÓN: Conservamos solo el primer registro de cada ticket
    distinct(ATEMED_ID, .keep_all = TRUE) %>%
    mutate(
      ESP = str_remove_all(str_trim(toupper(PROF_ESP_ATE)), "[\r\n\"]"),
      TIPO_ATE = str_remove_all(str_trim(toupper(ATEMED_TIP_ATE)), "[\r\n\"]")
    )
  
  # ====================================================================
  # PASO 2: FILTRADO DE ESPECIALIDAD Y CONTEO INTRA/EXTRA
  # ====================================================================
  # Filtramos solo las atenciones que coincidan con las 4 ramas solicitadas
  base_filtrada <- base_limpia %>%
    filter(str_detect(ESP, especialidades_validas))
  
  # Conteo directo
  intra <- nrow(base_filtrada %>% filter(str_detect(TIPO_ATE, "INTRAMURAL")))
  extra <- nrow(base_filtrada %>% filter(str_detect(TIPO_ATE, "EXTRAMURAL")))
  
  cat("   -> Total Intramurales:", intra, "\n")
  cat("   -> Total Extramurales:", extra, "\n")
  
  # Función para dejar la celda limpia ("") si el valor es 0
  v_intra <- if (intra > 0) intra else ""
  v_extra <- if (extra > 0) extra else ""
  
  # ====================================================================
  # 3. ESCRITURA EN GOOGLE SHEETS
  # ====================================================================
  # Fila 9: Intramural | Fila 10: Extramural (Pestaña: EMF. CRONICAS)
  rango_intra <- paste0("EMF. CRONICAS!", letra_col, "9")
  rango_extra <- paste0("EMF. CRONICAS!", letra_col, "10")
  
  range_write(ss = id_google_sheet, data = data.frame(v_intra), range = rango_intra, col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_extra), range = rango_extra, col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 4 finalizado. Matriz de Lugar de Atención actualizada.\n")
}