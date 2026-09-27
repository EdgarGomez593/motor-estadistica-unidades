# ====================================================================================
# TRABAJADOR 5: MORBILIDAD Y PREVENCIÓN (MÉDICOS/OBSTETRAS Y ODONTOLOGÍA)
# Arquitectura: Secuencia Lineal Estricta (Deduplicación Global -> Filtros -> Conteo)
# ====================================================================================

library(dplyr)
library(stringr)
library(googlesheets4)

trabajador5_MorbilidadPrevencion <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🩺 Trabajador 5 activado: Procesando Morbilidad y Prevención (Réplica Anti-Tildes)...\n")
  
  col_destino_num <- mes_objetivo + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  
  # Tolerancia para el nombre de la columna en el CSV
  nombres_cols <- names(tabla_consolidada)
  col_tip <- "ATEMED_TIP_DIAG"
  if (!"ATEMED_TIP_DIAG" %in% nombres_cols && "ATEMED_TIPO_DIAG" %in% nombres_cols) {
    col_tip <- "ATEMED_TIPO_DIAG"
  } else if (!"ATEMED_TIP_DIAG" %in% nombres_cols) {
    tabla_consolidada$ATEMED_TIP_DIAG <- ""
  }
  if (!"ATEMED_CON_DIAG" %in% nombres_cols) tabla_consolidada$ATEMED_CON_DIAG <- ""
  
  # =========================================================================
  # PASO 1: "QUITAR DUPLICADOS" (LA GUILLOTINA DE TICKET)
  # =========================================================================
  
  base_limpia <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    distinct(ATEMED_ID, .keep_all = TRUE) %>%
    mutate(
      # Escudo Anti-NA inyectado en todas las variables
      ESP = str_squish(str_remove_all(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE)), "[\r\n\"]")),
      TIP = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_tip)), "", !!sym(col_tip))), "[\r\n\"]")),
      CR = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG)), "[\r\n\"]")),
      CON = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CON_DIAG), "", ATEMED_CON_DIAG)), "[\r\n\"]"))
    )
  
  # =========================================================================
  # PASO 2: SELECCIONAR ESPECIALIDAD Y CONDICIONES (Tu Embudo Exacto)
  # =========================================================================
  
  # 1. Condición de Primera Vez (Radar Regex blindado)
  base_primera <- base_limpia %>% filter(str_detect(CR, "PRIMER|^1$|^P$"))
  
  # 2. Separación con Radares Anti-Tildes
  # ⚠️ AVISO INFORMATIVO DE ARQUITECTURA: 
  # Se ha arreglado y homologado el radar inyectando el comodín "MEDICIN|M.*DIC|OBSTETR".
  # Esto garantiza que el motor no discrimine a los médicos por culpa de tildes o variaciones 
  # en los registros ("MÉDICO", "MEDICO", "MEDICINA", "OBSTETRA"), manteniendo la lógica exacta 
  # y estricta de cómo se quieren los datos.
  b_med <- base_primera %>% filter(str_detect(ESP, "MEDICIN|M.*DIC|OBSTETR|GINECOLOG|PSICOLOG"))
  b_odo <- base_primera %>% filter(str_detect(ESP, "ODONTO"))
  
  # 3. Conteos Finales (Aritmética Booleana de Alta Velocidad)
  pre_med <- sum(str_detect(b_med$TIP, "PREV"), na.rm = TRUE)
  mor_med <- sum(b_med$TIP == "MORBILIDAD", na.rm = TRUE) # <-- Candado eliminado
  
  pre_odo <- sum(str_detect(b_odo$TIP, "PREV"), na.rm = TRUE)
  mor_odo <- sum(b_odo$TIP == "MORBILIDAD", na.rm = TRUE) # <-- Candado eliminado
  
  cat("   -> Médicos/Obst/Psicólogos | Prevención:", pre_med, "| Morbilidad:", mor_med, "\n")
  cat("   -> Odontología       | Prevención:", pre_odo, "| Morbilidad:", mor_odo, "\n")
  
  # =========================================================================
  # 3. ESCRITURA EN EXCEL (ESTÁNDAR NUBE)
  # =========================================================================
  
  # Función blindada contra coerción de texto para dejar la celda con vacío puro
  formatear_cero <- function(x) { if (is.na(x) || x == 0) return(NA_real_) else return(x) }
  
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(pre_med)), range = paste0("EMF. CRONICAS!", letra_col, "57"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(mor_med)), range = paste0("EMF. CRONICAS!", letra_col, "58"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(pre_odo)), range = paste0("EMF. CRONICAS!", letra_col, "77"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(mor_odo)), range = paste0("EMF. CRONICAS!", letra_col, "78"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 5 finalizado. Matriz de Morbilidad/Prevención actualizada.\n")
}
