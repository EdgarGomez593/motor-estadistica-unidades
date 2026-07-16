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
  
  # Catálogo Exacto
  condiciones_morbi <- c("DEFINITIVO INICIAL", "DEFINITIVO INICIAL CONFIRMADO POR LABORATORIO", "DEFINITIVO INICIAL CONFIRMADO")
  
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
      ESP = str_squish(str_remove_all(toupper(PROF_ESP_ATE), "[\r\n\"]")),
      TIP = str_squish(str_remove_all(toupper(!!sym(col_tip)), "[\r\n\"]")),
      CR = str_squish(str_remove_all(toupper(ATEMED_CRON_DIAG), "[\r\n\"]")),
      CON = str_squish(str_remove_all(toupper(ATEMED_CON_DIAG), "[\r\n\"]"))
    )
  
  # =========================================================================
  # PASO 2: SELECCIONAR ESPECIALIDAD Y CONDICIONES (Tu Embudo Exacto)
  # =========================================================================
  # 1. Condición de Primera Vez
  base_primera <- base_limpia %>% filter(CR %in% c("PRIMERA", "1"))
  
  # 2. Separación con Radares Anti-Tildes ("MEDICINA", "OBSTETRICIA", "ODONTO")
  b_med <- base_primera %>% filter(str_detect(ESP, "MEDICINA|OBSTETRICIA"))
  b_odo <- base_primera %>% filter(str_detect(ESP, "ODONTO"))
  
  # 3. Conteos Finales
  pre_med <- nrow(b_med %>% filter(str_detect(TIP, "PREV")))
  mor_med <- nrow(b_med %>% filter(TIP == "MORBILIDAD" & CON %in% condiciones_morbi))
  
  pre_odo <- nrow(b_odo %>% filter(str_detect(TIP, "PREV")))
  mor_odo <- nrow(b_odo %>% filter(TIP == "MORBILIDAD" & CON %in% condiciones_morbi))
  
  cat("   -> Médicos/Obstetras | Prevención:", pre_med, "| Morbilidad:", mor_med, "\n")
  cat("   -> Odontología       | Prevención:", pre_odo, "| Morbilidad:", mor_odo, "\n")
  
  # =========================================================================
  # 3. ESCRITURA EN EXCEL
  # =========================================================================
  formatear_cero <- function(x) { if (x > 0) return(x) else return("") }
  
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(pre_med)), range = paste0("EMF. CRONICAS!", letra_col, "57"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(mor_med)), range = paste0("EMF. CRONICAS!", letra_col, "58"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(pre_odo)), range = paste0("EMF. CRONICAS!", letra_col, "77"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(mor_odo)), range = paste0("EMF. CRONICAS!", letra_col, "78"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 5 finalizado. Matriz de Morbilidad/Prevención actualizada.\n")
}