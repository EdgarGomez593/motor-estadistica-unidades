# ====================================================================================
# TRABAJADOR 1: CENSO OBSTÉTRICO (CONTROL PRENATAL) - ESTÁNDAR OFICIAL MSP
# Arquitectura: Purga por Ticket -> Especialidad Oficial -> Catálogo 14 Códigos CIE-10
# ====================================================================================

library(dplyr)
library(stringr)
library(googlesheets4)

trabajador1_Embarazadas <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🤰 Trabajador 1 activado: Calculando Censo Obstétrico Oficial...\n")
  
  # 1. MAPA DE MESES
  mes_anterior <- mes_objetivo
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  # 2. BLINDAJE Y TOLERANCIA A CABECERAS
  nombres_cols <- names(tabla_consolidada)
  if (!"ATEMED_ID" %in% nombres_cols) tabla_consolidada$ATEMED_ID <- paste0("FILA_", row_number())
  if (!"PROF_ESP_ATE" %in% nombres_cols) tabla_consolidada$PROF_ESP_ATE <- ""
  if (!"ATEMED_CRON_DIAG" %in% nombres_cols) tabla_consolidada$ATEMED_CRON_DIAG <- ""
  
  # ====================================================================
  # CATÁLOGOS OFICIALES SEGÚN FICHA METODOLÓGICA MSP
  # ====================================================================
  especialidades_validas <- "MEDICINA GENERAL INTEGRAL|MEDICINA FAMILIAR Y COMUNITARIA|MEDICINA RURAL|MEDICO RURAL|MÉDICO RURAL|GINECOLOGÍA Y OBSTETRICIA|GINECOLOGIA Y OBSTETRICIA|ATENCIÓN PRIMARIA EN SALUD|ATENCION PRIMARIA EN SALUD|OBSTETRICIA|OBSTETRICIA RURAL|MEDICINA GENERAL"
  
  codigos_emb <- c("Z340", "Z348", "Z349", "Z350", "Z351", "Z352", "Z353", "Z354", "Z355", "Z356", "Z357", "Z358", "Z359", "Z321")
  
  # ====================================================================
  # PASO 1: PURIFICACIÓN Y FILTRADO QUIRÚRGICO DE LA BASE
  # ====================================================================
  base_procesada <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    mutate(
      TICKET = str_trim(ATEMED_ID),
      CIE = str_remove_all(str_trim(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10))), "[\r\n\"\\.]"),
      ESP = str_remove_all(str_trim(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE))), "[\r\n\"]"),
      CRO = str_remove_all(str_trim(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG))), "[\r\n\"]")
    ) %>%
    # Filtro estricto por Catálogo de 14 códigos y Especialidades Autorizadas
    filter(
      CIE %in% codigos_emb,
      str_detect(ESP, especialidades_validas)
    )
  
  # ====================================================================
  # PASO 2: PURGA POR TICKET (1 Ticket = 1 Consulta Real)
  # ====================================================================
  # Aislamos las primeras consultas y aplicamos distinct por ticket
  base_primeras <- base_procesada %>%
    filter(str_detect(CRO, "PRIMER|^P$|^1$|^N$")) %>%
    distinct(TICKET, .keep_all = TRUE)
  
  # Aislamos las subsecuentes y aplicamos distinct por ticket
  base_subsecuentes <- base_procesada %>%
    filter(str_detect(CRO, "SUBSEC|^S$|^2$")) %>%
    distinct(TICKET, .keep_all = TRUE)
  
  totalPrimeras <- nrow(base_primeras)
  totalSubsecuentes <- nrow(base_subsecuentes)
  
  # Validación de escritura para evitar Fantasmas en la nube
  valor_primeras <- if (totalPrimeras > 0) totalPrimeras else NA_real_
  valor_subsecuentes <- if (totalSubsecuentes > 0) totalSubsecuentes else NA_real_
  
  cat("    -> Total Primeras encontradas (Purificadas):", totalPrimeras, "\n")
  cat("    -> Total Subsecuentes encontradas (Purificadas):", totalSubsecuentes, "\n")
  
  # ====================================================================
  # 3. ESCRITURA EN EL EXCEL (Censo Obstétrico - Fila 2 y 3)
  # ====================================================================
  rango_primeras <- paste0("Censo_Obstetrico!", letra_col, "2")
  rango_subsecuentes <- paste0("Censo_Obstetrico!", letra_col, "3")
  
  range_write(ss = id_google_sheet, data = data.frame(valor_primeras), range = rango_primeras, col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(valor_subsecuentes), range = rango_subsecuentes, col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 1 finalizado con éxito. Datos guardados en la columna:", letra_col, "\n")
}
