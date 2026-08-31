# ====================================================================================
# TRABAJADOR 2: OBSTETRICIA EDADES Y VULNERABILIDAD (ESTÁNDAR OFICIAL MSP)
# Arquitectura: Purga por Ticket -> Filtro Especialidad Exacto -> Catálogo 14 Códigos
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador2_General <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("📊 Trabajador 2 activado: Analizando Obstetricia General y Vulnerabilidad...\n")
  
  # 1. MAPA DE MESES
  mes_anterior <- mes_objetivo
  
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  # ====================================================================
  # 2. DEFINICIÓN DE CATÁLOGOS OFICIALES (Ficha Metodológica)
  # ====================================================================
  especialidades_validas <- "MEDICINA GENERAL INTEGRAL|MEDICINA FAMILIAR Y COMUNITARIA|MEDICINA RURAL|MEDICO RURAL|MÉDICO RURAL|GINECOLOGÍA Y OBSTETRICIA|GINECOLOGIA Y OBSTETRICIA|ATENCIÓN PRIMARIA EN SALUD|ATENCION PRIMARIA EN SALUD|OBSTETRICIA|OBSTETRICIA RURAL|MEDICINA GENERAL"
  
  codigos_emb <- "Z340|Z348|Z349|Z350|Z351|Z352|Z353|Z354|Z355|Z356|Z357|Z358|Z359|Z321"
  
  # Blindaje de columnas (Por si algún archivo mensual no trae cédula o discapacidad)
  nombres_cols <- names(tabla_consolidada)
  if (!"PCTE_DISC" %in% nombres_cols) tabla_consolidada$PCTE_DISC <- "NO"
  if (!"PCTE_IDE" %in% nombres_cols) tabla_consolidada$PCTE_IDE <- ""
  if (!"ATEMED_CRON_DIAG" %in% nombres_cols) tabla_consolidada$ATEMED_CRON_DIAG <- ""
  if (!"PROF_ESP_ATE" %in% nombres_cols) tabla_consolidada$PROF_ESP_ATE <- ""
  
  # ====================================================================
  # PASO 1: LA PURGA INICIAL (1 Ticket = 1 Consulta)
  # ====================================================================
  base_limpia <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    distinct(ATEMED_ID, .keep_all = TRUE) %>%
    mutate(
      CIE = str_remove_all(str_trim(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10))), "[\r\n\"]"),
      ESP = str_remove_all(str_trim(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE))), "[\r\n\"]"),
      EDAD = as.numeric(PCTE_ANIOS), 
      CED = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_IDE), "", PCTE_IDE))), "[\r\n\"]"),
      DISC = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_DISC), "", PCTE_DISC))), "[\r\n\"]"),
      CRO = str_remove_all(str_trim(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG))), "[\r\n\"]")
    )
  
  # ====================================================================
  # PASO 2: FILTRADO CLÍNICO Y DEMOGRÁFICO
  # ====================================================================
  
  # Filtrar solo por las especialidades médicas/obstétricas permitidas
  base_especialidades <- base_limpia %>% 
    filter(str_detect(ESP, especialidades_validas))
  
  # -- BLOQUE 1: ATENCIONES DE EMBARAZO POR EDADES --
  base_embarazo <- base_especialidades %>% 
    filter(str_detect(CIE, codigos_emb) & !is.na(EDAD))
  
  m9 <- nrow(base_embarazo %>% filter(EDAD < 9))
  m1014 <- nrow(base_embarazo %>% filter(EDAD >= 10 & EDAD <= 14))
  m1519 <- nrow(base_embarazo %>% filter(EDAD >= 15 & EDAD <= 19))
  m40 <- nrow(base_embarazo %>% filter(EDAD >= 40))
  
  # Discapacidad: Filtramos SI, excluimos cédulas vacías, contamos la cédula una sola vez
  disc <- base_embarazo %>%
    filter(DISC == "SI" & CED != "") %>%
    distinct(CED) %>%
    nrow()
  
  # -- BLOQUE 2: PRIMER CONTROL POSTPARTO --
  post <- base_especialidades %>%
    filter(CIE == "Z392" & str_detect(CRO, "PRIMER|^P$|^1$|^N$")) %>%
    nrow()
  
  # ====================================================================
  # 3. PREPARACIÓN Y ESCRITURA EN GOOGLE SHEETS
  # ====================================================================
  
  formatear_cero <- function(x) { if (is.na(x)) return(NA_real_); if (x > 0) return(x) else return(NA_real_) }
  
  v_m9 <- formatear_cero(m9)
  v_m1014 <- formatear_cero(m1014)
  v_m1519 <- formatear_cero(m1519)
  v_disc <- formatear_cero(disc)
  v_post <- formatear_cero(post)
  v_m40 <- formatear_cero(m40)
  
  range_write(ss = id_google_sheet, data = data.frame(v_m9), range = paste0("Censo_Obstetrico!", letra_col, "14"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_m1014), range = paste0("Censo_Obstetrico!", letra_col, "15"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_m1519), range = paste0("Censo_Obstetrico!", letra_col, "16"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_disc), range = paste0("Censo_Obstetrico!", letra_col, "18"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_post), range = paste0("Censo_Obstetrico!", letra_col, "19"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_m40), range = paste0("Censo_Obstetrico!", letra_col, "20"), col_names = FALSE, reformat = FALSE)
  
  range_write(ss = id_google_sheet, data = data.frame(v_m9), range = "Censo_Obstetrico!A12", col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_m1014), range = "Censo_Obstetrico!B12", col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_m1519), range = "Censo_Obstetrico!C12", col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_disc), range = "Censo_Obstetrico!D12", col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_post), range = "Censo_Obstetrico!E12", col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 2 finalizado. Reporte demográfico obstétrico escrito con éxito.\n")
}
