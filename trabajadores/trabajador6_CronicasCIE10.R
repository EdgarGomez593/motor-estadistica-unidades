# ====================================================================================
# TRABAJADOR 6: ENFERMEDADES CRÓNICAS (TOTALES HTA, DIABETES, OBESIDAD)
# Arquitectura: Base completa (con duplicados) -> Morbilidad -> Especialidad -> Patologías
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador6_CronicasCIE10 <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🩸 Trabajador 6 activado: Procesando Totales de Crónicas...\n")
  
  # 1. MAPA DE MESES
  mes_anterior <- mes_objetivo
  
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  # 2. CATÁLOGOS Y BLINDAJE
  # ⚠️ AVISO INFORMATIVO DE ARQUITECTURA (CATÁLOGO):
  # Se mejoró el mapa original con precisión quirúrgica usando "." (un solo caracter).
  # Esto garantiza que atrape tanto "MEDICINA" como "MÉDICO" (o si la É se corrompe en la nube)
  # sin comprometer la misión ni atrapar especialidades no deseadas.
  esp_validas <- "MEDICINA GENERAL|M.DICO GENERAL|MEDICINA FAMILIAR|M.DICO FAMILIAR|MEDICINA RURAL|M.DICO RURAL|MEDICINA DEL TRABAJO|M.DICO DEL TRABAJO|MEDICINA OCUPACIONAL|M.DICO OCUPACIONAL"
  
  nombres_cols <- names(tabla_consolidada)
  if (!"ATEMED_CON_DIAG" %in% nombres_cols) tabla_consolidada$ATEMED_CON_DIAG <- ""
  
  base_limpia <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    # IMPORTANTE: Aquí NO hay distinct(ATEMED_ID) por instrucción de la arquitectura original
    mutate(
      ESP = str_squish(str_remove_all(toupper(PROF_ESP_ATE), "[\r\n\"]")),
      CIE = str_squish(str_remove_all(toupper(ATEMED_CIE10), "[\r\n\"\\.]")),
      TIP = str_squish(str_remove_all(toupper(ATEMED_TIP_DIAG), "[\r\n\"]")),
      CR = str_squish(str_remove_all(toupper(ATEMED_CRON_DIAG), "[\r\n\"]")),
      CON = str_squish(str_remove_all(toupper(ATEMED_CON_DIAG), "[\r\n\"]"))
    ) %>%
    # FILTRO ESTRICTO 1: Solo Morbilidad
    filter(str_detect(TIP, "MORBILIDAD"))
  
  # 3. IDENTIFICACIÓN PATOLÓGICA (Mapeo CIE-10)
  base_identificada <- base_limpia %>%
    mutate(
      esHTA = str_starts(CIE, "I10"),
      esOB = str_starts(CIE, "E669"),
      esDiabetes = str_starts(CIE, "E10|E11|E12|E13|E14"),
      esEspecialidad = str_detect(ESP, esp_validas),
      esPrimera = str_detect(CR, "PRIMER|^P$|^1$|^N$"),
      esSubse = str_detect(CR, "SUBSEC|^S$|^2$"),
      esDefInicial = str_detect(CON, "DEFINITIVO INICIAL"),
      esDefControl = str_detect(CON, "DEFINITIVO CONTROL")
    )
  
  # 4. EXTRACCIÓN DIRECCIONADA (Reglas de Negocio Originales)
  hta_p <- nrow(base_identificada %>% filter(esPrimera & esDefInicial & esEspecialidad & esHTA))
  dm_p  <- nrow(base_identificada %>% filter(esPrimera & esDefInicial & esEspecialidad & esDiabetes))
  ob_p  <- nrow(base_identificada %>% filter(esPrimera & esDefInicial & esEspecialidad & esOB))
  
  hta_s <- nrow(base_identificada %>% filter(esSubse & esDefControl & esEspecialidad & esHTA))
  ob_s  <- nrow(base_identificada %>% filter(esSubse & esDefControl & esEspecialidad & esOB))
  
  # TRAJE A LA MEDIDA: Diabetes subsecuente corre sin filtros extra de especialidad o control
  dm_s  <- nrow(base_identificada %>% filter(esSubse & esDiabetes))
  
  # 5. ESCRITURA EN EXCEL (ESTÁNDAR NUBE)
  
  # ⚠️ AVISO INFORMATIVO DE ARQUITECTURA (LÓGICA DE CEROS):
  # Por instrucción estricta de la dirección del proyecto, se mantiene el retorno de "" (texto vacío) 
  # original de RStudio para preservar la lógica visual de los reportes.
  formatear_cero <- function(x) { if (x > 0) return(x) else return("") }
  
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(hta_p)), range = paste0("EMF. CRONICAS!", letra_col, "15"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(dm_p)),  range = paste0("EMF. CRONICAS!", letra_col, "16"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(ob_p)),  range = paste0("EMF. CRONICAS!", letra_col, "17"), col_names = FALSE, reformat = FALSE)
  
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(hta_s)), range = paste0("EMF. CRONICAS!", letra_col, "22"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(dm_s)),  range = paste0("EMF. CRONICAS!", letra_col, "23"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(formatear_cero(ob_s)),  range = paste0("EMF. CRONICAS!", letra_col, "24"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 6 finalizado. Totales de crónicas actualizados.\n")
}
