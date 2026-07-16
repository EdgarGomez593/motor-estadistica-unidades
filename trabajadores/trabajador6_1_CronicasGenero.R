# ====================================================================================
# TRABAJADOR 6.1: CRÓNICAS Y CÁNCER POR GÉNERO
# Arquitectura: Base completa -> Morbilidad -> Especialidad -> Género (Doble Columna)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador6_1_CronicasGenero <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🧬 Trabajador 6.1 activado: Desagregando Crónicas y Cáncer por Género...\n")
  
  # 1. MAPA DE MESES (DOBLE COLUMNA)
  mes_anterior <- mes_objetivo
  
  # 'ENE': 2, 'FEB': 4, 'MAR': 6... Esto es (Mes * 2)
  col_hombres_num <- mes_anterior * 2
  col_mujeres_num <- col_hombres_num + 1
  
  letras_columnas <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
  letra_h <- letras_columnas[col_hombres_num]
  letra_m <- letras_columnas[col_mujeres_num]
  
  # 2. CATÁLOGOS Y BLINDAJE
  esp_validas <- "MEDICINA FAMILIAR Y COMUNITARIA|MEDICINA GENERAL|MEDICINA RURAL"
  codigos_cancer <- "C169|C509|C61X|C539"
  
  nombres_cols <- names(tabla_consolidada)
  if (!"ATEMED_CON_DIAG" %in% nombres_cols) tabla_consolidada$ATEMED_CON_DIAG <- ""
  if (!"PCTE_SEXO" %in% nombres_cols) tabla_consolidada$PCTE_SEXO <- ""
  
  base_limpia <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    mutate(
      ESP = str_squish(str_remove_all(toupper(PROF_ESP_ATE), "[\r\n\"]")),
      CIE = str_squish(str_remove_all(toupper(ATEMED_CIE10), "[\r\n\"\\.]")),
      TIP = str_squish(str_remove_all(toupper(ATEMED_TIP_DIAG), "[\r\n\"]")),
      CR = str_squish(str_remove_all(toupper(ATEMED_CRON_DIAG), "[\r\n\"]")),
      CON = str_squish(str_remove_all(toupper(ATEMED_CON_DIAG), "[\r\n\"]")),
      SEX = str_squish(str_remove_all(toupper(PCTE_SEXO), "[\r\n\"]"))
    ) %>%
    # Filtros estructurales
    filter(str_detect(TIP, "MORBILIDAD")) %>%
    filter(str_detect(ESP, esp_validas))
  
  # 3. IDENTIFICACIÓN PATOLÓGICA Y GÉNERO
  base_identificada <- base_limpia %>%
    mutate(
      esHTA = str_starts(CIE, "I10"),
      esOB = str_starts(CIE, "E669"),
      esDiabetes = str_starts(CIE, "E10|E11|E12|E13|E14"),
      esCancer = str_starts(CIE, codigos_cancer),
      
      esHombre = (SEX == "HOMBRE" | SEX == "M" | SEX == "MASCULINO"),
      esMujer = (SEX == "MUJER" | SEX == "F" | SEX == "FEMENINO"),
      
      esPrimera = str_detect(CR, "PRIMER|^P$|^1$|^N$"),
      esSubse = str_detect(CR, "SUBSEC|^S$|^2$"),
      esDefInicial = str_detect(CON, "DEFINITIVO INICIAL")
    ) %>%
    # Nos quedamos solo con pacientes con género identificado
    filter(esHombre | esMujer)
  
  # 4. EXTRACCIÓN Y CONTEO (Primeras y Subsecuentes por Género)
  conteo <- function(filtro_temporal, patologia, genero) {
    nrow(base_identificada %>% filter(!!sym(filtro_temporal) & !!sym(patologia) & !!sym(genero)))
  }
  
  base_identificada$esPrimeraInicial <- base_identificada$esPrimera & base_identificada$esDefInicial
  
  p_hta_h <- conteo("esPrimeraInicial", "esHTA", "esHombre"); p_hta_m <- conteo("esPrimeraInicial", "esHTA", "esMujer")
  p_dia_h <- conteo("esPrimeraInicial", "esDiabetes", "esHombre"); p_dia_m <- conteo("esPrimeraInicial", "esDiabetes", "esMujer")
  p_obe_h <- conteo("esPrimeraInicial", "esOB", "esHombre"); p_obe_m <- conteo("esPrimeraInicial", "esOB", "esMujer")
  p_can_h <- conteo("esPrimeraInicial", "esCancer", "esHombre"); p_can_m <- conteo("esPrimeraInicial", "esCancer", "esMujer")
  
  s_hta_h <- conteo("esSubse", "esHTA", "esHombre"); s_hta_m <- conteo("esSubse", "esHTA", "esMujer")
  s_dia_h <- conteo("esSubse", "esDiabetes", "esHombre"); s_dia_m <- conteo("esSubse", "esDiabetes", "esMujer")
  s_obe_h <- conteo("esSubse", "esOB", "esHombre"); s_obe_m <- conteo("esSubse", "esOB", "esMujer")
  s_can_h <- conteo("esSubse", "esCancer", "esHombre"); s_can_m <- conteo("esSubse", "esCancer", "esMujer")
  
  # 5. ESCRITURA EN EXCEL
  fc <- function(x) { if (x > 0) return(x) else return("") }
  
  # Primeras
  range_write(ss = id_google_sheet, data = data.frame(fc(p_hta_h)), range = paste0("EMF. CRONICAS!", letra_h, "94"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(p_hta_m)), range = paste0("EMF. CRONICAS!", letra_m, "94"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(p_dia_h)), range = paste0("EMF. CRONICAS!", letra_h, "95"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(p_dia_m)), range = paste0("EMF. CRONICAS!", letra_m, "95"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(p_obe_h)), range = paste0("EMF. CRONICAS!", letra_h, "96"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(p_obe_m)), range = paste0("EMF. CRONICAS!", letra_m, "96"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(p_can_h)), range = paste0("EMF. CRONICAS!", letra_h, "97"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(p_can_m)), range = paste0("EMF. CRONICAS!", letra_m, "97"), col_names = FALSE, reformat = FALSE)
  
  # Subsecuentes
  range_write(ss = id_google_sheet, data = data.frame(fc(s_hta_h)), range = paste0("EMF. CRONICAS!", letra_h, "102"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(s_hta_m)), range = paste0("EMF. CRONICAS!", letra_m, "102"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(s_dia_h)), range = paste0("EMF. CRONICAS!", letra_h, "103"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(s_dia_m)), range = paste0("EMF. CRONICAS!", letra_m, "103"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(s_obe_h)), range = paste0("EMF. CRONICAS!", letra_h, "104"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(s_obe_m)), range = paste0("EMF. CRONICAS!", letra_m, "104"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(s_can_h)), range = paste0("EMF. CRONICAS!", letra_h, "105"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(s_can_m)), range = paste0("EMF. CRONICAS!", letra_m, "105"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 6.1 finalizado. Matriz de género y cáncer completada.\n")
}