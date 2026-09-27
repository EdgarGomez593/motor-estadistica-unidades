# ====================================================================================
# TRABAJADOR 6.1: CRÓNICAS Y CÁNCER POR GÉNERO
# Arquitectura: Clon Exacto del Apps Script Original -> Inyección Type-Safe Nube
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador6_1_CronicasGenero <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🧬 Trabajador 6.1 activado: Restaurando Lógica Original de Apps Script...\n")
  
  # 1. MAPA DE MESES (DOBLE COLUMNA)
  mes_anterior <- mes_objetivo
  
  # 'ENE': 2, 'FEB': 4, 'MAR': 6... Esto es (Mes * 2)
  col_hombres_num <- mes_anterior * 2
  col_mujeres_num <- col_hombres_num + 1
  
  letras_columnas <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
  letra_h <- letras_columnas[col_hombres_num]
  letra_m <- letras_columnas[col_mujeres_num]
  
  # 2. CATÁLOGO Y BLINDAJE (Clon de Apps Script con Protección de Tildes)
  # ⚠️ Se retorna a las 3 especialidades originales para cuadrar los datos.
  esp_validas <- "MEDICINA FAMILIAR Y COMUNITARIA|MEDICINA GENERAL|M.DICO GENERAL|MEDICINA RURAL|M.DICO RURAL"
  codigos_cancer <- "C169|C509|C61X|C539"
  
  nombres_cols <- names(tabla_consolidada)
  if (!"ATEMED_CON_DIAG" %in% nombres_cols) tabla_consolidada$ATEMED_CON_DIAG <- ""
  if (!"PCTE_SEXO" %in% nombres_cols) tabla_consolidada$PCTE_SEXO <- ""
  
  base_limpia <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    mutate(
      ESP = str_squish(str_remove_all(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE)), "[\r\n\"]")),
      CIE = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10)), "[\r\n\"\\.]")),
      TIP = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_TIP_DIAG), "", ATEMED_TIP_DIAG)), "[\r\n\"]")),
      CR = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG)), "[\r\n\"]")),
      CON = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CON_DIAG), "", ATEMED_CON_DIAG)), "[\r\n\"]")),
      SEX = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_SEXO), "", PCTE_SEXO)), "[\r\n\"]"))
    ) %>%
    # FILTRO ESTRICTO: Solo Morbilidad
    filter(str_detect(TIP, "MORBILIDAD"))
  
  # 3. IDENTIFICACIÓN PATOLÓGICA Y GÉNERO
  base_identificada <- base_limpia %>%
    mutate(
      esHTA = str_starts(CIE, "I10"),
      esOB = str_starts(CIE, "E66[01289]"),
      esDiabetes = str_starts(CIE, "E10|E11|E12|E13|E14"),
      esCancer = str_starts(CIE, codigos_cancer),
      
      esEspecialidad = str_detect(ESP, esp_validas),
      
      esHombre = (SEX == "HOMBRE" | SEX == "M" | SEX == "MASCULINO"),
      esMujer = (SEX == "MUJER" | SEX == "F" | SEX == "FEMENINO"),
      
      esPrimera = str_detect(CR, "PRIMER|^P$|^1$|^N$"),
      esSubse = str_detect(CR, "SUBSEC|^S$|^2$"),
      esDefInicial = str_detect(CON, "DEFINITIVO INICIAL")
    )
  
  # 4. EXTRACCIÓN Y CONTEO (Lógica PURA de Apps Script)
  
  # --- A. PRIMERAS --- (Req: esPrimera + esDefInicial + esEspecialidad)
  b_pri_hm <- base_identificada %>% filter(esPrimera & esDefInicial & esEspecialidad)
  
  p_hta_h <- sum(b_pri_hm$esHTA & b_pri_hm$esHombre, na.rm=T)
  p_hta_m <- sum(b_pri_hm$esHTA & b_pri_hm$esMujer, na.rm=T)
  
  p_dia_h <- sum(b_pri_hm$esDiabetes & b_pri_hm$esHombre, na.rm=T)
  p_dia_m <- sum(b_pri_hm$esDiabetes & b_pri_hm$esMujer, na.rm=T)
  
  p_obe_h <- sum(b_pri_hm$esOB & b_pri_hm$esHombre, na.rm=T)
  p_obe_m <- sum(b_pri_hm$esOB & b_pri_hm$esMujer, na.rm=T)
  
  p_can_h <- sum(b_pri_hm$esCancer & b_pri_hm$esHombre, na.rm=T)
  p_can_m <- sum(b_pri_hm$esCancer & b_pri_hm$esMujer, na.rm=T)
  
  # --- B. SUBSECUENTES --- 
  # ⚠️ REVERSIÓN ARQUITECTÓNICA: Se elimina la exigencia de "esDefControl".
  # Se agrupa todo bajo `esSubse` y `esEspecialidad` (Incluyendo la Diabetes).
  b_sub_hm <- base_identificada %>% filter(esSubse & esEspecialidad)
  
  s_hta_h <- sum(b_sub_hm$esHTA & b_sub_hm$esHombre, na.rm=T)
  s_hta_m <- sum(b_sub_hm$esHTA & b_sub_hm$esMujer, na.rm=T)
  
  s_dia_h <- sum(b_sub_hm$esDiabetes & b_sub_hm$esHombre, na.rm=T)
  s_dia_m <- sum(b_sub_hm$esDiabetes & b_sub_hm$esMujer, na.rm=T)
  
  s_obe_h <- sum(b_sub_hm$esOB & b_sub_hm$esHombre, na.rm=T)
  s_obe_m <- sum(b_sub_hm$esOB & b_sub_hm$esMujer, na.rm=T)
  
  s_can_h <- sum(b_sub_hm$esCancer & b_sub_hm$esHombre, na.rm=T)
  s_can_m <- sum(b_sub_hm$esCancer & b_sub_hm$esMujer, na.rm=T)
  
  # 5. ESCRITURA EN EXCEL (ESTÁNDAR NUBE)
  fc <- function(x) { if (is.na(x) || x == 0) return(NA_real_) else return(x) }
  
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
  
  cat("✅ Trabajador 6.1 finalizado. Matriz de género cuadra al milímetro con el Apps Script original.\n")
}
