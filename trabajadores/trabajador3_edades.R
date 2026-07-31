# ====================================================================================
# TRABAJADOR 3: ETNIA, NACIONALIDAD Y PUEBLO (ESTÁNDAR PROVINCIAL - 2 PASOS)
# Arquitectura: Base -> Filtro Preventivo/Primera -> Clasificadores Interculturales
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador3_Edades <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🌍 Trabajador 3 activado: Clasificando Etnia, Nacionalidad y Pueblo...\n")
  
  # ====================================================================
  # 1. MAPA DE MESES Y COORDENADAS
  # ====================================================================
  mes_anterior <- mes_objetivo
  
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q")
  letra_col <- letras_columnas[col_destino_num]
  letra_resumen <- "Q" # La columna 17 de tu código original
  
  # ====================================================================
  # 2. DEFINICIÓN DE CATÁLOGOS ORIGINALES
  # ====================================================================
  especialidades_validas <- "MEDICINA FAMILIAR Y COMUNITARIA|MEDICINA RURAL|OBSTETRICIA|PSICOLOGÍA CLÍNICA|PSICOLOGIA CLINICA|ODONTOLOGÍA|ODONTOLOGIA|ENFERMERÍA|ENFERMERIA"
  codigos_emb <- "Z351|Z353|Z359|Z358|Z348|Z340|Z356|Z352"
  
  # Blindaje de columnas (Exclusivamente cabeceras oficiales PRAS)
  nombres_cols <- names(tabla_consolidada)
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  # Tolerancia para ATEMED_TIP_DIAG
  col_tipo_diag <- "ATEMED_TIP_DIAG"
  if (!("ATEMED_TIP_DIAG" %in% nombres_cols) && ("ATEMED_TIPO_DIAG" %in% nombres_cols)) {
    col_tipo_diag <- "ATEMED_TIPO_DIAG"
  }
  
  # ====================================================================
  # PASO 1: LA PURGA INICIAL Y PREPARACIÓN DE COLUMNAS
  # ====================================================================
  
  base_limpia <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    
    # ⚠️ AVISO INFORMATIVO: EL ESCUDO ESTÁ APAGADO A PROPÓSITO
    # Por validaciones exhaustivas de RStudio, encender la purga (distinct) 
    # inflaba los datos de primeras atenciones en el censo. Se mantiene apagado 
    # para no perder diagnósticos secundarios vitales de embarazo.
    # distinct(ATEMED_ID, .keep_all = TRUE) %>%
    
    mutate(
      CR = str_remove_all(str_trim(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG))), "[\r\n\"]"),
      TP = str_remove_all(str_trim(toupper(ifelse(is.na(!!sym(col_tipo_diag)), "", !!sym(col_tipo_diag)))), "[\r\n\"]"),
      ES = str_remove_all(str_trim(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE))), "[\r\n\"]"),
      CI = str_remove_all(str_trim(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10))), "[\r\n\"\\.]"),
      ET = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_AUTID_ETN), "", PCTE_AUTID_ETN))), "[\r\n\"]"),
      NA_col = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_NAC_ETN), "", PCTE_NAC_ETN))), "[\r\n\"]"),
      PU = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_PUEBLO), "", PCTE_PUEBLO))), "[\r\n\"]")
    )
  
  # ====================================================================
  # PASO 2: FILTRADO ESTRICTO (Primera Atención, Preventiva y Embarazo)
  # ====================================================================
  # ⚠️ AVISO INFORMATIVO DE LÓGICA ESTADÍSTICA:
  # Estos candados (PREV y PRIMER control) vienen perfectos desde RStudio.
  # No se alteran para mantener la fidelidad de los datos consolidados provinciales.
  
  base_intercultural <- base_limpia %>%
    # FILTRO 1: TIPO DE DIAGNÓSTICO PREVENCIÓN (Adaptado a TP)
    filter(str_detect(TP, "PREV")) %>%
    
    # FILTRO 2: CIE-10 Obstétrico (Adaptado a CI)
    filter(CI == "Z321" | str_starts(CI, "Z34") | str_starts(CI, "Z35")) %>%
    
    # FILTRO 3: SOLO PRIMER CONTROL (Filtro estricto adaptado a CR)
    filter(str_detect(CR, "PRIMER") | CR == "P" | CR == "1" | CR == "N")
  
  # ====================================================================
  # PASO 3: APLICANDO CLASIFICADORES INTERCULTURALES (DICCIONARIO PROVINCIAL)
  # ====================================================================
  base_clasificada <- base_intercultural %>%
    mutate(
      iE = case_when(
        str_detect(ET, 'AFRO.*') ~ 0,
        str_detect(ET, 'BLANC.*') ~ 1,
        str_detect(ET, 'IND.*GENA') ~ 2,
        str_detect(ET, 'MESTIZ.*') ~ 3,
        str_detect(ET, 'MONTUBI.*|MONTUVI.*') ~ 4,
        str_detect(ET, 'MULAT.*') ~ 5,
        str_detect(ET, 'NEGR.*') ~ 6,
        str_detect(ET, 'OTR.*') ~ 7,
        str_detect(ET, 'APLICA') | ET == "" ~ 8,
        str_detect(ET, 'SABE.*|RESPONDE') ~ 9,
        TRUE ~ -1
      ),
      iN = case_when(
        str_detect(NA_col, 'ACHUAR') ~ 0,
        str_detect(NA_col, 'ANDOA') ~ 1,
        str_detect(NA_col, 'AWA') ~ 2,
        str_detect(NA_col, 'CHACHI') ~ 3,
        str_detect(NA_col, 'COFAN') ~ 4,
        str_detect(NA_col, 'EPERA') ~ 5,
        str_detect(NA_col, 'HUANCAVILCA') ~ 6,
        str_detect(NA_col, 'KICHWA') ~ 7,
        str_detect(NA_col, 'SECOYA') ~ 8,
        str_detect(NA_col, 'SHIWIAR') ~ 9,
        str_detect(NA_col, 'SHUAR') ~ 10,
        str_detect(NA_col, 'TS.*CHILA') ~ 11,
        str_detect(NA_col, 'WAORANI|HUAORANI') ~ 12,
        str_detect(NA_col, 'ZAPARA') ~ 13,
        str_detect(NA_col, 'APLICA') | NA_col == "" ~ 14, 
        str_detect(NA_col, 'SABE.*|RESPONDE') ~ 15,
        TRUE ~ -1
      ),
      iP = case_when(
        str_detect(PU, 'CHIBULEO') ~ 0,
        str_detect(PU, 'HUANCAVILCA') ~ 1,
        str_detect(PU, 'KA.*ARI|CA.*ARI') ~ 2,
        str_detect(PU, 'KARANKI|CARANQUI') ~ 3,
        str_detect(PU, 'KAYAMBI|CAYAMBI') ~ 4,
        str_detect(PU, 'KICHWA AMAZ.*') ~ 5,
        str_detect(PU, 'KISAPINCHA|QUISAPINCHA') ~ 6,
        str_detect(PU, 'KITUKARA|QUITUCARA') ~ 7,
        str_detect(PU, 'NATABUELA') ~ 8,
        str_detect(PU, 'OTAVALO') ~ 9,
        str_detect(PU, 'PALTAS') ~ 10,
        str_detect(PU, 'PANZALEO') ~ 11,
        str_detect(PU, 'PASTOS') ~ 12,
        str_detect(PU, 'PURUH.*') ~ 13,
        str_detect(PU, 'SALASA.*') ~ 14,
        str_detect(PU, 'SARAGURO') ~ 15,
        str_detect(PU, 'TOMABELA') ~ 16,
        str_detect(PU, 'WARAMKA|GUARANCA') ~ 17,
        str_detect(PU, 'APLICA') | PU == "" ~ 18, 
        str_detect(PU, 'SABE.*|RESPONDE') ~ 19,
        TRUE ~ -1
      )
    )
  
  # ====================================================================
  # PASO 4: CONTEO Y PREPARACIÓN DE MATRICES (VECTORES EXPANDIDOS)
  # ====================================================================
  
  cE <- numeric(11)
  cN <- numeric(17)
  cP <- numeric(21)
  
  for(i in 0:10) { cE[i+1] <- sum(base_clasificada$iE == i, na.rm = TRUE) }
  for(i in 0:16) { cN[i+1] <- sum(base_clasificada$iN == i, na.rm = TRUE) }
  for(i in 0:20) { cP[i+1] <- sum(base_clasificada$iP == i, na.rm = TRUE) }
  
  # Función vectorizada de alto rendimiento (Protege de coerción en la nube)
  formatear_ceros_vector <- function(v) { 
    v <- as.numeric(v)
    v[v == 0 | is.na(v)] <- NA_real_ 
    return(v)
  }
  
  v_Etnia <- formatear_ceros_vector(cE)
  v_Nac   <- formatear_ceros_vector(cN)
  v_Pueb  <- formatear_ceros_vector(cP)
  
  # ====================================================================
  # PASO 5: ESCRITURA MÚLTIPLE EN GOOGLE SHEETS (NUBE ESTÁNDAR)
  # ====================================================================
  
  # Escritura para el Mes Actual (Columna destino)
  range_write(ss = id_google_sheet, data = data.frame(v_Etnia), range = paste0("Censo_Obstetrico!", letra_col, "24"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_Nac),   range = paste0("Censo_Obstetrico!", letra_col, "38"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_Pueb),  range = paste0("Censo_Obstetrico!", letra_col, "58"), col_names = FALSE, reformat = FALSE)
  
  # Escritura del Resumen Final (Columna Q)
  range_write(ss = id_google_sheet, data = data.frame(v_Etnia), range = paste0("Censo_Obstetrico!", letra_resumen, "24"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_Nac),   range = paste0("Censo_Obstetrico!", letra_resumen, "38"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(v_Pueb),  range = paste0("Censo_Obstetrico!", letra_resumen, "58"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 3 finalizado. Matriz obstétrica intercultural actualizada en nuevas coordenadas.\n")
}
