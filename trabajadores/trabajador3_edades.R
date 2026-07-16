# ====================================================================================
# TRABAJADOR 3: ETNIA, NACIONALIDAD Y PUEBLO (ESTÁNDAR PROVINCIAL - 2 PASOS)
# Arquitectura: Base -> Filtro Preventivo/Primera -> Clasificadores Interculturales
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador3_Edades <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🌍 Trabajador 3 activado: Clasificando Etnia, Nacionalidad y Pueblo...\n")
  
  # 1. MAPA DE MESES
  mes_anterior <- mes_objetivo
  
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q")
  letra_col <- letras_columnas[col_destino_num]
  letra_resumen <- "Q" # La columna 17 de tu código original
  
  # 2. DEFINICIÓN DE CATÁLOGOS ORIGINALES
  especialidades_validas <- "MEDICINA FAMILIAR Y COMUNITARIA|MEDICINA RURAL|OBSTETRICIA|PSICOLOGÍA CLÍNICA|PSICOLOGIA CLINICA|ODONTOLOGÍA|ODONTOLOGIA|ENFERMERÍA|ENFERMERIA"
  codigos_emb <- "Z351|Z353|Z359|Z358|Z348|Z340|Z356|Z352"
  
  # Blindaje de columnas (Tolerancia a variaciones en la matriz)
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
    distinct(ATEMED_ID, .keep_all = TRUE) %>%
    mutate(
      CR = str_remove_all(str_trim(toupper(ATEMED_CRON_DIAG)), "[\r\n\"]"),
      TP = str_remove_all(str_trim(toupper(!!sym(col_tipo_diag))), "[\r\n\"]"),
  ES = str_remove_all(str_trim(toupper(PROF_ESP_ATE)), "[\r\n\"]"),
  CI = str_remove_all(str_trim(toupper(ATEMED_CIE10)), "[\r\n\"\\.]"),
  ET = str_remove_all(str_trim(toupper(PCTE_AUTID_ETN)), "[\r\n\"]"),
  NA_col = str_remove_all(str_trim(toupper(PCTE_NAC_ETN)), "[\r\n\"]"),
  PU = str_remove_all(str_trim(toupper(PCTE_PUEBLO)), "[\r\n\"]")
  )

# ====================================================================
# PASO 2: FILTRADO ESTRICTO (Primera Atención, Preventiva, Especialidad y Embarazo)
# ====================================================================
base_intercultural <- base_limpia %>%
  filter(str_detect(CR, "PRIMER|^P$|^1$")) %>%
  filter(str_detect(TP, "PREVENC|PREVENTIV")) %>%
  filter(str_detect(ES, especialidades_validas)) %>%
  filter(str_detect(CI, codigos_emb))

# ====================================================================
# PASO 3: APLICANDO TUS CLASIFICADORES INTERCULTURALES EXACTOS
# ====================================================================
# Equivalente Rápido a tus funciones clasE, clasN y clasP
base_clasificada <- base_intercultural %>%
  mutate(
    iE = case_when(
      str_detect(ET, 'AFRO') ~ 0, str_detect(ET, 'BLANC') ~ 1, str_detect(ET, 'INDIGENA|INDÍGENA') ~ 2,
      str_detect(ET, 'MESTIZ') ~ 3, str_detect(ET, 'MONTUBI|MONTUVI') ~ 4, str_detect(ET, 'MULAT') ~ 5,
      str_detect(ET, 'NEGR') ~ 6, str_detect(ET, 'APLICA') ~ 7, str_detect(ET, 'SABE|RESPONDE') ~ 8,
      str_detect(ET, 'OTR') ~ 9, TRUE ~ -1
    ),
    iN = case_when(
      str_detect(NA_col, 'ACHUAR') ~ 0, str_detect(NA_col, 'ANDOA') ~ 1, str_detect(NA_col, 'KICHWA|QUICHUA') ~ 2,
      str_detect(NA_col, 'APLICA') ~ 3, str_detect(NA_col, 'SABE|RESPONDE') ~ 4, str_detect(NA_col, 'SHUAR') ~ 5,
      str_detect(NA_col, 'WAORANI|HUAORANI') ~ 6, TRUE ~ -1
    ),
    iP = case_when(
      PU == "" ~ -1, # Ignora los vacíos tal como tu código original
      str_detect(PU, 'CHIBULEO') ~ 0, str_detect(PU, 'TOMABELA') ~ 1, str_detect(PU, 'KISAPINCHA|QUISAPINCHA') ~ 2,
      str_detect(PU, 'SALASAKA|SALASACA') ~ 3, str_detect(PU, 'PANZALEO') ~ 4, str_detect(PU, 'PURUHA|PURUHÁ') ~ 5,
      str_detect(PU, 'OTAVALO') ~ 6, str_detect(PU, 'APLICA') ~ 7, str_detect(PU, 'SABE|RESPONDE') ~ 8,
      str_detect(PU, 'WARAMKA|GUARANCA') ~ 9, TRUE ~ -1
    )
  )

# ====================================================================
# PASO 4: CONTEO Y PREPARACIÓN DE MATRICES
# ====================================================================
# Creamos tus arreglos de ceros (cE = 10, cN = 7, cP = 10)
cE <- numeric(10)
cN <- numeric(7)
cP <- numeric(10)

# Contamos cuántas coincidencias hay para cada índice de interculturalidad
for(i in 0:9) { cE[i+1] <- sum(base_clasificada$iE == i, na.rm = TRUE) }
for(i in 0:6) { cN[i+1] <- sum(base_clasificada$iN == i, na.rm = TRUE) }
for(i in 0:9) { cP[i+1] <- sum(base_clasificada$iP == i, na.rm = TRUE) }

# Función para dejar la celda limpia ("") si el valor es 0
formatear_ceros_vector <- function(v) { sapply(v, function(x) if(x > 0) x else "") }

v_Etnia <- formatear_ceros_vector(cE)
v_Nac   <- formatear_ceros_vector(cN)
v_Pueb  <- formatear_ceros_vector(cP)

# ====================================================================
# 5. ESCRITURA MÚLTIPLE EN GOOGLE SHEETS
# ====================================================================
# En R, le enviamos toda la columna junta y él la pega hacia abajo de golpe

# Escritura para el Mes Actual (Columna destino)
range_write(ss = id_google_sheet, data = data.frame(v_Etnia), range = paste0("Censo_Obstetrico!", letra_col, "24"), col_names = FALSE, reformat = FALSE)
range_write(ss = id_google_sheet, data = data.frame(v_Nac),   range = paste0("Censo_Obstetrico!", letra_col, "37"), col_names = FALSE, reformat = FALSE)
range_write(ss = id_google_sheet, data = data.frame(v_Pueb),  range = paste0("Censo_Obstetrico!", letra_col, "47"), col_names = FALSE, reformat = FALSE)

# Escritura del Resumen Final (Columna Q, correspondiente al último mes)
range_write(ss = id_google_sheet, data = data.frame(v_Etnia), range = paste0("Censo_Obstetrico!", letra_resumen, "24"), col_names = FALSE, reformat = FALSE)
range_write(ss = id_google_sheet, data = data.frame(v_Nac),   range = paste0("Censo_Obstetrico!", letra_resumen, "37"), col_names = FALSE, reformat = FALSE)
range_write(ss = id_google_sheet, data = data.frame(v_Pueb),  range = paste0("Censo_Obstetrico!", letra_resumen, "47"), col_names = FALSE, reformat = FALSE)

cat("✅ Trabajador 3 finalizado. Matriz de interculturalidad actualizada.\n")
}