# ====================================================================================
# TRABAJADOR 8: INTERCULTURALIDAD (ETNIA, NACIONALIDAD, PUEBLO)
# Arquitectura: Búsqueda Exacta Oficial (==) y Escritura en 3 Bloques Verticales
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador8_InterculturalidadTotal <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🌾 Trabajador 8 activado: Procesando Totales Generales de Interculturalidad...\n")
  
  # 1. MAPA DE MESES Y COLUMNAS FRANCOTIRADOR
  mes_anterior <- mes_objetivo
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  # 2. CATÁLOGOS Y BLINDAJE
  esp_validas <- "MEDICIN|MÉDIC|OBSTETR|PSICOLOG|ODONTO"
  nombres_cols <- names(tabla_consolidada)
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  # 3. LIMPIEZA RESPETANDO TILDES (Para la búsqueda milimétrica oficial)
  base_filtrada <- tabla_consolidada %>%
    mutate(
      ESP = str_squish(str_remove_all(toupper(PROF_ESP_ATE), "[\r\n\"]")),
      # Se aplica toupper para normalizar, pero MANTIENE las tildes y las barras (/)
      ETN = str_squish(str_remove_all(toupper(PCTE_AUTID_ETN), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(PCTE_NAC_ETN), "[\r\n\"]")),
      PUE = str_squish(str_remove_all(toupper(PCTE_PUEBLO), "[\r\n\"]"))
    ) %>%
    filter(str_detect(ESP, esp_validas))
  
  # ====================================================================
  # 4. CONTEO MATEMÁTICO MILIMÉTRICO (Igualdad Exacta)
  # ====================================================================
  
  # --- BLOQUE 1: ETNIAS (10 Filas exactas, del 4 al 13) ---
  etn_counts <- c(
    sum(base_filtrada$ETN == "AFROECUATORIANO/AFRODESCENDIENTE", na.rm=TRUE), # Fila 4
    sum(base_filtrada$ETN == "BLANCO/A", na.rm=TRUE),                         # Fila 5
    sum(base_filtrada$ETN == "INDÍGENA", na.rm=TRUE),                         # Fila 6
    sum(base_filtrada$ETN == "MESTIZO/A", na.rm=TRUE),                        # Fila 7
    sum(base_filtrada$ETN == "MONTUBIO/A", na.rm=TRUE),                       # Fila 8
    sum(base_filtrada$ETN == "MULATO/A", na.rm=TRUE),                         # Fila 9
    sum(base_filtrada$ETN == "NEGRO/A", na.rm=TRUE),                          # Fila 10
    sum(base_filtrada$ETN == "NO APLICA", na.rm=TRUE),                        # Fila 11
    sum(base_filtrada$ETN == "NO SABE/NO RESPONDE", na.rm=TRUE),              # Fila 12
    sum(base_filtrada$ETN == "OTRO/A", na.rm=TRUE)                            # Fila 13
  )
  
  # --- BLOQUE 2: NACIONALIDADES (10 Filas exactas, del 18 al 27) ---
  # Aquí mantenemos str_detect robusto anclado a la raíz por si hay variaciones
  nac_counts <- c(
    sum(str_detect(base_filtrada$NAC, "ACHUAR"), na.rm=TRUE),      # 18
    sum(str_detect(base_filtrada$NAC, "ANDOA"), na.rm=TRUE),       # 19
    sum(str_detect(base_filtrada$NAC, "AWA|AWÁ"), na.rm=TRUE),     # 20
    sum(str_detect(base_filtrada$NAC, "COFAN|COFÁN"), na.rm=TRUE), # 21
    sum(str_detect(base_filtrada$NAC, "HUANCAVILCA"), na.rm=TRUE), # 22
    sum(str_detect(base_filtrada$NAC, "KICHWA|QUICHUA"), na.rm=TRUE), # 23
    sum(str_detect(base_filtrada$NAC, "SECOYA"), na.rm=TRUE),      # 24
    sum(str_detect(base_filtrada$NAC, "SHUAR"), na.rm=TRUE),       # 25
    sum(str_detect(base_filtrada$NAC, "WAORANI|HUAORANI"), na.rm=TRUE), # 26
    sum(str_detect(base_filtrada$NAC, "SABE|RESPONDE"), na.rm=TRUE) # 27
  )
  
  # --- BLOQUE 3: PUEBLOS (17 Filas exactas, del 32 al 48) ---
  pue_counts <- c(
    sum(str_detect(base_filtrada$PUE, "CHIBULEO"), na.rm=TRUE),     # 32
    sum(str_detect(base_filtrada$PUE, "KAÑARI|KANARI"), na.rm=TRUE),# 33
    sum(str_detect(base_filtrada$PUE, "KARANKI"), na.rm=TRUE),      # 34
    sum(str_detect(base_filtrada$PUE, "KAYAMBI"), na.rm=TRUE),      # 35
    sum(str_detect(base_filtrada$PUE, "AMAZONIC|AMAZÓNIC"), na.rm=TRUE), # 36
    sum(str_detect(base_filtrada$PUE, "KISAPINCHA|QUISAPINCHA"), na.rm=TRUE), # 37
    sum(str_detect(base_filtrada$PUE, "KITUKARA"), na.rm=TRUE),     # 38
    sum(str_detect(base_filtrada$PUE, "NATABUELA"), na.rm=TRUE),    # 39
    sum(str_detect(base_filtrada$PUE, "SABE|RESPONDE"), na.rm=TRUE),# 40
    sum(str_detect(base_filtrada$PUE, "OTAVALO"), na.rm=TRUE),      # 41
    sum(str_detect(base_filtrada$PUE, "PANZALEO"), na.rm=TRUE),     # 42
    sum(str_detect(base_filtrada$PUE, "PASTOS"), na.rm=TRUE),       # 43
    sum(str_detect(base_filtrada$PUE, "PURUHA|PURUHÁ"), na.rm=TRUE),# 44
    sum(str_detect(base_filtrada$PUE, "SALASAKA|SALASACA"), na.rm=TRUE), # 45
    sum(str_detect(base_filtrada$PUE, "SARAGURO"), na.rm=TRUE),     # 46
    sum(str_detect(base_filtrada$PUE, "TOMABELA"), na.rm=TRUE),     # 47
    sum(str_detect(base_filtrada$PUE, "WARANKA|GUARANCA|WARAMKA"), na.rm=TRUE) # 48
  )
  
  # ====================================================================
  # 5. TRANSFORMACIÓN Y ESCRITURA EN 3 DISPAROS (ANTI-BLOQUEO Y ANTI-BORRADO)
  # ====================================================================
  
  # Convertimos los ceros a espacios en blanco ("") para que tu hoja se vea limpia
  df_etn <- data.frame(VALOR = ifelse(etn_counts > 0, etn_counts, ""))
  df_nac <- data.frame(VALOR = ifelse(nac_counts > 0, nac_counts, ""))
  df_pue <- data.frame(VALOR = ifelse(pue_counts > 0, pue_counts, ""))
  
  # Configuramos las coordenadas exactas de impacto (Ej: B4, B18, B32)
  rango_etn <- paste0("ATEN POR AUTO-ETN!", letra_col, "4")
  rango_nac <- paste0("ATEN POR AUTO-ETN!", letra_col, "18")
  rango_pue <- paste0("ATEN POR AUTO-ETN!", letra_col, "32")
  
  # Disparamos las columnas completas (3 peticiones en vez de 40)
  range_write(ss = id_google_sheet, data = df_etn, range = rango_etn, col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = df_nac, range = rango_nac, col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = df_pue, range = rango_pue, col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 8 finalizado. Interculturalidad mapeada con precisión absoluta en 3 bloques.\n")
}
