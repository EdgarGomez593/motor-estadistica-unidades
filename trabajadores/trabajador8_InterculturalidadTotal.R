# ====================================================================================
# TRABAJADOR 8: INTERCULTURALIDAD (ETNIA, NACIONALIDAD, PUEBLO)
# Arquitectura: Conteo Total (Sin Guillotina) -> Filtro Especialidad -> Mapeo Exacto a Filas
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador8_InterculturalidadTotal <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🌾 Trabajador 8 activado: Procesando Totales Generales de Interculturalidad...\n")
  
  # ====================================================================
  # PASO 1. MAPA DE MESES Y COORDENADAS
  # ====================================================================
  mes_anterior <- mes_objetivo
  
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  # ====================================================================
  # PASO 2. CATÁLOGOS DE ESPECIALIDADES Y BLINDAJE
  # ====================================================================
  # ⚠️ AVISO DE ARQUITECTURA: Radar homologado con el Trabajador 4 y 7. 
  # Inyección del comodín (.*) en M.*DIC para blindar al código de fallas de lectura en la nube.
  esp_validas <- "MEDICIN|M.*DIC|OBSTETR|PSICOLOG|ODONTO"
  
  # Escudo de Columnas: Protege si el archivo CSV del PRAS viene con columnas faltantes
  nombres_cols <- names(tabla_consolidada)
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"PCTE_NAC_ETN" %in% nombres_cols) tabla_consolidada$PCTE_NAC_ETN <- ""
  if (!"PCTE_PUEBLO" %in% nombres_cols) tabla_consolidada$PCTE_PUEBLO <- ""
  
  # ====================================================================
  # PASO 3. LIMPIEZA Y FILTRADO POR ESPECIALIDAD (Base Completa, con duplicados)
  # ====================================================================
  base_filtrada <- tabla_consolidada %>%
    mutate(
      # Escudo Anti-NA para purificar textos antes de las búsquedas regex
      ESP = str_squish(str_remove_all(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE)), "[\r\n\"]")),
      ETN = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_AUTID_ETN), "", PCTE_AUTID_ETN)), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_NAC_ETN), "", PCTE_NAC_ETN)), "[\r\n\"]")),
      PUE = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_PUEBLO), "", PCTE_PUEBLO)), "[\r\n\"]"))
    ) %>%
    filter(str_detect(ESP, esp_validas))
  
  # ====================================================================
  # PASO 4: CLASIFICADORES ESTRICTOS DE FILAS (case_when)
  # ====================================================================
  # Catálogos extendidos y mejorados (Guerrero de la Selva)
  base_clasificada <- base_filtrada %>%
    mutate(
      Fila_ETN = case_when(
        str_detect(ETN, "AFRO.*") ~ 4,
        str_detect(ETN, "BLANC.*") ~ 5,
        str_detect(ETN, "IND.*GENA") ~ 6,
        str_detect(ETN, "MESTIZ.*") ~ 7,
        str_detect(ETN, "MONTUB.*|MONTUV.*") ~ 8,
        str_detect(ETN, "MULAT.*") ~ 9,
        str_detect(ETN, "NEGR.*") ~ 10,
        str_detect(ETN, "OTR.*") ~ 11,
        str_detect(ETN, "SABE.*|RESPONDE") ~ 12,
        str_detect(ETN, "APLICA") | ETN == "" ~ 13,
        TRUE ~ -1 # Fila 14 queda libre
      ),
      Fila_NAC = case_when(
        str_detect(NAC, "ACHUAR") ~ 19,
        str_detect(NAC, "ANDOA") ~ 20,
        str_detect(NAC, "AWA|AWÁ") ~ 21,
        str_detect(NAC, "CHACHI") ~ 22,
        str_detect(NAC, "COFAN|COFÁN") ~ 23,
        str_detect(NAC, "EPERA") ~ 24,
        str_detect(NAC, "HUANCAVILCA") ~ 25,
        str_detect(NAC, "KICHWA|QUICHUA") ~ 26,
        str_detect(NAC, "SECOYA") ~ 27,
        str_detect(NAC, "SHIWIAR") ~ 28,
        str_detect(NAC, "SHUAR") ~ 29,
        str_detect(NAC, "TS.*CHILA") ~ 30,
        str_detect(NAC, "WAORANI|HUAORANI") ~ 31,
        str_detect(NAC, "ZAPARA") ~ 32,
        str_detect(NAC, "SABE.*|RESPONDE") ~ 33,
        str_detect(NAC, "APLICA") | NAC == "" ~ 34,
        TRUE ~ -1 # Fila 35 queda libre
      ),
      Fila_PUE = case_when(
        str_detect(PUE, "CHIBULEO") ~ 40,
        str_detect(PUE, "HUANCAVILCA") ~ 41,
        str_detect(PUE, "KA.*ARI|CA.*ARI") ~ 42,
        str_detect(PUE, "KARANKI") ~ 43,
        str_detect(PUE, "KAYAMBI") ~ 44,
        str_detect(PUE, "KICHWA AMAZ.*") ~ 45,
        str_detect(PUE, "KISAPINCHA|QUISAPINCHA") ~ 46,
        str_detect(PUE, "KITUKARA") ~ 47,
        str_detect(PUE, "NATABUELA") ~ 48,
        str_detect(PUE, "OTAVALO") ~ 49,
        str_detect(PUE, "PALTAS") ~ 50,
        str_detect(PUE, "PANZALEO") ~ 51,
        str_detect(PUE, "PASTOS") ~ 52,
        str_detect(PUE, "PURUH.*") ~ 53,
        str_detect(PUE, "SALASA.*") ~ 54,
        str_detect(PUE, "SARAGURO") ~ 55,
        str_detect(PUE, "TOMABELA") ~ 56,
        str_detect(PUE, "WARAMKA|GUARANCA") ~ 57,
        str_detect(PUE, "SABE.*|RESPONDE") ~ 58,
        str_detect(PUE, "APLICA") | PUE == "" ~ 59,
        TRUE ~ -1 # Fila 60 queda libre
      )
    )
  
  # ====================================================================
  # PASO 5. AGREGACIÓN Y ESCRITURA EN GOOGLE SHEETS (NUBE ESTÁNDAR)
  # ====================================================================
  
  # Creamos una lista vacía para almacenar los conteos finales por fila
  conteos_finales <- list()
  
  # Función auxiliar para contar y guardar
  contar_y_guardar <- function(columna_fila) {
    # Extraemos todos los números de fila que no sean -1
    filas_validas <- columna_fila[columna_fila != -1]
    
    # Si hay datos, contamos cuántas veces se repite cada número de fila
    if(length(filas_validas) > 0) {
      tabla_frecuencias <- table(filas_validas)
      
      # Iteramos sobre los resultados y los guardamos en la lista maestra
      for(fila_str in names(tabla_frecuencias)) {
        valor <- as.numeric(tabla_frecuencias[[fila_str]])
        conteos_finales[[fila_str]] <<- valor # Usamos <<- para modificar la lista global
      }
    }
  }
  
  # Aplicamos la función a las tres columnas de clasificación
  contar_y_guardar(base_clasificada$Fila_ETN)
  contar_y_guardar(base_clasificada$Fila_NAC)
  contar_y_guardar(base_clasificada$Fila_PUE)
  
  # Escribimos los resultados en el Sheets usando range_write nativo
  for (fila in names(conteos_finales)) {
    valor_a_escribir <- conteos_finales[[fila]]
    rango_destino <- paste0("ATEN POR AUTO-ETN!", letra_col, fila)
    range_write(ss = id_google_sheet, data = data.frame(valor_a_escribir), range = rango_destino, col_names = FALSE, reformat = FALSE)
  }
  
  cat("✅ Trabajador 8 finalizado. Interculturalidad total mapeada a las filas correctas.\n")
}
