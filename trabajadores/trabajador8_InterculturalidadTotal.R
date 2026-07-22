# ====================================================================================
# TRABAJADOR 8: INTERCULTURALIDAD (ETNIA, NACIONALIDAD, PUEBLO)
# Arquitectura: Conteo Total (Sin Guillotina) -> Filtro Especialidad -> Mapeo Exacto a Filas
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador8_InterculturalidadTotal <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🌾 Trabajador 8 activado: Procesando Totales Generales de Interculturalidad...\n")
  
  # 1. MAPA DE MESES
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
  
  # 3. LIMPIEZA Y FILTRADO POR ESPECIALIDAD (Base Completa, con duplicados)
  base_filtrada <- tabla_consolidada %>%
    mutate(
      ESP = str_squish(str_remove_all(toupper(PROF_ESP_ATE), "[\r\n\"]")),
      ETN = str_squish(str_remove_all(toupper(PCTE_AUTID_ETN), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(PCTE_NAC_ETN), "[\r\n\"]")),
      PUE = str_squish(str_remove_all(toupper(PCTE_PUEBLO), "[\r\n\"]"))
    ) %>%
    filter(str_detect(ESP, esp_validas))
  
  # ====================================================================
  # PASO 4: CLASIFICADORES ESTRICTOS DE FILAS (case_when)
  # ====================================================================
  base_clasificada <- base_filtrada %>%
    mutate(
      Fila_ETN = case_when(
        str_detect(ETN, "AFRO") ~ 4,
        str_detect(ETN, "BLANC") ~ 5,
        str_detect(ETN, "INDIGENA|INDÍGENA") ~ 6,
        str_detect(ETN, "MESTIZ") ~ 7,
        str_detect(ETN, "MONTUB|MONTUV") ~ 8,
        str_detect(ETN, "MULAT") ~ 9,
        str_detect(ETN, "NEGR") ~ 10,
        str_detect(ETN, "APLICA") ~ 11,
        str_detect(ETN, "SABE|RESPONDE") ~ 12,
        str_detect(ETN, "OTR") ~ 13,
        TRUE ~ -1 # Ignorar
      ),
      Fila_NAC = case_when(
        str_detect(NAC, "ACHUAR") ~ 18,
        str_detect(NAC, "ANDOA") ~ 19,
        str_detect(NAC, "AWA|AWÁ") ~ 20,
        str_detect(NAC, "COFAN|COFÁN") ~ 21,
        str_detect(NAC, "HUANCAVILCA") ~ 22,
        str_detect(NAC, "KICHWA|QUICHUA") ~ 23,
        str_detect(NAC, "SECOYA") ~ 24,
        str_detect(NAC, "SHUAR") ~ 25,
        str_detect(NAC, "WAORANI|HUAORANI") ~ 26,
        str_detect(NAC, "SABE|RESPONDE") ~ 27,
        TRUE ~ -1
      ),
      Fila_PUE = case_when(
        str_detect(PUE, "CHIBULEO") ~ 32,
        str_detect(PUE, "KAÑARI|KANARI") ~ 33,
        str_detect(PUE, "KARANKI") ~ 34,
        str_detect(PUE, "KAYAMBI") ~ 35,
        str_detect(PUE, "AMAZONIC|AMAZÓNIC") ~ 36,
        str_detect(PUE, "KISAPINCHA|QUISAPINCHA") ~ 37,
        str_detect(PUE, "KITUKARA") ~ 38,
        str_detect(PUE, "NATABUELA") ~ 39,
        str_detect(PUE, "SABE|RESPONDE") ~ 40,
        str_detect(PUE, "OTAVALO") ~ 41,
        str_detect(PUE, "PANZALEO") ~ 42,
        str_detect(PUE, "PASTOS") ~ 43,
        str_detect(PUE, "PURUHA|PURUHÁ") ~ 44,
        str_detect(PUE, "SALASAKA|SALASACA") ~ 45,
        str_detect(PUE, "SARAGURO") ~ 46,
        str_detect(PUE, "TOMABELA") ~ 47,
        str_detect(PUE, "WARAMKA|GUARANCA") ~ 48,
        TRUE ~ -1
      )
    )
  
  # ====================================================================
  # 5. AGREGACIÓN Y ESCRITURA EN GOOGLE SHEETS
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
  
  # Escribimos los resultados en el Sheets, iterando por cada fila encontrada
  # (Exactamente igual que tu bucle: for (var fila in res.data))
  for (fila in names(conteos_finales)) {
    valor_a_escribir <- conteos_finales[[fila]]
    rango_destino <- paste0("ATEN POR AUTO-ETN!", letra_col, fila)
    range_write(ss = id_google_sheet, data = data.frame(valor_a_escribir), range = rango_destino, col_names = FALSE, reformat = FALSE)
  }
  
  cat("✅ Trabajador 8 finalizado. Interculturalidad total mapeada a las filas correctas.\n")
}
