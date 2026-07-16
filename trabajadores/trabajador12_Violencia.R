# ====================================================================================
# TRABAJADOR 12: INTELIGENCIA OPERATIVA - VIOLENCIA FAMILIAR
# Arquitectura: Morbilidad -> Primeras -> Radar de Cabeceras -> Prioridad LGBTI
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador12_Violencia <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🛡️ Trabajador 12 activado: Procesando Inteligencia Operativa - Violencia...\n")
  
  
  # 1. RADAR DE CABECERAS (Tolerancia a nombres de columnas variables)
  nombres_cols <- names(tabla_consolidada)
  
  # Buscar columna de Orientación
  col_ori <- "PCTE_ORI_SEX" 
  if (any(str_detect(nombres_cols, "ORI_SEX|ORIENTACION"))) {
    col_ori <- nombres_cols[str_detect(nombres_cols, "ORI_SEX|ORIENTACION")][1]
  } else {
    tabla_consolidada$PCTE_ORI_SEX <- ""
  }
  
  # Buscar columna de Etnia
  col_etn <- "PCTE_AUTID_ETN"
  if (any(str_detect(nombres_cols, "AUTOIDENTIFICACION|ETNICA|ETN"))) {
    col_etn <- nombres_cols[str_detect(nombres_cols, "AUTOIDENTIFICACION|ETNICA|ETN")][1]
  } else {
    tabla_consolidada$PCTE_AUTID_ETN <- ""
  }
  
  # Buscar columna de Sexo
  col_sex <- "PCTE_SEXO"
  if (any(str_detect(nombres_cols, "^PCTE_SEXO$|PCTE_SEXO_BIOLOGICO"))) {
    col_sex <- nombres_cols[str_detect(nombres_cols, "^PCTE_SEXO$|PCTE_SEXO_BIOLOGICO")][1]
  } else {
    tabla_consolidada$PCTE_SEXO <- ""
  }
  
  # Buscar columna de Tipo de Diagnóstico
  col_tip <- "ATEMED_TIP_DIAG"
  if (any(str_detect(nombres_cols, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG"))) {
    col_tip <- nombres_cols[str_detect(nombres_cols, "ATEMED_TIP_DIAG|ATEMED_TIPO_DIAG")][1]
  } else {
    tabla_consolidada$ATEMED_TIP_DIAG <- ""
  }
  
  # 2. LIMPIEZA Y FILTRADO ESTRICTO
  base_filtrada <- tabla_consolidada %>%
    mutate(
      CIE = str_squish(str_remove_all(toupper(ATEMED_CIE10), "[\r\n\"\\.]")),
      CRON = str_squish(str_remove_all(toupper(ATEMED_CRON_DIAG), "[\r\n\"]")),
      TIP = str_squish(str_remove_all(toupper(!!sym(col_tip)), "[\r\n\"]")),
      SEXO = str_squish(str_remove_all(toupper(!!sym(col_sex)), "[\r\n\"]")),
      ORI = str_squish(str_remove_all(toupper(!!sym(col_ori)), "[\r\n\"]")),
      ETNIA = str_squish(str_remove_all(toupper(!!sym(col_etn)), "[\r\n\"]"))
    ) %>%
    # Filtramos por las familias T74 (Violencia)
    filter(CIE %in% c("T740", "T741", "T742", "T743", "T748", "T749")) %>%
    filter(str_detect(TIP, "MORB")) %>%
    filter(str_detect(CRON, "PRIMER|^P$|^1$|^N$")) %>%
    # Reglas de negocio para Género y Etnia
    mutate(
      esLGBTI = str_detect(ORI, "GAY|LESBIANA|BISEXUAL"),
      # Si es LGBTI, NO se cuenta como Hombre ni Mujer (tu regla de exclusión)
      esHombre = !esLGBTI & str_detect(SEXO, "HOMBRE|^M$"),
      esMujer = !esLGBTI & str_detect(SEXO, "MUJER|^F$"),
      
      ind = str_detect(ETNIA, "INDÍGENA|INDIGENA"),
      mes = str_detect(ETNIA, "MESTIZ"),
      afr = str_detect(ETNIA, "AFRO|NEGR"), # Unifica Afro y Negro como en tu script
      mul = str_detect(ETNIA, "MULAT"),
      bla = str_detect(ETNIA, "BLANC")
    )
  
  # 3. AGRUPACIÓN (Equivalente al objeto dataViolencia)
  resumen_violencia <- base_filtrada %>%
    group_by(CIE) %>%
    summarise(
      h = sum(esHombre, na.rm = TRUE),
      m = sum(esMujer, na.rm = TRUE),
      l = sum(esLGBTI, na.rm = TRUE),
      ind = sum(ind, na.rm = TRUE),
      mes = sum(mes, na.rm = TRUE),
      afr = sum(afr, na.rm = TRUE),
      mul = sum(mul, na.rm = TRUE),
      bla = sum(bla, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 4. ESCRITURA DIRECTA EN GOOGLE SHEETS
  # Diccionario de filas exactas
  mapa_filas <- c("T740" = 3, "T741" = 4, "T742" = 5, "T743" = 6, "T748" = 7, "T749" = 8)
  
  fc <- function(x) { if (x > 0) return(x) else return("") }
  
  if (nrow(resumen_violencia) > 0) {
    for (i in 1:nrow(resumen_violencia)) {
      row <- resumen_violencia[i, ]
      fila <- mapa_filas[[row$CIE]]
      
      if (!is.null(fila)) {
        # Bloque de Género y LGBTI (B, C, D)
        range_write(ss = id_google_sheet, data = data.frame(fc(row$h)), range = paste0("VIOLENCIA!B", fila), col_names = FALSE, reformat = FALSE)
        range_write(ss = id_google_sheet, data = data.frame(fc(row$m)), range = paste0("VIOLENCIA!C", fila), col_names = FALSE, reformat = FALSE)
        range_write(ss = id_google_sheet, data = data.frame(fc(row$l)), range = paste0("VIOLENCIA!D", fila), col_names = FALSE, reformat = FALSE)
        
        # Bloque de Etnias (F, G, H, I, J)
        range_write(ss = id_google_sheet, data = data.frame(fc(row$ind)), range = paste0("VIOLENCIA!F", fila), col_names = FALSE, reformat = FALSE)
        range_write(ss = id_google_sheet, data = data.frame(fc(row$mes)), range = paste0("VIOLENCIA!G", fila), col_names = FALSE, reformat = FALSE)
        range_write(ss = id_google_sheet, data = data.frame(fc(row$afr)), range = paste0("VIOLENCIA!H", fila), col_names = FALSE, reformat = FALSE)
        range_write(ss = id_google_sheet, data = data.frame(fc(row$mul)), range = paste0("VIOLENCIA!I", fila), col_names = FALSE, reformat = FALSE)
        range_write(ss = id_google_sheet, data = data.frame(fc(row$bla)), range = paste0("VIOLENCIA!J", fila), col_names = FALSE, reformat = FALSE)
      }
    }
  }
  
  cat("✅ Trabajador 12 finalizado. Casos de violencia mapeados correctamente.\n")
}