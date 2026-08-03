# ====================================================================================
# TRABAJADOR 7: COBERTURA DE ATENCIONES (LUGAR DE ATENCIÓN)
# Arquitectura: Deduplicación -> Inclusión Méd+Obs+Odo+Psi -> Clasificación de 7 Cajones
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador7_CoberturaAtenciones <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🗺️ Trabajador 7 activado: Clasificando Cobertura y Lugares de Atención...\n")
  
  # 1. MAPA DE MESES
  mes_anterior <- mes_objetivo
  
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  # 2. BLINDAJE Y LIMPIEZA
  nombres_cols <- names(tabla_consolidada)
  if (!"ATEMED_LUG_ATE" %in% nombres_cols) tabla_consolidada$ATEMED_LUG_ATE <- ""
  
  base_limpia <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    # 1. Deduplicación Global (La Guillotina)
    distinct(ATEMED_ID, .keep_all = TRUE) %>%
    mutate(
      ESP = str_squish(str_remove_all(toupper(PROF_ESP_ATE), "[\r\n\"]")),
      LUG = str_squish(str_remove_all(toupper(ATEMED_LUG_ATE), "[\r\n\"]"))
    )
  
  # 2. FILTRO UNIVERSAL (Médicos, Obstetras, Odontólogos y Psicólogos)
  # ⚠️ AVISO DE ARQUITECTURA: Radar mejorado por la dirección del proyecto con comodín (.*) 
  # para máxima tolerancia a fallos de codificación en servidores nube (Ej: MÃ‰DICO)
  esp_validas <- "MEDICIN|M.*DIC|OBSTETR|PSICOLOG|ODONTO"
  
  base_filtrada <- base_limpia %>%
    filter(str_detect(ESP, esp_validas))
  
  # ====================================================================
  # PASO 3: CLASIFICACIÓN QUIRÚRGICA EN 7 CAJONES (case_when)
  # ====================================================================
  base_clasificada <- base_filtrada %>%
    mutate(
      cajon = case_when(
        LUG == "ESTABLECIMIENTO DE SALUD" ~ "est",
        # ⚠️ AVISO DE ARQUITECTURA: Modificado a str_detect() para habilitar el uso 
        # del comodín (.*) y blindar la palabra "EDUCACIÓN" contra corrupción de tildes.
        str_detect(LUG, "CENTROS EDUCATIVOS|CENTROS DE EDUCACI.*N INICIAL|ESCUELAS") ~ "edu",
        LUG == "CENTROS DE DESARROLLO INFANTIL - CDI" ~ "cdi",
        LUG == "CRECIENDO CON NUESTROS HIJOS CNH" ~ "cnh",
        LUG == "COMUNIDAD" ~ "com",
        LUG %in% c("DOMICILIO", "VISITA DOMICILIARIA") ~ "dom",
        TRUE ~ "otr" # El cajón 7: Todo lo que no cumpla lo de arriba cae aquí.
      )
    )
  
  # Conteo de los cajones
  c_est <- nrow(base_clasificada %>% filter(cajon == "est"))
  c_edu <- nrow(base_clasificada %>% filter(cajon == "edu"))
  c_cdi <- nrow(base_clasificada %>% filter(cajon == "cdi"))
  c_cnh <- nrow(base_clasificada %>% filter(cajon == "cnh"))
  c_com <- nrow(base_clasificada %>% filter(cajon == "com"))
  c_dom <- nrow(base_clasificada %>% filter(cajon == "dom"))
  c_otr <- nrow(base_clasificada %>% filter(cajon == "otr"))
  
  # ====================================================================
  # 4. ESCRITURA EN GOOGLE SHEETS (EMF. CRONICAS, Filas 36 a 42)
  # ====================================================================
  fc <- function(x) { if (x > 0) return(x) else return("") }
  
  range_write(ss = id_google_sheet, data = data.frame(fc(c_est)), range = paste0("EMF. CRONICAS!", letra_col, "36"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(c_edu)), range = paste0("EMF. CRONICAS!", letra_col, "37"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(c_cdi)), range = paste0("EMF. CRONICAS!", letra_col, "38"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(c_cnh)), range = paste0("EMF. CRONICAS!", letra_col, "39"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(c_com)), range = paste0("EMF. CRONICAS!", letra_col, "40"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(c_dom)), range = paste0("EMF. CRONICAS!", letra_col, "41"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(c_otr)), range = paste0("EMF. CRONICAS!", letra_col, "42"), col_names = FALSE, reformat = FALSE)
  
  cat("✅ Trabajador 7 finalizado. Lugares de atención procesados con éxito.\n")
}
