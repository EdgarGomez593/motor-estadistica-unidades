# ====================================================================================
# TRABAJADOR 11: INTELIGENCIA OPERATIVA DEL ESTADO NUTRICIONAL
# Arquitectura: Clasificador por Edades (Niños) y Filtro Prenatal (Embarazadas)
# Secreto de Producción: Se respetan las filas 7, 18 y 28 (TOTALES)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador11_EstadoNutricional <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🍼 Trabajador 11 activado: Calculando Inteligencia Nutricional...\n")
  
  # 1. MAPA DE MESES
  mes_actual_num <- mes_objetivo
  
  col_destino_num <- mes_actual_num + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")
  letra_col <- letras_columnas[col_destino_num]
  
  # 2. BLINDAJE DE COLUMNAS
  nombres_cols <- names(tabla_consolidada)
  if (!"PCTE_ANIOS_EN_MESES" %in% nombres_cols) tabla_consolidada$PCTE_ANIOS_EN_MESES <- NA
  if (!"PCTE_GRP_PRI" %in% nombres_cols) tabla_consolidada$PCTE_GRP_PRI <- ""
  if (!"PCTE_IND_ANEMIA_HEMOGLOBINA" %in% nombres_cols) tabla_consolidada$PCTE_IND_ANEMIA_HEMOGLOBINA <- ""
  if (!"PCTE_AUTID_ETN" %in% nombres_cols) tabla_consolidada$PCTE_AUTID_ETN <- ""
  if (!"ATEMED_TIP_DIAG" %in% nombres_cols) tabla_consolidada$ATEMED_TIP_DIAG <- tabla_consolidada$ATEMED_TIPO_DIAG
  
  # 3. LIMPIEZA GENERAL (¡Sin Guillotina, es conteo de atenciones totales!)
  base_limpia <- tabla_consolidada %>%
    mutate(
      EDAD_MESES = as.numeric(PCTE_ANIOS_EN_MESES),
      CIE = str_squish(str_remove_all(toupper(ATEMED_CIE10), "[\r\n\"\\.]")),
      GRP = str_squish(str_remove_all(toupper(PCTE_GRP_PRI), "[\r\n\"]")),
      HEMO = str_squish(str_remove_all(toupper(PCTE_IND_ANEMIA_HEMOGLOBINA), "[\r\n\"]")),
      TIP = str_squish(str_remove_all(toupper(ATEMED_TIP_DIAG), "[\r\n\"]")),
      CR = str_squish(str_remove_all(toupper(ATEMED_CRON_DIAG), "[\r\n\"]")),
      ETN = str_squish(str_remove_all(toupper(PCTE_AUTID_ETN), "[\r\n\"]"))
    )
  
  # 4. CLASIFICADOR DE ETNIAS (Para las columnas Q=17 a Z=26)
  letras_etnia <- c("Q","R","S","T","U","V","W","X","Y","Z")
  
  base_limpia <- base_limpia %>%
    mutate(
      idx_Etnia = case_when(
        str_detect(ETN, "INDIGENA|INDÍGENA") ~ 1,
        str_detect(ETN, "MESTIZ") ~ 2,
        str_detect(ETN, "AFRO") ~ 3,
        str_detect(ETN, "MONTUB|MONTUV") ~ 4,
        str_detect(ETN, "MULAT") ~ 5,
        str_detect(ETN, "NEGR") ~ 6,
        str_detect(ETN, "BLANC") ~ 7,
        str_detect(ETN, "APLICA") ~ 8,
        str_detect(ETN, "SABE|RESPONDE") ~ 9,
        str_detect(ETN, "OTR") ~ 10,
        TRUE ~ 10 # Por defecto a 'OTRO' para atrapar vacíos en Etnia si el paciente es válido
      ),
      esPrimeraGeneral = str_detect(CR, "PRIMER|^P$|^1$|^N$"),
      esAguda = (CIE == "E441"),
      esCronica = (CIE %in% c("E43X", "E43")),
      esSobrepeso = (CIE %in% c("E660", "E669")),
      esAnemiaNino = (CIE %in% c("D46X", "D46"))
    )
  
  # ====================================================================
  # BLOQUE INFANTIL (Menores de 24 y Menores de 60 meses)
  # ====================================================================
  base_ninos <- base_limpia %>% filter(!is.na(EDAD_MESES))
  
  b_24 <- base_ninos %>% filter(EDAD_MESES < 24)
  b_59 <- base_ninos %>% filter(EDAD_MESES < 60) # Incluye a los < 24
  
  # Conteo Mensual
  n24_pri <- nrow(b_24 %>% filter(esPrimeraGeneral))
  n24_agu <- nrow(b_24 %>% filter(esAguda))
  n24_cro <- nrow(b_24 %>% filter(esCronica))
  n24_sob <- nrow(b_24 %>% filter(esSobrepeso))
  
  n59_pri <- nrow(b_59 %>% filter(esPrimeraGeneral))
  n59_agu <- nrow(b_59 %>% filter(esAguda))
  n59_cro <- nrow(b_59 %>% filter(esCronica))
  n59_sob <- nrow(b_59 %>% filter(esSobrepeso))
  
  # ====================================================================
  # BLOQUE EMBARAZADAS
  # ====================================================================
  # 1. Clon de la Lógica del Trabajador 1 (Solo para contar las Primeras)
  emb_pri <- nrow(base_limpia %>% filter(str_detect(TIP, "PREV") & str_detect(CIE, "^Z321|^Z34|^Z35") & esPrimeraGeneral))
  
  # 2. Patologías Maternas (Solo en grupo prioritario EMBARAZADAS)
  b_emb <- base_limpia %>% filter(str_detect(GRP, "EMBARAZADAS"))
  
  emb_ane <- nrow(b_emb %>% filter(str_detect(HEMO, "ANEMIA LEVE|ANEMIA MODERADA")))
  emb_obe <- nrow(b_emb %>% filter(esSobrepeso)) # CIE E660 o E669
  
  # ====================================================================
  # ESCRITURA EN EL BLOQUE DE MESES (Columna Dinámica)
  # ====================================================================
  fc <- function(x) { if (x > 0) return(x) else return("") }
  
  # Bloque 24 meses (Filas 3 a 6)
  range_write(ss = id_google_sheet, data = data.frame(fc(n24_pri)), range = paste0("ESTADO_NUTRI!", letra_col, "3"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(n24_cro)), range = paste0("ESTADO_NUTRI!", letra_col, "4"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(n24_agu)), range = paste0("ESTADO_NUTRI!", letra_col, "5"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(n24_sob)), range = paste0("ESTADO_NUTRI!", letra_col, "6"), col_names = FALSE, reformat = FALSE)
  
  # Bloque 59 meses (Filas 12 a 15)
  range_write(ss = id_google_sheet, data = data.frame(fc(n59_pri)), range = paste0("ESTADO_NUTRI!", letra_col, "12"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(n59_cro)), range = paste0("ESTADO_NUTRI!", letra_col, "13"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(n59_agu)), range = paste0("ESTADO_NUTRI!", letra_col, "14"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(n59_sob)), range = paste0("ESTADO_NUTRI!", letra_col, "15"), col_names = FALSE, reformat = FALSE)
  
  # Bloque Embarazadas (Fila 23 y 25. La 28 es Anemia en el total, la 26 es en Etnias)
  range_write(ss = id_google_sheet, data = data.frame(fc(emb_pri)), range = paste0("ESTADO_NUTRI!", letra_col, "23"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(emb_obe)), range = paste0("ESTADO_NUTRI!", letra_col, "25"), col_names = FALSE, reformat = FALSE)
  
  # ====================================================================
  # ESCRITURA EN EL BLOQUE DE ETNIAS (Múltiples Columnas: Q a Z)
  # ====================================================================
  # Leemos el estado actual de las tablas de etnias (para sumar)
  tabla_etnia_actual <- read_sheet(ss = id_google_sheet, sheet = "ESTADO_NUTRI", range = "Q3:Z26", col_names = FALSE)
  
  escribir_etnia <- function(base_filtro, indicador, fila_destino) {
    for (i in 1:10) { # Recorremos los 10 grupos étnicos
      # Contamos los casos en la base filtrada para esta etnia
      conteo <- nrow(base_filtro %>% filter(!!sym(indicador) & idx_Etnia == i))
      if (conteo > 0) {
        # Fila destino en Sheets, ajustada a la matriz leída (que empieza en Q3)
        fila_matriz <- fila_destino - 2 
        # Valor existente + Nuevo conteo
        valor_existente <- as.numeric(tabla_etnia_actual[fila_matriz, i])
        if (is.na(valor_existente)) valor_existente <- 0
        nuevo_valor <- valor_existente + conteo
        
        # Escribimos el nuevo acumulado
        range_write(ss = id_google_sheet, data = data.frame(nuevo_valor), range = paste0("ESTADO_NUTRI!", letras_etnia[i], fila_destino), col_names = FALSE, reformat = FALSE)
      }
    }
  }
  
  escribir_etnia(b_24, "esAguda", 3)
  escribir_etnia(b_24, "esCronica", 4)
  escribir_etnia(b_24, "esSobrepeso", 5)
  escribir_etnia(b_24, "esAnemiaNino", 6)
  
  escribir_etnia(b_59, "esAguda", 12)
  escribir_etnia(b_59, "esCronica", 13)
  escribir_etnia(b_59, "esSobrepeso", 14)
  escribir_etnia(b_59, "esAnemiaNino", 15)
  
  # Obesidad materna y Anemia Hemo
  b_emb$esObesidadCie <- (b_emb$CIE %in% c("E660", "E669"))
  b_emb$esAnemiaHemo <- str_detect(b_emb$HEMO, "ANEMIA LEVE|ANEMIA MODERADA")
  
  escribir_etnia(b_emb, "esObesidadCie", 25)
  escribir_etnia(b_emb, "esAnemiaHemo", 26)
  
  cat("✅ Trabajador 11 finalizado. Matriz de Estado Nutricional actualizada.\n")
}