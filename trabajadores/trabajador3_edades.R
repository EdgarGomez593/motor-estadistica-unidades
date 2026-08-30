# ====================================================================================
# TRABAJADOR 3: ETNIA, NACIONALIDAD Y PUEBLO (ESTÁNDAR PROVINCIAL)
# Arquitectura: Inclusión "No Aplica" en Etnia -> Cascada Perfecta 1:1 -> Nuevas Coordenadas
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador3_Edades <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🌍 Trabajador 3 activado: Clasificando Censo Obstétrico Intercultural...\n")
  if(mes_objetivo > 12) return(invisible(NULL))
  
  mes_anterior <- mes_objetivo
  col_destino_num <- mes_anterior + 1
  letras_columnas <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q")
  letra_col <- letras_columnas[col_destino_num]
  letra_resumen <- "Q" 
  
  esp_validas <- "MEDICINA FAMILIAR Y COMUNITARIA|MEDICINA RURAL|OBSTETRICIA|PSICOLOGÍA CLÍNICA|PSICOLOGIA CLINICA|ODONTOLOGÍA|ODONTOLOGIA|ENFERMERÍA|ENFERMERIA"
  
  nombres_cols <- names(tabla_consolidada)
  col_tipo_diag <- if("ATEMED_TIPO_DIAG" %in% nombres_cols && !"ATEMED_TIP_DIAG" %in% nombres_cols) "ATEMED_TIPO_DIAG" else "ATEMED_TIP_DIAG"
  
  base_intercultural <- tabla_consolidada %>%
    filter(!is.na(ATEMED_ID) & str_trim(ATEMED_ID) != "") %>%
    mutate(
      CR = str_remove_all(str_trim(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG))), "[\r\n\"]"),
      TP = str_remove_all(str_trim(toupper(ifelse(is.na(!!sym(col_tipo_diag)), "", !!sym(col_tipo_diag)))), "[\r\n\"]"),
      ES = str_remove_all(str_trim(toupper(ifelse(is.na(PROF_ESP_ATE), "", PROF_ESP_ATE))), "[\r\n\"]"),
      CI = str_remove_all(str_trim(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10))), "[\r\n\"\\.]"),
      ET = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_AUTID_ETN), "", PCTE_AUTID_ETN))), "[\r\n\"]"),
      NA_col = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_NAC_ETN), "", PCTE_NAC_ETN))), "[\r\n\"]"),
      PU = str_remove_all(str_trim(toupper(ifelse(is.na(PCTE_PUEBLO), "", PCTE_PUEBLO))), "[\r\n\"]")
    ) %>%
    filter(str_detect(TP, "PREV")) %>%
    filter(CI == "Z321" | str_starts(CI, "Z34") | str_starts(CI, "Z35")) %>%
    filter(str_detect(CR, "PRIMER|^P$|^1$|^N$")) %>%
    filter(str_detect(ES, esp_validas))
  
  base_clasificada <- base_intercultural %>%
    mutate(
      es_pueblo_huancavilca = str_detect(PU, "HUANCAVILCA"),
      tiene_pueblo = str_detect(PU, "CHIBULEO|HUANCAVILCA|KA.*ARI|CA.*ARI|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|KICHWA AMAZ.*|KISAPINCHA|QUISAPINCHA|KITUKARA|QUITUCARA|NATABUELA|OTAVALO|PALTAS|PANZALEO|PASTOS|PURUH.*|PURUHÁ|PURUHA|SALASA.*|SARAGURO|TOMABELA|WARAMKA|GUARANCA"),
      tiene_nac = str_detect(NA_col, "ACHUAR|ANDOA|AWA|AWÁ|CHACHI|COF.*N|KOF.*N|EPERA|.*PERA|HUANCAVILCA|KICHWA|QUICHUA|SECOYA|SHIWIAR|SHUAR|TS.*CHILA|TSÁCHILA|WAORANI|HUAORANI|ZAPARA|ZÁPARA"),
      
      ETN_RESC = case_when(tiene_pueblo | tiene_nac ~ "INDÍGENA", TRUE ~ ET),
      NAC_RESC = case_when(tiene_pueblo & es_pueblo_huancavilca ~ "HUANCAVILCA", tiene_pueblo & !es_pueblo_huancavilca ~ "KICHWA", tiene_nac ~ NA_col, TRUE ~ ""),
      
      # 🚀 NIVEL 1: 10 Filas (Del 65 al 74)
      iE = case_when(
        str_detect(ETN_RESC, 'AFRO.*') ~ 65, str_detect(ETN_RESC, 'BLANC.*') ~ 66, str_detect(ETN_RESC, 'IND.*GENA|INDÍGENA') ~ 67,
        str_detect(ETN_RESC, 'MESTIZ.*') ~ 68, str_detect(ETN_RESC, 'MONTUBI.*|MONTUVI.*') ~ 69, str_detect(ETN_RESC, 'MULAT.*') ~ 70,
        str_detect(ETN_RESC, 'NEGR.*') ~ 71, str_detect(ETN_RESC, 'OTR.*') ~ 72, str_detect(ETN_RESC, 'APLICA') ~ 73,
        str_detect(ETN_RESC, 'SABE.*|RESPONDE') ~ 74, TRUE ~ 74 # Vacíos a 74
      ),
      
      # 🚀 NIVEL 2: Solo Indígenas
      iN = case_when(
        iE == 67 ~ case_when(
          str_detect(NAC_RESC, 'ACHUAR') ~ 79, str_detect(NAC_RESC, 'ANDOA') ~ 80, str_detect(NAC_RESC, 'AWA') ~ 81,
          str_detect(NAC_RESC, 'CHACHI') ~ 82, str_detect(NAC_RESC, 'COFAN') ~ 83, str_detect(NAC_RESC, 'EPERA') ~ 84,
          str_detect(NAC_RESC, 'HUANCAVILCA') ~ 85, str_detect(NAC_RESC, 'KICHWA') ~ 86, str_detect(NAC_RESC, 'SECOYA') ~ 87,
          str_detect(NAC_RESC, 'SHIWIAR') ~ 88, str_detect(NAC_RESC, 'SHUAR') ~ 89, str_detect(NAC_RESC, 'TS.*CHILA') ~ 90,
          str_detect(NAC_RESC, 'WAORANI|HUAORANI') ~ 91, str_detect(NAC_RESC, 'ZAPARA') ~ 92, TRUE ~ 93
        ), TRUE ~ -1
      ),
      
      # 🚀 NIVEL 3: Cascada 1:1
      iP = case_when(
        iE == 67 ~ case_when(
          iN == 86 & tiene_pueblo & !es_pueblo_huancavilca ~ case_when(
            str_detect(PU, 'CHIBULEO') ~ 99, str_detect(PU, 'KA.*ARI|CA.*ARI') ~ 101, str_detect(PU, 'KARANKI|CARANQUI') ~ 102,
            str_detect(PU, 'KAYAMBI|CAYAMBI') ~ 103, str_detect(PU, 'KICHWA AMAZ.*') ~ 104, str_detect(PU, 'KISAPINCHA|QUISAPINCHA') ~ 105,
            str_detect(PU, 'KITUKARA|QUITUCARA') ~ 106, str_detect(PU, 'NATABUELA') ~ 107, str_detect(PU, 'OTAVALO') ~ 108,
            str_detect(PU, 'PALTAS') ~ 109, str_detect(PU, 'PANZALEO') ~ 110, str_detect(PU, 'PASTOS') ~ 111,
            str_detect(PU, 'PURUH.*') ~ 112, str_detect(PU, 'SALASA.*') ~ 113, str_detect(PU, 'SARAGURO') ~ 114,
            str_detect(PU, 'TOMABELA') ~ 115, str_detect(PU, 'WARAMKA|GUARANCA') ~ 116, TRUE ~ 117
          ),
          iN == 85 & es_pueblo_huancavilca ~ 100, TRUE ~ 117
        ), TRUE ~ -1
      )
    )
  
  matEtn <- numeric(10) # Ahora son 10 por el "No Aplica"
  matNac <- numeric(15)
  matPue <- numeric(19)
  
  if(nrow(base_clasificada) > 0) {
    cEtn <- base_clasificada %>% filter(iE != -1) %>% count(iE)
    cNac <- base_clasificada %>% filter(iN != -1) %>% count(iN)
    cPue <- base_clasificada %>% filter(iP != -1) %>% count(iP)
    
    if(nrow(cEtn) > 0) { for(i in 1:nrow(cEtn)) matEtn[cEtn$iE[i] - 64] <- cEtn$n[i] }
    if(nrow(cNac) > 0) { for(i in 1:nrow(cNac)) matNac[cNac$iN[i] - 78] <- cNac$n[i] }
    if(nrow(cPue) > 0) { for(i in 1:nrow(cPue)) matPue[cPue$iP[i] - 98] <- cPue$n[i] }
  }
  
  matEtn[matEtn == 0] <- NA_real_
  matNac[matNac == 0] <- NA_real_
  matPue[matPue == 0] <- NA_real_
  
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matEtn), range = paste0("Censo_Obstetrico!", letra_col, "65"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matNac), range = paste0("Censo_Obstetrico!", letra_col, "79"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(VALOR = matPue), range = paste0("Censo_Obstetrico!", letra_col, "99"), col_names = FALSE, reformat = FALSE)
  
  tryCatch({
    range_write(ss = id_google_sheet, data = data.frame(VALOR = matEtn), range = paste0("Censo_Obstetrico!", letra_resumen, "65"), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = data.frame(VALOR = matNac), range = paste0("Censo_Obstetrico!", letra_resumen, "79"), col_names = FALSE, reformat = FALSE)
    range_write(ss = id_google_sheet, data = data.frame(VALOR = matPue), range = paste0("Censo_Obstetrico!", letra_resumen, "99"), col_names = FALSE, reformat = FALSE)
  }, error = function(e) { cat("⚠️ Resumen omitido (columna Q inexistente).\n") })
  
  cat("✅ Trabajador 3 finalizado. Matriz obstétrica intercultural actualizada.\n")
}
