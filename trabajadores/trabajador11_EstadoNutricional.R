# ====================================================================================
# TRABAJADOR 11: MOTOR DE ESTADO NUTRICIONAL E INTERCULTURALIDAD EXPANDIDA
# Arquitectura: Híbrido -> Pediatría (Fila a Fila) + Obstetricia (Ticket + Ficha MSP)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador11_EstadoNutricional <- function(tabla_consolidada, id_google_sheet, mes_objetivo, cubeta_completa = NULL) {
  
  if(mes_objetivo > 12) return(invisible(NULL))
  
  col_destino_num <- mes_objetivo + 1
  letra_col_conteo <- LETTERS[col_destino_num] 
  
  nombres_cols <- names(tabla_consolidada)
  nombres_upper <- toupper(nombres_cols)
  
  # Radares de Cabecera
  col_edad_meses <- if(any(str_detect(nombres_upper, "PCTE_ANIOS_EN_MESES"))) nombres_cols[str_detect(nombres_upper, "PCTE_ANIOS_EN_MESES")][1] else { tabla_consolidada$PCTE_ANIOS_EN_MESES <- NA; "PCTE_ANIOS_EN_MESES" }
  col_imc <- if(any(str_detect(nombres_upper, "PCTE_ULT_IMC$"))) nombres_cols[str_detect(nombres_upper, "PCTE_ULT_IMC$")][1] else { tabla_consolidada$PCTE_ULT_IMC <- NA; "PCTE_ULT_IMC" }
  col_esp <- if(any(str_detect(nombres_upper, "PROF_ESP_ATE"))) nombres_cols[str_detect(nombres_upper, "PROF_ESP_ATE")][1] else { tabla_consolidada$PROF_ESP_ATE <- ""; "PROF_ESP_ATE" }
  
  col_etn <- if(any(str_detect(nombres_upper, "AUTOIDENTIFICACION|ETNICA|AUTID_ETN"))) nombres_cols[str_detect(nombres_upper, "AUTOIDENTIFICACION|ETNICA|AUTID_ETN")][1] else { tabla_consolidada$PCTE_AUTID_ETN <- ""; "PCTE_AUTID_ETN" }
  col_nac <- if("PCTE_NAC_ETN" %in% nombres_upper) nombres_cols[nombres_upper == "PCTE_NAC_ETN"][1] else if(any(str_detect(nombres_upper, "NAC_ETN|NACIONALIDAD_ETN"))) nombres_cols[str_detect(nombres_upper, "NAC_ETN|NACIONALIDAD_ETN")][1] else { tabla_consolidada$PCTE_NAC_ETN <- ""; "PCTE_NAC_ETN" }
  col_pue <- if(any(str_detect(nombres_upper, "PUEBLO"))) nombres_cols[str_detect(nombres_upper, "PUEBLO")][1] else { tabla_consolidada$PCTE_PUEBLO <- ""; "PCTE_PUEBLO" }
  
  # Base Maestra y Limpieza
  base_maestra <- tabla_consolidada %>%
    mutate(
      TICKET = str_trim(ifelse(is.na(ATEMED_ID) | ATEMED_ID == "", paste0("FILA_VACIA_", row_number()), ATEMED_ID)),
      EDAD_MESES = as.numeric(ifelse(is.na(!!sym(col_edad_meses)), -1, !!sym(col_edad_meses))),
      IMC = as.numeric(str_replace_all(ifelse(is.na(!!sym(col_imc)), -1, !!sym(col_imc)), ",", ".")),
      CIE = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10)), "[\r\n\"\\.]")),
      ESP = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_esp)), "", !!sym(col_esp))), "[\r\n\"]")),
      CRON = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG)), "[\r\n\"]")),
      
      ETN_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_etn)), "", !!sym(col_etn))), "[\r\n\"]")),
      NAC_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_nac)), "", !!sym(col_nac))), "[\r\n\"]")),
      PUE_RAW = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_pue)), "", !!sym(col_pue))), "[\r\n\"]"))
    ) %>%
    mutate(
      # Cascada Étnica
      es_pueblo_huancavilca = str_detect(PUE_RAW, "HUANCAVILCA"),
      tiene_pueblo = str_detect(PUE_RAW, "CHIBULEO|HUANCAVILCA|KA.*ARI|CA.*ARI|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|KICHWA AMAZ.*|KISAPINCHA|QUISAPINCHA|KITUKARA|QUITUCARA|NATABUELA|OTAVALO|PALTAS|PANZALEO|PASTOS|PURUH.*|PURUHÁ|PURUHA|SALASA.*|SARAGURO|TOMABELA|WARAMKA|GUARANCA"),
      tiene_nac = str_detect(NAC_RAW, "ACHUAR|ANDOA|AWA|AWÁ|CHACHI|COF.*N|KOF.*N|EPERA|.*PERA|HUANCAVILCA|KICHWA|QUICHUA|SECOYA|SHIWIAR|SHUAR|TS.*CHILA|TSÁCHILA|WAORANI|HUAORANI|ZAPARA|ZÁPARA"),
      
      ETNIA_RESC = case_when(tiene_pueblo | tiene_nac ~ "INDÍGENA", TRUE ~ ETN_RAW),
      NAC_RESC = NAC_RAW, 
      
      iE = case_when(
        str_detect(ETNIA_RESC, 'AFRO.*') ~ 1, str_detect(ETNIA_RESC, 'BLANC.*') ~ 2,
        str_detect(ETNIA_RESC, 'IND.*GENA|INDÍGENA') ~ 3, str_detect(ETNIA_RESC, 'MESTIZ.*') ~ 4,
        str_detect(ETNIA_RESC, 'MONTUBI.*|MONTUVI.*') ~ 5, str_detect(ETNIA_RESC, 'MULAT.*') ~ 6,
        str_detect(ETNIA_RESC, 'NEGR.*') ~ 7, str_detect(ETNIA_RESC, 'OTR.*') ~ 8,
        str_detect(ETNIA_RESC, 'APLICA') ~ 10,
        str_detect(ETNIA_RESC, 'SABE.*|RESPONDE') | ETNIA_RESC == "" | is.na(ETNIA_RESC) ~ 9, 
        TRUE ~ 9
      ),
      
      iN = case_when(
        iE == 3 ~ case_when(
          str_detect(NAC_RESC, 'ACHUAR') ~ 1, str_detect(NAC_RESC, 'ANDOA') ~ 2, str_detect(NAC_RESC, 'AWA') ~ 3,
          str_detect(NAC_RESC, 'CHACHI') ~ 4, str_detect(NAC_RESC, 'COF.*N|KOF.*N') ~ 5, str_detect(NAC_RESC, 'EPERA|.*PERA') ~ 6,
          str_detect(NAC_RESC, 'HUANCAVILCA') ~ 7, str_detect(NAC_RESC, 'KICHWA|QUICHUA') ~ 8, str_detect(NAC_RESC, 'SECOYA') ~ 9,
          str_detect(NAC_RESC, 'SHIWIAR') ~ 10, str_detect(NAC_RESC, 'SHUAR') ~ 11, str_detect(NAC_RESC, 'TS.*CHILA') ~ 12,
          str_detect(NAC_RESC, 'WAORANI|HUAORANI') ~ 13, str_detect(NAC_RESC, 'ZAPARA') ~ 14, 
          TRUE ~ 15
        ), TRUE ~ -1 
      ),
      
      iP = case_when(
        iE == 3 ~ case_when(
          tiene_pueblo & !es_pueblo_huancavilca ~ case_when(
            str_detect(PUE_RAW, 'CHIBULEO') ~ 1, str_detect(PUE_RAW, 'KA.*ARI|CA.*ARI') ~ 3, str_detect(PUE_RAW, 'KARANKI|CARANQUI') ~ 4,
            str_detect(PUE_RAW, 'KAYAMBI|CAYAMBI') ~ 5, str_detect(PUE_RAW, 'KICHWA AMAZ.*') ~ 6, str_detect(PUE_RAW, 'KISAPINCHA|QUISAPINCHA') ~ 7,
            str_detect(PUE_RAW, 'KITUKARA|QUITUCARA') ~ 8, str_detect(PUE_RAW, 'NATABUELA') ~ 9, str_detect(PUE_RAW, 'OTAVALO') ~ 10,
            str_detect(PUE_RAW, 'PALTAS') ~ 11, str_detect(PUE_RAW, 'PANZALEO') ~ 12, str_detect(PUE_RAW, 'PASTOS') ~ 13,
            str_detect(PUE_RAW, 'PURUH.*') ~ 14, str_detect(PUE_RAW, 'SALASA.*') ~ 15, str_detect(PUE_RAW, 'SARAGURO') ~ 16,
            str_detect(PUE_RAW, 'TOMABELA') ~ 17, str_detect(PUE_RAW, 'WARAMKA|GUARANCA') ~ 18, TRUE ~ 19
          ),
          es_pueblo_huancavilca ~ 2,
          TRUE ~ 19
        ), TRUE ~ -1 
      ),
      
      # Clínicos Generales
      esPrimeraGeneral = str_detect(CRON, "PRIMER|^P$|^1$|^N$"),
      esSubsecuenteGeneral = str_detect(CRON, "SUBSECUENTE|^S$|^2$"),
      esAguda = (CIE == "E441"),
      esCronica = (CIE %in% c("E43X", "E43")),
      esSobrepeso = (CIE %in% c("E660", "E669")),
      esAnemiaNino = (CIE %in% c("D46X", "D46"))
    )
  
  # 👶 PEDIATRÍA (Fila a Fila, se mantiene intacto)
  b_24 <- base_maestra %>% filter(EDAD_MESES >= 0 & EDAD_MESES < 24)
  b_59 <- base_maestra %>% filter(EDAD_MESES >= 0 & EDAD_MESES < 60)
  
  # 🤰 OBSTETRICIA (Implante de ADN de Ficha Metodológica)
  b_emb <- base_maestra %>% 
    filter(
      CIE %in% c("Z340", "Z348", "Z349", "Z350", "Z351", "Z352", "Z353", "Z354", "Z355", "Z356", "Z357", "Z358", "Z359", "Z321"),
      str_detect(ESP, "MEDICINA GENERAL INTEGRAL|MEDICINA FAMILIAR Y COMUNITARIA|MEDICINA RURAL|MEDICO RURAL|MÉDICO RURAL|GINECOLOGÍA Y OBSTETRICIA|GINECOLOGIA Y OBSTETRICIA|ATENCIÓN PRIMARIA EN SALUD|ATENCION PRIMARIA EN SALUD|OBSTETRICIA|OBSTETRICIA RURAL|MEDICINA GENERAL"),
      esPrimeraGeneral == TRUE
    ) %>%
    # 🚀 LA PURGA POR TICKET: Unifica la métrica con el T2
    distinct(TICKET, .keep_all = TRUE) %>%
    mutate(
      esBajoPesoEmb = (IMC > 0 & IMC < 18),
      esSobrepesoEmb = (IMC >= 25 & IMC <= 29.9),
      esObesidadEmb = (IMC >= 30)
    )
  
  # Sumatorias de Poblaciones
  t24_pri <- sum(b_24$esPrimeraGeneral, na.rm=T)
  t24_cro <- sum(b_24$esCronica, na.rm=T)
  t24_agu <- sum(b_24$esAguda, na.rm=T)
  t24_sob <- sum(b_24$esSobrepeso, na.rm=T)
  t24_ane <- sum(b_24$esAnemiaNino, na.rm=T)
  
  t59_pri <- sum(b_59$esPrimeraGeneral, na.rm=T)
  t59_cro <- sum(b_59$esCronica, na.rm=T)
  t59_agu <- sum(b_59$esAguda, na.rm=T)
  t59_sob <- sum(b_59$esSobrepeso, na.rm=T)
  t59_sub <- sum(b_59$esSubsecuenteGeneral, na.rm=T)
  
  tem_pri <- nrow(b_emb) 
  tem_obe <- sum(b_emb$esObesidadEmb, na.rm=T)
  tem_baj <- sum(b_emb$esBajoPesoEmb, na.rm=T)
  tem_sob <- sum(b_emb$esSobrepesoEmb, na.rm=T)
  
  fc <- function(x) { if (x > 0) return(x) else return(NA_real_) }
  
  # Inyección Superior
  range_write(ss = id_google_sheet, data = data.frame(fc(t24_pri)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "3"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t24_cro)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "4"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t24_agu)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "5"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t24_sob)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "6"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t24_ane)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "7"), col_names = FALSE, reformat = FALSE)
  
  range_write(ss = id_google_sheet, data = data.frame(fc(t59_pri)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "12"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t59_cro)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "13"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t59_agu)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "14"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t59_sob)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "15"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(t59_sub)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "18"), col_names = FALSE, reformat = FALSE)

  range_write(ss = id_google_sheet, data = data.frame(fc(tem_pri)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "23"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(tem_obe)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "25"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(tem_baj)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "26"), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = data.frame(fc(tem_sob)), range = paste0("ESTADO_NUTRI!", letra_col_conteo, "27"), col_names = FALSE, reformat = FALSE)
  
  # Construcción Matricial (50 Columnas, GPS Intacto)
  salto_filas <- (mes_objetivo - 1) * 8
  f_inicio_24 <- 35 + salto_filas
  f_inicio_59 <- 132 + salto_filas
  f_inicio_emb <- 229 + salto_filas
  
  construir_fila_plana <- function(base_filtro, indicador_logico) {
    vE <- numeric(10); vN <- numeric(15); vP <- numeric(19)
    sub <- base_filtro %>% filter(!!sym(indicador_logico) == TRUE)
    
    for(i in 1:10) vE[i] <- sum(sub$iE == i, na.rm=T)
    for(i in 1:15) vN[i] <- sum(sub$iN == i & sub$iE == 3, na.rm=T) 
    for(i in 1:19) vP[i] <- sum(sub$iP == i & sub$iE == 3, na.rm=T)
    
    fila_completa <- c(vE, NA_real_, NA_real_, vN, NA_real_, NA_real_, NA_real_, vP, NA_real_)
    fila_completa[fila_completa == 0] <- NA_real_
    return(fila_completa)
  }
  
  matriz_24 <- as.data.frame(matrix(NA_real_, nrow = 4, ncol = 50))
  matriz_24[1, ] <- construir_fila_plana(b_24, "esAguda")
  matriz_24[2, ] <- construir_fila_plana(b_24, "esCronica")
  matriz_24[3, ] <- construir_fila_plana(b_24, "esSobrepeso")
  matriz_24[4, ] <- construir_fila_plana(b_24, "esAnemiaNino")
  
  matriz_59 <- as.data.frame(matrix(NA_real_, nrow = 3, ncol = 50))
  matriz_59[1, ] <- construir_fila_plana(b_59, "esAguda")
  matriz_59[2, ] <- construir_fila_plana(b_59, "esCronica")
  matriz_59[3, ] <- construir_fila_plana(b_59, "esSobrepeso")
  
  matriz_emb <- as.data.frame(matrix(NA_real_, nrow = 3, ncol = 50))
  matriz_emb[1, ] <- construir_fila_plana(b_emb, "esObesidadEmb")
  matriz_emb[2, ] <- construir_fila_plana(b_emb, "esBajoPesoEmb")
  matriz_emb[3, ] <- construir_fila_plana(b_emb, "esSobrepesoEmb")
  
  matriz_24 <- matriz_24 %>% mutate(across(everything(), as.numeric))
  matriz_59 <- matriz_59 %>% mutate(across(everything(), as.numeric))
  matriz_emb <- matriz_emb %>% mutate(across(everything(), as.numeric))
  
  range_write(ss = id_google_sheet, data = matriz_24, range = paste0("ESTADO_NUTRI!B", f_inicio_24), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = matriz_59, range = paste0("ESTADO_NUTRI!B", f_inicio_59), col_names = FALSE, reformat = FALSE)
  range_write(ss = id_google_sheet, data = matriz_emb, range = paste0("ESTADO_NUTRI!B", f_inicio_emb), col_names = FALSE, reformat = FALSE)
}
