# ====================================================================================
# TRABAJADOR 13: ADOLESCENTES EN RIESGO (10 a 19 años)
# Arquitectura: Doble Pista de Aterrizaje -> Matriz Clínica (Arriba) + Intercultural (Abajo)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador13_AdolescentesRiesgo <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🎯 Trabajador 13 activado: Analizando Doble Matriz de Adolescentes en Riesgo...\n")
  
  # ====================================================================
  # 1. MAPA DE MESES Y COLUMNAS DESTINO (Matriz Superior)
  # ====================================================================
  mes_actual_num <- mes_objetivo
  
  col_h_num <- (mes_actual_num * 2) + 1
  col_m_num <- (mes_actual_num * 2) + 2
  col_emb_num <- mes_actual_num + 2
  
  letras <- c(LETTERS, paste0("A", LETTERS))
  letra_H <- letras[col_h_num]
  letra_M <- letras[col_m_num]
  letra_EMB <- letras[col_emb_num]
  
  # ====================================================================
  # 2. RADARES DE SELVA (Escudos Anti-NA)
  # ====================================================================
  nombres_cols <- names(tabla_consolidada)
  
  col_etn <- if(any(str_detect(nombres_cols, "PCTE_AUT"))) nombres_cols[str_detect(nombres_cols, "PCTE_AUT")][1] else if(any(str_detect(nombres_cols, "ETNICA|AUTOIDENTIFICACION"))) nombres_cols[str_detect(nombres_cols, "ETNICA|AUTOIDENTIFICACION")][1] else { tabla_consolidada$PCTE_AUTID_ETN <- ""; "PCTE_AUTID_ETN" }
  col_nac <- if(any(str_detect(nombres_cols, "PCTE_NAC_ETN"))) "PCTE_NAC_ETN" else if(any(str_detect(nombres_cols, "NACIONALIDAD"))) nombres_cols[str_detect(nombres_cols, "NACIONALIDAD")][1] else { tabla_consolidada$PCTE_NAC_ETN <- ""; "PCTE_NAC_ETN" }
  col_pue <- if(any(str_detect(nombres_cols, "PCTE_PUEBLO"))) "PCTE_PUEBLO" else if(any(str_detect(nombres_cols, "PUEBLO"))) nombres_cols[str_detect(nombres_cols, "PUEBLO")][1] else { tabla_consolidada$PCTE_PUEBLO <- ""; "PCTE_PUEBLO" }
  
  # ====================================================================
  # 3. LIMPIEZA, FILTRADO Y CLASIFICADORES
  # ====================================================================
  base_limpia <- tabla_consolidada %>%
    mutate(
      EDAD = as.numeric(ifelse(is.na(PCTE_ANIOS), -1, PCTE_ANIOS)),
      CIE = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CIE10), "", ATEMED_CIE10)), "[\r\n\"\\.]")),
      TIP = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_TIP_DIAG), "", ATEMED_TIP_DIAG)), "[\r\n\"]")),
      CRON = str_squish(str_remove_all(toupper(ifelse(is.na(ATEMED_CRON_DIAG), "", ATEMED_CRON_DIAG)), "[\r\n\"]")),
      DISC = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_DISC), "", PCTE_DISC)), "[\r\n\"]")),
      TIP_DISC = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_TIP_DISC), "", PCTE_TIP_DISC)), "[\r\n\"]")),
      SEXO = str_squish(str_remove_all(toupper(ifelse(is.na(PCTE_SEXO), "", PCTE_SEXO)), "[\r\n\"]")),
      ETNIA = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_etn)), "NO APLICA", !!sym(col_etn))), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_nac)), "NO APLICA", !!sym(col_nac))), "[\r\n\"]")),
      PUEB = str_squish(str_remove_all(toupper(ifelse(is.na(!!sym(col_pue)), "NO APLICA", !!sym(col_pue))), "[\r\n\"]"))
    ) %>%
    # FILTRO MAESTRO: 10 a 19 años, PRIMERA VEZ
    filter(EDAD >= 10 & EDAD <= 19) %>%
    filter(str_detect(CRON, "PRIMER|^P$|^1$|^N$")) %>%
    mutate(
      esMujer = str_detect(SEXO, "MUJER|FEMENINO|^F$"),
      esHombre = !str_detect(SEXO, "MUJER|FEMENINO|^F$"),
      
      # ⚠️ RADARES DE SELVA ACTIVADOS (Blindaje ortográfico total .*)
      iE = case_when(str_detect(ETNIA, 'AFRO.*') ~ 1, str_detect(ETNIA, 'BLANC.*') ~ 2, str_detect(ETNIA, 'IND.*GENA') ~ 3, str_detect(ETNIA, 'MESTIZ.*') ~ 4, str_detect(ETNIA, 'MONTUB.*|MONTUV.*') ~ 5, str_detect(ETNIA, 'MULAT.*') ~ 6, str_detect(ETNIA, 'NEGR.*') ~ 7, str_detect(ETNIA, 'OTR.*') ~ 8, str_detect(ETNIA, 'SABE.*|RESPONDE') ~ 9, str_detect(ETNIA, 'APLICA|^$') ~ 10, TRUE ~ 10),
      iN = case_when(str_detect(NAC, 'ACHUAR') ~ 1, str_detect(NAC, 'ANDOA') ~ 2, str_detect(NAC, 'AWA|AWÁ') ~ 3, str_detect(NAC, 'CHACHI') ~ 4, str_detect(NAC, 'COF.*N|KOF.*N') ~ 5, str_detect(NAC, 'EPERA|.*PERA') ~ 6, str_detect(NAC, 'HUANCAVILCA') ~ 7, str_detect(NAC, 'KICH.*|QUICH.*') | str_detect(PUEB, 'CHIBULEO|SALASA|TOMABELA|KISAPINCHA|QUISAPINCHA|NATABUELA|OTAVALO|PURUH|KARANKI|CARANQUI|KAYAMBI|CAYAMBI|PANZALEO|PASTOS|WARAMKA|GUARANCA|KA.*ARI|CA.*ARI') ~ 8, str_detect(NAC, 'SECOYA') ~ 9, str_detect(NAC, 'SHIWIAR') ~ 10, str_detect(NAC, 'SHUAR') ~ 11, str_detect(NAC, 'TS.*CHILA') ~ 12, str_detect(NAC, 'WAORANI|HUAORANI') ~ 13, str_detect(NAC, 'Z.*PARA') ~ 14, str_detect(NAC, 'SABE.*|RESPONDE') ~ 15, str_detect(NAC, 'APLICA|^$') ~ 16, TRUE ~ 16),
      iP = case_when(str_detect(PUEB, 'CHIBULEO') ~ 1, str_detect(PUEB, 'HUANCAVILCA') ~ 2, str_detect(PUEB, 'KA.*ARI|CA.*ARI') ~ 3, str_detect(PUEB, 'KARANKI|CARANQUI') ~ 4, str_detect(PUEB, 'KAYAMBI|CAYAMBI') ~ 5, str_detect(PUEB, 'KICHWA AMAZ.*') ~ 6, str_detect(PUEB, 'KISAPINCHA|QUISAPINCHA') ~ 7, str_detect(PUEB, 'KITUKARA|QUITUCARA') ~ 8, str_detect(PUEB, 'NATABUELA') ~ 9, str_detect(PUEB, 'OTAVALO') ~ 10, str_detect(PUEB, 'PALTAS') ~ 11, str_detect(PUEB, 'PANZALEO') ~ 12, str_detect(PUEB, 'PASTOS') ~ 13, str_detect(PUEB, 'PURUH.*') ~ 14, str_detect(PUEB, 'SALASA.*') ~ 15, str_detect(PUEB, 'SARAGURO') ~ 16, str_detect(PUEB, 'TOMABELA') ~ 17, str_detect(PUEB, 'WARAMKA|GUARANCA') ~ 18, str_detect(PUEB, 'SABE.*|RESPONDE') ~ 19, str_detect(PUEB, 'APLICA|^$') ~ 20, TRUE ~ 20)
    )
  
  # ====================================================================
  # FUNCIONES DE INYECCIÓN MATRIZ SUPERIOR (Clínica / Género)
  # ====================================================================
  procesar_top_hm <- function(base, condicion, fila_destino) {
    sub_df <- base %>% filter(eval(parse(text = condicion)))
    if(nrow(sub_df) > 0) {
      h <- nrow(sub_df %>% filter(esHombre)); m <- nrow(sub_df %>% filter(esMujer))
      if(h > 0) range_write(ss = id_google_sheet, data = data.frame(h), range = paste0("ADOLESCENTES_RIESGO!", letra_H, fila_destino), col_names=F, reformat=F)
      if(m > 0) range_write(ss = id_google_sheet, data = data.frame(m), range = paste0("ADOLESCENTES_RIESGO!", letra_M, fila_destino), col_names=F, reformat=F)
    }
  }
  
  procesar_top_emb <- function(base, condicion, fila_destino) {
    sub_df <- base %>% filter(eval(parse(text = condicion)))
    if(nrow(sub_df) > 0) {
      total <- nrow(sub_df)
      range_write(ss = id_google_sheet, data = data.frame(total), range = paste0("ADOLESCENTES_RIESGO!", letra_EMB, fila_destino), col_names=F, reformat=F)
    }
  }
  
  # ====================================================================
  # FUNCIÓN MOTOR: MATRIZ INFERIOR (Pista de Aterrizaje Intercultural)
  # EVASIÓN DE FÓRMULAS: Crea 3 bloques exactos (E=10, N=16, P=20)
  # ====================================================================
  generar_bloques_inter <- function(lista_dfs) {
    num_rows <- length(lista_dfs)
    mE <- matrix(NA_real_, nrow=num_rows, ncol=10)
    mN <- matrix(NA_real_, nrow=num_rows, ncol=16)
    mP <- matrix(NA_real_, nrow=num_rows, ncol=20)
    
    for(r in 1:num_rows) {
      sub_df <- lista_dfs[[r]]
      if(nrow(sub_df) > 0) {
        for(i in 1:10) { c <- sum(sub_df$iE == i, na.rm=T); if(c > 0) mE[r, i] <- c }
        for(i in 1:16) { c <- sum(sub_df$iN == i, na.rm=T); if(c > 0) mN[r, i] <- c }
        for(i in 1:20) { c <- sum(sub_df$iP == i, na.rm=T); if(c > 0) mP[r, i] <- c }
      }
    }
    
    list(
      E = as.data.frame(mE) %>% mutate(across(everything(), as.numeric)),
      N = as.data.frame(mN) %>% mutate(across(everything(), as.numeric)),
      P = as.data.frame(mP) %>% mutate(across(everything(), as.numeric))
    )
  }
  
  # ====================================================================
  # PROCESAMIENTO 1: DISCAPACIDADES
  # ====================================================================
  b_disc <- base_limpia %>% filter(str_detect(DISC, "^SI|^SÍ")) %>% distinct(ATEMED_ID, TIP_DISC, .keep_all = TRUE)
  
  # INYECCIÓN SUPERIOR (Mantiene histórico de filas 4 al 9)
  procesar_top_hm(b_disc, 'str_detect(TIP_DISC, "AUDITIV")', 4)
  procesar_top_hm(b_disc, 'str_detect(TIP_DISC, "FISIC|F.*SIC")', 5)
  procesar_top_hm(b_disc, 'str_detect(TIP_DISC, "INTELECTUAL")', 6)
  procesar_top_hm(b_disc, 'str_detect(TIP_DISC, "LENGUAJE")', 7)
  procesar_top_hm(b_disc, 'str_detect(TIP_DISC, "PSICOSOCIAL")', 8)
  procesar_top_hm(b_disc, 'str_detect(TIP_DISC, "VISUAL")', 9)
  
  # 🚀 INYECCIÓN INFERIOR: PISTA DE ATERRIZAJE (6 Filas Exactas, Salto de 10)
  # ⚠️ Corrección Senior Aplicada: Se reintegró la discapacidad Auditiva.
  f_disc <- 49 + (mes_objetivo - 1) * 10
  
  dfs_disc <- list(
    b_disc %>% filter(str_detect(TIP_DISC, "AUDITIV")),
    b_disc %>% filter(str_detect(TIP_DISC, "FISIC|F.*SIC")),
    b_disc %>% filter(str_detect(TIP_DISC, "INTELECTUAL")),
    b_disc %>% filter(str_detect(TIP_DISC, "LENGUAJE")),
    b_disc %>% filter(str_detect(TIP_DISC, "PSICOSOCIAL")),
    b_disc %>% filter(str_detect(TIP_DISC, "VISUAL"))
  )
  
  bloques_disc <- generar_bloques_inter(dfs_disc)
  range_write(ss = id_google_sheet, data = bloques_disc$E, range = paste0("ADOLESCENTES_RIESGO!C", f_disc), col_names=F, reformat=F)
  range_write(ss = id_google_sheet, data = bloques_disc$N, range = paste0("ADOLESCENTES_RIESGO!O", f_disc), col_names=F, reformat=F)
  range_write(ss = id_google_sheet, data = bloques_disc$P, range = paste0("ADOLESCENTES_RIESGO!AG", f_disc), col_names=F, reformat=F)
  
  # ====================================================================
  # PROCESAMIENTO 2: VIOLENCIA
  # ====================================================================
  b_violencia <- base_limpia %>% filter(CIE %in% c("T740", "T741", "T742", "T743", "T748", "T749") & str_detect(TIP, "MORB"))
  
  # INYECCIÓN SUPERIOR
  procesar_top_hm(b_violencia, 'EDAD >= 10 & EDAD <= 14', 18)
  procesar_top_hm(b_violencia, 'EDAD >= 15 & EDAD <= 19', 19)
  
  # 🚀 INYECCIÓN INFERIOR (2 Filas Exactas, Salto de 7)
  f_viol <- 169 + (mes_objetivo - 1) * 7
  
  dfs_viol <- list(
    b_violencia %>% filter(EDAD >= 10 & EDAD <= 14),
    b_violencia %>% filter(EDAD >= 15 & EDAD <= 19)
  )
  
  bloques_viol <- generar_bloques_inter(dfs_viol)
  range_write(ss = id_google_sheet, data = bloques_viol$E, range = paste0("ADOLESCENTES_RIESGO!C", f_viol), col_names=F, reformat=F)
  range_write(ss = id_google_sheet, data = bloques_viol$N, range = paste0("ADOLESCENTES_RIESGO!O", f_viol), col_names=F, reformat=F)
  range_write(ss = id_google_sheet, data = bloques_viol$P, range = paste0("ADOLESCENTES_RIESGO!AG", f_viol), col_names=F, reformat=F)
  
  # ====================================================================
  # PROCESAMIENTO 3: ESTADO NUTRICIONAL
  # ====================================================================
  b_nutri <- base_limpia %>% filter(CIE %in% c("E43X", "E43", "E441", "E660", "E669"))
  
  # INYECCIÓN SUPERIOR
  procesar_top_hm(b_nutri, 'CIE %in% c("E43X", "E43", "E441")', 28)
  procesar_top_hm(b_nutri, 'CIE %in% c("E660", "E669")', 29)
  
  # 🚀 INYECCIÓN INFERIOR (2 Filas Exactas, Salto de 7)
  f_nutri <- 253 + (mes_objetivo - 1) * 7
  
  dfs_nutri <- list(
    b_nutri %>% filter(CIE %in% c("E43X", "E43", "E441")),
    b_nutri %>% filter(CIE %in% c("E660", "E669"))
  )
  
  bloques_nutri <- generar_bloques_inter(dfs_nutri)
  range_write(ss = id_google_sheet, data = bloques_nutri$E, range = paste0("ADOLESCENTES_RIESGO!C", f_nutri), col_names=F, reformat=F)
  range_write(ss = id_google_sheet, data = bloques_nutri$N, range = paste0("ADOLESCENTES_RIESGO!O", f_nutri), col_names=F, reformat=F)
  range_write(ss = id_google_sheet, data = bloques_nutri$P, range = paste0("ADOLESCENTES_RIESGO!AG", f_nutri), col_names=F, reformat=F)
  
  # ====================================================================
  # PROCESAMIENTO 4: SALUD REPRODUCTIVA
  # ====================================================================
  b_repro <- base_limpia %>% filter(esMujer)
  
  cond_emb_14 <- 'str_detect(TIP, "PREV") & str_detect(CIE, "^Z321|^Z34|^Z35") & EDAD >= 10 & EDAD <= 14'
  cond_emb_19 <- 'str_detect(TIP, "PREV") & str_detect(CIE, "^Z321|^Z34|^Z35") & EDAD >= 15 & EDAD <= 19'
  cond_post_14 <- 'CIE == "Z392" & EDAD >= 10 & EDAD <= 14'
  cond_post_19 <- 'CIE == "Z392" & EDAD >= 15 & EDAD <= 19'
  
  # INYECCIÓN SUPERIOR
  procesar_top_emb(b_repro, cond_emb_14, 38)
  procesar_top_emb(b_repro, cond_emb_19, 39)
  procesar_top_emb(b_repro, cond_post_14, 40)
  procesar_top_emb(b_repro, cond_post_19, 41)
  
  # 🚀 INYECCIÓN INFERIOR (4 Filas Exactas, Salto de 9)
  f_repro <- 337 + (mes_objetivo - 1) * 9
  
  dfs_repro <- list(
    b_repro %>% filter(eval(parse(text = cond_emb_14))),
    b_repro %>% filter(eval(parse(text = cond_emb_19))),
    b_repro %>% filter(eval(parse(text = cond_post_14))),
    b_repro %>% filter(eval(parse(text = cond_post_19)))
  )
  
  bloques_repro <- generar_bloques_inter(dfs_repro)
  range_write(ss = id_google_sheet, data = bloques_repro$E, range = paste0("ADOLESCENTES_RIESGO!C", f_repro), col_names=F, reformat=F)
  range_write(ss = id_google_sheet, data = bloques_repro$N, range = paste0("ADOLESCENTES_RIESGO!O", f_repro), col_names=F, reformat=F)
  range_write(ss = id_google_sheet, data = bloques_repro$P, range = paste0("ADOLESCENTES_RIESGO!AG", f_repro), col_names=F, reformat=F)
  
  cat("✅ Trabajador 13 finalizado. Pista de aterrizaje desplegada sin sobreescribir fórmulas.\n")
}
