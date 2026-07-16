# ====================================================================================
# TRABAJADOR 13: ADOLESCENTES EN RIESGO (10 a 19 años)
# Arquitectura: Filtro Edad 10-19 -> Primeras -> 4 Bloques Temáticos (Disc, Viol, Nutri, Repro)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador13_AdolescentesRiesgo <- function(tabla_consolidada, id_google_sheet, mes_objetivo) {
  cat("🎯 Trabajador 13 activado: Analizando matriz de Adolescentes en Riesgo...\n")
  
  # 1. MAPA DE MESES Y COLUMNAS DESTINO
  mes_actual_num <- mes_objetivo
  
  # Columnas para Bloques 1, 2 y 3 (Con separación Hombre/Mujer)
  col_h_num <- (mes_actual_num * 2) + 1
  col_m_num <- (mes_actual_num * 2) + 2
  
  # Columnas para Bloque 4 (Embarazadas, 1 sola columna por mes)
  col_emb_num <- mes_actual_num + 2
  
  letras <- c(LETTERS, paste0("A", LETTERS))
  letra_H <- letras[col_h_num]
  letra_M <- letras[col_m_num]
  letra_EMB <- letras[col_emb_num]
  
  # 2. LIMPIEZA, FILTRADO DE EDAD (10-19) Y PRIMERAS
  base_limpia <- tabla_consolidada %>%
    mutate(
      EDAD = as.numeric(PCTE_ANIOS),
      CIE = str_squish(str_remove_all(toupper(ATEMED_CIE10), "[\r\n\"\\.]")),
      TIP = str_squish(str_remove_all(toupper(ATEMED_TIP_DIAG), "[\r\n\"]")),
      CRON = str_squish(str_remove_all(toupper(ATEMED_CRON_DIAG), "[\r\n\"]")),
      DISC = str_squish(str_remove_all(toupper(PCTE_DISC), "[\r\n\"]")),
      TIP_DISC = str_squish(str_remove_all(toupper(PCTE_TIP_DISC), "[\r\n\"]")),
      SEXO = str_squish(str_remove_all(toupper(PCTE_SEXO), "[\r\n\"]")),
      ETN = str_squish(str_remove_all(toupper(PCTE_AUTID_ETN), "[\r\n\"]"))
    ) %>%
    # FILTRO MAESTRO: Solo de 10 a 19 años y de PRIMERA VEZ
    filter(EDAD >= 10 & EDAD <= 19) %>%
    filter(str_detect(CRON, "PRIMER|^P$|^1$|^N$")) %>%
    mutate(
      esMujer = str_detect(SEXO, "MUJER|FEMENINO|^F$"),
      esHombre = !str_detect(SEXO, "MUJER|FEMENINO|^F$"),
      iEtnia = case_when(
        str_detect(ETN, "AFRO") ~ 1, str_detect(ETN, "BLANC") ~ 2, str_detect(ETN, "INDIGENA|INDÍGENA") ~ 3,
        str_detect(ETN, "MESTIZ") ~ 4, str_detect(ETN, "MONTUB|MONTUV") ~ 5, str_detect(ETN, "MULAT") ~ 6,
        str_detect(ETN, "NEGR") ~ 7, str_detect(ETN, "APLICA") ~ 8, str_detect(ETN, "SABE|RESPONDE") ~ 9,
        str_detect(ETN, "OTR") ~ 10, TRUE ~ 10
      )
    )
  
  fc <- function(x) { if(is.na(x) || x == 0) return("") else return(x) }
  
  # ====================================================================
  # BLOQUE 1: DISCAPACIDADES (Filas 4 a 9)
  # ====================================================================
  b_disc <- base_limpia %>% filter(str_detect(DISC, "^SI|^SÍ")) %>% distinct(ATEMED_ID, TIP_DISC, .keep_all = TRUE)
  
  procesar_y_escribir <- function(base, condicion, fila_destino) {
    if(nrow(base) == 0) return()
    sub_df <- base %>% filter(eval(parse(text = condicion)))
    
    if(nrow(sub_df) > 0) {
      h <- nrow(sub_df %>% filter(esHombre)); m <- nrow(sub_df %>% filter(esMujer))
      
      # AQUÍ ESTÁ LA CORRECCIÓN: Agregué ss=, data=, y range=
      if(h>0) range_write(ss = id_google_sheet, data = data.frame(h), range = paste0("ADOLESCENTES_RIESGO!", letra_H, fila_destino), col_names=F, reformat=F)
      if(m>0) range_write(ss = id_google_sheet, data = data.frame(m), range = paste0("ADOLESCENTES_RIESGO!", letra_M, fila_destino), col_names=F, reformat=F)
      
      for(i in 1:10) {
        c_etn <- nrow(sub_df %>% filter(iEtnia == i))
        if(c_etn > 0) range_write(ss = id_google_sheet, data = data.frame(c_etn), range = paste0("ADOLESCENTES_RIESGO!", letras[30 + i], fila_destino), col_names=F, reformat=F)
      }
    }
  }
  
  procesar_y_escribir(b_disc, 'str_detect(TIP_DISC, "AUDITIV")', 4)
  procesar_y_escribir(b_disc, 'str_detect(TIP_DISC, "FISIC|FÍSIC")', 5)
  procesar_y_escribir(b_disc, 'str_detect(TIP_DISC, "INTELECTUAL")', 6)
  procesar_y_escribir(b_disc, 'str_detect(TIP_DISC, "LENGUAJE")', 7)
  procesar_y_escribir(b_disc, 'str_detect(TIP_DISC, "PSICOSOCIAL")', 8)
  procesar_y_escribir(b_disc, 'str_detect(TIP_DISC, "VISUAL")', 9)
  
  # ====================================================================
  # BLOQUE 2: VIOLENCIA (Filas 18 y 19)
  # ====================================================================
  b_violencia <- base_limpia %>% filter(CIE %in% c("T740", "T741", "T742", "T743", "T748", "T749") & str_detect(TIP, "MORB"))
  
  procesar_y_escribir(b_violencia, 'EDAD >= 10 & EDAD <= 14', 18)
  procesar_y_escribir(b_violencia, 'EDAD >= 15 & EDAD <= 19', 19)
  
  # ====================================================================
  # BLOQUE 3: ESTADO NUTRICIONAL (Filas 28 y 29)
  # ====================================================================
  b_nutri <- base_limpia %>% filter(CIE %in% c("E43X", "E43", "E441", "E660", "E669"))
  
  procesar_y_escribir(b_nutri, 'CIE %in% c("E43X", "E43", "E441")', 28)
  procesar_y_escribir(b_nutri, 'CIE %in% c("E660", "E669")', 29)
  
  # ====================================================================
  # BLOQUE 4: SALUD REPRODUCTIVA (Filas 38, 39, 40, 41)
  # ====================================================================
  b_repro <- base_limpia %>% filter(esMujer)
  
  procesar_repro_y_escribir <- function(base, condicion, fila_destino) {
    if(nrow(base) == 0) return()
    sub_df <- base %>% filter(eval(parse(text = condicion)))
    
    if(nrow(sub_df) > 0) {
      total <- nrow(sub_df)
      range_write(ss = id_google_sheet, data = data.frame(total), range = paste0("ADOLESCENTES_RIESGO!", letra_EMB, fila_destino), col_names=F, reformat=F)
      
      for(i in 1:10) {
        c_etn <- nrow(sub_df %>% filter(iEtnia == i))
        if(c_etn > 0) range_write(ss = id_google_sheet, data = data.frame(c_etn), range = paste0("ADOLESCENTES_RIESGO!", letras[16 + i], fila_destino), col_names=F, reformat=F)
      }
    }
  }
  
  cond_emb_14 <- 'str_detect(TIP, "PREV") & str_detect(CIE, "^Z321|^Z34|^Z35") & EDAD >= 10 & EDAD <= 14'
  cond_emb_19 <- 'str_detect(TIP, "PREV") & str_detect(CIE, "^Z321|^Z34|^Z35") & EDAD >= 15 & EDAD <= 19'
  cond_post_14 <- 'CIE == "Z392" & EDAD >= 10 & EDAD <= 14'
  cond_post_19 <- 'CIE == "Z392" & EDAD >= 15 & EDAD <= 19'
  
  procesar_repro_y_escribir(b_repro, cond_emb_14, 38)
  procesar_repro_y_escribir(b_repro, cond_emb_19, 39)
  procesar_repro_y_escribir(b_repro, cond_post_14, 40)
  procesar_repro_y_escribir(b_repro, cond_post_19, 41)
  
  cat("✅ Trabajador 13 finalizado. Pestaña ADOLESCENTES EN RIESGO actualizada.\n")
}