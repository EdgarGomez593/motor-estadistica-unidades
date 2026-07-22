# ====================================================================================
# TRABAJADOR 9: MOTOR EPIDEMIOLÓGICO (VISIÓN ANUAL + BLINDAJE DE CABECERAS)
# ====================================================================================
library(dplyr)
library(stringr)
library(googlesheets4)

trabajador9_PerfilEpidemiologico <- function(tabla_consolidada_anual, id_google_sheet, mes_maximo) {
  cat("🦠 Trabajador 9 activado: Procesando Perfil Epidemiológico Anual con Detector Inteligente...\n")
  
  # ====================================================================
  # 1. DETECTOR INTELIGENTE DE CABECERAS (Evita que el script muera si cambia un nombre)
  # ====================================================================
  nombres_cols <- names(tabla_consolidada_anual)
  
  encontrar_col <- function(posibles) {
    for(p in posibles) { if(p %in% nombres_cols) return(p) }
    return(posibles[1]) # Retorna el primero por defecto para crearlo vacío si falta
  }
  
  c_esp  <- encontrar_col(c("PROF_ESP_ATE", "ESPECIALIDAD"))
  c_tip  <- encontrar_col(c("ATEMED_TIP_DIAG", "ATEMED_TIPO_DIAG"))
  c_cron <- encontrar_col(c("ATEMED_CRON_DIAG", "ATEMED_CRONOLOGIA"))
  c_con  <- encontrar_col(c("ATEMED_CON_DIAG", "ATEMED_COND_DIAG"))
  c_cie  <- encontrar_col(c("ATEMED_CIE10", "CIE10"))
  c_des  <- encontrar_col(c("ATEMED_DES_CIE10", "DES_CIE10"))
  c_sex  <- encontrar_col(c("PCTE_SEXO", "SEXO"))
  c_etn  <- encontrar_col(c("PCTE_AUTID_ETN", "AUTID_ETN"))
  c_nac  <- encontrar_col(c("PCTE_NAC_ETN", "NAC_ETN"))
  c_pue  <- encontrar_col(c("PCTE_PUEBLO", "PUEBLO"))
  
  # Si alguna columna vital no viene en el CSV, la inyectamos vacía para blindar el sistema
  if (!c_esp %in% nombres_cols) tabla_consolidada_anual$PROF_ESP_ATE <- ""
  if (!c_tip %in% nombres_cols) tabla_consolidada_anual$ATEMED_TIP_DIAG <- ""
  if (!c_cron %in% nombres_cols) tabla_consolidada_anual$ATEMED_CRON_DIAG <- ""
  if (!c_con %in% nombres_cols) tabla_consolidada_anual$ATEMED_CON_DIAG <- ""
  if (!c_cie %in% nombres_cols) tabla_consolidada_anual$ATEMED_CIE10 <- ""
  if (!c_des %in% nombres_cols) tabla_consolidada_anual$ATEMED_DES_CIE10 <- ""
  if (!c_sex %in% nombres_cols) tabla_consolidada_anual$PCTE_SEXO <- ""
  if (!c_etn %in% nombres_cols) tabla_consolidada_anual$PCTE_AUTID_ETN <- ""
  if (!c_nac %in% nombres_cols) tabla_consolidada_anual$PCTE_NAC_ETN <- ""
  if (!c_pue %in% nombres_cols) tabla_consolidada_anual$PCTE_PUEBLO <- ""

  # 2. LIMPIEZA Y FILTRADO SEGURO
  base_limpia <- tabla_consolidada_anual %>%
    mutate(
      ESP = str_squish(str_remove_all(toupper(!!sym(c_esp)), "[\r\n\"]")),
      TIP = str_squish(str_remove_all(toupper(!!sym(c_tip)), "[\r\n\"]")),
      CR  = str_squish(str_remove_all(toupper(!!sym(c_cron)), "[\r\n\"]")),
      CON = str_squish(str_remove_all(toupper(!!sym(c_con)), "[\r\n\"]")),
      CIE = str_squish(str_remove_all(toupper(!!sym(c_cie)), "[\r\n\"\\.]")),
      DES = str_squish(str_remove_all(toupper(!!sym(c_des)), "[\r\n\"]")),
      SEX = str_squish(str_remove_all(toupper(!!sym(c_sex)), "[\r\n\"]")),
      ETN = str_squish(str_remove_all(toupper(!!sym(c_etn)), "[\r\n\"]")),
      NAC = str_squish(str_remove_all(toupper(!!sym(c_nac)), "[\r\n\"]")),
      PUE = str_squish(str_remove_all(toupper(!!sym(c_pue)), "[\r\n\"]"))
    ) %>%
    filter(
      str_detect(TIP, "MORBILIDAD") & 
        str_detect(CR, "PRIMER|^P$|^1$|^N$") & 
        str_detect(CON, "DEFINITIVO INICIAL")
    ) %>%
    mutate(
      esMed = str_detect(ESP, "MEDICIN|OBSTETR|PSICOLOG"), 
      esOdo = str_detect(ESP, "ODONTO"), 
      esMujer = str_detect(SEX, "MUJER|^F$"),
      esHombre = !str_detect(SEX, "MUJER|^F$")
    ) %>%
    filter(esMed | esOdo) 
  
  if(nrow(base_limpia) == 0) {
    cat("⚠️ ADVERTENCIA: La base filtrada del Trabajador 9 está en 0 registros. Revisa si los nombres de los diagnósticos coinciden.\n")
    return(NULL)
  }
  
  procesar_motor_anual <- function(base_datos, nombre_hoja, fGlobal_100, fGlobal_10, fMes_10, nombre_rama) {
    if(nrow(base_datos) == 0) {
      cat("   -> Sin datos para procesar en:", nombre_rama, "\n")
      return(NULL)
    }
    cat("   -> Procesando Top 100 para:", nombre_rama, "en la pestaña", nombre_hoja, "...\n")
    
    resumen_enfermedades <- base_datos %>%
      group_by(CIE, DES) %>%
      summarise(
        TOTAL_ANUAL = n(),
        
        H_1 = sum(MES_ORIGEN == 1 & esHombre, na.rm=T), M_1 = sum(MES_ORIGEN == 1 & esMujer, na.rm=T),
        H_2 = sum(MES_ORIGEN == 2 & esHombre, na.rm=T), M_2 = sum(MES_ORIGEN == 2 & esMujer, na.rm=T),
        H_3 = sum(MES_ORIGEN == 3 & esHombre, na.rm=T), M_3 = sum(MES_ORIGEN == 3 & esMujer, na.rm=T),
        H_4 = sum(MES_ORIGEN == 4 & esHombre, na.rm=T), M_4 = sum(MES_ORIGEN == 4 & esMujer, na.rm=T),
        H_5 = sum(MES_ORIGEN == 5 & esHombre, na.rm=T), M_5 = sum(MES_ORIGEN == 5 & esMujer, na.rm=T),
        H_6 = sum(MES_ORIGEN == 6 & esHombre, na.rm=T), M_6 = sum(MES_ORIGEN == 6 & esMujer, na.rm=T),
        H_7 = sum(MES_ORIGEN == 7 & esHombre, na.rm=T), M_7 = sum(MES_ORIGEN == 7 & esMujer, na.rm=T),
        H_8 = sum(MES_ORIGEN == 8 & esHombre, na.rm=T), M_8 = sum(MES_ORIGEN == 8 & esMujer, na.rm=T),
        H_9 = sum(MES_ORIGEN == 9 & esHombre, na.rm=T), M_9 = sum(MES_ORIGEN == 9 & esMujer, na.rm=T),
        H_10 = sum(MES_ORIGEN == 10 & esHombre, na.rm=T), M_10 = sum(MES_ORIGEN == 10 & esMujer, na.rm=T),
        H_11 = sum(MES_ORIGEN == 11 & esHombre, na.rm=T), M_11 = sum(MES_ORIGEN == 11 & esMujer, na.rm=T),
        H_12 = sum(MES_ORIGEN == 12 & esHombre, na.rm=T), M_12 = sum(MES_ORIGEN == 12 & esMujer, na.rm=T),
        
        E_Indigena = sum(str_detect(ETN, "IND.GENA"), na.rm=T), # COMODÍN REGEX APLICADO
        E_Mestizo  = sum(str_detect(ETN, "MESTIZ"), na.rm=T),
        E_Afro     = sum(str_detect(ETN, "AFRO"), na.rm=T),
        E_Montubio = sum(str_detect(ETN, "MONTUB|MONTUV"), na.rm=T),
        E_Mulato   = sum(str_detect(ETN, "MULAT"), na.rm=T),
        E_Negro    = sum(str_detect(ETN, "NEGR"), na.rm=T),
        E_Blanco   = sum(str_detect(ETN, "BLANC"), na.rm=T),
        E_NoAplica = sum(str_detect(ETN, "APLICA"), na.rm=T),
        E_NoSabe   = sum(str_detect(ETN, "SABE|RESPONDE"), na.rm=T),
        
        T10G_E_Indigena_H = sum(str_detect(ETN, "IND.GENA") & esHombre, na.rm=T),
        T10G_E_Indigena_M = sum(str_detect(ETN, "IND.GENA") & esMujer, na.rm=T),
        T10G_E_Mestizo_H  = sum(str_detect(ETN, "MESTIZ") & esHombre, na.rm=T),
        T10G_E_Mestizo_M  = sum(str_detect(ETN, "MESTIZ") & esMujer, na.rm=T),
        T10G_E_Blanco_H   = sum(str_detect(ETN, "BLANC") & esHombre, na.rm=T),
        T10G_E_Blanco_M   = sum(str_detect(ETN, "BLANC") & esMujer, na.rm=T),
        T10G_E_Aplica_H   = sum(str_detect(ETN, "APLICA") & esHombre, na.rm=T),
        T10G_E_Aplica_M   = sum(str_detect(ETN, "APLICA") & esMujer, na.rm=T),
        T10G_E_Montubio_H = sum(str_detect(ETN, "MONTUB|MONTUV") & esHombre, na.rm=T),
        T10G_E_Montubio_M = sum(str_detect(ETN, "MONTUB|MONTUV") & esMujer, na.rm=T),
        T10G_E_Sabe_H     = sum(str_detect(ETN, "SABE|RESPONDE") & esHombre, na.rm=T),
        T10G_E_Sabe_M     = sum(str_detect(ETN, "SABE|RESPONDE") & esMujer, na.rm=T),
        T10G_E_Afro_H     = sum(str_detect(ETN, "AFRO|MULAT|NEGR") & esHombre, na.rm=T),
        T10G_E_Afro_M     = sum(str_detect(ETN, "AFRO|MULAT|NEGR") & esMujer, na.rm=T),
        
        T10G_N_Kichwa_H   = sum(str_detect(NAC, "KICHWA|QUICHUA") & esHombre, na.rm=T),
        T10G_N_Kichwa_M   = sum(str_detect(NAC, "KICHWA|QUICHUA") & esMujer, na.rm=T),
        T10G_N_Sabe_H     = sum(str_detect(NAC, "SABE|RESPONDE") & esHombre, na.rm=T),
        T10G_N_Sabe_M     = sum(str_detect(NAC, "SABE|RESPONDE") & esMujer, na.rm=T),
        T10G_N_Shuar_H    = sum(str_detect(NAC, "SHUAR") & esHombre, na.rm=T),
        T10G_N_Shuar_M    = sum(str_detect(NAC, "SHUAR") & esMujer, na.rm=T),
        
        T10G_P_Tomabela_H = sum(str_detect(PUE, "TOMABELA") & esHombre, na.rm=T),
        T10G_P_Tomabela_M = sum(str_detect(PUE, "TOMABELA") & esMujer, na.rm=T),
        T10G_P_Chibuleo_H = sum(str_detect(PUE, "CHIBULEO") & esHombre, na.rm=T),
        T10G_P_Chibuleo_M = sum(str_detect(PUE, "CHIBULEO") & esMujer, na.rm=T),
        T10G_P_Otavalo_H  = sum(str_detect(PUE, "OTAVALO") & esHombre, na.rm=T),
        T10G_P_Otavalo_M  = sum(str_detect(PUE, "OTAVALO") & esMujer, na.rm=T),
        T10G_P_Puruha_H   = sum(str_detect(PUE, "PURUH.") & esHombre, na.rm=T),
        T10G_P_Puruha_M   = sum(str_detect(PUE, "PURUH.") & esMujer, na.rm=T),
        T10G_P_Salasaca_H = sum(str_detect(PUE, "SALASAKA|SALASACA") & esHombre, na.rm=T),
        T10G_P_Salasaca_M = sum(str_detect(PUE, "SALASAKA|SALASACA") & esMujer, na.rm=T),
        T10G_P_Guaranca_H = sum(str_detect(PUE, "WARANKA|GUARANCA") & esHombre, na.rm=T),
        T10G_P_Guaranca_M = sum(str_detect(PUE, "WARANKA|GUARANCA") & esMujer, na.rm=T),
        T10G_P_Karanki_H  = sum(str_detect(PUE, "KARANKI") & esHombre, na.rm=T),
        T10G_P_Karanki_M  = sum(str_detect(PUE, "KARANKI") & esMujer, na.rm=T),
        T10G_P_Sabe_H     = sum(str_detect(PUE, "SABE|RESPONDE") & esHombre, na.rm=T),
        T10G_P_Sabe_M     = sum(str_detect(PUE, "SABE|RESPONDE") & esMujer, na.rm=T),
        T10G_P_VacioInd_H = sum((PUE == "" | is.na(PUE)) & str_detect(ETN, "IND.GENA") & esHombre, na.rm=T),
        T10G_P_VacioInd_M = sum((PUE == "" | is.na(PUE)) & str_detect(ETN, "IND.GENA") & esMujer, na.rm=T),
        
        T10M_E_Indigena_H = sum(str_detect(ETN, "IND.GENA") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Indigena_M = sum(str_detect(ETN, "IND.GENA") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Mestizo_H  = sum(str_detect(ETN, "MESTIZ") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Mestizo_M  = sum(str_detect(ETN, "MESTIZ") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Blanco_H   = sum(str_detect(ETN, "BLANC") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Blanco_M   = sum(str_detect(ETN, "BLANC") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Aplica_H   = sum(str_detect(ETN, "APLICA") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Aplica_M   = sum(str_detect(ETN, "APLICA") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Montubio_H = sum(str_detect(ETN, "MONTUB|MONTUV") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Montubio_M = sum(str_detect(ETN, "MONTUB|MONTUV") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Sabe_H     = sum(str_detect(ETN, "SABE|RESPONDE") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Sabe_M     = sum(str_detect(ETN, "SABE|RESPONDE") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Afro_H     = sum(str_detect(ETN, "AFRO|MULAT|NEGR") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_E_Afro_M     = sum(str_detect(ETN, "AFRO|MULAT|NEGR") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        
        T10M_N_Kichwa_H   = sum(str_detect(NAC, "KICHWA|QUICHUA") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_N_Kichwa_M   = sum(str_detect(NAC, "KICHWA|QUICHUA") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_N_Sabe_H     = sum(str_detect(NAC, "SABE|RESPONDE") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_N_Sabe_M     = sum(str_detect(NAC, "SABE|RESPONDE") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_N_Shuar_H    = sum(str_detect(NAC, "SHUAR") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_N_Shuar_M    = sum(str_detect(NAC, "SHUAR") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        
        T10M_P_Tomabela_H = sum(str_detect(PUE, "TOMABELA") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Tomabela_M = sum(str_detect(PUE, "TOMABELA") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Chibuleo_H = sum(str_detect(PUE, "CHIBULEO") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Chibuleo_M = sum(str_detect(PUE, "CHIBULEO") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Otavalo_H  = sum(str_detect(PUE, "OTAVALO") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Otavalo_M  = sum(str_detect(PUE, "OTAVALO") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Puruha_H   = sum(str_detect(PUE, "PURUH.") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Puruha_M   = sum(str_detect(PUE, "PURUH.") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Salasaca_H = sum(str_detect(PUE, "SALASAKA|SALASACA") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Salasaca_M = sum(str_detect(PUE, "SALASAKA|SALASACA") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Guaranca_H = sum(str_detect(PUE, "WARANKA|GUARANCA") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Guaranca_M = sum(str_detect(PUE, "WARANKA|GUARANCA") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Karanki_H  = sum(str_detect(PUE, "KARANKI") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Karanki_M  = sum(str_detect(PUE, "KARANKI") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Sabe_H     = sum(str_detect(PUE, "SABE|RESPONDE") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_Sabe_M     = sum(str_detect(PUE, "SABE|RESPONDE") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_VacioInd_H = sum((PUE == "" | is.na(PUE)) & str_detect(ETN, "IND.GENA") & esHombre & MES_ORIGEN == mes_maximo, na.rm=T),
        T10M_P_VacioInd_M = sum((PUE == "" | is.na(PUE)) & str_detect(ETN, "IND.GENA") & esMujer & MES_ORIGEN == mes_maximo, na.rm=T),
        
        .groups = 'drop'
      ) %>%
      arrange(desc(TOTAL_ANUAL))
    
    mat_100_demo = matrix("", nrow = 100, ncol = 26); mat_100_etn = matrix("", nrow = 100, ncol = 10)
    mat_10G_E = matrix("", nrow=10, ncol=16); mat_10G_N = matrix("", nrow=10, ncol=8); mat_10G_P = matrix("", nrow=10, ncol=20)
    mat_10M_E = matrix("", nrow=10, ncol=16); mat_10M_N = matrix("", nrow=10, ncol=8); mat_10M_P = matrix("", nrow=10, ncol=20)
    
    fc <- function(val) { if(!is.na(val) && val > 0) return(val) else return("") }
    
    for(i in 1:min(100, nrow(resumen_enfermedades))) {
      r <- resumen_enfermedades[i, ]
      mat_100_demo[i, 1] <- r$CIE; mat_100_demo[i, 2] <- r$DES
      mat_100_demo[i,3]<-fc(r$H_1); mat_100_demo[i,4]<-fc(r$M_1); mat_100_demo[i,5]<-fc(r$H_2); mat_100_demo[i,6]<-fc(r$M_2)
      mat_100_demo[i,7]<-fc(r$H_3); mat_100_demo[i,8]<-fc(r$M_3); mat_100_demo[i,9]<-fc(r$H_4); mat_100_demo[i,10]<-fc(r$M_4)
      mat_100_demo[i,11]<-fc(r$H_5); mat_100_demo[i,12]<-fc(r$M_5); mat_100_demo[i,13]<-fc(r$H_6); mat_100_demo[i,14]<-fc(r$M_6)
      mat_100_demo[i,15]<-fc(r$H_7); mat_100_demo[i,16]<-fc(r$M_7); mat_100_demo[i,17]<-fc(r$H_8); mat_100_demo[i,18]<-fc(r$M_8)
      mat_100_demo[i,19]<-fc(r$H_9); mat_100_demo[i,20]<-fc(r$M_9); mat_100_demo[i,21]<-fc(r$H_10); mat_100_demo[i,22]<-fc(r$M_10)
      mat_100_demo[i,23]<-fc(r$H_11); mat_100_demo[i,24]<-fc(r$M_11); mat_100_demo[i,25]<-fc(r$H_12); mat_100_demo[i,26]<-fc(r$M_12)
      
      mat_100_etn[i,1]<-fc(r$E_Indigena); mat_100_etn[i,2]<-fc(r$E_Mestizo); mat_100_etn[i,3]<-fc(r$E_Afro)
      mat_100_etn[i,4]<-fc(r$E_Montubio); mat_100_etn[i,5]<-fc(r$E_Mulato);  mat_100_etn[i,6]<-fc(r$E_Negro)
      mat_100_etn[i,7]<-fc(r$E_Blanco);   mat_100_etn[i,8]<-fc(r$E_NoAplica);mat_100_etn[i,9]<-fc(r$E_NoSabe)
      otros <- r$TOTAL_ANUAL - sum(c(r$E_Indigena, r$E_Mestizo, r$E_Afro, r$E_Montubio, r$E_Mulato, r$E_Negro, r$E_Blanco, r$E_NoAplica, r$E_NoSabe))
      mat_100_etn[i,10]<-fc(otros)
    }
    
    for(i in 1:min(10, nrow(resumen_enfermedades))) {
      r <- resumen_enfermedades[i, ]
      mat_10G_E[i,1]<-r$CIE; mat_10G_E[i,2]<-r$DES; mat_10G_N[i,1]<-r$CIE; mat_10G_N[i,2]<-r$DES; mat_10G_P[i,1]<-r$CIE; mat_10G_P[i,2]<-r$DES
      mat_10G_E[i,3]<-fc(r$T10G_E_Indigena_H); mat_10G_E[i,4]<-fc(r$T10G_E_Indigena_M); mat_10G_E[i,5]<-fc(r$T10G_E_Mestizo_H); mat_10G_E[i,6]<-fc(r$T10G_E_Mestizo_M)
      mat_10G_E[i,7]<-fc(r$T10G_E_Blanco_H); mat_10G_E[i,8]<-fc(r$T10G_E_Blanco_M); mat_10G_E[i,9]<-fc(r$T10G_E_Aplica_H); mat_10G_E[i,10]<-fc(r$T10G_E_Aplica_M)
      mat_10G_E[i,11]<-fc(r$T10G_E_Montubio_H); mat_10G_E[i,12]<-fc(r$T10G_E_Montubio_M); mat_10G_E[i,13]<-fc(r$T10G_E_Sabe_H); mat_10G_E[i,14]<-fc(r$T10G_E_Sabe_M)
      mat_10G_E[i,15]<-fc(r$T10G_E_Afro_H); mat_10G_E[i,16]<-fc(r$T10G_E_Afro_M)
      
      mat_10G_N[i,3]<-fc(r$T10G_N_Kichwa_H); mat_10G_N[i,4]<-fc(r$T10G_N_Kichwa_M); mat_10G_N[i,5]<-fc(r$T10G_N_Sabe_H); mat_10G_N[i,6]<-fc(r$T10G_N_Sabe_M)
      mat_10G_N[i,7]<-fc(r$T10G_N_Shuar_H); mat_10G_N[i,8]<-fc(r$T10G_N_Shuar_M)
      
      mat_10G_P[i,3]<-fc(r$T10G_P_Tomabela_H); mat_10G_P[i,4]<-fc(r$T10G_P_Tomabela_M); mat_10G_P[i,5]<-fc(r$T10G_P_Chibuleo_H); mat_10G_P[i,6]<-fc(r$T10G_P_Chibuleo_M)
      mat_10G_P[i,7]<-fc(r$T10G_P_Otavalo_H); mat_10G_P[i,8]<-fc(r$T10G_P_Otavalo_M); mat_10G_P[i,9]<-fc(r$T10G_P_Puruha_H); mat_10G_P[i,10]<-fc(r$T10G_P_Puruha_M)
      mat_10G_P[i,11]<-fc(r$T10G_P_Salasaca_H); mat_10G_P[i,12]<-fc(r$T10G_P_Salasaca_M); mat_10G_P[i,13]<-fc(r$T10G_P_Guaranca_H); mat_10G_P[i,14]<-fc(r$T10G_P_Guaranca_M)
      mat_10G_P[i,15]<-fc(r$T10G_P_Karanki_H); mat_10G_P[i,16]<-fc(r$T10G_P_Karanki_M); mat_10G_P[i,17]<-fc(r$T10G_P_Sabe_H); mat_10G_P[i,18]<-fc(r$T10G_P_Sabe_M)
      mat_10G_P[i,19]<-fc(r$T10G_P_VacioInd_H); mat_10G_P[i,20]<-fc(r$T10G_P_VacioInd_M)
      
      mat_10M_E[i,1]<-r$CIE; mat_10M_E[i,2]<-r$DES; mat_10M_N[i,1]<-r$CIE; mat_10M_N[i,2]<-r$DES; mat_10M_P[i,1]<-r$CIE; mat_10M_P[i,2]<-r$DES
      mat_10M_E[i,3]<-fc(r$T10M_E_Indigena_H); mat_10M_E[i,4]<-fc(r$T10M_E_Indigena_M); mat_10M_E[i,5]<-fc(r$T10M_E_Mestizo_H); mat_10M_E[i,6]<-fc(r$T10M_E_Mestizo_M)
      mat_10M_E[i,7]<-fc(r$T10M_E_Blanco_H); mat_10M_E[i,8]<-fc(r$T10M_E_Blanco_M); mat_10M_E[i,9]<-fc(r$T10M_E_Aplica_H); mat_10M_E[i,10]<-fc(r$T10M_E_Aplica_M)
      mat_10M_E[i,11]<-fc(r$T10M_E_Montubio_H); mat_10M_E[i,12]<-fc(r$T10M_E_Montubio_M); mat_10M_E[i,13]<-fc(r$T10M_E_Sabe_H); mat_10M_E[i,14]<-fc(r$T10M_E_Sabe_M)
      mat_10M_E[i,15]<-fc(r$T10M_E_Afro_H); mat_10M_E[i,16]<-fc(r$T10M_E_Afro_M)
      
      mat_10M_N[i,3]<-fc(r$T10M_N_Kichwa_H); mat_10M_N[i,4]<-fc(r$T10M_N_Kichwa_M); mat_10M_N[i,5]<-fc(r$T10M_N_Sabe_H); mat_10M_N[i,6]<-fc(r$T10M_N_Sabe_M)
      mat_10M_N[i,7]<-fc(r$T10M_N_Shuar_H); mat_10M_N[i,8]<-fc(r$T10M_N_Shuar_M)
      
      mat_10M_P[i,3]<-fc(r$T10M_P_Tomabela_H); mat_10M_P[i,4]<-fc(r$T10M_P_Tomabela_M); mat_10M_P[i,5]<-fc(r$T10M_P_Chibuleo_H); mat_10M_P[i,6]<-fc(r$T10M_P_Chibuleo_M)
      mat_10M_P[i,7]<-fc(r$T10M_P_Otavalo_H); mat_10M_P[i,8]<-fc(r$T10M_P_Otavalo_M); mat_10M_P[i,9]<-fc(r$T10M_P_Puruha_H); mat_10M_P[i,10]<-fc(r$T10M_P_Puruha_M)
      mat_10M_P[i,11]<-fc(r$T10M_P_Salasaca_H); mat_10M_P[i,12]<-fc(r$T10M_P_Salasaca_M); mat_10M_P[i,13]<-fc(r$T10M_P_Guaranca_H); mat_10M_P[i,14]<-fc(r$T10M_P_Guaranca_M)
      mat_10M_P[i,15]<-fc(r$T10M_P_Karanki_H); mat_10M_P[i,16]<-fc(r$T10M_P_Karanki_M); mat_10M_P[i,17]<-fc(r$T10M_P_Sabe_H); mat_10M_P[i,18]<-fc(r$T10M_P_Sabe_M)
      mat_10M_P[i,19]<-fc(r$T10M_P_VacioInd_H); mat_10M_P[i,20]<-fc(r$T10M_P_VacioInd_M)
    }
    
    escribir <- function(mat, r) { 
      df <- as.data.frame(mat); df[,3:ncol(df)] <- lapply(df[,3:ncol(df)], as.numeric); 
      range_write(id_google_sheet, df, range = paste0(nombre_hoja, "!", r), col_names = FALSE, reformat = FALSE) 
    }
    
    cat("📝 Escribiendo Matrices para:", nombre_rama, "...\n")
    escribir(mat_100_demo, paste0("B", fGlobal_100))
    escribir(mat_100_etn, paste0("AF", fGlobal_100))
    
    escribir(mat_10G_E, paste0("BK", fGlobal_10))
    escribir(mat_10G_N, paste0("CE", fGlobal_10))
    escribir(mat_10G_P, paste0("CQ", fGlobal_10))
    
    escribir(mat_10M_E, paste0("BK", fMes_10))
    escribir(mat_10M_N, paste0("CE", fMes_10))
    escribir(mat_10M_P, paste0("CQ", fMes_10))
  }
  
  base_med <- base_limpia %>% filter(esMed)
  base_odo <- base_limpia %>% filter(esOdo)
  
  procesar_motor_anual(base_med, "PERFIL EPIDEMIOLOGICO", 6, 6, 22, "Medicina")
  procesar_motor_anual(base_odo, "CAUSAS ODONTOLOGICA", 6, 6, 22, "Odontología")
  
  cat("✅ Trabajador 9 finalizado. Perfil Epidemiológico Anual y Top 10 Intercultural completado.\n")
}
