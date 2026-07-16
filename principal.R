# ====================================================================================
# CEREBRO CENTRAL - AUTOMATIZACIÓN DE SALA SITUACIONAL (NIVEL OPERATIVO)
# ====================================================================================

library(googledrive)
library(googlesheets4)
library(dplyr)
library(stringr)

# Autenticación temporal
drive_auth(email = "oedgargs89pico@gmail.com")
gs4_auth(email = "oedgargs89pico@gmail.com") 

# CONTRATACIÓN DE TRABAJADORES
source("trabajadores/trabajador1_embarazadas.R")
source("trabajadores/trabajador2_general.R")
source("trabajadores/trabajador3_edades.R")
source("trabajadores/trabajador4_LugarAtencion.R")
source("trabajadores/trabajador5_MorbilidadPrevencion.R")
source("trabajadores/trabajador6_CronicasCIE10.R")
source("trabajadores/trabajador6_1_CronicasGenero.R")
source("trabajadores/trabajador7_CoberturaAtenciones.R")
source("trabajadores/trabajador8_InterculturalidadTotal.R")
source("trabajadores/trabajador9_PerfilEpidemiologico.R")
source("trabajadores/trabajador10_Discapacidades.R")
source("trabajadores/trabajador11_EstadoNutricional.R")
source("trabajadores/trabajador12_Violencia.R")
source("trabajadores/trabajador13_AdolescentesRiesgo.R")

# FUNCIÓN MAESTRA EN LA NUBE
ejecutar_sala_situacional_maestra <- function() {
  tryCatch({
    # --- ATRAPAR LOS DATOS QUE ENVÍA GOOGLE SHEETS ---
    modo <- Sys.getenv("MODO_EJECUCION")
    id_carpeta_raiz <- Sys.getenv("ID_CARPETA")
    id_google_sheet <- Sys.getenv("ID_SHEET")
    
    if(modo == "") modo <- "ultimo"
    if(id_carpeta_raiz == "") stop("Falta el ID de la carpeta raíz de Google Drive.")
    
    cat("\n======================================================\n")
    cat("🚀 INICIANDO CEREBRO | MODO:", toupper(modo), "\n")
    
    # --- GPS INTELIGENTE: BUSCAR CARPETA 2026 ---
    anio_buscado <- as.character(format(Sys.Date(), "%Y"))
    cat("📍 Buscando el año:", anio_buscado, "dentro de la carpeta raíz...\n")
    
    carpetas_raiz <- drive_ls(as_id(id_carpeta_raiz), type = "folder")
    carpeta_anio <- carpetas_raiz %>% filter(name == anio_buscado)
    
    if(nrow(carpeta_anio) == 0) stop(paste("No se encontró la carpeta", anio_buscado))
    
    # --- BUSCAR MESES (01_enero, 02_febrero, etc) ---
    carpetas_meses <- drive_ls(as_id(carpeta_anio$id[1]), type = "folder") %>% arrange(name)
    if(nrow(carpetas_meses) == 0) stop("No hay carpetas de meses.")
    
    lista_archivos_validos <- list()
    for (i in 1:nrow(carpetas_meses)) {
      archivos_csv <- drive_ls(as_id(carpetas_meses$id[i]), type = "csv")
      if(nrow(archivos_csv) > 0) {
        mes_num <- as.numeric(str_extract(carpetas_meses$name[i], "^\\d+"))
        lista_archivos_validos[[length(lista_archivos_validos) + 1]] <- list(
          mes_numero = mes_num,
          carpeta = carpetas_meses$name[i],
          archivos = archivos_csv
        )
      }
    }
    
    if(length(lista_archivos_validos) == 0) stop("Ningún mes tiene archivos CSV.")
    if (modo == "ultimo") {
      lista_archivos_validos <- list(lista_archivos_validos[[length(lista_archivos_validos)]])
    }
    
    # --- BUCLE DE PROCESAMIENTO ---
    for (item in lista_archivos_validos) {
      mes_objetivo <- item$mes_numero
      carpeta_actual <- item$carpeta
      archivos_del_mes <- item$archivos
      
      cat("\n⚙️ Descargando datos de:", carpeta_actual, "...\n")
      cubeta_de_datos <- list()
      
      for (i in 1:nrow(archivos_del_mes)) {
        archivo_actual <- archivos_del_mes[i, ]
        nombre_temporal <- paste0("archivo_temp_", i, ".csv")
        suppressMessages(drive_download(as_id(archivo_actual$id), path = nombre_temporal, overwrite = TRUE))
        cubeta_de_datos[[i]] <- read.csv(nombre_temporal, sep = ",", fileEncoding = "latin1")
      }
      
      base_mensual_consolidada <- bind_rows(cubeta_de_datos)
      cat("✅ Registros listos:", nrow(base_mensual_consolidada), "\n")
      
      # Ejecución de los 13 escuadrones
      trabajador1_Embarazadas(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador2_General(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador3_Edades(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador4_LugarAtencion(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador5_MorbilidadPrevencion(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador6_CronicasCIE10(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador6_1_CronicasGenero(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador7_CoberturaAtenciones(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador8_InterculturalidadTotal(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador9_PerfilEpidemiologico(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador10_Discapacidades(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador11_EstadoNutricional(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador12_Violencia(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
      trabajador13_AdolescentesRiesgo(base_mensual_consolidada, id_google_sheet, mes_objetivo); Sys.sleep(2)
    }
    cat("\n🎉 ¡SISTEMA ACTUALIZADO CON ÉXITO!\n")
  }, error = function(e) {
    cat("\n🛑 ALERTA CRÍTICA:", e$message, "\n")
  })
}

ejecutar_sala_situacional_maestra()
