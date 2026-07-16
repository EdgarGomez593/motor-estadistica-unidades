# ====================================================================================
# CEREBRO CENTRAL - AUTOMATIZACIÓN DE SALA SITUACIONAL (NIVEL OPERATIVO - UNIDADES DE SALUD)
# ====================================================================================

# 1. LIBRERÍAS NECESARIAS
library(googledrive)
library(googlesheets4)
library(dplyr)
library(stringr)

# 2. AUTENTICACIÓN AUTOMÁTICA
drive_auth(email = "oedgargs89pico@gmail.com")
gs4_auth(email = "oedgargs89pico@gmail.com") 

# 3. CONTRATACIÓN DE TRABAJADORES (Solo el Pelotón Operativo de 13)
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

# 4. FUNCIÓN MAESTRA (EL DIRECTOR DE ORQUESTA)
ejecutar_sala_situacional_maestra <- function(oficina_actual, id_google_sheet, modo = "ultimo") {
  
  tryCatch({
    cat("\n======================================================\n")
    cat("🚀 INICIANDO CEREBRO PARA:", oficina_actual, "| MODO:", toupper(modo), "\n")
    
    # --- A. GPS INTELIGENTE EN GOOGLE DRIVE ---
    anio_buscado <- as.numeric(format(Sys.Date(), "%Y"))
    ruta_anio <- paste0("oficinas_tecnicas/", oficina_actual, "/base_de_datos/", anio_buscado, "/")
    cat("📍 Escaneando Drive:", ruta_anio, "\n")
    
    carpetas_meses <- drive_ls(path = ruta_anio, type = "folder")
    if(nrow(carpetas_meses) == 0) stop("No hay carpetas de meses en este año.")
    
    carpetas_meses <- carpetas_meses %>% arrange(name) 
    
    # --- B. DETECCIÓN DE MESES CON DATOS DISPONIBLES ---
    lista_archivos_validos <- list()
    for (i in 1:nrow(carpetas_meses)) {
      ruta_mes <- paste0(ruta_anio, carpetas_meses$name[i])
      archivos_csv <- drive_ls(path = ruta_mes, type = "csv")
      
      if(nrow(archivos_csv) > 0) {
        mes_num <- as.numeric(str_extract(carpetas_meses$name[i], "^\\d+"))
        lista_archivos_validos[[length(lista_archivos_validos) + 1]] <- list(
          mes_numero = mes_num,
          carpeta = carpetas_meses$name[i],
          archivos = archivos_csv
        )
      }
    }
    
    if(length(lista_archivos_validos) == 0) stop("Ninguna carpeta tiene archivos CSV subidos.")
    
    # Aplicar la lógica del modo (Todos vs Último)
    if (modo == "ultimo") {
      lista_archivos_validos <- list(lista_archivos_validos[[length(lista_archivos_validos)]])
      cat("🔍 MODO ÚLTIMO: Detectado el mes", lista_archivos_validos[[1]]$carpeta, "como el más reciente.\n")
    } else {
      cat("📚 MODO TODOS: Se procesarán", length(lista_archivos_validos), "meses históricos.\n")
    }
    
    # --- C. BUCLE DE PROCESAMIENTO (Itera mes por mes) ---
    for (item in lista_archivos_validos) {
      mes_objetivo <- item$mes_numero
      carpeta_actual <- item$carpeta
      archivos_del_mes <- item$archivos
      
      cat("\n⚙️ Descargando y fusionando datos de:", carpeta_actual, "...\n")
      cubeta_de_datos <- list()
      
      for (i in 1:nrow(archivos_del_mes)) {
        archivo_actual <- archivos_del_mes[i, ]
        nombre_temporal <- paste0("archivo_temp_", i, ".csv")
        suppressMessages(drive_download(file = archivo_actual, path = nombre_temporal, overwrite = TRUE))
        cubeta_de_datos[[i]] <- read.csv(nombre_temporal, sep = ",", fileEncoding = "latin1")
      }
      
      base_mensual_consolidada <- bind_rows(cubeta_de_datos)
      cat("✅ Registros listos para", carpeta_actual, ":", nrow(base_mensual_consolidada), "\n")
      
      # --- EJECUCIÓN DE TRABAJADORES CONSOLIDADOS ---
      trabajador1_Embarazadas(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador2_General(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador3_Edades(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador4_LugarAtencion(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador5_MorbilidadPrevencion(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador6_CronicasCIE10(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador6_1_CronicasGenero(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador7_CoberturaAtenciones(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador8_InterculturalidadTotal(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador9_PerfilEpidemiologico(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador10_Discapacidades(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador11_EstadoNutricional(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador12_Violencia(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
      trabajador13_AdolescentesRiesgo(base_mensual_consolidada, id_google_sheet, mes_objetivo)
      Sys.sleep(3)
    }
    
    cat("\n🎉 ¡SISTEMA ACTUALIZADO CON ÉXITO PARA:", oficina_actual, "!\n")
    
  }, error = function(e) {
    cat("\n🛑 ALERTA CRÍTICA EN EL SISTEMA:", e$message, "\n")
  })
}

# ====================================================================================
# DISPARADOR DE ACTUALIZACIÓN (NIVEL OPERATIVO)
# ====================================================================================

catálogo_oficinas <- data.frame(
  oficina = c("Oficina_Tecnica_18D01"),
  sheet_id = c("1L_ANGWvYeTmZilNYFYZ1vsFtaTqElwOWmjk6x0xEJcI"), 
  stringsAsFactors = FALSE
)

modo_ejecucion <- "ultimo" 

for (i in 1:nrow(catálogo_oficinas)) {
  oficina_v <- catálogo_oficinas$oficina[i]
  sheet_v <- catálogo_oficinas$sheet_id[i]
  
  ejecutar_sala_situacional_maestra(oficina_actual = oficina_v, id_google_sheet = sheet_v, modo = modo_ejecucion)
}