# ====================================================================================
# CEREBRO CENTRAL - AUTOMATIZACIÓN DE SALA SITUACIONAL (NIVEL OPERATIVO)
# ====================================================================================

library(googledrive)
library(googlesheets4)
library(dplyr)
library(stringr)

# ====================================================================================
# AUTENTICACIÓN INTELIGENTE (Nube vs Local)
# ====================================================================================
llave_json <- Sys.getenv("GCP_CREDENTIALS")

if (llave_json != "") {
  cat("🔐 Detectado entorno de nube: Autenticando con Cédula Robótica...\n")
  writeLines(llave_json, "service_account.json")
  drive_auth(path = "service_account.json")
  gs4_auth(path = "service_account.json")
} else {
  cat("💻 Detectado entorno local: Autenticando con cuenta de usuario...\n")
  drive_auth(email = "oedgargs89pico@gmail.com")
  gs4_auth(email = "oedgargs89pico@gmail.com") 
}

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

# ====================================================================================
# FUNCIÓN MAESTRA EN LA NUBE (VERSIÓN ESCÁNER HÍBRIDO + BÚNKER NEÓN)
# ====================================================================================
ejecutar_sala_situacional_maestra <- function() {
  tryCatch({
    # --- 1. ATRAPAR LOS DATOS QUE ENVÍA GOOGLE SHEETS ---
    modo_original <- Sys.getenv("MODO_EJECUCION")
    id_carpeta_raiz <- Sys.getenv("ID_CARPETA")
    id_google_sheet <- Sys.getenv("ID_SHEET")
    
    if(modo_original == "") modo_original <- "ultimo"
    if(id_carpeta_raiz == "") stop("Falta el ID de la carpeta raíz de Google Drive.")
    
    # --- 2. LÓGICA DE LA MÁQUINA DEL TIEMPO (BÚNKER NEÓN) ---
    modo <- modo_original
    anio_auditoria <- ""
    mes_especifico <- ""
    
    # Si Google Sheets envió la palabra secreta desde la terminal (ej: "auditoria_2026_5")
    if (grepl("auditoria_", modo)) {
      partes <- strsplit(modo, "_")[[1]]
      anio_auditoria <- partes[2]        # Atrapa el año (ej: "2026")
      mes_especifico <- partes[3]        # Atrapa el mes (ej: "5" o "todos")
      
      if (mes_especifico != "todos") {
        modo <- "ultimo" # Forzamos la lógica para procesar 1 solo mes
      } else {
        modo <- "todos"  # Procesamos el año completo
      }
    }
    
    # --- 3. GPS INTELIGENTE: DETERMINAR EL AÑO A BUSCAR ---
    cat("\n======================================================\n")
    if(anio_auditoria != "") {
      anio_buscado <- anio_auditoria
      cat("🕰️ MODO BÚNKER: Viajando en el tiempo al año:", anio_buscado, "\n")
    } else {
      anio_buscado <- as.character(format(Sys.Date(), "%Y"))
      cat("📍 MODO NORMAL: Buscando el año actual:", anio_buscado, "\n")
    }
    
    carpetas_raiz <- drive_ls(as_id(id_carpeta_raiz), type = "folder")
    carpeta_anio <- carpetas_raiz %>% filter(name == anio_buscado)
    
    if(nrow(carpeta_anio) == 0) stop(paste("No se encontró la carpeta del año:", anio_buscado))
    
    # --- 4. ESCÁNER DE INFRAESTRUCTURA HÍBRIDA ---
    cat("🔍 Analizando estructura interna...\n")
    subcarpetas_meses <- drive_ls(as_id(carpeta_anio$id[1]), type = "folder") %>% arrange(name)
    archivos_csv_sueltos <- drive_ls(as_id(carpeta_anio$id[1]), type = "csv")
    
    lista_archivos_validos <- list()
    
    # >> ESCENARIO A: OFICINAS TÉCNICAS (Subcarpetas "01_enero", "02_febrero"...)
    if (nrow(subcarpetas_meses) > 0) {
      cat("🏢 Estructura detectada: OFICINA TÉCNICA (Carpetas Mensuales)\n")
      for (i in 1:nrow(subcarpetas_meses)) {
        archivos_csv <- drive_ls(as_id(subcarpetas_meses$id[i]), type = "csv")
        if(nrow(archivos_csv) > 0) {
          mes_num <- as.numeric(str_extract(subcarpetas_meses$name[i], "^\\d+"))
          if(!is.na(mes_num)) {
            lista_archivos_validos[[length(lista_archivos_validos) + 1]] <- list(
              mes_numero = mes_num,
              carpeta = subcarpetas_meses$name[i],
              archivos = archivos_csv
            )
          }
        }
      }
    }
    # >> ESCENARIO B: UNIDADES DE SALUD (CSVs sueltos PRASS_ENE_2026.csv)
    else if (nrow(archivos_csv_sueltos) > 0) {
      cat("🏥 Estructura detectada: UNIDAD DE SALUD (Archivos Sueltos)\n")
      meses_diccionario <- c("ENE"=1, "FEB"=2, "MAR"=3, "ABR"=4, "MAY"=5, "JUN"=6,
                             "JUL"=7, "AGO"=8, "SEP"=9, "OCT"=10, "NOV"=11, "DIC"=12)
      
      archivos_csv_sueltos$mes_texto <- str_extract(archivos_csv_sueltos$name, "(ENE|FEB|MAR|ABR|MAY|JUN|JUL|AGO|SEP|OCT|NOV|DIC)")
      archivos_validos <- archivos_csv_sueltos %>% filter(!is.na(mes_texto))
      
      if(nrow(archivos_validos) > 0) {
        archivos_validos$mes_numero <- meses_diccionario[archivos_validos$mes_texto]
        meses_unicos <- sort(unique(archivos_validos$mes_numero))
        
        for (m in meses_unicos) {
          archivos_del_mes <- archivos_validos %>% filter(mes_numero == m)
          lista_archivos_validos[[length(lista_archivos_validos) + 1]] <- list(
            mes_numero = m,
            carpeta = paste("Mes consolidado:", archivos_del_mes$mes_texto[1]),
            archivos = archivos_del_mes
          )
        }
      }
    }
    
    if(length(lista_archivos_validos) == 0) stop("Ningún mes tiene archivos CSV en la estructura detectada.")
    
    # --- 5. FILTROS DE TIEMPO (BÚNKER O NORMAL) ---
    # Si estamos en modo Búnker y pidieron un mes específico
    if (mes_especifico != "" && mes_especifico != "todos") {
      lista_archivos_validos <- lista_archivos_validos[sapply(lista_archivos_validos, function(x) x$mes_numero == as.numeric(mes_especifico))]
      if(length(lista_archivos_validos) == 0) stop("La carpeta del año existe, pero no hay datos CSV para el mes seleccionado.")
    } else if (modo == "ultimo") {
      # Si estamos en modo normal jalamos solo el mes más reciente
      lista_archivos_validos <- list(lista_archivos_validos[[length(lista_archivos_validos)]])
    }
    
    # --- 6. BUCLE DE PROCESAMIENTO A LOS TRABAJADORES ---
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
