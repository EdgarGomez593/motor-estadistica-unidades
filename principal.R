# ====================================================================================
# CEREBRO CENTRAL - AUTOMATIZACIÓN DE SALA SITUACIONAL PROVINCIAL (VERSIÓN GITHUB)
# ====================================================================================

library(googledrive)
library(googlesheets4)
library(dplyr)
library(stringr)

# ====================================================================================
# 1. AUTENTICACIÓN INTELIGENTE (Cédula Robótica para GitHub)
# ====================================================================================
llave_json <- Sys.getenv("GCP_CREDENTIALS")

if (llave_json != "") {
  cat("🔐 Detectado entorno de nube (GitHub): Autenticando con Cédula Robótica...\n")
  writeLines(llave_json, "service_account.json")
  drive_auth(path = "service_account.json")
  gs4_auth(path = "service_account.json")
} else {
  cat("💻 Detectado entorno local: Autenticando con cuenta de usuario...\n")
  drive_auth(email = "oedgargs89pico@gmail.com")
  gs4_auth(email = "oedgargs89pico@gmail.com") 
}

# ====================================================================================
# 2. CONTRATACIÓN DE TRABAJADORES (Línea de Ensamblaje Completa)
# ====================================================================================
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
source("trabajadores/trabajador14_Top10Intercultural.R")

# ====================================================================================
# 3. FUNCIÓN MAESTRA (EL DIRECTOR DE ORQUESTA AUTÓNOMO)
# ====================================================================================
ejecutar_sala_situacional_maestra <- function() {
  
  tryCatch({
    # --- A. ATRAPAR LOS DATOS DEL GATILLO (Google Sheets) ---
    modo_original <- Sys.getenv("MODO_EJECUCION")
    id_carpeta_raiz <- Sys.getenv("ID_CARPETA")
    id_google_sheet <- Sys.getenv("ID_SHEET")
    
    if(modo_original == "") modo_original <- "ultimo"
    if(id_carpeta_raiz == "") stop("Falta el ID de la carpeta raíz de Google Drive enviada por la interfaz.")
    
    # --- B. LÓGICA DE LA MÁQUINA DEL TIEMPO (BÚNKER) ---
    modo <- modo_original
    anio_auditoria <- ""
    mes_especifico <- ""
    
    if (grepl("auditoria_", modo)) {
      partes <- strsplit(modo, "_")[[1]]
      anio_auditoria <- partes[2]
      mes_especifico <- partes[3]
      
      if (mes_especifico != "todos") {
        modo <- "ultimo"
      } else {
        modo <- "todos"
      }
    }
    
    cat("\n======================================================\n")
    if(anio_auditoria != "") {
      anio_buscado <- anio_auditoria
      cat("🕰️ MODO BÚNKER: Viajando en el tiempo al año:", anio_buscado, "\n")
    } else {
      anio_buscado <- as.character(format(Sys.Date(), "%Y"))
      cat("📍 MODO NORMAL: Buscando el año actual:", anio_buscado, "\n")
    }
    
    # --- C. ESCÁNER DE INFRAESTRUCTURA HÍBRIDA ---
    carpetas_raiz <- drive_ls(as_id(id_carpeta_raiz), type = "folder")
    carpeta_anio <- carpetas_raiz %>% filter(name == anio_buscado)
    
    if(nrow(carpeta_anio) == 0) stop(paste("No se encontró la carpeta del año:", anio_buscado))
    
    cat("🔍 Analizando estructura interna...\n")
    subcarpetas_meses <- drive_ls(as_id(carpeta_anio$id[1]), type = "folder") %>% arrange(name)
    archivos_csv_sueltos <- drive_ls(as_id(carpeta_anio$id[1]), type = "csv")
    
    lista_archivos_COMPLETA <- list()
    
    # Escenario 1: Carpetas por mes
    if (nrow(subcarpetas_meses) > 0) {
      cat("🏢 Estructura detectada: OFICINA TÉCNICA (Carpetas Mensuales)\n")
      for (i in 1:nrow(subcarpetas_meses)) {
        archivos_csv <- drive_ls(as_id(subcarpetas_meses$id[i]), type = "csv")
        if(nrow(archivos_csv) > 0) {
          mes_num <- as.numeric(str_extract(subcarpetas_meses$name[i], "^\\d+"))
          if(!is.na(mes_num)) {
            lista_archivos_COMPLETA[[length(lista_archivos_COMPLETA) + 1]] <- list(
              mes_numero = mes_num,
              carpeta = subcarpetas_meses$name[i],
              archivos = archivos_csv
            )
          }
        }
      }
    } 
    # Escenario 2: Archivos sueltos por unidad
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
          lista_archivos_COMPLETA[[length(lista_archivos_COMPLETA) + 1]] <- list(
            mes_numero = m,
            carpeta = paste("Mes consolidado:", archivos_del_mes$mes_texto[1]),
            archivos = archivos_del_mes
          )
        }
      }
    }
    
    if(length(lista_archivos_COMPLETA) == 0) stop("Ningún mes tiene archivos CSV en la estructura detectada.")
    
    # --- D. FILTROS DE TIEMPO (Normal, Bunker, Último) ---
    lista_archivos_validos <- lista_archivos_COMPLETA
    
    if (mes_especifico != "" && mes_especifico != "todos") {
      lista_archivos_validos <- lista_archivos_validos[sapply(lista_archivos_validos, function(x) x$mes_numero == as.numeric(mes_especifico))]
      if(length(lista_archivos_validos) == 0) stop("La carpeta del año existe, pero no hay datos CSV para el mes seleccionado.")
    } else if (modo == "ultimo") {
      lista_archivos_validos <- list(lista_archivos_COMPLETA[[length(lista_archivos_COMPLETA)]])
      cat("🔍 MODO ÚLTIMO: Detectado el mes", lista_archivos_validos[[1]]$carpeta, "como el más reciente.\n")
    } else {
      cat("📚 MODO TODOS: Se procesarán", length(lista_archivos_validos), "meses históricos.\n")
    }
    
    # --- E. EL ESCUDO DE TITANIO (Tolerancia a fallos de Google API) ---
    ejecutar_escudo <- function(funcion_trabajador, nombre, ...) {
      tryCatch({
        funcion_trabajador(...)
        Sys.sleep(1.5) # Ritmo matemático Anti-Bloqueos
      }, error = function(e) {
        if(grepl("429", e$message) || grepl("Quota", e$message)) {
          cat("⏳ Límite de Google rozado en", nombre, ". Respiro de 10s...\n")
          Sys.sleep(10)
          try(funcion_trabajador(...), silent=TRUE)
        } else {
          cat("⚠️ Omitiendo", nombre, ". Detalle:", e$message, "\n")
        }
      })
    }
    
    # --- F. BUCLE DE PROCESAMIENTO MÚLTIPLE ---
    cat("🚀 INICIANDO DESPLIEGUE DE TRABAJADORES...\n")
    
    # ⚠️ REPARACIÓN DE ARQUITECTURA: Uso de listas nombradas para evitar NULLs fantasmas
    memoria_historica_trimestral <- list() 
    
    for (item in lista_archivos_validos) {
      mes_objetivo <- item$mes_numero
      carpeta_actual <- item$carpeta
      archivos_del_mes <- item$archivos
      
      cat("\n⚙️ Procesando Mes:", mes_objetivo, "(", carpeta_actual, ")...\n")
      cubeta_de_datos <- list()
      
      for (i in 1:nrow(archivos_del_mes)) {
        archivo_actual <- archivos_del_mes[i, ]
        nombre_temporal <- paste0("archivo_temp_", i, ".csv")
        suppressMessages(drive_download(as_id(archivo_actual$id), path = nombre_temporal, overwrite = TRUE))
        cubeta_de_datos[[i]] <- read.csv(nombre_temporal, sep = ",", fileEncoding = "latin1")
      }
      
      base_mensual_consolidada <- bind_rows(cubeta_de_datos)
      cat("✅ Registros listos para mes", mes_objetivo, ":", nrow(base_mensual_consolidada), "\n")
      
      # Alimentamos la Memoria Histórica usando el número real como nombre ("mes_3")
      memoria_historica_trimestral[[as.character(mes_objetivo)]] <- base_mensual_consolidada
      
      # Despliegue Escudado de los 13 Trabajadores
      ejecutar_escudo(trabajador1_Embarazadas, "T1_Embarazadas", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador2_General, "T2_General", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador3_Edades, "T3_Edades", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador4_LugarAtencion, "T4_LugarAtencion", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador5_MorbilidadPrevencion, "T5_MorbilidadPrevencion", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador6_CronicasCIE10, "T6_CronicasCIE10", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador6_1_CronicasGenero, "T6_1_CronicasGenero", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador7_CoberturaAtenciones, "T7_CoberturaAtenciones", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador8_InterculturalidadTotal, "T8_Interculturalidad", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador9_PerfilEpidemiologico, "T9_PerfilEpidemiologico", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador10_Discapacidades, "T10_Discapacidades", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador11_EstadoNutricional, "T11_EstadoNutricional", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador12_Violencia, "T12_Violencia", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador13_AdolescentesRiesgo, "T13_AdolescentesRiesgo", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      
      
      # --- G. RADAR TRIMESTRAL PARA TRABAJADOR 14 (MODO SIGILO) ---
      # Si ejecutamos el "Mes Actual" y estamos en Cierre de Trimestre (Marzo, Junio, Sept, Dic)
      if (modo == "ultimo" && mes_objetivo %in% c(3, 6, 9, 12)) {
        cat("\n📡 RADAR TRIMESTRAL ACTIVADO: Detectado mes de cierre (", mes_objetivo, "). Rescatando historial para T14...\n")
        
        meses_a_rescatar <- c(mes_objetivo - 2, mes_objetivo - 1)
        
        for (m_rescate in meses_a_rescatar) {
          # Buscamos en la lista original detectada si existe el mes a rescatar
          item_rescate <- lista_archivos_COMPLETA[sapply(lista_archivos_COMPLETA, function(x) x$mes_numero == m_rescate)]
          
          if (length(item_rescate) > 0) {
            item_r <- item_rescate[[1]]
            cat("   -> Descargando en sigilo mes", m_rescate, "...\n")
            cubeta_temp <- list()
            
            for (j in 1:nrow(item_r$archivos)) {
              arch <- item_r$archivos[j, ]
              nom_temp <- paste0("temp_rescate_", m_rescate, "_", j, ".csv")
              suppressMessages(drive_download(as_id(arch$id), path = nom_temp, overwrite = TRUE))
              cubeta_temp[[j]] <- read.csv(nom_temp, sep = ",", fileEncoding = "latin1")
            }
            # Guardamos con nombre explícito ("mes_X") para evitar NULLs en R
            memoria_historica_trimestral[[as.character(m_rescate)]] <- bind_rows(cubeta_temp)
          }
        }
      }
      
      # Despliegue del Escuadrón 14 (Con su memoria llena y libre de NULLs)
      # Para que T14 ordene los meses correctamente (ej. "10", "11", "12"), extraemos y ordenamos la lista
      indices_ordenados <- sort(as.numeric(names(memoria_historica_trimestral)))
      memoria_ordenada <- lapply(as.character(indices_ordenados), function(nm) memoria_historica_trimestral[[nm]])
      
      ejecutar_escudo(trabajador14_Top10Intercultural, "T14_Top10Intercultural", base_mensual_consolidada, id_google_sheet, mes_objetivo, cubeta_completa = memoria_ordenada)
    }
    
    cat("\n🎉 ¡SISTEMA MAESTRO ACTUALIZADO CON ÉXITO!\n")
    
  }, error = function(e) {
    cat("\n🛑 ALERTA CRÍTICA EN EL CEREBRO CENTRAL:", e$message, "\n")
  })
}

# ENCENDIDO DEL MOTOR 
ejecutar_sala_situacional_maestra()
