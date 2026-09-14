# ====================================================================================
# CEREBRO CENTRAL - NODO OFICINAS TÉCNICAS (COSECHA PROFUNDA DISTRIBUIDA)
# ====================================================================================

library(googledrive)
library(googlesheets4)
library(dplyr)
library(stringr)

# ====================================================================================
# 1. AUTENTICACIÓN INTELIGENTE
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
# 2. CONTRATACIÓN DE TRABAJADORES
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
source("trabajadores/trabajador15_SaludMental.R")

# ====================================================================================
# 3. FUNCIÓN MAESTRA
# ====================================================================================
ejecutar_sala_oficinas <- function() {
  
  tryCatch({
    # --- A. ATRAPAR LOS DATOS DEL GATILLO ---
    modo_original <- Sys.getenv("MODO_EJECUCION")
    id_carpeta_raiz <- Sys.getenv("ID_CARPETA")
    id_google_sheet <- Sys.getenv("ID_SHEET")
    
    if(modo_original == "") modo_original <- "ultimo"
    if(id_carpeta_raiz == "") stop("Falta el ID de la carpeta 'UNIDADES'.")
    
    # --- B. LÓGICA DE TIEMPO ---
    modo <- modo_original
    anio_auditoria <- ""
    mes_especifico <- ""
    
    if (grepl("auditoria_", modo)) {
      partes <- strsplit(modo, "_")[[1]]
      anio_auditoria <- partes[2]
      mes_especifico <- partes[3]
      if (mes_especifico != "todos") modo <- "ultimo" else modo <- "todos"
    }
    
    cat("\n======================================================\n")
    if(anio_auditoria != "") {
      anio_buscado <- anio_auditoria
      cat("🕰️ MODO BÚNKER: Viajando en el tiempo al año:", anio_buscado, "\n")
    } else {
      anio_buscado <- as.character(format(Sys.Date(), "%Y"))
      cat("📍 MODO NORMAL: Buscando el año actual:", anio_buscado, "\n")
    }
    
    # --- C. ESCÁNER PROFUNDO DE UNIDADES ---
    cat("🏢 Iniciando Cosecha Profunda en la carpeta 'UNIDADES'...\n")
    carpetas_unidades <- drive_ls(as_id(id_carpeta_raiz), type = "folder")
    inventario_meses <- list()
    
    for (u in 1:nrow(carpetas_unidades)) {
      cat(" -> Inspeccionando Unidad:", carpetas_unidades$name[u], "\n")
      
      sub_pras <- tryCatch(drive_ls(as_id(carpetas_unidades$id[u]), type = "folder") %>% filter(name == "PRAS"), error = function(e) NULL)
      
      if (!is.null(sub_pras) && nrow(sub_pras) > 0) {
        sub_anio <- tryCatch(drive_ls(as_id(sub_pras$id[1]), type = "folder") %>% filter(name == anio_buscado), error = function(e) NULL)
        
        if (!is.null(sub_anio) && nrow(sub_anio) > 0) {
          carpetas_meses <- tryCatch(drive_ls(as_id(sub_anio$id[1]), type = "folder"), error = function(e) NULL)
          
          if (!is.null(carpetas_meses) && nrow(carpetas_meses) > 0) {
            for (m in 1:nrow(carpetas_meses)) {
              archivos_csv <- tryCatch(drive_ls(as_id(carpetas_meses$id[m]), type = "csv"), error = function(e) NULL)
              
              if (!is.null(archivos_csv) && nrow(archivos_csv) > 0) {
                mes_num <- as.numeric(str_extract(carpetas_meses$name[m], "^\\d+"))
                
                if (!is.na(mes_num)) {
                  if (is.null(inventario_meses[[as.character(mes_num)]])) {
                    inventario_meses[[as.character(mes_num)]] <- archivos_csv
                  } else {
                    inventario_meses[[as.character(mes_num)]] <- bind_rows(inventario_meses[[as.character(mes_num)]], archivos_csv)
                  }
                }
              }
            }
          }
        }
      }
    }
    
    # --- D. FILTROS DE TIEMPO Y CONSOLIDACIÓN ---
    lista_archivos_validos <- list()
    meses_encontrados <- as.numeric(names(inventario_meses))
    
    if(length(meses_encontrados) > 0) {
      meses_ordenados <- sort(meses_encontrados)
      for (m_num in meses_ordenados) {
        lista_archivos_validos[[length(lista_archivos_validos) + 1]] <- list(
          mes_numero = m_num,
          carpeta = paste("Consolidado OT - Mes", m_num),
          archivos = inventario_meses[[as.character(m_num)]]
        )
      }
    }
    
    if(length(lista_archivos_validos) == 0) stop("No se encontraron datos CSV válidos en las unidades.")
    
    if (mes_especifico != "" && mes_especifico != "todos") {
      lista_archivos_validos <- lista_archivos_validos[sapply(lista_archivos_validos, function(x) x$mes_numero == as.numeric(mes_especifico))]
    } else if (modo == "ultimo") {
      lista_archivos_validos <- list(lista_archivos_validos[[length(lista_archivos_validos)]])
      cat("🔍 MODO ÚLTIMO: Detectado el mes", lista_archivos_validos[[1]]$mes_numero, "como el más reciente.\n")
    }
    
    # --- E. EL ESCUDO DE TITANIO ---
    ejecutar_escudo <- function(funcion_trabajador, nombre, ...) {
      tryCatch({
        funcion_trabajador(...)
        Sys.sleep(1.5) 
      }, error = function(e) {
        if(grepl("429", e$message) || grepl("Quota", e$message)) {
          cat("⏳ Respiro de 10s...\n")
          Sys.sleep(10)
          try(funcion_trabajador(...), silent=TRUE)
        } else {
          cat("⚠️ Omitiendo", nombre, ". Detalle:", e$message, "\n")
        }
      })
    }
    
    # --- F. BUCLE DE PROCESAMIENTO MÚLTIPLE ---
    cat("🚀 INICIANDO DESPLIEGUE DE TRABAJADORES (OFICINA TÉCNICA)...\n")
    memoria_historica_trimestral <- list() 
    
    for (item in lista_archivos_validos) {
      mes_objetivo <- item$mes_numero
      archivos_del_mes <- item$archivos
      cat("\n⚙️ Procesando Mes:", mes_objetivo, "...\n")
      
      cubeta_de_datos <- list()
      for (i in 1:nrow(archivos_del_mes)) {
        archivo_actual <- archivos_del_mes[i, ]
        nombre_temporal <- paste0("archivo_temp_", i, ".csv")
        
        tryCatch({
          suppressMessages(drive_download(as_id(archivo_actual$id), path = nombre_temporal, overwrite = TRUE))
          
          # Lector Inteligente Bilingüe
          primera_linea <- readLines(nombre_temporal, n = 1, warn = FALSE)
          separador_real <- if (grepl(";", primera_linea)) ";" else ","
          
          # 🛡️ BLINDAJE DE TIPOS DE DATOS: Convertir todo a texto puro al instante
          datos_temp <- read.csv(nombre_temporal, sep = separador_real, fileEncoding = "latin1") %>%
                        mutate(across(everything(), as.character))
          
          cubeta_de_datos[[length(cubeta_de_datos) + 1]] <- datos_temp
          
        }, error = function(e) {
          cat("\n   ⚠️ ALERTA DE DATO SUCIO: El archivo [", archivo_actual$name, "] falló. Se omitirá. Detalle:", e$message, "\n")
        })
      }
      
      if(length(cubeta_de_datos) == 0) stop("Todos los archivos CSV de este mes están corruptos o vacíos.")
      
      base_mensual_consolidada <- bind_rows(cubeta_de_datos)
      cat("✅ Registros listos para mes", mes_objetivo, ":", nrow(base_mensual_consolidada), "\n")
      memoria_historica_trimestral[[as.character(mes_objetivo)]] <- base_mensual_consolidada
      
      # Ejecución de los 15 Trabajadores
      ejecutar_escudo(trabajador1_Embarazadas, "T1", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador2_General, "T2", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador3_Edades, "T3", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador4_LugarAtencion, "T4", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador5_MorbilidadPrevencion, "T5", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador6_CronicasCIE10, "T6", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador6_1_CronicasGenero, "T6_1", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador7_CoberturaAtenciones, "T7", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador8_InterculturalidadTotal, "T8", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador9_PerfilEpidemiologico, "T9", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador10_Discapacidades, "T10", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador11_EstadoNutricional, "T11", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador12_Violencia, "T12", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador13_AdolescentesRiesgo, "T13", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      ejecutar_escudo(trabajador15_SaludMental, "T15", base_mensual_consolidada, id_google_sheet, mes_objetivo)
      
      # Radar Trimestral Trabajador 14
      if (modo == "ultimo" && mes_objetivo %in% c(3, 6, 9, 12)) {
        cat("\n📡 RADAR TRIMESTRAL ACTIVADO...\n")
        meses_a_rescatar <- c(mes_objetivo - 2, mes_objetivo - 1)
        for (m_rescate in meses_a_rescatar) {
          archs_rescate <- inventario_meses[[as.character(m_rescate)]]
          if (!is.null(archs_rescate)) {
            cub_temp <- list()
            for (j in 1:nrow(archs_rescate)) {
              nom_temp <- paste0("temp_res_", m_rescate, "_", j, ".csv")
              
              tryCatch({
                suppressMessages(drive_download(as_id(archs_rescate$id[j]), path = nom_temp, overwrite = TRUE))
                
                primera_linea_res <- readLines(nom_temp, n = 1, warn = FALSE)
                separador_res <- if (grepl(";", primera_linea_res)) ";" else ","
                
                # 🛡️ BLINDAJE DE TIPOS DE DATOS HISTÓRICOS
                datos_res <- read.csv(nom_temp, sep = separador_res, fileEncoding = "latin1") %>%
                             mutate(across(everything(), as.character))
                             
                cub_temp[[length(cub_temp) + 1]] <- datos_res
              }, error = function(e) {
                cat("\n   ⚠️ ALERTA HISTÓRICA: Archivo rescatado falló. Detalle:", e$message, "\n")
              })
              
            }
            if(length(cub_temp) > 0) {
              memoria_historica_trimestral[[as.character(m_rescate)]] <- bind_rows(cub_temp)
            }
          }
        }
        indices_ordenados <- sort(as.numeric(names(memoria_historica_trimestral)))
        memoria_ordenada <- lapply(as.character(indices_ordenados), function(nm) memoria_historica_trimestral[[nm]])
        ejecutar_escudo(trabajador14_Top10Intercultural, "T14", base_mensual_consolidada, id_google_sheet, mes_objetivo, cubeta_completa = memoria_ordenada)
      }
    }
    
    cat("\n🎉 ¡COSECHA DE OFICINA TÉCNICA FINALIZADA CON ÉXITO!\n")
    
  }, error = function(e) {
    cat("\n🛑 ALERTA CRÍTICA EN CEREBRO OT:", e$message, "\n")
  })
}

# ENCENDIDO DEL MOTOR
ejecutar_sala_oficinas()
