# ============================================================
# PDET: ESTIMACIÓN IV + ERRORES ESTÁNDAR DE CONLEY
# Cuatro ecuaciones:
#   1. IV sin controles
#   2. Primera etapa sin controles
#   3. IV con controles
#   4. Primera etapa con controles
#
# Requisitos:
#   - panel_modelo cargado en el entorno de R
#   - Shapefile(s) municipales en la carpeta indicada
#   - fastconley, fixest y sf instalados
# ============================================================


# ============================================================
# 0. CONFIGURACIÓN
# ============================================================

# Carpeta con los archivos espaciales
SHP_DIR <- "C:/Users/danii/Desktop/PDET/Municipios_Junio_2025_shp"

# Variable identificadora del municipio en panel_modelo
PANEL_ID <- "codmpio"

# Campo del shapefile que identifica al municipio.
# Dejar NULL para detectar automáticamente uno de los nombres habituales.
# Si la detección falla, escribe el nombre exacto, por ejemplo:
# SHP_ID_FIELD <- "MPIO_CCDGO"
SHP_ID_FIELD <- NULL

# Radios de Conley en kilómetros.
# MODIFICA SOLAMENTE ESTA LÍNEA PARA PROBAR OTROS RADIOS.
RADIOS_CONLEY_KM <- seq(200, 1250, by = 50)

# Kernel espacial
KERNEL_CONLEY <- "bartlett"

# Función de distancia
DISTANCIA_CONLEY <- "spherical"

# Rezagos temporales.
# Se crea un índice consecutivo de periodos para que 2014, 2018 y
# 2022 se interpreten como las tres olas observadas del panel.
# lag_cutoff = 2 permite considerar la dependencia entre las tres olas.
LAG_CUTOFF <- 2

# Correcciones de muestra finita y de semidefinición positiva
SSC_CONLEY <- TRUE
PSD_FIX_CONLEY <- TRUE

# Número de núcleos. NULL deja que fastconley decida.
NCORES_CONLEY <- NULL

# Carpeta para guardar resultados
OUTPUT_DIR <- file.path(SHP_DIR, "resultados_conley")


# ============================================================
# 1. PAQUETES
# ============================================================

paquetes <- c(
  "sf",
  "dplyr",
  "fixest",
  "fastconley"
)

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  stop(
    "Instala primero estos paquetes: ",
    paste(faltantes, collapse = ", ")
  )
}

library(sf)
library(dplyr)
library(fixest)
library(fastconley)


# ============================================================
# 2. COMPROBAR LA BASE DE DATOS
# ============================================================

if (!exists("panel_modelo")) {
  stop(
    "No existe el objeto panel_modelo. ",
    "Cárgalo antes de ejecutar el script."
  )
}

if (!is.data.frame(panel_modelo)) {
  stop("panel_modelo debe ser un data.frame o tibble.")
}

variables_necesarias <- c(
  PANEL_ID,
  "ano",
  "PROP_VOTOSIZQ",
  "pdet_post",
  "z6_post",
  "z3_post",
  "indrural",
  "va_percapita",
  "participacion"
)

faltantes_panel <- setdiff(
  variables_necesarias,
  names(panel_modelo)
)

if (length(faltantes_panel) > 0) {
  stop(
    "Faltan variables en panel_modelo: ",
    paste(faltantes_panel, collapse = ", ")
  )
}


# ============================================================
# 3. FUNCIÓN PARA NORMALIZAR CÓDIGOS MUNICIPALES
# ============================================================

normalizar_codigo <- function(x) {
  
  x <- trimws(as.character(x))
  
  # Corregir códigos importados como números con decimal
  x <- sub("\\.0+$", "", x)
  
  # Convertir representaciones vacías en NA
  x[x == ""] <- NA_character_
  
  # Los códigos DANE municipales suelen tener cinco dígitos.
  # El relleno conserva los ceros iniciales perdidos al importar
  # el código como variable numérica.
  x <- ifelse(
    !is.na(x) & grepl("^[0-9]+$", x) & nchar(x) <= 5,
    sprintf("%05s", x),
    x
  )
  
  # sprintf("%05s") rellena con espacios en algunas situaciones;
  # se normalizan explícitamente a ceros.
  x <- ifelse(
    !is.na(x) & grepl("^ +[0-9]+$", x),
    sub(" +", "", x),
    x
  )
  
  x <- ifelse(
    !is.na(x) & grepl("^[0-9]+$", x) & nchar(x) < 5,
    paste0(
      strrep("0", 5 - nchar(x)),
      x
    ),
    x
  )
  
  x
}


# ============================================================
# 4. LOCALIZAR LOS ARCHIVOS SHAPEFILE
# ============================================================

if (!dir.exists(SHP_DIR)) {
  stop("No existe la carpeta: ", SHP_DIR)
}

shp_files <- list.files(
  path = SHP_DIR,
  pattern = "\\.shp$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

if (length(shp_files) == 0) {
  stop("No se encontraron archivos .shp en la carpeta indicada.")
}

if (length(shp_files) == 1) {
  
  SHP_FILE <- shp_files[1]
  
} else {
  
  cat("\nShapefiles encontrados:\n")
  
  print(
    data.frame(
      numero = seq_along(shp_files),
      archivo = shp_files
    ),
    row.names = FALSE
  )
  
  # Si existen varios archivos, intenta localizar el de municipios.
  candidatos <- shp_files[
    grepl(
      "municip|mpio",
      basename(shp_files),
      ignore.case = TRUE
    )
  ]
  
  if (length(candidatos) == 1) {
    
    SHP_FILE <- candidatos[1]
    
  } else {
    
    stop(
      "Se encontraron varios shapefiles y no se pudo elegir uno ",
      "sin ambigüedad. Asigna manualmente SHP_FILE al archivo ",
      "municipal correcto y vuelve a ejecutar desde esta sección."
    )
    
  }
}

cat("\nShapefile seleccionado:\n", SHP_FILE, "\n")


# ============================================================
# 5. LEER EL SHAPEFILE
# ============================================================

municipios_sf <- sf::st_read(
  SHP_FILE,
  quiet = FALSE
)

if (is.na(sf::st_crs(municipios_sf))) {
  stop(
    "El shapefile no tiene un CRS definido. ",
    "Debes identificar su sistema de coordenadas antes de continuar."
  )
}

cat("\nCampos disponibles en el shapefile:\n")
print(names(municipios_sf))

municipios_sf <- municipios_sf %>%
  mutate(codmpio = as.numeric(MpCodigo))

# ============================================================
# 6. IDENTIFICAR EL CÓDIGO MUNICIPAL DEL SHAPEFILE
# ============================================================

if (is.null(SHP_ID_FIELD)) {
  
  campos_candidatos <- c(
    "MPIO_CCDGO",
    "mpio_ccdgo",
    "COD_MPIO",
    "CODMPIO",
    "codmpio",
    "CODIGO_MPIO",
    "COD_DANE",
    "DANE",
    "MPIO_CDPMP",
    "CODIGO",
    "codigo"
  )
  
  coincidencias <- campos_candidatos[
    campos_candidatos %in% names(municipios_sf)
  ]
  
  if (length(coincidencias) == 0) {
    stop(
      "No se pudo identificar automáticamente el código municipal. ",
      "Revisa names(municipios_sf) y asigna SHP_ID_FIELD ",
      "al nombre correcto."
    )
  }
  
  SHP_ID_FIELD <- coincidencias[1]
}

if (!SHP_ID_FIELD %in% names(municipios_sf)) {
  stop(
    "El campo ", SHP_ID_FIELD,
    " no existe en el shapefile."
  )
}

cat("\nCampo identificador seleccionado: ", SHP_ID_FIELD, "\n")


# ============================================================
# 7. NORMALIZAR LOS CÓDIGOS Y DISOLVER POLÍGONOS
# ============================================================

municipios_sf <- municipios_sf %>%
  mutate(
    id_municipio = normalizar_codigo(
      .data[[SHP_ID_FIELD]]
    )
  ) %>%
  filter(!is.na(id_municipio))

# Si existen varios polígonos para un mismo municipio,
# se disuelven en una sola geometría.
#
# Esto evita duplicar observaciones cuando se unan las coordenadas
# a la base del panel.

municipios_dissolved <- municipios_sf %>%
  group_by(id_municipio) %>%
  summarise(
    geometry = sf::st_union(geometry),
    .groups = "drop"
  )

cat(
  "\nNúmero de registros espaciales originales: ",
  nrow(municipios_sf),
  "\n"
)

cat(
  "Número de municipios después de disolver polígonos: ",
  nrow(municipios_dissolved),
  "\n"
)


# ============================================================
# 8. CALCULAR COORDENADAS REPRESENTATIVAS
# ============================================================

# Se utiliza una proyección métrica para construir un punto
# representativo de cada municipio. Después se transforma a WGS84
# para obtener longitud y latitud.

municipios_proyectados <- sf::st_transform(
  municipios_dissolved,
  9377
)

puntos_municipales <- sf::st_point_on_surface(
  municipios_proyectados
)

puntos_wgs84 <- sf::st_transform(
  puntos_municipales,
  4326
)

coordenadas <- sf::st_coordinates(
  puntos_wgs84
)

coordenadas_municipales <- data.frame(
  id_municipio = puntos_wgs84$id_municipio,
  lon = coordenadas[, 1],
  lat = coordenadas[, 2]
) %>%
  distinct(id_municipio, .keep_all = TRUE)

stopifnot(
  !anyDuplicated(coordenadas_municipales$id_municipio)
)


# ============================================================
# 9. PREPARAR Y EMPATAR EL PANEL CON LAS COORDENADAS
# ============================================================

panel_preparado <- panel_modelo %>%
  mutate(
    id_municipio = normalizar_codigo(.data[[PANEL_ID]])
  )

if (anyNA(panel_preparado$id_municipio)) {
  stop("Hay códigos municipales vacíos en panel_modelo.")
}

# La llave municipio-año debe ser única.
duplicados_panel <- panel_preparado %>%
  count(id_municipio, ano) %>%
  filter(n > 1)

if (nrow(duplicados_panel) > 0) {
  print(duplicados_panel)
  
  stop(
    "Hay municipios-año duplicados en panel_modelo. ",
    "Resuelve los duplicados antes de estimar."
  )
}

municipios_panel <- unique(panel_preparado$id_municipio)

municipios_sin_shp <- setdiff(
  municipios_panel,
  coordenadas_municipales$id_municipio
)

if (length(municipios_sin_shp) > 0) {
  
  cat("\nMunicipios del panel sin correspondencia espacial:\n")
  print(municipios_sin_shp)
  
  stop(
    "Hay municipios del panel sin coordenadas. ",
    "Revisa los códigos DANE y el campo seleccionado del shapefile. ",
    "No se eliminarán silenciosamente."
  )
}

panel_conley <- panel_preparado %>%
  left_join(
    coordenadas_municipales,
    by = "id_municipio"
  ) %>%
  arrange(id_municipio, ano) %>%
  mutate(
    # Índice consecutivo de olas: 2014 = 1, 2018 = 2, 2022 = 3.
    periodo = match(ano, sort(unique(ano))),
    codmpio = factor(id_municipio),
    ano = as.numeric(as.character(ano))
  )

if (anyNA(panel_conley$lon) || anyNA(panel_conley$lat)) {
  stop("Existen coordenadas faltantes después del emparejamiento.")
}

cat("\nObservaciones del panel antes de filtrar valores faltantes: ",
    nrow(panel_conley), "\n")

cat("Municipios del panel: ",
    dplyr::n_distinct(panel_conley$id_municipio), "\n")

cat("Periodos: ",
    paste(sort(unique(panel_conley$ano)), collapse = ", "), "\n")


# ============================================================
# 10. CREAR UNA MUESTRA COMÚN PARA LAS CUATRO ECUACIONES
# ============================================================

# Usamos la misma muestra en las cuatro especificaciones.
# Esto evita que los cambios en los resultados se deban a muestras
# diferentes entre los modelos con y sin controles.

variables_modelo <- c(
  "PROP_VOTOSIZQ",
  "pdet_post",
  "z6_post",
  "z3_post",
  "indrural",
  "va_percapita",
  "participacion",
  "codmpio",
  "id_municipio",
  "ano",
  "periodo",
  "lon",
  "lat"
)

panel_conley <- panel_conley %>%
  filter(
    if_all(
      all_of(variables_modelo),
      ~ !is.na(.x)
    )
  ) %>%
  arrange(codmpio, periodo)

# Comprobar que no haya infinitos en las variables numéricas
variables_numericas <- c(
  "PROP_VOTOSIZQ",
  "pdet_post",
  "z6_post",
  "z3_post",
  "indrural",
  "va_percapita",
  "participacion",
  "lon",
  "lat"
)

for (v in variables_numericas) {
  if (any(!is.finite(panel_conley[[v]]))) {
    stop(
      "La variable ", v,
      " contiene valores Inf o -Inf."
    )
  }
}


# ============================================================
# 11. COMPROBAR EL BALANCE DEL PANEL
# ============================================================

tabla_balance <- panel_conley %>%
  count(id_municipio, name = "n_periodos")

print(table(tabla_balance$n_periodos))

if (n_distinct(tabla_balance$n_periodos) != 1) {
  stop(
    "El panel no está balanceado después del filtro de muestra. ",
    "No se utilizará balanced_pnl = TRUE."
  )
}

if (n_distinct(panel_conley$periodo) < 2) {
  stop("Se necesitan al menos dos periodos para este análisis.")
}

if (anyDuplicated(panel_conley[c("id_municipio", "periodo")])) {
  stop("Hay municipios-periodo duplicados.")
}

cat("\nPanel balanceado correctamente.\n")
cat("Municipios:", n_distinct(panel_conley$id_municipio), "\n")
cat("Periodos:", n_distinct(panel_conley$periodo), "\n")
cat("Observaciones:", nrow(panel_conley), "\n")


# ============================================================
# 12. ESTIMAR LAS CUATRO ECUACIONES
# ============================================================

# Ecuación 1: IV sin controles
#
# Y ~ efectos fijos | variable endógena ~ instrumentos

did_iv <- fixest::feols(
  PROP_VOTOSIZQ ~ 1 |
    codmpio + ano |
    pdet_post ~ z6_post + z3_post,
  data = panel_conley,
  demeaned = TRUE
)


# Ecuación 2: Primera etapa sin controles

first_stage <- fixest::feols(
  pdet_post ~ z6_post + z3_post |
    codmpio + ano,
  data = panel_conley,
  demeaned = TRUE
)


# Ecuación 3: IV con controles

did_r <- fixest::feols(
  PROP_VOTOSIZQ ~
    indrural + va_percapita + participacion |
    codmpio + ano |
    pdet_post ~ z6_post + z3_post,
  data = panel_conley,
  demeaned = TRUE
)


# Ecuación 4: Primera etapa con controles

first_stage2 <- fixest::feols(
  pdet_post ~
    z6_post + z3_post +
    indrural + va_percapita + participacion |
    codmpio + ano,
  data = panel_conley,
  demeaned = TRUE
)


modelos <- list(
  "IV sin controles" = did_iv,
  "Primera etapa sin controles" = first_stage,
  "IV con controles" = did_r,
  "Primera etapa con controles" = first_stage2
)


# ============================================================
# 13. FUNCIÓN PARA CALCULAR LA MATRIZ DE CONLEY
# ============================================================

vcov_conley <- function(modelo, radio_km, datos = panel_conley) {
  fastconley::vcovSpHAC(
    reg = modelo,
    unit = "codmpio",
    time = "periodo",
    lat = "lat",
    lon = "lon",
    kernel = KERNEL_CONLEY,
    dist_fn = DISTANCIA_CONLEY,
    dist_cutoff = radio_km,
    lag_cutoff = LAG_CUTOFF,
    balanced_pnl = TRUE,
    ncores = NCORES_CONLEY,
    ssc = SSC_CONLEY,
    psd_fix = PSD_FIX_CONLEY,
    data = datos
  )
}


# ============================================================
# 14. ESTIMAR ERRORES ESTÁNDAR PARA CADA RADIO
# ============================================================

resultados <- list()
resultados_wald <- list()

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

for (radio in RADIOS_CONLEY_KM) {
  
  cat("\n")
  cat("====================================================\n")
  cat("RADIO CONLEY:", radio, "km\n")
  cat("====================================================\n")
  
  for (nombre_modelo in names(modelos)) {
    
    modelo <- modelos[[nombre_modelo]]
    
    cat("\n\n--------------------------------------------\n")
    cat(nombre_modelo, "\n")
    cat("--------------------------------------------\n")
    
    V_conley <- vcov_conley(
      modelo = modelo,
      radio_km = radio
    )
    
    # Imprimir resultados con errores estándar de Conley
    resumen <- summary(
      modelo,
      vcov = V_conley
    )
    
    print(resumen)
    
    # Extraer tabla de coeficientes
    tabla_coef <- as.data.frame(
      resumen$coeftable
    )
    
    tabla_coef$termino <- rownames(tabla_coef)
    rownames(tabla_coef) <- NULL
    
    names(tabla_coef)[1:4] <- c(
      "estimacion",
      "error_estandar",
      "estadistico",
      "valor_p"
    )
    
    tabla_coef$modelo <- nombre_modelo
    tabla_coef$radio_km <- radio
    
    resultados[[
      paste(
        gsub(" ", "_", nombre_modelo),
        radio,
        sep = "_"
      )
    ]] <- tabla_coef
    
    
    # --------------------------------------------------------
    # Prueba conjunta de instrumentos en primeras etapas
    # --------------------------------------------------------
    
    if (nombre_modelo %in% c(
      "Primera etapa sin controles",
      "Primera etapa con controles"
    )) {
      
      cat("\nPrueba conjunta de instrumentos z6_post y z3_post:\n")
      
      prueba_wald <- fixest::wald(
        modelo,
        keep = "^(z6_post|z3_post)$",
        vcov = V_conley
      )
      
      print(prueba_wald)
      
      # Conservar la prueba para el resumen final
      resultados_wald[[
        paste(
          gsub(" ", "_", nombre_modelo),
          radio,
          sep = "_"
        )
      ]] <- list(
        modelo = nombre_modelo,
        radio_km = radio,
        prueba = prueba_wald
      )
    }
  }
}


# ============================================================
# 15. EXPORTAR RESULTADOS
# ============================================================

tabla_final <- dplyr::bind_rows(resultados)

write.csv(
  tabla_final,
  file = file.path(
    OUTPUT_DIR,
    "resultados_conley_cuatro_ecuaciones.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n\n====================================================\n")
cat("RESUMEN DE COEFICIENTES Y ERRORES ESTÁNDAR\n")
cat("====================================================\n")

print(
  tabla_final %>%
    select(
      modelo,
      radio_km,
      termino,
      estimacion,
      error_estandar,
      estadistico,
      valor_p
    ),
  row.names = FALSE
)

cat("\nArchivo guardado en:\n")
cat(
  file.path(
    OUTPUT_DIR,
    "resultados_conley_cuatro_ecuaciones.csv"
  ),
  "\n"
)

cat("\nAnálisis terminado.\n")


# ============================================================
# 16. PRESENTACIÓN CONVENCIONAL DE LAS CUATRO ECUACIONES
#     FORMATO TIPO summary(modelo), CON ERRORES DE CONLEY
# ============================================================

# Seleccionar el radio para presentar los resultados.
# Puedes cambiarlo sin modificar el resto del apartado.
RADIO_PRESENTACION <- 1000

if (!RADIO_PRESENTACION %in% RADIOS_CONLEY_KM) {
  stop(
    "RADIO_PRESENTACION debe estar incluido en RADIOS_CONLEY_KM."
  )
}

cat("\n\n")
cat("############################################################\n")
cat("RESULTADOS DE LAS CUATRO ECUACIONES\n")
cat("Errores estándar espaciales de Conley:",
    RADIO_PRESENTACION, "km\n")
cat("############################################################\n")


# ------------------------------------------------------------
# Ecuación 1. IV sin controles
# ------------------------------------------------------------

cat("\n\n")
cat("============================================================\n")
cat("ECUACIÓN 1: IV SIN CONTROLES\n")
cat("============================================================\n")

V1 <- vcov_conley(
  modelo = did_iv,
  radio_km = RADIO_PRESENTACION
)

print(
  summary(
    did_iv,
    vcov = V1
  )
)


# ------------------------------------------------------------
# Ecuación 2. Primera etapa sin controles
# ------------------------------------------------------------

cat("\n\n")
cat("============================================================\n")
cat("ECUACIÓN 2: PRIMERA ETAPA SIN CONTROLES\n")
cat("============================================================\n")

V2 <- vcov_conley(
  modelo = first_stage,
  radio_km = RADIO_PRESENTACION
)

print(
  summary(
    first_stage,
    vcov = V2
  )
)


# ------------------------------------------------------------
# Ecuación 3. IV con controles
# ------------------------------------------------------------

cat("\n\n")
cat("============================================================\n")
cat("ECUACIÓN 3: IV CON CONTROLES\n")
cat("============================================================\n")

V3 <- vcov_conley(
  modelo = did_r,
  radio_km = RADIO_PRESENTACION
)

print(
  summary(
    did_r,
    vcov = V3
  )
)


# ------------------------------------------------------------
# Ecuación 4. Primera etapa con controles
# ------------------------------------------------------------

cat("\n\n")
cat("============================================================\n")
cat("ECUACIÓN 4: PRIMERA ETAPA CON CONTROLES\n")
cat("============================================================\n")

V4 <- vcov_conley(
  modelo = first_stage2,
  radio_km = RADIO_PRESENTACION
)

print(
  summary(
    first_stage2,
    vcov = V4
  )
)


cat("\n\n")
cat("############################################################\n")
cat("FIN DE LOS RESÚMENES DE LAS CUATRO ECUACIONES\n")
cat("############################################################\n")


###### DIAGNOSTICOS ######
# ============================================================
# TABLA DE PRIMERA ETAPA: WALD ROBUSTO A CONLEY
# ============================================================

resultados_primera_etapa <- list()

for (nombre in names(modelos_primera_etapa)) {
  
  modelo <- modelos_primera_etapa[[nombre]]
  
  for (radio in RADIOS) {
    
    V_conley <- vcov_conley(modelo, radio)
    
    prueba <- fixest::wald(
      modelo,
      keep = "^(z6_post|z3_post)$",
      vcov = V_conley
    )
    
    # Extraer cada resultado como un escalar numérico
    resultados_primera_etapa[[paste(nombre, radio, sep = "_")]] <-
      data.frame(
        Modelo = nombre,
        Radio_km = radio,
        Estadistico_Wald = as.numeric(prueba$stat),
        p_valor = as.numeric(prueba$p),
        stringsAsFactors = FALSE
      )
  }
}

# Unir resultados
tabla_primera_etapa <- do.call(
  rbind,
  resultados_primera_etapa
)

rownames(tabla_primera_etapa) <- NULL

print(tabla_primera_etapa, row.names = FALSE)


# ============================================================
# 2. WU-HAUSMAN MEDIANTE FUNCION DE CONTROL
# ============================================================
# ============================================================
# WU-HAUSMAN / FUNCION DE CONTROL CON MATRIZ CONLEY
# Radios: 500, 650 y 800 km
# ============================================================
# ============================================================
# WU-HAUSMAN / FUNCION DE CONTROL CON CONLEY
# Radios espaciales: 500, 650 y 800 km
# Especificaciones: sin controles y con controles
# Requiere: fixest, fastconley y la configuracion Conley
# existente en el entorno
# ============================================================

# ------------------------------------------------------------
# 0. Configuracion
# ------------------------------------------------------------

RADIOS_WU <- c(500, 650, 800)

variables_wu <- c(
  "PROP_VOTOSIZQ",
  "pdet_post",
  "z6_post",
  "z3_post",
  "indrural",
  "va_percapita",
  "participacion",
  "codmpio",
  "ano",
  "periodo",
  "lat",
  "lon"
)

# ------------------------------------------------------------
# 1. Validar variables y preparar muestra comun
# ------------------------------------------------------------

faltantes <- setdiff(variables_wu, names(panel_conley))

if (length(faltantes) > 0) {
  stop(
    "Faltan estas variables en panel_conley: ",
    paste(faltantes, collapse = ", ")
  )
}

datos_wu <- panel_conley[
  complete.cases(panel_conley[, variables_wu]),
  ,
  drop = FALSE
]

rownames(datos_wu) <- NULL

if (nrow(datos_wu) == 0) {
  stop("La muestra para la prueba de funcion de control esta vacia.")
}

# Comprobar que el panel conserva tres periodos por municipio
conteo_periodos <- table(datos_wu$codmpio)

if (any(conteo_periodos != 3)) {
  stop(
    "La muestra no tiene exactamente tres observaciones por municipio. ",
    "No se puede asumir balanced_pnl = TRUE sin revisar el panel."
  )
}

# ------------------------------------------------------------
# 2. Primera etapa sin controles
# ------------------------------------------------------------

fs_sin_wu <- fixest::feols(
  pdet_post ~ z6_post + z3_post |
    codmpio + ano,
  data = datos_wu,
  demeaned = TRUE
)

# ------------------------------------------------------------
# 3. Primera etapa con controles
# ------------------------------------------------------------

fs_con_wu <- fixest::feols(
  pdet_post ~ z6_post + z3_post +
    indrural + va_percapita + participacion |
    codmpio + ano,
  data = datos_wu,
  demeaned = TRUE
)

# ------------------------------------------------------------
# 4. Verificar muestras y calcular residuos
# ------------------------------------------------------------

if (
  nobs(fs_sin_wu) != nrow(datos_wu) ||
  nobs(fs_con_wu) != nrow(datos_wu)
) {
  stop(
    "Las primeras etapas no usan todas las filas de datos_wu. ",
    "Hay que alinear las muestras antes de continuar."
  )
}

datos_wu$uhat_sin <- as.numeric(resid(fs_sin_wu))
datos_wu$uhat_con <- as.numeric(resid(fs_con_wu))

if (
  anyNA(datos_wu$uhat_sin) ||
  anyNA(datos_wu$uhat_con)
) {
  stop("Se encontraron residuos faltantes en las primeras etapas.")
}

# ------------------------------------------------------------
# 5. Regresion auxiliar sin controles
# ------------------------------------------------------------

aux_sin_wu <- fixest::feols(
  PROP_VOTOSIZQ ~ pdet_post + uhat_sin |
    codmpio + ano,
  data = datos_wu,
  demeaned = TRUE
)

# ------------------------------------------------------------
# 6. Regresion auxiliar con controles
# ------------------------------------------------------------

aux_con_wu <- fixest::feols(
  PROP_VOTOSIZQ ~ pdet_post + uhat_con +
    indrural + va_percapita + participacion |
    codmpio + ano,
  data = datos_wu,
  demeaned = TRUE
)

# ------------------------------------------------------------
# 7. Verificar modelos auxiliares
# ------------------------------------------------------------

stopifnot(
  nobs(aux_sin_wu) == nrow(datos_wu),
  nobs(aux_con_wu) == nrow(datos_wu),
  !is.null(aux_sin_wu$X_demeaned),
  !is.null(aux_con_wu$X_demeaned)
)

# ------------------------------------------------------------
# 8. Calcular pruebas Wald con matriz Conley
# ------------------------------------------------------------

modelos_wu <- list(
  Sin_controles = list(
    modelo = aux_sin_wu,
    residuo = "uhat_sin"
  ),
  Con_controles = list(
    modelo = aux_con_wu,
    residuo = "uhat_con"
  )
)

resultados_wu <- list()

for (nombre in names(modelos_wu)) {
  
  modelo <- modelos_wu[[nombre]]$modelo
  variable_residuo <- modelos_wu[[nombre]]$residuo
  
  for (radio in RADIOS_WU) {
    
    # Matriz de covarianza Conley para este modelo y radio
    V_conley <- fastconley::vcovSpHAC(
      reg = modelo,
      unit = "codmpio",
      time = "periodo",
      lat = "lat",
      lon = "lon",
      kernel = KERNEL_CONLEY,
      dist_fn = DISTANCIA_CONLEY,
      dist_cutoff = radio,
      lag_cutoff = LAG_CUTOFF,
      balanced_pnl = TRUE,
      ncores = NCORES_CONLEY,
      ssc = SSC_CONLEY,
      psd_fix = PSD_FIX_CONLEY,
      data = datos_wu
    )
    
    # H0: coeficiente del residuo de primera etapa = 0
    prueba <- fixest::wald(
      modelo,
      keep = paste0("^", variable_residuo, "$"),
      vcov = V_conley
    )
    
    resultados_wu[[paste(nombre, radio, sep = "_")]] <-
      data.frame(
        Modelo = nombre,
        Radio_km = radio,
        Estadistico_Wald = as.numeric(prueba$stat),
        p_valor = as.numeric(prueba$p),
        row.names = NULL
      )
  }
}

# ------------------------------------------------------------
# 9. Consolidar resultados
# ------------------------------------------------------------

tabla_wu_conley <- do.call(rbind, resultados_wu)

rownames(tabla_wu_conley) <- NULL

tabla_wu_conley <- tabla_wu_conley[
  order(
    match(
      tabla_wu_conley$Modelo,
      c("Sin_controles", "Con_controles")
    ),
    tabla_wu_conley$Radio_km
  ),
]

# ------------------------------------------------------------
# 10. Mostrar resultados
# ------------------------------------------------------------

cat("\nPRUEBA DE FUNCION DE CONTROL CON CONLEY\n\n")

print(tabla_wu_conley, row.names = FALSE)

cat("\nNota: H0 establece que el coeficiente del residuo es cero.\n")
cat("La inferencia usa la matriz Conley del modelo auxiliar.\n")
cat("La validez completa del contraste requiere justificar\n")
cat("la inferencia con residuos estimados y dependencia espacial-temporal.\n")



# ============================================================
# 3. ANDERSON-RUBIN CON CONLEY
# ============================================================

RADIOS <- c(500, 650, 800)

prueba_AR <- function(beta0, radio, con_controles = FALSE) {
  
  datos_AR <- panel_conley
  
  datos_AR$Y_AR <-
    datos_AR$PROP_VOTOSIZQ -
    beta0 * datos_AR$pdet_post
  
  if (con_controles) {
    
    modelo_AR <- fixest::feols(
      Y_AR ~ z6_post + z3_post +
        indrural + va_percapita + participacion |
        codmpio + ano,
      data = datos_AR,
      demeaned = TRUE
    )
    
    especificacion <- "Con controles"
    
  } else {
    
    modelo_AR <- fixest::feols(
      Y_AR ~ z6_post + z3_post |
        codmpio + ano,
      data = datos_AR,
      demeaned = TRUE
    )
    
    especificacion <- "Sin controles"
  }
  
  V_conley <- vcov_conley(modelo_AR, radio)
  
  prueba <- fixest::wald(
    modelo_AR,
    keep = "^(z6_post|z3_post)$",
    vcov = V_conley
  )
  
  data.frame(
    Especificacion = especificacion,
    Beta_nulo = beta0,
    Radio_km = radio,
    Estadistico_Wald = as.numeric(prueba$stat),
    p_valor = as.numeric(prueba$p),
    df1 = as.numeric(prueba$df1),
    df2 = as.numeric(prueba$df2)
  )
}

# ------------------------------------------------------------
# Anderson-Rubin para H0: beta = 0
# ------------------------------------------------------------

resultados_AR_cero <- list()

for (controles in c(FALSE, TRUE)) {
  
  for (radio in RADIOS) {
    
    nombre <- paste(
      ifelse(controles, "Con_controles", "Sin_controles"),
      radio,
      sep = "_"
    )
    
    resultados_AR_cero[[nombre]] <- prueba_AR(
      beta0 = 0,
      radio = radio,
      con_controles = controles
    )
  }
}

tabla_AR_cero <- do.call(rbind, resultados_AR_cero)

rownames(tabla_AR_cero) <- NULL

tabla_AR_cero <- tabla_AR_cero[
  order(
    match(
      tabla_AR_cero$Especificacion,
      c("Sin controles", "Con controles")
    ),
    tabla_AR_cero$Radio_km
  ),
]

print(tabla_AR_cero, row.names = FALSE)

# ============================================================
# HANSEN J ROBUSTO A DEPENDENCIA ESPACIAL Y TEMPORAL
# GMM EN DOS ETAPAS
#
# Instrumentos excluidos: z6_post, z3_post
# Variable endogena:      pdet_post
# FE:                     codmpio + ano
# Radios espaciales:      500, 650, 800 km
# Kernel espacial:        Bartlett
# Kernel temporal:        Bartlett
# ============================================================


# ------------------------------------------------------------
# 1. Configuracion
# ------------------------------------------------------------

RADIOS_J <- c(500, 650, 800)

controles_J <- c(
  "indrural",
  "va_percapita",
  "participacion"
)

instrumentos_J <- c(
  "z6_post",
  "z3_post"
)

variables_J <- c(
  "PROP_VOTOSIZQ",
  "pdet_post",
  instrumentos_J,
  controles_J,
  "codmpio",
  "ano",
  "lat",
  "lon"
)


# ------------------------------------------------------------
# 2. Preparar una muestra comun y balanceada
# ------------------------------------------------------------

faltantes <- setdiff(variables_J, names(panel_conley))

if (length(faltantes) > 0) {
  stop(
    "Faltan variables: ",
    paste(faltantes, collapse = ", ")
  )
}

datos_J <- panel_conley[
  complete.cases(panel_conley[, variables_J]),
  ,
  drop = FALSE
]

# Indice temporal consecutivo:
# 2014 -> 1, 2018 -> 2, 2022 -> 3
# Esto evita interpretar la distancia entre elecciones
# como una diferencia de cuatro unidades temporales.

anios <- sort(unique(datos_J$ano))

datos_J$periodo_J <- match(datos_J$ano, anios)

datos_J <- datos_J[
  order(datos_J$codmpio, datos_J$periodo_J),
  ,
  drop = FALSE
]

rownames(datos_J) <- NULL

# Verificaciones del panel

if (length(anios) != 3) {
  stop("Se esperaban tres periodos electorales.")
}

conteo <- table(datos_J$codmpio)

if (any(conteo != 3)) {
  stop(
    "El panel no esta balanceado: cada municipio debe ",
    "tener exactamente tres observaciones."
  )
}

if (anyDuplicated(datos_J[c("codmpio", "periodo_J")])) {
  stop("Hay municipios con observaciones duplicadas por periodo.")
}

cat("Observaciones:", nrow(datos_J), "\n")
cat("Municipios:", length(unique(datos_J$codmpio)), "\n")
cat("Anios:", paste(anios, collapse = ", "), "\n")


# ------------------------------------------------------------
# 3. Eliminar efectos fijos mediante residualizacion
# ------------------------------------------------------------

# Residualizamos cada variable respecto a los efectos fijos
# municipales y temporales.
#
# Esto aplica el teorema de Frisch-Waugh-Lovell antes de
# construir los momentos del estimador GMM.

residualizar_FE <- function(variable, datos) {
  
  formula_FE <- stats::as.formula(
    paste0(variable, " ~ 1 | codmpio + ano")
  )
  
  modelo_FE <- fixest::feols(
    formula_FE,
    data = datos
  )
  
  as.numeric(resid(modelo_FE))
}

variables_transformar <- c(
  "PROP_VOTOSIZQ",
  "pdet_post",
  instrumentos_J,
  controles_J
)

datos_transformados <- as.data.frame(
  lapply(
    variables_transformar,
    residualizar_FE,
    datos = datos_J
  )
)

names(datos_transformados) <- variables_transformar


# ------------------------------------------------------------
# 4. Construir matrices de regresores e instrumentos
# ------------------------------------------------------------

Y <- as.matrix(datos_transformados[, "PROP_VOTOSIZQ", drop = FALSE])

D <- as.matrix(datos_transformados[, "pdet_post", drop = FALSE])

Z_excluidos <- as.matrix(datos_transformados[, instrumentos_J, drop = FALSE])

C <- as.matrix(datos_transformados[, controles_J, drop = FALSE])

# Sin controles:
# X = D
# Z = z6_post, z3_post

# Con controles:
# X = D, controles
# Z = instrumentos excluidos, controles

especificaciones_J <- list(
  
  "Sin controles" = list(
    X = D,
    Z = Z_excluidos
  ),
  
  "Con controles" = list(
    X = cbind(D, C),
    Z = cbind(Z_excluidos, C)
  )
  
)


# ------------------------------------------------------------
# 5. Distancia geografica esferica en kilometros
# ------------------------------------------------------------

distancia_esferica <- function(lat, lon) {
  
  R <- 6371.0088
  
  lat_rad <- lat * pi / 180
  lon_rad <- lon * pi / 180
  
  dlat <- outer(lat_rad, lat_rad, "-")
  dlon <- outer(lon_rad, lon_rad, "-")
  
  a <- sin(dlat / 2)^2 +
    outer(cos(lat_rad), cos(lat_rad), "*") *
    sin(dlon / 2)^2
  
  a <- pmax(0, pmin(1, a))
  
  2 * R * asin(sqrt(a))
}


# ------------------------------------------------------------
# 6. Covarianza Conley de los momentos instrumentales
# ------------------------------------------------------------

cov_momentos_conley <- function(
    Z,
    u,
    datos,
    radio_km,
    lag_cutoff = 2
) {
  
  n <- nrow(Z)
  q <- ncol(Z)
  
  if (length(u) != n) {
    stop("El vector de residuos no coincide con Z.")
  }
  
  scores <- sweep(
    Z,
    MARGIN = 1,
    STATS = as.numeric(u),
    FUN = "*"
  )
  
  Omega <- matrix(0, nrow = q, ncol = q)
  
  # ----------------------------------------------------------
  # A. Dependencia espacial dentro de cada periodo
  # ----------------------------------------------------------
  
  periodos <- sort(unique(datos$periodo_J))
  
  for (tt in periodos) {
    
    idx <- which(datos$periodo_J == tt)
    
    distancias <- distancia_esferica(
      datos$lat[idx],
      datos$lon[idx]
    )
    
    # Asegurar explícitamente que sea una matriz.
    distancias <- matrix(
      distancias,
      nrow = length(idx),
      ncol = length(idx)
    )
    
    # Kernel Bartlett espacial.
    W_espacial <- 1 - distancias / radio_km
    
    # Truncar pesos fuera del radio.
    W_espacial[distancias > radio_km] <- 0
    W_espacial[W_espacial < 0] <- 0
    
    # Diagonal unitaria.
    diag(W_espacial) <- 1
    
    S_t <- scores[idx, , drop = FALSE]
    
    Omega <- Omega +
      crossprod(S_t, W_espacial %*% S_t)
  }
  
  # ----------------------------------------------------------
  # B. Dependencia temporal dentro de cada municipio
  # ----------------------------------------------------------
  
  grupos <- split(
    seq_len(n),
    datos$codmpio
  )
  
  for (idx in grupos) {
    
    idx <- idx[
      order(datos$periodo_J[idx])
    ]
    
    if (length(idx) < 2) {
      next
    }
    
    for (a in seq_len(length(idx) - 1)) {
      
      for (b in (a + 1):length(idx)) {
        
        i <- idx[a]
        j <- idx[b]
        
        rezago <- abs(
          datos$periodo_J[i] -
            datos$periodo_J[j]
        )
        
        if (rezago > 0 && rezago <= lag_cutoff) {
          
          peso <- 1 - rezago / (lag_cutoff + 1)
          
          si <- matrix(scores[i, ], ncol = 1)
          sj <- matrix(scores[j, ], ncol = 1)
          
          Omega <- Omega +
            peso * (
              si %*% t(sj) +
                sj %*% t(si)
            )
        }
      }
    }
  }
  
  Omega <- (Omega + t(Omega)) / 2
  
  Omega
}
  


# ------------------------------------------------------------
# 7. Inversa estable para matrices simetricas
# ------------------------------------------------------------

inversa_estable <- function(A, tol = 1e-10) {
  
  A <- (A + t(A)) / 2
  
  descomp <- eigen(
    A,
    symmetric = TRUE
  )
  
  escala <- max(
    abs(descomp$values),
    1
  )
  
  # Regularizacion exclusivamente para evitar problemas
  # numericos por autovalores no positivos muy pequenos.
  
  valores <- pmax(
    descomp$values,
    escala * tol
  )
  
  A_inv <- descomp$vectors %*%
    diag(1 / valores, nrow = length(valores)) %*%
    t(descomp$vectors)
  
  (A_inv + t(A_inv)) / 2
}


# ------------------------------------------------------------
# 8. Hansen J de dos etapas para una especificacion y radio
# ------------------------------------------------------------

hansen_J_conley <- function(
    X,
    Z,
    Y,
    datos,
    radio_km,
    lag_cutoff = 2
) {
  
  n <- nrow(X)
  p <- ncol(X)
  q <- ncol(Z)
  
  df_J <- q - p
  
  if (df_J <= 0) {
    stop(
      "El modelo no esta sobreidentificado: ",
      "numero de instrumentos <= numero de regresores."
    )
  }
  
  # Verificar rango de matrices
  
  if (qr(X)$rank < p) {
    stop("La matriz X no tiene rango completo.")
  }
  
  if (qr(Z)$rank < q) {
    stop("La matriz Z no tiene rango completo.")
  }
  
  # ----------------------------------------------------------
  # Primera etapa GMM: 2SLS
  # ----------------------------------------------------------
  
  W0 <- inversa_estable(crossprod(Z))
  
  XZ <- crossprod(X, Z)
  ZX <- crossprod(Z, X)
  ZY <- crossprod(Z, Y)
  
  A0 <- XZ %*% W0 %*% ZX
  b0 <- XZ %*% W0 %*% ZY
  
  beta_2sls <- solve(A0, b0)
  
  u0 <- as.numeric(
    Y - X %*% beta_2sls
  )
  
  # ----------------------------------------------------------
  # Estimar covarianza Conley de los momentos con residuos
  # de la estimacion inicial
  # ----------------------------------------------------------
  
  Omega <- cov_momentos_conley(
    Z = Z,
    u = u0,
    datos = datos,
    radio_km = radio_km,
    lag_cutoff = lag_cutoff
  )
  
  W1 <- inversa_estable(Omega)
  
  # ----------------------------------------------------------
  # Segunda etapa GMM eficiente respecto a W1
  # ----------------------------------------------------------
  
  A1 <- XZ %*% W1 %*% ZX
  b1 <- XZ %*% W1 %*% ZY
  
  beta_gmm <- solve(A1, b1)
  
  u1 <- as.numeric(
    Y - X %*% beta_gmm
  )
  
  # Momentos agregados: sum_i z_i * u_i
  
  momentos <- crossprod(
    Z,
    matrix(u1, ncol = 1)
  )
  
  # Hansen J usando la ponderacion estimada en la
  # primera etapa GMM.
  
  J <- as.numeric(
    t(momentos) %*% W1 %*% momentos
  )
  
  # Limpiar negativos minimos por redondeo numerico.
  J <- max(J, 0)
  
  p_valor <- pchisq(
    J,
    df = df_J,
    lower.tail = FALSE
  )
  
  data.frame(
    Radio_km = radio_km,
    Observaciones = n,
    Instrumentos = q,
    Regresores = p,
    gl = df_J,
    Hansen_J = J,
    p_valor = p_valor
  )
}


# ------------------------------------------------------------
# 9. Ejecutar ambas especificaciones para los tres radios
# ------------------------------------------------------------

resultados_J <- list()

for (nombre in names(especificaciones_J)) {
  
  X <- especificaciones_J[[nombre]]$X
  Z <- especificaciones_J[[nombre]]$Z
  
  for (radio in RADIOS_J) {
    
    resultado <- hansen_J_conley(
      X = X,
      Z = Z,
      Y = Y,
      datos = datos_J,
      radio_km = radio,
      lag_cutoff = LAG_CUTOFF
    )
    
    resultado$Especificacion <- nombre
    
    resultados_J[[
      paste(nombre, radio, sep = "_")
    ]] <- resultado
  }
}


# ------------------------------------------------------------
# 10. Tabla final
# ------------------------------------------------------------

tabla_hansen_conley <- do.call(
  rbind,
  resultados_J
)

rownames(tabla_hansen_conley) <- NULL

tabla_hansen_conley <- tabla_hansen_conley[
  ,
  c(
    "Especificacion",
    "Radio_km",
    "Observaciones",
    "Instrumentos",
    "Regresores",
    "gl",
    "Hansen_J",
    "p_valor"
  )
]

tabla_hansen_conley <- tabla_hansen_conley[
  order(
    match(
      tabla_hansen_conley$Especificacion,
      c("Sin controles", "Con controles")
    ),
    tabla_hansen_conley$Radio_km
  ),
]

cat("\nHANSEN J CON COVARIANZA ESPACIAL-TEMPORAL\n\n")

print(
  tabla_hansen_conley,
  row.names = FALSE,
  digits = 6
)




# ============================================================
# CONJUNTOS DE CONFIANZA ANDERSON-RUBIN (95 %) CON CONLEY
# Radios espaciales: 500, 650 y 800 km
# Especificaciones: Sin controles y Con controles
# ============================================================

# 1. Definir parámetros de la grilla de búsqueda y radios
RADIOS_AR <- c(500, 650, 800)

# Rango amplio para evaluar beta_0 (ajustar min/max si el intervalo toca los extremos)
GRILLA_BETA <- seq(-2, 2, by = 0.001)

# Valor crítico Chi-cuadrado para df = 2 (2 instrumentos excluidos) al 95%
VALOR_CRITICO_CHI2 <- qchisq(0.95, df = 2) # ~5.991476

# 2. Función auxiliar para evaluar la prueba AR en un beta0 específico
evaluar_AR_beta0 <- function(beta0, radio, con_controles = FALSE) {
  
  datos_AR <- panel_conley
  datos_AR$Y_AR <- datos_AR$PROP_VOTOSIZQ - beta0 * datos_AR$pdet_post
  
  if (con_controles) {
    modelo_AR <- fixest::feols(
      Y_AR ~ z6_post + z3_post + indrural + va_percapita + participacion | codmpio + ano,
      data = datos_AR,
      demeaned = TRUE
    )
  } else {
    modelo_AR <- fixest::feols(
      Y_AR ~ z6_post + z3_post | codmpio + ano,
      data = datos_AR,
      demeaned = TRUE
    )
  }
  
  # Covarianza de Conley
  V_conley <- vcov_conley(modelo_AR, radio)
  
  # Test de Wald sobre los instrumentos excluidos
  prueba <- fixest::wald(
    modelo_AR,
    keep = "^(z6_post|z3_post)$",
    vcov = V_conley
  )
  
  stat_wald <- as.numeric(prueba$stat)
  p_val <- as.numeric(prueba$p)
  
  # Retornar TRUE si NO se rechaza H0 (es decir, beta0 pertenece al IC del 95%)
  es_aceptado <- stat_wald <= VALOR_CRITICO_CHI2
  
  return(data.frame(beta0 = beta0, stat = stat_wald, p_val = p_val, aceptado = es_aceptado))
}

# 3. Calcular conjuntos de confianza mediante inversión
resultados_intervalos_AR <- list()

for (controles in c(FALSE, TRUE)) {
  nombre_especificacion <- ifelse(controles, "Con controles", "Sin controles")
  
  for (radio in RADIOS_AR) {
    cat("Calculando conjunto AR 95% para:", nombre_especificacion, "| Radio:", radio, "km...\n")
    
    # Evaluar grilla
    evaluaciones <- lapply(GRILLA_BETA, function(b) {
      evaluar_AR_beta0(beta0 = b, radio = radio, con_controles = controles)
    })
    
    df_eval <- do.call(rbind, evaluaciones)
    betas_aceptados <- df_eval$beta0[df_eval$aceptado]
    
    if (length(betas_aceptados) == 0) {
      ic_texto <- "Conjunto Vacío"
      lim_inf <- NA
      lim_sup <- NA
    } else {
      lim_inf <- min(betas_aceptados)
      lim_sup <- max(betas_aceptados)
      
      # Verificar si fue un intervalo continuo
      ic_texto <- sprintf("[%0.4f, %0.4f]", lim_inf, lim_sup)
    }
    
    resultados_intervalos_AR[[paste(nombre_especificacion, radio, sep = "_")]] <- data.frame(
      Especificacion = nombre_especificacion,
      Radio_km = radio,
      IC_95_AR = ic_texto,
      Limite_Inferior = lim_inf,
      Limite_Superior = lim_sup,
      stringsAsFactors = FALSE
    )
  }
}

# 4. Consolidar y presentar tabla de resultados
tabla_intervalos_AR <- do.call(rbind, resultados_intervalos_AR)
rownames(tabla_intervalos_AR) <- NULL

cat("\n====================================================================\n")
cat("CONJUNTOS DE CONFIANZA ANDERSON-RUBIN (95 %) ROBUSTOS A CONLEY\n")
cat("====================================================================\n\n")

print(tabla_intervalos_AR[, c("Especificacion", "Radio_km", "IC_95_AR")], row.names = FALSE)
