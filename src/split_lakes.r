library("readr") # para leer y escribir archivos CSV
library("dplyr") # para manipular datos

# ---------------------------------------------------------
# Parámetros
# ---------------------------------------------------------

N_STUDENTS <- 7 # número de subconjuntos (uno por estudiante)
LAKES_PER_STUDENT <- 500 # tamaño objetivo de cada subconjunto
OUTPUT_DIR <- "students" # carpeta de salida (se crea si no existe)
SEED <- 42 # semilla base para reproducibilidad

# ---------------------------------------------------------
# Cargar datos
# ---------------------------------------------------------

p7 <- readr::read_csv(
  "2007/nla2007_profile_20091008.csv",
  show_col_types = FALSE
)

i7 <- readr::read_csv(
  "2007/nla2007_sampledlakeinformation_20091113.csv",
  show_col_types = FALSE
)

# ---------------------------------------------------------
# Obtener un lago -> ecorregión únicos, solo lagos con perfil
# ---------------------------------------------------------

lake_eco <- i7 |>
  dplyr::distinct(SITE_ID, WSA_ECO9) |> # una fila por lago
  dplyr::semi_join(p7, by = "SITE_ID") # solo lagos presentes en el perfil

total_lakes <- nrow(lake_eco)
message(sprintf(
  "Lagos con perfil: %d en %d ecorregiones",
  total_lakes,
  dplyr::n_distinct(lake_eco$WSA_ECO9)
))

# Proporción a muestrear de cada ecorregión para obtener ~LAKES_PER_STUDENT en total
sample_prop <- LAKES_PER_STUDENT / total_lakes

# ---------------------------------------------------------
# Crear la carpeta de salida
# ---------------------------------------------------------

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR)
  message("Carpeta creada: ", OUTPUT_DIR)
}

# ---------------------------------------------------------
# Generar un subconjunto estratificado por ecorregión para cada estudiante
# ---------------------------------------------------------

for (i in seq_len(N_STUDENTS)) {
  set.seed(SEED + i) # semilla distinta por estudiante, reproducible

  # Muestreo estratificado: cada ecorregión contribuye proporcionalmente
  lakes_i <- lake_eco |>
    dplyr::group_by(WSA_ECO9) |>
    dplyr::slice_sample(prop = sample_prop) |> # misma proporción en cada ecorregión
    dplyr::ungroup() |>
    dplyr::pull(SITE_ID)

  # Filas del perfil que pertenecen a esos lagos
  subset_i <- p7 |>
    dplyr::filter(SITE_ID %in% lakes_i)

  # Nombre del archivo de salida
  filename <- file.path(OUTPUT_DIR, paste0("profile_student_", i, ".csv"))
  readr::write_csv(subset_i, filename)

  message(sprintf(
    "Grupo %d: %d lagos, %d filas -> %s",
    i,
    length(lakes_i),
    nrow(subset_i),
    filename
  ))
}

message("\nListo. Archivos guardados en: ", OUTPUT_DIR)

message("\nListo. Archivos guardados en: ", OUTPUT_DIR)
