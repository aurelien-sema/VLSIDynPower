## Script de préparation du dataset 'vlsi_data_stream'

# 1. Génération de données brutes (Simulation d'un bus 32-bits)
set.seed(42)
n_cycles <- 2000
n_bits <- 32

# Création d'une matrice binaire
raw_matrix <- matrix(
  sample(c(0, 1), n_cycles * n_bits, replace = TRUE),
  nrow = n_cycles,
  ncol = n_bits
)

# 2. Conversion au format "Stream" (Liste de vecteurs)
# Vos fonctions attendent une liste où chaque élément est un mot binaire
vlsi_data_stream <- split(raw_matrix, row(raw_matrix))
names(vlsi_data_stream) <- NULL

# 3. Exportation vers un CSV pour archivage (optionnel mais recommandé)
if (!dir.exists("inst/extdata")) dir.create("inst/extdata", recursive = TRUE)
write.csv(as.data.frame(raw_matrix), "inst/extdata/vlsi_data.csv", row.names = FALSE)

# 4. Incorporation officielle au package
# Cela crée le fichier data/vlsi_data_stream.rda
usethis::use_data(vlsi_data_stream, overwrite = TRUE)
