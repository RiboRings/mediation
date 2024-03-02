library(mia)
library(MGnifyR)

# Set up the MGnify client instance
client <- MgnifyClient(usecache = TRUE, cache_dir = '~/Desktop/tmp/MGnify_cache')

# Retrieve the list of analyses associated with a study
accession_list <- searchAnalysis(mgclnt, "studies", "MGYS00005058", usecache = TRUE)

# Download all associated study/sample and analysis metadata
meta_dataframe <- getMetadata(mgclnt, accession_list, usecache = TRUE)

# Convert analyses outputs to a single `MultiAssayExperiment` object
mae <- getResult(mgclnt, meta_dataframe$analysis_accession, usecache = TRUE)
mae