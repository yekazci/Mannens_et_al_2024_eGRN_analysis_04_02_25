
here::here()

# load the R environment with the necessary packages such as Epiregulon:

my_epiregulon_lib <- here::here("renv", "library/linux-rhel-9.4/R-4.4/x86_64-unknown-linux-gnu")

.libPaths(new = my_epiregulon_lib, include.site = FALSE)

.libPaths()

regulon.w <- readRDS(here::here('r_objects', 'regulon_weighted_by_celltypes.RDS'))

GeneExpressionMatrix <- readRDS(here::here('r_objects', 'GeneExpressionMatrix_used_for_epiregulon.RDS'))

score.combine <- readRDS(here::here('r_objects', 'tf_activity_by_cells_matrix.RDS'))

score.combined_by_celltypes <- epiregulon::calculateActivity(expMatrix = GeneExpressionMatrix, 
                                   regulon = regulon.w, 
                                   mode = "weight", 
                                   method = "weightedMean", 
                                   exp_assay = "logcounts",
                                   clusters = GeneExpressionMatrix$main_cell_types_renamed)

score.combined_by_celltypes |> saveRDS(here::here('r_objects', 'tf_activity_by_CELL_TYPES_matrix.RDS'))

message('The object was successfully saved!')