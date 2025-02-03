
library(readxl)
library(usethis)

devtools::load_all(".")


# Input Variables ---------------------------------------------------------

# Excel spreadsheet with Ambient Water Quality Data
df_wq <- readxl::read_excel("data/ARII_Xtab_AmbWQ_SCI_downloaded_20250102.xlsx")

# Excel spreadsheet with Rapid Stream Assessment (RSA) reach and point data
df_reach <- readxl::read_excel("data/StreamReaches_20241105.xlsx")
df_point <- readxl::read_excel("data/StreamPoints_20241105_INT.xlsx")

# Date ranges used for filtering water quality data
start_date <- "2015-07-01"
end_date <- "2020-06-30"

# Date ranges used for filtering RSA data (for trees and dumpsites)
rsa_start_date <- "2020-01-01"
rsa_end_date <- "2024-12-31"


# Field name prefixes for RSA dataset
# Prefixes change depending on layer and table names pulled from geodatabase
reach_prefix_from_table <- "StreamReachAttributes"  # Should match name of stream reach attribute table from RSA .gdb
reach_prefix_from_layer <- "StreamReaches"          # Should match name of stream reach polyline layer from RSA .gdb

point_prefix_from_table <- "StreamPointAttributes"  # Should match name of stream point attribute table from RSA .gdb
point_prefix_from_layer <- "StreamPoints_Intersect" # Should match name of intersected stream point layer from RSA .gdb




# Lookup Tables -----------------------------------------------------------


# If data in lookup_tables.xlsx is modified, run the corresponding lines of code
#    below to update the relevant lookup table. Otherwise, keep code as comments

    # location_id <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "location_id")
    # usethis::use_data(location_id, overwrite = TRUE)
    #
    # location_name <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "location_name")
    # usethis::use_data(location_name, overwrite = TRUE)
    #
    # dumpsite_score <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "dumpsite_score")
    # usethis::use_data(dumpsite_score, overwrite = TRUE)
    #
    # dumpsite_weight <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "dumpsite_weight")
    # usethis::use_data(dumpsite_weight, overwrite = TRUE)
    #
    # trash_score <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "trash_score")
    # usethis::use_data(trash_score, overwrite = TRUE)
    #
    # connectivity_summary <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "connectivity_summary")
    # usethis::use_data(connectivity_summary, overwrite = TRUE)
    #
    # fish_summary <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "fish_summary")
    # usethis::use_data(fish_summary, overwrite = TRUE)
    #
    # habitat_summary <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "habitat_summary")
    # usethis::use_data(habitat_summary, overwrite = TRUE)
    #
    # macroinvertebrate_summary <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "macroinvertebrate_summary")
    # usethis::use_data(macroinvertebrate_summary, overwrite = TRUE)
    #
    # eia_subsheds <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "eia_subsheds")
    # usethis::use_data(eia_subsheds, overwrite = TRUE)
    #
    # eia_rock_creek <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "eia_rock_creek")
    # usethis::use_data(eia_rock_creek, overwrite = TRUE)
    #
    # eia_score <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "eia_score")
    # usethis::use_data(eia_score, overwrite = TRUE)





# Run Calculations --------------------------------------------------------

df_wq_formatted <- format_wq(df_wq)
df_wq_processed <- process_wq(df_wq_formatted, start_date, end_date)

# Water Quality - Nutrients
## Units for criteria are in mg/L. Original dataset does not include units for non-detects. Function does not filter for units so ND results get included
tp <- assess_wq_nutrients(df_wq_processed, parameter_name = "Phosphorus, Total (as P)", piedmont_criteria = 0.04, coastal_plain_criteria = 0.0225)
tn <- assess_wq_nutrients(df_wq_processed, parameter_name = "Nitrogen", piedmont_criteria = 1.295, coastal_plain_criteria = 0.395)


# Water Quality - Non-nutrient parameters
temp <- assess_wq(df_wq_processed, parameter_name = "Temperature, water", unit = "deg C", max_criteria = 24)
ph <- assess_wq(df_wq_processed, parameter_name = "pH",  unit = "none", max_criteria = 8.5, min_criteria = 6.0)
do <- assess_wq(df_wq_processed, parameter_name = "Dissolved oxygen (DO)", unit = "mg/L", min_criteria = 5.0)
ecoli <- assess_wq(df_wq_processed, parameter_name = "Escherichia coli", unit = "MPN/100mL", max_criteria = 410)
turb <- assess_wq(df_wq_processed, parameter_name = "Turbidity", unit = "NTU", max_criteria = 20.9)
cond <- assess_wq(df_wq_processed, parameter_name = "Conductivity", unit = "uS/cm", max_criteria = 300)


# Aquatic Biology - Pull static results from lookup table
conn <- connectivity_summary %>% dplyr::select(sci_subshed,
                                               Connectivity = score)
fish <- fish_summary %>% dplyr::select(sci_subshed,
                                       Fish = score)
hab <- habitat_summary %>% dplyr::select(sci_subshed,
                                         Habitat = score)
macro <- macroinvertebrate_summary %>% dplyr::select(sci_subshed,
                                                     Macroinvertebrates = score)


# Assess RSA Data
trash <- assess_trash(df_reach, rsa_start_date, rsa_end_date, reach_prefix_from_table, reach_prefix_from_layer)
dumpsite <- assess_dumpsites(df_point, df_reach,
                             rsa_start_date, rsa_end_date,
                             reach_prefix_from_table, reach_prefix_from_layer,
                             point_prefix_from_table, point_prefix_from_layer)

# EIA
eia <- assess_eia()

# Compile scores
all_scores <- trash[["score"]] %>%
  dplyr::full_join(dumpsite[["score"]]) %>%
  dplyr::full_join(eia[["score"]]) %>%
  dplyr::full_join(temp[["score"]]) %>%
  dplyr::full_join(ph[["score"]]) %>%
  dplyr::full_join(do[["score"]]) %>%
  dplyr::full_join(ecoli[["score"]]) %>%
  dplyr::full_join(turb[["score"]]) %>%
  dplyr::full_join(cond[["score"]]) %>%
  dplyr::full_join(tn[["score"]]) %>%
  dplyr::full_join(tp[["score"]]) %>%
  dplyr::full_join(conn) %>%
  dplyr::full_join(fish) %>%
  dplyr::full_join(hab) %>%
  dplyr::full_join(macro)

# Write results file
write.csv(all_scores, "output/all_scores.csv")





