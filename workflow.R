
library(readxl)
library(usethis)


devtools::load_all(".")

# Import Data -------------------------------------------------------------


# Lookup Tables - Only needed if data in Excel gets updated
    # location <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "location")
    # usethis::use_data(location, overwrite = TRUE)
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



df_wq <- readxl::read_excel("data/ARII_Xtab_AmbWQ_SCI.xlsx")

df_wq_formatted <- format_results(df_wq)

df_wq_processed <- process_wq(df_wq_formatted)

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
conn <- connectivity_summary %>% dplyr::select(watershed, score)
fish <- fish_summary %>% dplyr::select(watershed, score)
hab <- habitat_summary %>% dplyr::select(watershed, score)
macro <- macroinvertebrate_summary %>% dplyr::select(watershed, score)


# Assess RSA Data
df_reach <- readxl::read_excel("J:/DDOEIP/GIS/GISData/Rapid_Stream_Assessment/2023/StreamReaches_20231002_INT.xlsx")
df_point <- readxl::read_excel("J:/DDOEIP/GIS/GISData/Rapid_Stream_Assessment/2023/StreamPoints_20231002_INT.xlsx")

trash_summary <- assess_trash(df_reach)
dumpsite_summary <- assess_dumpsites(df_point, df_reach)



# EIA
eia_summary <- assess_eia("data/sgs_export_01-01-2000_to_12-31-2023_subshed_intersect.xlsx")

