
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
    #    # dumpsite_score <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "dumpsite_score")
    # usethis::use_data(dumpsite_score, overwrite = TRUE)
    #
    # dumpsite_weight <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "dumpsite_weight")
    # usethis::use_data(dumpsite_weight, overwrite = TRUE)
    #
    # trash_score <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "trash_score")
    # usethis::use_data(trash_score, overwrite = TRUE)
    #
    # connectivity_score <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "connectivity_score")
    # usethis::use_data(connectivity_score, overwrite = TRUE)
    #
    # eia_area <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "eia_area")
    # usethis::use_data(eia_area, overwrite = TRUE)


# Other Data
    # bmp_data <- read.csv("data/ipmt_export_01-01-2000_to_04-15-2024.csv")
    # usethis::use_data(bmp_data, overwrite = TRUE)

df_wq <- readxl::read_excel("data/ARII_Xtab_AmbWQ_SCI.xlsx")

df_wq_formatted <- format_results(df_wq)

df_wq_processed <- process_wq(df_wq_formatted)

# Water Quality - Nutrients
## Units for criteria are in mg/L. Original dataset does not include units for non-detects. Function does not filter for units so ND results get included
TP_summary <- assess_wq_nutrients(df_wq_processed, parameter_name = "Phosphorus, Total (as P)", piedmont_criteria = 0.04, coastal_plain_criteria = 0.0225)
TN_summary <- assess_wq_nutrients(df_wq_processed, parameter_name = "Nitrogen", piedmont_criteria = 1.295, coastal_plain_criteria = 0.395)


# Water Quality - Non-nutrient parameters
temperature_summary <- assess_wq(df_wq_processed, parameter_name = "Temperature, water", unit = "deg C", max_criteria = 24)
pH_summary <- assess_wq(df_wq_processed, parameter_name = "pH", max_criteria = 8.5, pH = "none", min_criteria = 6.0)
DO_summary <- assess_wq(df_wq_processed, parameter_name = "Dissolved oxygen (DO)", unit = "mg/L", min_criteria = 5.0)
Ecoli_summary <- assess_wq(df_wq_processed, parameter_name = "Escherichia coli", unit = "MPN/100mL", max_criteria = 410)
turbidity_summary <- assess_wq(df_wq_processed, parameter_name = "Turbidity", unit = "NTU", max_criteria = 20.9)
conductivity_summary <- assess_wq(df_wq_processed, parameter_name = "Conductivity", unit = "uS/cm", max_criteria = 300)






df_reach <- readxl::read_excel("J:/DDOEIP/GIS/GISData/Rapid_Stream_Assessment/2023/StreamReaches_20231002_INT.xlsx")
df_point <- readxl::read_excel("J:/DDOEIP/GIS/GISData/Rapid_Stream_Assessment/2023/StreamPoints_20231002_INT.xlsx")

trash_summary <- assess_trash(df_reach)

dumpsite_summary <- assess_dumpsites(df_point, df_reach)

