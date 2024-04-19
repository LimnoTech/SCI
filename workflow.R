
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


x <- df_wq_processed %>% dplyr::filter(parameter == "pH", sample_id == "RCR01_170117")


pH_summary <- assess_wq(df_wq_processed, parameter_name = "pH", max_criteria = 8.5, min_criteria = 6.0)

turbidity_summary <- assess_wq(df_wq_processed, parameter_name = "Turbidity", max_criteria = 20.9)



df_reach <- readxl::read_excel("J:/DDOEIP/GIS/GISData/Rapid_Stream_Assessment/2023/StreamReaches_20231002_INT.xlsx")
df_point <- readxl::read_excel("J:/DDOEIP/GIS/GISData/Rapid_Stream_Assessment/2023/StreamPoints_20231002_INT.xlsx")

trash_summary <- assess_trash(df_reach)

dumpsite_summary <- assess_dumpsites(df_point, df_reach)

