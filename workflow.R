
library(readxl)
library(usethis)


devtools::load_all(".")

# Import Data -------------------------------------------------------------


# Only needed if data in Excel gets updated
    # dumpsite_score <- readxl::read_excel("data/lookup_tables.xlsx", sheet = "dumpsite_score")
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


df_reach <- readxl::read_excel("J:/DDOEIP/GIS/GISData/Rapid_Stream_Assessment/2023/StreamReaches_20231002_INT.xlsx")
df_point <- readxl::read_excel("J:/DDOEIP/GIS/GISData/Rapid_Stream_Assessment/2023/StreamPoints_20231002_INT.xlsx")

trash_summary <- assess_trash(df_reach)

dumpsite_summary <- assess_dumpsites(df_point, df_reach)

# assign_score(2)

# assign_score2(2, dumpsite_score)
