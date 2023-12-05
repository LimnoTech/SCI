
library(readxl)
library(usethis)



# Import Data -------------------------------------------------------------


# Only needed if data in Excel gets updated
    # dumpsite_score <- read_excel("data/lookup_tables.xlsx", sheet = "dumpsite_score")
    # use_data(dumpsite_score)
    #
    # dumpsite_weight <- read_excel("data/lookup_tables.xlsx", sheet = "dumpsite_weight")
    # use_data(dumpsite_weight)
    #
    # trash_score <- read_excel("data/lookup_tables.xlsx", sheet = "trash_score")
    # use_data(trash_score)
    #
    # connectivity_score <- read_excel("data/lookup_tables.xlsx", sheet = "connectivity_score")
    # use_data(connectivity_score)


df <- readxl::read_excel("J:/DDOEIP/GIS/GISData/Rapid_Stream_Assessment/2023/StreamReaches_20231002_INT.xlsx")


trash_summary <- assess_trash(df)
