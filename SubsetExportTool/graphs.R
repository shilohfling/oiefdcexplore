##### Scratch work for radar visualization #####
library(devtools)
devtools::install_github("ricardo-bion/ggradar", 
                         dependencies=TRUE)
library(ggradar)

## Example from the package developer
# mtcars %>%
#         rownames_to_column( var = "group" ) %>%
#         mutate_at(vars(-group),funs(rescale)) %>%
#         tail(4) %>% select(1:10) -> mtcars_radar
# ggradar(mtcars_radar)
