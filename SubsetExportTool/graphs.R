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

## Additional examples
# http://boot.rdata.work/contrib/radar-example/
# https://medium.com/@rhdzmota/alcohol-and-radar-plots-in-r-with-ggplot2-9ba7ad8c92c
# http://pcwww.liv.ac.uk/~william/Geodemographic%20Classifiability/func%20CreateRadialPlot.r

## Accessibility - Notes
# https://medium.com/square-corner-blog/accessible-colors-for-data-visualization-2ad64ac4ee7e

## Need to scale the data before we can use ggradar()