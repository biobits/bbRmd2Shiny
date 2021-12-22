# bbRmd2Shiny
A shiny runtime for parametrized r markdown reports 

## Description
The bbRmd2Shiny package with its knit_rmd_to_shiny function starts a shiny app that reads the YAML header of a given rmd document 
and generates a corresponding input form for the parameters defined in the yaml header. knit_rmd_to_shiny is a modified version of the knit_params_ask function
from the rmarkdown package that is used for parametrized rmd reports in RStudio.    In addition to that special YAML tags can be used to modify 
the appearance of the shiny app
