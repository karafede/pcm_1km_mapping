
# install.packages('devtools')
# install.packages('rsconnect')
# library(devtools)
# devtools::install_github('rstudio/shinyapps')
# library(shinyapps)
# shinyapps::setAccountInfo(name='karafede', token='1C1E6ADAE1277BEE30DFE97A6CC6D497', secret='/ABjn1qbBlyFHX/Nvlb1EIXqNsmLwn53pZzlfP46')
# library(rsconnect)
# rsconnect::setAccountInfo(name='karafede', token='1C1E6ADAE1277BEE30DFE97A6CC6D497', secret='/ABjn1qbBlyFHX/Nvlb1EIXqNsmLwn53pZzlfP46')

library(rsconnect)
rsconnect::setAccountInfo(name='karafede', token='1C1E6ADAE1277BEE30DFE97A6CC6D497', secret='/ABjn1qbBlyFHX/Nvlb1EIXqNsmLwn53pZzlfP46')
# shinyapps::deployApp('path/to/your/app')
# shinyapps::deployApp('C:/RICARDO-AEA/Octile_app_new')
rsconnect::deployApp('C:/RICARDO-AEA/pcm_1km_mapping/shiny_pcm')


