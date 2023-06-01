# get shiny server and R from the rocker project
FROM rocker/r-rspm:20.04

# Name of the maintainer
MAINTAINER MAHESHKUMAR UMBARKAR <m.umbarkar@anlitiks.com>

# install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libssl-dev

# update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
    
# install R-shiny packages    
RUN R -e 'install.packages(c("shiny", "shinythemes" ,"shinydashboard","shinyjs", "shinycssloaders","DT","dplyr","gtsummary", "flextable","writexl","plotrix","gt", "shinyBS","kableExtra","readxl","huxtable"))'

RUN R -e 'webshot::install_phantomjs()'

# make a directory
RUN  mkdir -p /home/rstudio/anlitiks/stats-report-automation/descriptive_shiny_app

# copy the script folder
COPY descriptive_shiny_app /home/rstudio/anlitiks/stats-report-automation/descriptive_shiny_app

# expose the connection port
EXPOSE 3838

# set the working directory
WORKDIR /home/rstudio/anlitiks/stats-report-automation/descriptive_shiny_app

# run the command for shiny app
CMD ["R", "-e", "shiny::runApp(appDir = '/home/rstudio/anlitiks/stats-report-automation/descriptive_shiny_app',port = 3838,host = '0.0.0.0')"]
