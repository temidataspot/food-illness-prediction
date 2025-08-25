# install rsconnect
install.packages("rsconnect")

# load rsconnect
library(rsconnect)

# setting up account with details from shinyapps.io
rsconnect::setAccountInfo(name='*****', token='******', secret='******')


# deploy app
# ensured my working directory is set to the folder containing my app.R and data files

rsconnect::deployApp()
