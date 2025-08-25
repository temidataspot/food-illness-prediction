# Step 1: Install required package
install.packages("rsconnect")

# Step 2: Load the library
library(rsconnect)

# Step 3: Set up your account (GET THIS FROM SHINYAPPS.IO AFTER SIGNUP)
rsconnect::setAccountInfo(name='ye2qsj-temi', token='48BD2FD5312305592834FB4F3854F109', secret='lSU9qKBKljY+Evd7mFnWd9oEsFfFfc69haISVGxF')



# Step 4: Deploy your app
# Make sure your working directory is set to the folder containing your app.R and data files

rsconnect::deployApp()
