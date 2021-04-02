## ######################################################################### ##
## Set up reproducible environment of R packages
## ######################################################################### ##

# rm(list = ls(), inherits = TRUE)

## ========================================================================= ##
## install packages
## ========================================================================= ##

# install.packages("renv")

## ========================================================================= ##
## installing linux system packages
## ========================================================================= ##

#' NOTE: some of the packages may require linux system packages to be installed:
#' libgsl0-dev   for topicmodels 
#' libssh-dev    for ssh
#' libxml2-dev   for xml2
#' 
#' Check if these are installed, especially if the data lab containers 
#' have been restarted.


## ========================================================================= ##
## initialize renv environment
## ========================================================================= ##

## initialize renv with a new or existing project:
renv::init()

#' This will set up your project with a private library, and also make sure 
#' to install all of the packages you’re using into that library. The packages 
#' used in your project will be recorded into a lockfile, called renv.lock.
#' Also, the .Rprofile file will be updated to automatically use this library.
#' As you work in your project, you may need to install or upgrade different packages. 
#' As these packages are installed, renv will automatically write renv.lock for you. 
#' The renv.lock lockfile records the state of your project’s private library, 
#' and can be used to restore the state of that library as required.
#' Later, if you need to port your project to a new machine, you can call 
#' renv::restore() to reinstall all of the packages as declared in the lockfile.

## update all packages that are used in the project so far:
renv::update()

## if new packages have been added, take a snapshot:
renv::snapshot()


## to deactivate renv:
# renv::deactivate()  ## and then delete the ./renv/ folder and renv.lock file, if desired
# renv::activate()

## ========================================================================= ##
## restore renv environment from existing snapshot (renv.lock file)
## ========================================================================= ##

## restore renv environment from existing snapshot (renv.lock file):
renv::restore()

#' NOTE (1): 
#' This should probably be done in the setup.R file at the beginning of each 
#' estimation run?
#' NOTE (2):
#' Sometimes, renv::restore() failed because of missing packages (dependencies).
#' Just re-running renv::restore() another time worked, though.
