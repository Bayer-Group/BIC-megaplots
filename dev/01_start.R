# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application and set some default {golem} options
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "Megaplots", # The name of the golem package containing the app (typically lowercase, no underscore or periods)
  pkg_title = "A 'shiny' Application for Pattern Detection", # What the Package Does (One Line, Title Case, No Period)
  pkg_description = "Depicts individual study courses per day.  Pre-specified
    events on certain days, for example drug intake, efficacy, or safety
    events, are indicated by colored lines.  The detection of patterns
    in the data or the temporal connection of study procedures and outcome
    events is supported by several functions, such as sorting, grouping,
    and changing the complexity of the graphical display.", # What the package does (one paragraph).
  authors = c(
    person(
      given = "Steffen", # Your First Name
      family = "Jeske", # Your Last Name
      email = "steffen.jeske.ext@bayer.com", # Your email
      role = c("aut", "cre")
    ),
    person(
      given = "Tammo", # Your First Name
      family = "Reinders", # Your Last Name
      email = "tammo.reinders.ext@bayer.com", # Your email
      role = c("aut", "cre")
    ),
    person(
      given = "Veronika", # Your First Name
      family = "Schmidt", # Your Last Name
      email = "veronika.schmidt.ext@bayer.com", # Your email
      role = c("aut", "cre")
    )
  ),
  repo_url = "github.com/Bayer-Group/BIC-megaplots", # The URL of the GitHub repo (optional),
  pkg_version = "2.0.1", # The version of the package containing the app
  set_options = TRUE # Set the global golem options
)

## Install the required dev dependencies ----
golem::install_dev_deps()

## Create Common Files ----
## See ?usethis for more information
usethis::use_gpl_license(version = 3) # You can set another license here
golem::use_readme_rmd(open = FALSE)
devtools::build_readme()
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
# usethis::use_code_of_conduct(contact = "Golem User")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
golem::remove_favicon() # Uncomment to remove the default favicon
golem::use_favicon(path = "inst/app/www/megaplot_hexsticker.png")
## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

## Use git ----
# usethis::use_git()
# ## Sets the remote associated with 'name' to 'url'
# usethis::use_git_remote(
#   name = "origin",
#   url = "https://github.com/<OWNER>/<REPO>.git"
# )

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
