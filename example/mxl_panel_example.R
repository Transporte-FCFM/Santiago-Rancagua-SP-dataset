# ==============================================================================
# EXAMPLE MODEL: Panel Mixed Logit (Error Component)
# Dataset: sp_santiago_rancagua_public.csv
# ==============================================================================
# Minimal estimation example for the Santiago-Rancagua SP dataset.
#
# Specification: MNL kernel + iid error components in ALL three alternatives,
# with a single shared sigma (Apollo Manual, Sec. 6.2).
# The error components are constant across the choice tasks of each
# respondent, capturing the panel effect. Identification is guaranteed by
# the independence of the draws across alternatives and the panel structure.
#
# How to run (from the repository root):
#   Rscript example/mxl_panel_example.R
# Requires the 'apollo' package: install.packages("apollo")
# ==============================================================================

library(apollo)

rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

apollo_initialise()

# 1. DATA ----------------------------------------------------------------------
# Path works when running from the repository root; falls back to example/.
db_path <- "sp_santiago_rancagua_public.csv"

database <- read.csv(db_path, fileEncoding = "UTF-8")

# Structural NAs (micro_* columns in 2-mode OD pairs) must be numeric for
# Apollo; availabilities (av_micro = 0) ensure they never enter the likelihood.
database[is.na(database)] <- 0

# Cost actually displayed to each respondent: standard or reduced fare,
# depending on the reduced_fare flag.
database$train_cost_shown <- ifelse(database$reduced_fare == 1,
                                    database$train_cost_reduced, database$train_cost)
database$bus_cost_shown   <- ifelse(database$reduced_fare == 1,
                                    database$bus_cost_reduced, database$bus_cost)
database$micro_cost_shown <- ifelse(database$reduced_fare == 1,
                                    database$micro_cost_reduced, database$micro_cost)

# Estimation filters: drop the dominated attention-check task (task 13)
# and opt-out choices (alternative 4).
database <- subset(database, task_id != 13 & choice != 4)

# 2. APOLLO SETTINGS -----------------------------------------------------------
apollo_control <- list(
  modelName       = "mxl_panel_example",
  modelDescr      = "Panel Mixed Logit - shared-sigma iid error components in all alternatives",
  indivID         = "respondent_id",
  mixing          = TRUE,
  nCores          = 4,
  outputDirectory = "output"
)

# 3. PARAMETERS ----------------------------------------------------------------
apollo_beta <- c(
  asc_train   = 0,
  asc_bus     = 0,
  asc_micro   = 0,

  # Level of service
  b_tt        = 0,
  b_cost      = 0,
  b_lmt       = 0,
  b_hw        = 0,

  # Comfort and reliability
  b_var       = 0,
  b_crowd     = 0,
  b_aircon    = 0,
  b_vendors   = 0,

  # Shared error-component scale. Starting value 0.5 (not 0) to avoid
  # starting in a flat region of the log-likelihood.
  sigma_panel = 0.5
)

# Train constant as reference
apollo_fixed <- "asc_train"

# 4. DRAWS ---------------------------------------------------------------------
# Three independent normal draws, one per alternative, constant across the
# choice tasks of each respondent (inter-individual draws = panel effect).
apollo_draws <- list(
  interDrawsType = "mlhs",
  interNDraws    = 1500,
  interUnifDraws = c(),
  interNormDraws = c("draw_train", "draw_bus", "draw_micro"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

# 5. RANDOM COMPONENTS ---------------------------------------------------------
apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list()
  randcoeff[["ec_train"]] <- sigma_panel * draw_train
  randcoeff[["ec_bus"]]   <- sigma_panel * draw_bus
  randcoeff[["ec_micro"]] <- sigma_panel * draw_micro
  return(randcoeff)
}

apollo_inputs <- apollo_validateInputs()

# 6. LIKELIHOOD ----------------------------------------------------------------
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P <- list()
  V <- list()

  V[["train"]] <- asc_train + ec_train +
    b_tt      * train_tt +
    b_cost    * train_cost_shown +
    b_lmt     * train_lmt +
    b_hw      * train_headway +
    b_var     * train_arrival_time_variability +
    b_crowd   * train_crowding +
    b_aircon  * train_aircon +
    b_vendors * train_vendors

  V[["bus"]] <- asc_bus + ec_bus +
    b_tt      * bus_tt +
    b_cost    * bus_cost_shown +
    b_lmt     * bus_lmt +
    b_hw      * bus_headway +
    b_var     * bus_arrival_time_variability +
    b_crowd   * bus_crowding +
    b_aircon  * bus_aircon +
    b_vendors * bus_vendors   # constant 0 by design; identified from train/micro

  V[["micro"]] <- asc_micro + ec_micro +
    b_tt      * micro_tt +
    b_cost    * micro_cost_shown +
    b_lmt     * micro_lmt +
    b_hw      * micro_headway +
    b_var     * micro_arrival_time_variability +
    b_crowd   * micro_crowding +
    b_aircon  * micro_aircon +
    b_vendors * micro_vendors

  mnl_settings <- list(
    alternatives = c(train = 1, bus = 2, micro = 3),
    avail        = list(train = av_train, bus = av_bus, micro = av_micro),
    choiceVar    = choice,
    utilities    = V
  )

  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}

# 7. ESTIMATION AND OUTPUT -----------------------------------------------------
model <- apollo_estimate(apollo_beta, apollo_fixed,
                         apollo_probabilities, apollo_inputs)

apollo_modelOutput(model)
apollo_saveOutput(model)
