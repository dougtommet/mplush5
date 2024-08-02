## code to prepare `ex3_1` dataset goes here
mc_mod <- MplusAutomation::mplusObject(
  TITLE = "this is an example of a simple linear
    regression for a continuous observed
    dependent variable with two covariates",
  MONTECARLO = "NAMES = y1 x1 x3;
              NOBSERVATIONS = 500;
              NREPS = 1;
              SEED = 53487;
              SAVE = ex3_1.dat;	",
  MODELPOPULATION = "[x1-x3@0];
          x1-x3@1;
          y1 ON x1*1 x3*.5;
          y1*1;
          [y1*.5];",
  MODEL = "y1 ON x1*1 x3*.5;
            y1*1;
            [y1*.5];"
)
fs::dir_create(here::here("data-raw", "ex3_1"))
setwd(here::here("data-raw", "ex3_1"))
MplusAutomation::mplusModeler(mc_mod, modelout = "mc_ex3_1.inp", run = 1)

ex3_1_df <- readr::read_fwf("ex3_1.dat", readr::fwf_widths(c(13, 13, 13), c("y1", "x1", "x3")))

mod <- MplusAutomation::mplusObject(
  TITLE = "this is an example of a simple linear
  regression for a continuous observed
  dependent variable with two covariates",
  MODEL = "y1 ON x1 x3;",
  OUTPUT = "standardized; cinterval;",
  SAVEDATA = "H5RESULTS = ex3_1.h5;",
  rdata = ex3_1_df

)

MplusAutomation::mplusModeler(mod, modelout = "ex3_1.inp", run = 1)
fs::file_copy(here::here("data-raw", "ex3_1", "ex3_1.h5"),
              here::here("inst", "extdata", "ex3_1.h5"),
              overwrite = TRUE)
setwd(here::here())

usethis::use_data(ex3_1, overwrite = TRUE)
