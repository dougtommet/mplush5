## code to prepare `ex5_5` dataset goes here
mc_mod <- MplusAutomation::mplusObject(
  TITLE = "this is an example of a two-parameter
logistic item response theory (IRT) model",
  MONTECARLO = "names = u1-u20;
           generate = u1-u20(1);
           categorical = u1-u20;
           nobs = 500;
           nreps = 1;
           save = ex5_5part2.dat;	",
  ANALYSIS = "ESTIMATOR = MLR;",
  MODELPOPULATION = "f by u1-u20*1;
            f@1;
            [u1$1-u10$1*-.5 u11$1-u20$1*.5];",
  MODEL = "f by u1-u20*1;
           f@1;
           [u1$1-u10$1*-.5 u11$1-u20$1*.5];",
  OUTPUT = "TECH8 tech9;"
)
fs::dir_create(here::here("data-raw", "ex5_5"))
setwd(here::here("data-raw", "ex5_5"))
MplusAutomation::mplusModeler(mc_mod, modelout = "mc_ex5_5.inp", run = 1)

ex5_5_df <- readr::read_fwf("ex5_5part2.dat",
                            readr::fwf_widths(c(13,   13,   13,   13,   13,   13,   13,   13,   13,   13,    13,    13,    13,    13,    13,    13,    13,    13,    13,    13),
                                              c("u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9", "u10", "u11", "u12", "u13", "u14", "u15", "u16", "u17", "u18", "u19", "u20")))

mod <- MplusAutomation::mplusObject(
  TITLE = "this is an example of a two-parameter logistic
item response theory (IRT) model",
  VARIABLE = "CATEGORICAL ARE u1-u20;",
  ANALYSIS = "ESTIMATOR = MLR; ",
  MODEL = "f BY u1-u20*;
       f@1;",
  OUTPUT = "standardized; cinterval; TECH1; TECH8;",
  SAVEDATA = "H5RESULTS = ex5_5.h5;",
  rdata = ex5_5_df

)

MplusAutomation::mplusModeler(mod, modelout = "ex5_5.inp", run = 1)
fs::file_copy(here::here("data-raw", "ex5_5", "ex5_5.h5"),
              here::here("inst", "extdata", "ex5_5.h5"),
              overwrite = TRUE)

# modifying the model slightly in order to get some mod indices
mod <- MplusAutomation::mplusObject(
  TITLE = "this is an example of a two-parameter logistic
item response theory (IRT) model",
  VARIABLE = "CATEGORICAL ARE u1-u20;",
  ANALYSIS = "ESTIMATOR = WLSMV; ",
  MODEL = "f BY u1-u20*;
       f@1;
        f by u1@.2;",
  OUTPUT = "modindices(all); standardized; cinterval;",
  SAVEDATA = "H5RESULTS = ex5_5_wlsmv.h5;",
  rdata = ex5_5_df

)

MplusAutomation::mplusModeler(mod, modelout = "ex5_5_wlsmv.inp", run = 1)
fs::file_copy(here::here("data-raw", "ex5_5", "ex5_5_wlsmv.h5"),
              here::here("inst", "extdata", "ex5_5_wlsmv.h5"),
              overwrite = TRUE)
setwd(here::here())

usethis::use_data(ex5_5, overwrite = TRUE)




