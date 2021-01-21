## Load example data:
data(poundbury, package = "rdss")

## Check:
dat <- dss_check_data(dtf = poundbury,
                      sex = "Sex",
                      females = "F",
                      males = "M",
                      tbd = "TBD")

## Select data:
target <- dat["Indet_1", ]
ref <- subset(dat, Sex %in% c("F", "M")) %>%
  droplevels() %>%           # remove unused factor levels
    dplyr::select_if(! is.na(target)) # keep only non-missing variables on target indiv
ref %<>% remove_na(which = "var",
                   prop_min = 0.5) %>%
    remove_na(which = "ind",
              prop_min = 0.5)

## Impute missing values:
imputed <- dss_impute_missing(dtf = ref, method = "missMDA")

## Perform sex estimation:
res <- dss_sex_estimation(ref = imputed,
                          target = target,
                          conf = 0.95,
                          method = "linda")

## Expected confusion matrix:
expected_loocv <- structure(list(F = c(18L, 2L),
                                 I = 3:2,
                                 M = c(0L, 15L)),
                            class = "data.frame",
                            row.names = c("F", "M"))

## Expected DSS results:
expected_dss <- structure(list(Indet_1 = c("F", "0", "Sex ~ LHML + LHMLD + LHHD + LFML + LTML + LTMLD",
                                           "21", "19", "12.5", "94.3", "100", "88.2")),
                          row.names = c("Sex estimate", "Prob(Sex==M)", "Model", "Number of females in ref. sample",
                                        "Number of males in ref. sample", "% indeterminate (LOOCV)",
                                        "% accuracy (LOOCV)", "% accuracy for females (LOOCV)",
                                        "% accuracy for males (LOOCV)"), class = "data.frame")

## Tests on LOOCV:
test_that("LOOCV is okay", {
    expect_equal(res$table_loocv,
                 expected_loocv)
})

## Tests on DSS:
test_that("DSS is okay", {
    expect_equal(res$res_dss,
                 expected_dss)
})
