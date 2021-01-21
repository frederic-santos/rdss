\name{dss_sex_estimation}
\alias{dss_sex_estimation}
\title{
  Perform secondary sex estimation.
}
\description{
  Estimate the sex of a target individual using an (imputed, complete)
  reference dataset of individuals of know sex. This functions is
  essentially a wrapper for various methods of supervised learning.
}
\usage{
dss_sex_estimation(ref, target, conf = 0.95,
                   method = c("lda", "glmnet", "linda", "rf"),
                   lda_selvar = c("none", "backward", "forward"),
                   rf_ntrees = 200, rf_downsampling = FALSE,
                   glmnet_type = 0,
                   glmnet_measure = c("deviance", "class"),
                   linda_alpha = 0.9)
}
\arguments{
  \item{ref}{
    dataframe (previously imputed if necessary) of reference
    individuals. No missing values allowed.
  }
  \item{target}{
    1-row dataframe, target individual.
  }
  \item{conf}{
    numeric value lying in [0.5, 1[; confidence level for sex estimation
    (i.e., posterior probability threshold).
  }
  \item{method}{
    character string; supervised learning method to be used for sex
    estimation. See Details below.
}
  \item{lda_selvar}{
    character string. Only parsed if \code{method = "lda"}. Method of
    variable selection to be used in LDA model.
  }
  \item{rf_ntrees}{
    numeric value. Only parsed if \code{method = "rf"}. Number of trees
    to be used in random forest model.
  }
  \item{rf_downsampling}{
    boolean. Only parsed if \code{method = "rf"}. Use a basic method of
    downsampling in case of unbalanced female/male classes in random
    forest model.
  }
  \item{glmnet_type}{
    numeric value. Only parsed if \code{method = "glmnet"}. Passed to
    \code{\link[glmnet]{glmnet}} as the \code{alpha} argument. In
    particular, choose `0` for ridge regression and `1` for lasso
    regression.
  }
  \item{glmnet_measure}{
    Only parsed if \code{method = "glmnet"}. Passed to
    \code{\link[glmnet]{cv.glmnet}} as the `type.measure` argument.
}
  \item{linda_alpha}{
    numeric value. Only parsed if \code{method = "linda"}. Passed to
    \code{\link[rrcov]{Lina}} as the `alpha` argument.
  }
}
\details{
  The argument \code{method} leaves the choice between four methods of
  supervised learning: classical linear discriminant analysis
  (\code{"lda"}) performed with \code{\link[MASS]{lda}}; robust
  discriminant analysis (\code{"linda"}) performed with
  \code{\link[rrcov]{Linda}}; random forests (\code{"rf"}) performed
  with \code{\link[randomForest]{randomForest}}; penalized logistic
  regression (\code{"glmnet"}) performed with
  \code{\link[glmnet]{glmnet}}. See their respective help pages for more
  details.

  Classification accuracy is automatically assessed using leave-one-out
  cross-validation (or out-of-bag error for random forests). The
  confusion matrix which is return thus corresponds to cross-validated
  results.
}
\value{
  A list of three components:
  \item{res_dss}{A dataframe of results for the target individual, with
    all necessary details about the model used for sex estimation.}
  \item{table_loocv}{A confusion matrix obtained by leave-one-out
  cross-validation on the reference sample \code{ref}.}
  \item{details}{Additional method-specific details, such as coefficient
    values or variable importance, depending on the value of the
  \code{method} argument.}
}
\author{
  Frédéric Santos.
}
\seealso{
  \code{\link[MASS]{lda}}, \code{\link[rrcov]{Linda}},
  \code{\link[randomForest]{randomForest}},
  \code{\link[glmnet]{glmnet}}, \code{\link[glmnet]{cv.glmnet}}
}
\concept{sex estimation}
