#' Computing Present Value of Semi-Annually Paid Ordinary Annuity or Annuity Due that is Compounded Semi-Annually.
#'@details
#'The method \code{pvSemiAnnuallyPaidAnnuityCompSemiAnnually()} is developed to compute Present Value of Semi-Annually Paid Ordinary Annuity or Annuity Due that is Compounded Semi-Annually and gives Present Value when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pmt} is amount of one annuity and \code{bgn} is the computational mode. (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments)
#'@param r A number.
#'@param n A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pmt} and \code{bgn}
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'pvSemiAnnuallyPaidAnnuityCompSemiAnnually(0.06,12, -200, 1)
#'pvSemiAnnuallyPaidAnnuityCompSemiAnnually(0.06,12, -200, 0)
#'pvSemiAnnuallyPaidAnnuityCompSemiAnnually(0.06,12, 200, 1)
#'pvSemiAnnuallyPaidAnnuityCompSemiAnnually(0.06,12, 200, 0)
#'@export
pvSemiAnnuallyPaidAnnuityCompSemiAnnually <- function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
   (pv_saAnnuitysa= round((pmt/(r/2) * (1 - 1/(1 + r/2)^(n*2))) * (1 + r)^bgn * (-1), digits=2))
  }
}


