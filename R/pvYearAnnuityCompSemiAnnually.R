#' Computing Present Value of Yearly Paid Ordinary Annuity or Annuity Due that is Compounded Semi-Annually.
#'@details
#'As \insertCite{humm;textual}{tvmComp} explains, while dealing with annuity scenarios, first we must identify whether the problem is that of an Ordinary Annuity or Annuity Due. In an Ordinary Annuity the payment is made at the end of the payment period, whereas, in Annuity Due payment is made in the beginning of the year.
#'To incorporate this effect, the method \code{pvYearlyAnnuityCompSemiAnnually()} is developed to compute Present Value of Yearly Paid Ordinary Annuity or Annuity Due that is compounded Semi-Annually. This method gives Present Value when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years,\code{pmt} is amount of one annuity and \code{bgn} is the mode. (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments).
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
#'pvYearlyAnnuityCompSemiAnnually(0.11,4,-300,0)
#'pvYearlyAnnuityCompSemiAnnually(0.11,4,-300,1)
#'pvYearlyAnnuityCompSemiAnnually(0.11,4,300,0)
#'pvYearlyAnnuityCompSemiAnnually(0.11,4,300,1)
#'@export
pvYearlyAnnuityCompSemiAnnually <- function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
        r <-(1+r/2) ^ 2 -1
        (pv_yAnnuitysa= round((pmt/r * (1 - 1/(1 + r)^n)) * (1 + r)^bgn * (-1), digits=2))
}
}

