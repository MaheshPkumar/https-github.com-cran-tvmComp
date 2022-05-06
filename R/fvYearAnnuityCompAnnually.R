#'Calculates Future Value of Yearly Paid Ordinary Annuity or Annuity Due that is Compounded Annually.
#'@details
#'According to \insertCite{humm;textual}{tvmComp}, classification of Annuities is done by the date of payment.
#'In an Ordinary Annuity, the function processes the payments as an end-of-period payment.
#'Whereas, in an Annuity Due, the function processes the payments as a beginning-of-period payment. Further \insertCite{brook;textual}{tvmComp} explains that he future value of an annuity can be determined by multiplying the payment or deposit with the Future Value Interest Factor of an annuity.
#'The method, \code{fvYearlyAnnuityCompAnnually()} is developed to calculate Future Value of Yearly-Paid Ordinary Annuity or Annuity Due that is Compounded Annually.The method gives Future Value when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pmt} is amount of one annuity and \code{bgn} is the computational mode (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments).
#'@param r A number.
#'@param n A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pmt} and \code{bgn}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertAllCited{}
#'@examples
#'fvYearlyAnnuityCompAnnually(0.11,4,-300,0)
#'fvYearlyAnnuityCompAnnually(0.11,4,-300,1)
#'fvYearlyAnnuityCompAnnually(0.11,4,300,0)
#'fvYearlyAnnuityCompAnnually(0.11,4,300,1)
#'@export
fvYearlyAnnuityCompAnnually <- function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
        (fv_yAnnuityYearly=round((pmt/r * ((1 + r)^n - 1)) * (1 + r)^bgn * (-1), digits=2))
  }
}




