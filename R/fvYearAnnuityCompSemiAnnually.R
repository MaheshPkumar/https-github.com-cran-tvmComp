#'Calculates Future Value of Yearly Paid Ordinary Annuity or Annuity Due that is Compounded Semi-Annually.
#'@details
#'As \insertCite{brook;textual}{tvmComp} mentions that Future Value calculations value cash flow at a single point in time in the future.
#'The method \code{fvYearlyAnnuityCompSemiAnnually()} is developed to calculate Future Value of Yearly-Paid Ordinary Annuity or Annuity Due that is Compounded Semi-Annually. The method gives Future Value when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pmt} is amount of one annuity and \code{bgn} is the computational mode. (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments)
#'@param r A number.
#'@param n A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pmt} and \code{bgn}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{brook}{tvmComp}
#'@examples
#'fvYearlyAnnuityCompSemiAnnually(0.11,4,-300,0)
#'fvYearlyAnnuityCompSemiAnnually(0.11,4,-300,1)
#'fvYearlyAnnuityCompSemiAnnually(0.11,4,300,0)
#'fvYearlyAnnuityCompSemiAnnually(0.11,4,300,1)
#'@export
fvYearlyAnnuityCompSemiAnnually <- function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
         eay <-(1+r/2) ^ 2 -1
    (fv_yAnnuitysa=round((pmt/eay * ((1 + eay)^n - 1)) * (1 + eay)^bgn * (-1), digits=2))
  }
}





