#'Calculates Future Value of Quarterly Paid Ordinary Annuity or Annuity Due that is Compounded Quarterly.
#'@details
#'According to \insertCite{brook;textual}{tvmComp}, an Annuity is a series of equal cash payments at regular intervals across time.
#'The method \code{fvQuarterlyPaidAnnuityCompQuarterly()} is developed to calculate Future Value of Quarterly-Paid Ordinary Annuity or Annuity Due that is Compounded Quarterly. The method gives FV when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pmt} is amount of one annuity and \code{bgn} is the mode (1 when annuity payment occurs at the beginning of the period; 0 for end of period payments)

#'@param r A number.
#'@param n A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pmt} and \code{bgn}.
#'@importFrom Rdpack reprompt
#'@author MaheshP. Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{brook}{tvmComp}
#'@examples
#'fvQuarterlyPaidAnnuityCompQuarterly(0.08,10,-50,1)
#'fvQuarterlyPaidAnnuityCompQuarterly(0.08,10,-50,0)
#'fvQuarterlyPaidAnnuityCompQuarterly(.08,10,50,1)
#'fvQuarterlyPaidAnnuityCompQuarterly(.08,10,50,0)
#'@export
fvQuarterlyPaidAnnuityCompQuarterly <- function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
   (fv_qAnnuityQtr=round((pmt/(r/4) * ((1 + r/4)^(n*4) - 1)) * (1 + r/4)^bgn * (-1), digits=2))
  }
}


