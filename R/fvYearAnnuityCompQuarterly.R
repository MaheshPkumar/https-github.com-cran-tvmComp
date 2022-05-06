#'Calculates Future Value of Yearly Paid Ordinary Annuity or Annuity Due that is Compounded Quarterly.
#'@details
#' According to \insertCite{humm;textual}{tvmComp}, Annuities are classified by the date of payment. In an Ordinary Annuity, payments are made at the end of each payment period. In an Annuity Due, payments are made at the beginning of each payment period. Loan payments, mortgage payments, and interest payments on bonds are all examples of Ordinary Annuities. Examples of annuities due include lease rental payments on real estate or equipment.
#'The method \code{fvYearlyAnnuityCompQuarterly()}is developed to calculate Future Value of Yearly-Paid Ordinary Annuity or Annuity Due that is Compounded Quarterly. The method gives Future Value when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pmt} is amount of one annuity and \code{bgn} is the computational mode. (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments
#'@param r A number.
#'@param n A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pmt} and \code{bgn}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'#'@references
#'\insertRef{humm}{tvmComp}
#'@examples
#'fvYearlyAnnuityCompQuarterly(0.11,4,-300,0)
#'fvYearlyAnnuityCompQuarterly(0.11,4,-300,1)
#'fvYearlyAnnuityCompQuarterly(0.11,4,300,0)
#'fvYearlyAnnuityCompQuarterly(0.11,4,300,1)
#'@export
fvYearlyAnnuityCompQuarterly <- function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
        eay <-(1+r/4) ^ 4 -1
    (fv_yAnnuityqtr=round((pmt/eay * ((1 + eay)^n - 1)) * (1 + eay)^bgn * (-1), digits=2))
  }
}




