#'Calculates Future Value of Monthly Paid Ordinary Annuity or Annuity Due that is Compounded Monthly.
#'@details
#'Explaining the variables involved in Time Value of Money equation, \insertCite{brook;textual}{tvmComp} points out that variables in a single cash flow (the lump-sum time value) of money equations are number of periods, r (Interest Rate or Rate of Return), pv (Present Value), and fv (Future Value), whereas annuity equation has an additional variable and that is pmt (payment).
#'The method \code{fvMonthlyPaidAnnuityCompMonthly()} calculates Future Value of Monthly Paid Ordinary Annuity or Annuity Due that is Compounded Monthly and gives fv when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pmt} is amount of one annuity and \code{bgn} is the computational mode. (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments)
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
#'fvMonthlyPaidAnnuityCompMonthly(0.08,10,-50,1)
#'fvMonthlyPaidAnnuityCompMonthly(0.08,10,-50,0)
#'fvMonthlyPaidAnnuityCompMonthly(.08,10,50,1)
#'fvMonthlyPaidAnnuityCompMonthly(.08,10,50,0)
#'@export
fvMonthlyPaidAnnuityCompMonthly <-function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
    message("Error: bgn should be 0 or 1")
  }
  else {
   (fv_mAnnuityMonthly= round((pmt/(r/12) * ((1 + r/12)^(n*12) - 1)) * (1 + r/12)^bgn * (-1), digits=2))
  }
}



