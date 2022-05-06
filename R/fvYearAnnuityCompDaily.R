#'Calculates Future Value of Yearly Paid Ordinary Annuity or Annuity Due that is Compounded Daily.
#'@details
#'On the topic of making Interest Rates comparable, \insertCite{tman;textual}{tvmComp}, remarks that  sometimes it is  difficult to determine exactly how much you are paying or earning on a loan. That is so because the loan might be quoted not as compounding annually but rather as compounding quarterly or daily. If there are two loans say, one that is quoted as 8.084 percent compounded annually and one that is quoted as 7.85 percent compounded quarterly, then these are difficult to compare because the interest on one is compounded annually (you pay or receive interest just once a year) but the interest on the other is compounded quarterly (you pay or receive interest four times a year). To allow borrowers to compare rates between different lenders, what is known as the annual percentage rate (APR) must be clearly specified in the deal document. The annual percentage rate (APR) indicates the interest rate paid or earned in one year without compounding. We can calculate APR as the interest rate per period (for example, per month or week) multiplied by the number of periods during which compounding occurs during the year. So an interest rate of 2 percent per month would mean 24 percent per year. APR does not help much when the rates being compared are not compounded for the same number of periods per year. In fact, the APR is also called the nominal or quoted (stated) interest rate because it is the rate that the lender states the borrower is paying. In the previous example, both 8.084 percent and 7.85 percent are the APRs, but these are not comparable because the loans have different compounding periods. To make the two rates comparable, we need to calculate their equivalent rates using an annual compounding period. We do this by calculating the effective annual rate (also known as effective annual yield), and the annually compounded rate that produces the same return as the nominal or stated rate.
#'So in the method \code{fvYearlyAnnuityCompDaily()} is developed to be used in a scenario similar to the one mentioned above. The method \code{fvYearlyAnnuityCompDaily()} internally computes and uses effective Annual rate(yield) for yearly payments that are compounded daily and then gives you Future Value of yearly-paid Ordinary Annuity or Annuity Due that is Compounded daily, when values are passed to its four arguments. Here \code{r} is annual rate, \code{n} is number of years, \code{pmt} is amount of one annuity and \code{bgn} is the computational mode. (Enter 1 when annuity payment occurs at the beginning of the period; 0 for end of period payments).
#'@param r A number.
#'@param n A number.
#'@param pmt A number.
#'@param bgn A number.
#'@return Input values to four arguments \code{r} , \code{n} , \code{pmt} and \code{bgn}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{tman}{tvmComp}
#'@examples
#'fvYearlyAnnuityCompDaily(0.11,4,-300,0)
#'fvYearlyAnnuityCompDaily(0.11,4,-300,1)
#'fvYearlyAnnuityCompDaily(0.11,4,300,0)
#'fvYearlyAnnuityCompDaily(0.11,4,300,1)
#'@export
fvYearlyAnnuityCompDaily <- function (r, n, pmt,bgn)
{
  if (bgn != 0 && bgn != 1) {
   message("Error: bgn should be 0 or 1")
  }
  else {
    # compute Effective Annual Rate(yield) for yearly payments that are compounded Daily
     eay <-(1+r/365) ^ 365 -1
    (fv_yAnnuityDaily=round((pmt/eay * ((1 + eay)^n - 1)) * (1 + eay)^bgn * (-1), digits=2))
  }
}




