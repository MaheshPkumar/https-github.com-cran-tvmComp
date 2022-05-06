#' Computing Future Value of a Single Cash Flow under Monthly Compounding Frequency.
#' @details
#' According to \insertCite{brook;textual}{tvmComp}, when you deposit money in a certificate of deposit (CD) at a bank, the bank is technically borrowing or renting money from you with a promise to repay that money with interest. Let us assume that you purchase a CD for $500 with a promised annual percentage rate of 5 percent. The annual percentage rate (APR) is the yearly rate that you earn by investing or in other words, this is a charge for borrowing. Although the financial institution quotes the 5 percent interest rate on an annual basis, these institutions in fact often pay interest quarterly, monthly, or even daily. The period in which the financial institution applies interest is the Compounding Period, and the number of times it adds interest to an account each year is the Compounding Periods per Year.
#'The method \code{fvMonthlyCompSingleCF()} computes Future Value (fv) of a Single Cash Flow under Monthly Compounding Frequency and gives fv when values are passed to its three arguments. Here \code{r} is annual rate, \code{n} is number of years and  \code{pv} is Present Value.
#'@param r A number.
#'@param n A number.
#'@param pv A number.
#'@return Input values to three arguments \code{r} , \code{n} and \code{pv}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'\insertRef{brook}{tvmComp}
#'@examples
#'fvMonthlyCompSingleCF(0.08,10,2000)
#'fvMonthlyCompSingleCF(0.08,10,-2000)
#'@export
fvMonthlyCompSingleCF <- function (r, n, pv)
{
  fv_MonthlySingleCF <- ((pv * (1 + r/12)^(n*12)) * (-1))
  (fv_MonthlySingleCF=round(fv_MonthlySingleCF, digits=2))
}


