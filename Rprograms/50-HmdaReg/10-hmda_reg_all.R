##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-12-03

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe); library(stargazer);
  library(starpolishr)})

source("00-preliminaries.R")


## -- Linear Probability Models -- ##

hmda.accepted.denied <- readRDS("../../Data/_Data_Final_/60-hmda_accepted_denied.rds") %>%
  setDT

mod1 <- felm(Denied ~ CA + log_LoanAmount + log_ApplicantIncome +
               ApplicantRace1 + ApplicantSex + year +
               Zip_Zhvi_AllHomes_ret_var_pre + Income.ret.lag + Pop.ret.lag +
               log_Zip_Zhvi_AllHomes + Zhvi_AllHomes_ret_lag + land.unavailable |
               0 | 0 | Zip3, hmda.accepted.denied[StateCode %in% c(sand.states)])

mod2 <- felm(Denied ~ CA + log_LoanAmount + log_ApplicantIncome +
               ApplicantRace1 + ApplicantSex + year +
               Zip_Zhvi_AllHomes_ret_var_pre + Income.ret.lag + Pop.ret.lag +
               log_Zip_Zhvi_AllHomes + Zhvi_AllHomes_ret_lag + land.unavailable |
               0 | 0 | Zip3, hmda.accepted.denied[StateCode %in% c(boom.states)])


## -- Loan growth -- ##

hmda.loan.growth <- readRDS("../../Data/_Data_Final_/60-hmda_loan_growth.rds") %>%
  setDT %>%
  .[, zip3 := substr(Zip, 1, 3)]

mod3 <- felm(LoanAmount_thousands_growth ~ CA + CA + ApplicantIncome_thousands_growth +
               hp.growth.2008.2009 + hp.growth.2010.2011 + hp.growth.2012.2014 +
               Income.growth.2008.2009 + Pop.growth.2008.2009 +
               Income.growth.2010.2011 + Pop.growth.2010.2011 +
               Income.growth.2012.2014 + Pop.growth.2012.2014 + land.unavailable |
               0 | 0 | zip3, hmda.loan.growth[StateCode %in% c(sand.states)])

mod4 <- felm(LoanAmount_thousands_growth ~ CA + CA + ApplicantIncome_thousands_growth +
               hp.growth.2008.2009 + hp.growth.2010.2011 + hp.growth.2012.2014 +
               Income.growth.2008.2009 + Pop.growth.2008.2009 +
               Income.growth.2010.2011 + Pop.growth.2010.2011 +
               Income.growth.2012.2014 + Pop.growth.2012.2014 + land.unavailable |
               0 | 0 | zip3, hmda.loan.growth[StateCode %in% c(boom.states)])


mod5 <- felm(formula.num <- numLoans_growth ~ CA + ApplicantIncome_thousands_growth +
               hp.growth.2008.2009 + hp.growth.2010.2011 + hp.growth.2012.2014 +
               Income.growth.2008.2009 + Pop.growth.2008.2009 +
               Income.growth.2010.2011 + Pop.growth.2010.2011 +
               Income.growth.2012.2014 + Pop.growth.2012.2014 + land.unavailable |
               0 | 0 | zip3,
             hmda.loan.growth[StateCode %in% c(sand.states)])



mod6 <- felm(formula.num <- numLoans_growth ~ CA + ApplicantIncome_thousands_growth +
               hp.growth.2008.2009 + hp.growth.2010.2011 + hp.growth.2012.2014 +
               Income.growth.2008.2009 + Pop.growth.2008.2009 +
               Income.growth.2010.2011 + Pop.growth.2010.2011 +
               Income.growth.2012.2014 + Pop.growth.2012.2014 + land.unavailable |
               0 | 0 | zip3,
             hmda.loan.growth[StateCode %in% c(boom.states)])

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type = "text", keep.stat = "n",
          keep = "CA")

##The stargazer output
star.out <- stargazer(mod1, mod2, mod3, mod4, mod5, mod6, keep = "CA",
                      keep.stat = c("n"),
                      title ="\\textbf{Probability of Denial and Loan Volume Growth After the CFPLs}",
                      label = "tab:hmda_reg",
                      star.char = "") %>%
    star_lhs_names(pattern = c("Denied", "LoanAmount\\\\_thousands\\\\_growth",
                               "numLoans\\\\_growth"),
                   line1 = c("Prob(Deny)", "Loan Growth (\\\\$)", "Loan Growth (Num)")
                   ) %>%
    ##Update the name for California
    star_rhs_names(pattern = "CA", line1 = "California") %>%
    ##The caption
  star_notes_tex(note.type = "caption",
                 note = "Regressions of the probability of mortgage denial and zip code level loan volume growth on an indicator for California and controls.  In columns (1) and (2),  the dependent variable takes a value of one if the mortgage application was denied and zero otherwise and the coefficients from a linear probability model. California takes a value of one for California and zero otherwise. Controls in columns (1) and (2) include the log of applicant income and loan amount; Zillow house price returns and IRS income and population growth in the year before the loan application was submitted; Land Unavailability; and factor variables for applicant race and applicant sex. The samples include only loans not sold to GSEs in AZ, CA, FL, and NV (column 1) and CA, CO, NY, and TX (column 2) from 2009 to 2014. Columns (3) - (4) and (5) - (6) show regressions where dollar loan volume growth or the growth in the number of loans represents the dependent variable. Loan volume growth is defined as  $(\\ln (\\text{Loan\\_vol}_{2009} + \\cdots + \\text{Loan\\_vol}_{2014})) - (\\ln (\\text{Loan\\_vol}_{2007}))$. The sample is restricted to loans not sold to GSEs. The key right-hand-side variable of interest is an indicator that takes a value of one for California. The data for these regressions are at the zip code level. Controls include Land Unavailability, applicant income growth and IRS income and population growth as well as Zillow zip code level house price growth for 2008-2009, 2010-2011, and 2012-2014. The regressions in columns (3) - (6) are weighted by the number of households. Robust standard errors are clustered at the 3-digit zip code level.")

##To insert some more information
string.sample <- c("\\hline \\\\[-1.8ex]",
            paste(c("Sample ", rep("& AZ,CA, & CA,CO, ", 3), "\\\\"), collapse = ""),
            paste(c(" ", rep("& FL,NV  & NY,TX ", 3), "\\\\"), collapse = ""),
            paste(c(" ", rep("& Loan Level ", 2), rep("& Zip Code ", 4), "\\\\"),
                  collapse = "")
            )
star.out <- star_insert_row(star.out, string = string.sample, insert.after = 17)

string.controls.est <- c(
    paste(c("Controls? ", rep(" & Yes ", 6), " \\\\"), collapse = ""),
    paste(c("Estimation Method ", rep(" & LPM ", 2), rep(" & OLS ", 4), " \\\\"),
          collapse = "")
)
star.out <- star_insert_row(star.out, string = string.controls.est, insert.after = 22)

##write
star_tex_write(star.out, file = "TexFiles/10-hmda_reg_star_out.tex", headers = TRUE)
