##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-11-18

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe); library(stargazer);
  library(starpolishr)})

judicial.states <- readRDS("../Data/_Data_Final_/10-Judicial_Foreclosure_States.rds")

DT <- fread("../DataLL/_DataFinal_/10-reo_forc_costs.csv") %>%
  .[, zip3 := sprintf("%03.f", zip3)] %>%
  .[, judicial.state := as.integer(state %in% judicial.states[["state.abb"]])]

char.vars <- c("zip3", "mac", "delin.status", "orig.fico.ventile",
               "orig.dti.ventile",
               "orig.ltv.ventile",
               "orig.loan.term",
               "current.upb.ventile", "next.monthly.mortg.payment.ventile",
               "remaining.interest.to.be.paid.ventile",
               "orig.interest.rate.ventile", "orig.upb.ventile")

DT <- DT[, c(char.vars) := lapply(.SD, as.character), .SDcols = char.vars]

mod1 <- felm(months.in.reo.forc ~ CA + CA.CFPL |
               reo.forc.date | 0 | state,
             DT[judicial.state == 0])

mod2 <- felm(months.in.reo.forc ~ CA.CFPL |
               zip3 + reo.forc.date | 0 | state,
             DT[judicial.state == 0])

mod3 <- felm(months.in.reo.forc ~ CA.CFPL  |
               orig.upb.ventile + current.upb.ventile + orig.interest.rate.ventile +
               loan.purpose + reo.forc.date + zip3 + orig.dti.ventile + mac +
               orig.fico.ventile + occupancy.status +
               orig.ltv.ventile + remaining.interest.to.be.paid.ventile +
               next.monthly.mortg.payment.ventile | 0 | state,
             DT[judicial.state == 0], exactDOF = TRUE)


mod4 <- felm(months.in.reo.forc ~ CA + CA.CFPL |
               reo.forc.date | 0 | state,
             DT)

mod5 <- felm(months.in.reo.forc ~ CA.CFPL |
               zip3 + reo.forc.date | 0 | state,
             DT)

mod6 <- felm(months.in.reo.forc ~ CA.CFPL  |
               orig.upb.ventile + current.upb.ventile + orig.interest.rate.ventile +
               loan.purpose + reo.forc.date + zip3 + orig.dti.ventile + mac +
               orig.fico.ventile + occupancy.status +
               orig.ltv.ventile + remaining.interest.to.be.paid.ventile +
               next.monthly.mortg.payment.ventile | 0 | state,
             DT, exactDOF = TRUE)

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type = "text", keep.stat = "n")

##The stargazer table
star.out <- stargazer(
  mod1, mod2, mod3, mod4, mod5, mod6,
  label = "tab:reg_forc_durations",
  title = "\\textbf{The Impact of the CFPLs on REO Foreclosure Durations}",
  type = "latex", keep.stat = "n",
  star.char = "") %>%
  gsub("CA.CFPL", "CA $\\\\\\times$ CFPL", x = .) %>%
  gsub("months.in.reo.forc", "Months in REO Foreclosure (Foreclosure Duration)", x = .)

##The CFPL, non-CA mean duration
non.CA.months.in.reo.forc.pre.non.judicial <- DT %>%
  .[judicial.state == 0] %>%
  .[CA == 0 & CFPL == 1, mean(months.in.reo.forc)] %>%
  sprintf("%.03f", .) %>%
  rep(., ((star_ncol(star.out) - 1) / 2)) %>%
  paste0(., collapse = " & ") %>%
  paste0("Avg(REO Forc Len) \\\\ Non-CA, CFPL & ", ., " & ")

non.CA.months.in.reo.forc.pre.all <- DT %>%
  .[CA == 0 & CFPL == 1, mean(months.in.reo.forc)] %>%
  sprintf("%.03f", .) %>%
  rep(., ((star_ncol(star.out) - 1)) / 2) %>%
  paste0(., collapse = " & ") %>%
  paste0(., " \\\\")

non.CA.months.in.reo.forc.pre <- paste0(
  non.CA.months.in.reo.forc.pre.non.judicial,
  non.CA.months.in.reo.forc.pre.all
)





##The Star string for FE, etc.
reo.date.fe <- rep("Yes", star_ncol(star.out) - 1) %>%
  paste0(., collapse = " & ") %>%
  paste0("REO Forc Date FE & ", ., " \\\\")
zip3.fe <- "Zip3 FE & No & Yes & Yes & No & Yes & Yes \\\\"
controls <- "Loan-level controls & No & No & Yes & No & No & Yes \\\\"
sample1 <- "  & Non-judicial & Non-judicial & Non-judicial & All & All & All \\\\"
sample2 <- rep("States", star_ncol(star.out) - 1) %>%
  paste0(., collapse = " & ") %>%
  paste0("Sample & ", ., " \\\\")

star.string <- c(non.CA.months.in.reo.forc.pre, "\\hline \\\\[-1.8ex]",
                 reo.date.fe, zip3.fe, controls, "\\hline \\\\[-1.8ex]",
                 sample1, sample2, "\\hline \\\\[-1.8ex]"
                 )

star.out <- star_insert_row(star.out, string = star.string,
                            insert.after = 21) %>%
  star_notes_tex(note.type = "caption", note = "Difference-in-differences regressions of the impact of the CFPLS on foreclosure maintenance and repair costs. See table \\ref{tab:reg_forc_costs_non_judicial} for the definition of foreclosures included in the data and the loan-level controls included. Columns (1) - (3) use only use data from non-judicial foreclosure states; columns (4) - (6) use data from all states.")

star_tex_write(star.out, file = "TexFiles/20-forc_duration_reg.tex",
               headers = TRUE)

