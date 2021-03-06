##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-10-17

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe); library(stargazer);
  library(starpolishr)})

DT <- fread("../DataLL/_DataFinal_/10-reo_forc_costs.csv") %>%
  .[, zip3 := sprintf("%03.f", zip3)]


##squared months in reo forc
DT <- DT[, months.in.reo.forc2 := months.in.reo.forc ^ 2]

char.vars <- c("zip3", "mac", "delin.status", "orig.fico.ventile",
               "orig.dti.ventile",
               "orig.ltv.ventile",
               "orig.loan.term",
               "current.upb.ventile", "next.monthly.mortg.payment.ventile",
               "remaining.interest.to.be.paid.ventile",
               "orig.interest.rate.ventile", "orig.upb.ventile")

DT <- DT[, c(char.vars) := lapply(.SD, as.character), .SDcols = char.vars]

mod1 <- felm(preservation.repair.costs ~ CA + CFPL + CA.CFPL | 0 | 0 | state,
             DT)

mod2 <- felm(preservation.repair.costs ~ CA + CFPL + CA.CFPL +
               months.in.reo.forc + months.in.reo.forc2| 0 | 0 | state, DT)

mod3 <- felm(preservation.repair.costs ~ CA + CA.CFPL + months.in.reo.forc +
               months.in.reo.forc2 |
               reo.forc.date | 0 | state, DT)

mod4 <- felm(preservation.repair.costs ~ CA.CFPL + months.in.reo.forc +
               months.in.reo.forc2 |
               reo.forc.date + zip3 | 0 | state, DT)

mod5 <- felm(preservation.repair.costs ~ CA.CFPL + months.in.reo.forc +
               months.in.reo.forc2 |
               orig.upb.ventile + current.upb.ventile + orig.interest.rate.ventile +
               loan.purpose + reo.forc.date + zip3 + orig.dti.ventile + mac +
               orig.fico.ventile + occupancy.status +
               orig.ltv.ventile + remaining.interest.to.be.paid.ventile +
               next.monthly.mortg.payment.ventile | 0 | state,
             DT, exactDOF = TRUE)

stargazer(mod1, mod2, mod3, mod4, mod5, type = "text", keep.stat = "n")

##Time trends
zip3.vars <- names(DT) %>% .[grep("zip3_", .)]
f.zip3.trends <- paste0(zip3.vars, collapse = " + ")
f.6 <- paste0(
  "preservation.repair.costs ~ CA.CFPL + months.in.reo.forc + months.in.reo.forc2 +",
  f.zip3.trends, " | orig.upb.ventile + current.upb.ventile + orig.interest.rate.ventile + loan.purpose + reo.forc.date + zip3 + orig.dti.ventile + mac + orig.fico.ventile + occupancy.status + orig.ltv.ventile + remaining.interest.to.be.paid.ventile + next.monthly.mortg.payment.ventile + zip3 + reo.forc.date | 0 | state"
)

mod6 <- felm(as.formula(f.6), DT, exactDOF = TRUE)

## Add in quadratic trends
zip3.trends <- names(DT) %>% .[grepl("zip3_", x = .)]
zip3.trends.2 <- paste0(zip3.trends, ".2")
DT <- DT[, (zip3.trends.2) := lapply(.SD, function(x) x ^ 2), .SDcols = zip3.trends]

zip3.vars <- names(DT) %>% .[grep("zip3_", .)]
f.zip3.trends <- paste0(zip3.vars, collapse = " + ")

f.7 <- paste0(
  "preservation.repair.costs ~ CA.CFPL + months.in.reo.forc + months.in.reo.forc2 + ",
  f.zip3.trends, " | orig.upb.ventile + current.upb.ventile + orig.interest.rate.ventile + loan.purpose + reo.forc.date + zip3 + orig.dti.ventile + mac + orig.fico.ventile + occupancy.status + orig.ltv.ventile + remaining.interest.to.be.paid.ventile + next.monthly.mortg.payment.ventile + zip3 + reo.forc.date | 0 | state"
)
mod7 <- felm(as.formula(f.7), DT, exactDOF = TRUE)


stargazer(
  mod1, mod2, mod3, mod4, mod5, mod6, mod7,
  type = "text", keep.stat = c("n"),
  star.char = "",
  title = "The Impact of the CFPLs on Foreclosure Costs",
  keep = c("Constant", "CA", "CFPL", "CA.CFPL", "months.in.reo.forc"))

star.out <- stargazer(
  mod1, mod2, mod3, mod4, mod5, mod6, mod7,
  title = "\\textbf{The Impact of the CFPLs on Foreclosure Maintenance and Repair Spending -- All States}",
  label = "tab:reg_forc_costs_all_states",
  star.char = "",
  font.size = "small",
  type = "latex", keep.stat = c("n"),
  keep = c("Constant", "CA", "CFPL", "CA.CFPL", "months.in.reo.forc")) %>%
  gsub("preservation.repair.costs",
       "Foreclosure Maintenance and Repair Spending (\\\\$'s)", x = .) %>%
  star_rhs_names(pattern = c("CA.CFPL", "months.in.reo.forc ", "months.in.reo.forc2 "),
                 line1 = c("CA $\\\\times$ CFPL", "Months in REO", "Months in REO"),
                 line2 = c("", "Foreclosure", "Foreclosure$^2$")
                 ) %>%
  star_notes_tex(note.type = "caption", note = "See the notes for table \\ref{tab:reg_forc_costs_non_judicial}. This table uses data from all states.")


reo.date.fe <- c("REO Forc Date FE & No & No & Yes & Yes & Yes & Yes & Yes \\\\")
zip3.fe <- c("Zip3 FE & No & No & No & Yes & Yes & Yes & Yes \\\\")
controls <- c("Other Loan-level Controls & No & No & No & No & Yes & Yes & Yes \\\\")
zip3.trends <- c("\\hline \\\\[-1.8ex] Zip3 Dummies $\\times$ \\\\ Linear REO Forc Date Trends  & No & No & No & No & No & Yes & Yes \\\\")
zip3.trends2 <- c(" \\\\[-2ex] Zip3 Dummies $\\times$ \\\\ Quadratic REO Forc Date Trends & No & No & No & No & No & No & Yes \\\\ ")
star.string <- c("\\hline \\\\[-1.8ex] ", reo.date.fe, zip3.fe, controls, zip3.trends,
                 zip3.trends2)

star.out <- star_insert_row(star.out, star.string, insert.after = 33)


##turn sideways
star.out <- star_sidewaystable(star.out)
star_tex_write(star.out, file = "TexFiles/11-forc_costs_reg_all_states.tex",
               headers = TRUE)
