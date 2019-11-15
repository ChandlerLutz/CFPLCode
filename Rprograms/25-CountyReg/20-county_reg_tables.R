##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-02-01



##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())



suppressMessages({library(CLmisc); library(stargazer); library(starpolishr)})

star.character <- ""

reg.all <- readRDS("RdsFiles/10-county_reg_base_full_all.rds")


star.main <- capture.output(
  stargazer(reg.all$model, type = "latex",
            star.char = star.character,
            single.row = TRUE,
            font.size = "small",
            keep = "forc.high_CA_time.char_",
            keep.stat = c("n", "rsq"))) %>%
  ##Update the coefficient names
  gsub("forc.high\\\\_CA\\\\_time.char\\\\_", "ForcHigh $\\\\times$ CA $\\\\times$ ", x = .) %>%
  gsub("\\\\_", "-", x = .) %>%
  ##Update the notes
  star_notes_tex(note.type = "caption",
                 note = "Column (1) shows the DDD estimates for each month for the baseline specification that only includes county and time fixed effects. Column (2) shows the full specification that includes county and time fixed effects as well as local macroeconomic and housing controls. In column (2) the controls are fully interacted with the time fixed effects. The controls in column (2) include the annual Bartik Labor Demand Shock from the CBP; the quarterly Bartik shock from the BLS QCEW; land unavailability; the percentage of non-owner occupied home purchases in 2005; the percentage of subprime loans in 2005; and median household income in 2007. Robust standard errors clustered at the county level are in parentheses.") %>%
  ##substitute for the long table
  gsub("\\\\begin\\{tabular\\}.*5pt\\}\\}", "\\\\begin\\{longtable\\}\\{", x = .) %>%
  ##Delete the line with the end tabular
  .[!grepl("\\\\end\\{tabular\\}", x = .)] %>%
  gsub("\\\\end\\{table\\}", "\\\\end\\{longtable\\}", x = .) %>%
  ##Change the dependent variable
  gsub("Dependent variable:", "Dep Var: REO Foreclosures / 10K Homes", x = .) %>%
  gsub("f.base.formula & f.full.formula", "Base Model & Full Model", x = .)

##Insert some info at the end of the table
string <- c("\\hline \\\\[-1.8ex]",
            "Time Fixed Effects & Yes & Yes \\\\",
            "County Fixed Effects & Yes & Yes \\\\",
            "Time FE $\\times$ Controls & No & Yes \\\\")
string.insert.after <- which(grepl("Observations", star.main)) + 1
star.main <- star_insert_row(star.main, string, insert.after = string.insert.after)

##Only keep the rows necessary for the table
end <- length(star.main)
start <- which(grepl("longtable", star.main))[1]
star.main <- star.main[start:end]

#add in the small
star.main <- c("\\small{", star.main, "}")

##Delete the row with "y" as the dependent variable
dep.var.y.line <- grepl("multicolumn\\{2\\}\\{c\\}\\{y\\}", star.main) %>% which
star.main <- star.main[-c(dep.var.y.line)]

star_tex_write(star.main, file = "TexFiles/20-county_reg_main.tex",
               headers = TRUE)


