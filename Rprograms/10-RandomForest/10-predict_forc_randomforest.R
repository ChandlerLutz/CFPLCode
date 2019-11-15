##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-01-18

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(randomForest); library(latex2exp)})

set.seed(12345)

##The training data for the model for 2008Q2
TrainData2008.2 <- readRDS("../../Data/_Data_Final_/20-forc_ml_data_train_2008Q2.rds") %>%
  .[, fips.code := NULL] %>%
  .[, state.fips := NULL] %>%
  setnames("dzillow.forc.2008.2", "y")

##The prediction data for the model for 2008Q3
PredData2008.3 <- readRDS("../../Data/_Data_Final_/20-forc_ml_data_pred_2008Q3.rds")

final.output <- PredData2008.3[, .(fips.code, dzillow.forc.2008.3)]

PredData2008.3 <- PredData2008.3 %>%
  .[, c("fips.code") := NULL] %>%
  .[, c("state.fips") := NULL] %>%
  setnames("dzillow.forc.2008.3", "y")

## -- Random Forest Model -- ##
rfModel <- randomForest(y ~ ., data = TrainData2008.2,
                        importance = TRUE,
                        ntree = 5000)

rf.pred <- predict(rfModel, PredData2008.3)
final.output <- final.output %>%
  .[, rf.pred := rf.pred] %>%
  .[, rf.sq.error := (dzillow.forc.2008.3 - rf.pred) ^ 2]

## -- linear ar(2) model -- ##

full.lm.model <- lm(y ~ .,
                    TrainData2008.2)
full.lm.pred <- predict(full.lm.model, PredData2008.3)
ar.model <- lm(y ~ dzillow.forc.t_1 + dzillow.forc.t_2,
               TrainData2008.2)
ar.pred <- predict(ar.model, PredData2008.3)
final.output <- final.output %>%
  .[, ar.pred := ar.pred] %>%
  .[, ar.sq.error := (dzillow.forc.2008.3 - ar.pred) ^ 2] %>%
  .[, full.lm.pred := full.lm.pred] %>%
  .[, full.lm.sq.error := (dzillow.forc.2008.3 - full.lm.pred) ^ 2]

## -- linear ar(2) model with just foreclosures and HP(A) (referee request) -- ##
ar.forc.hp <- lm(y ~ dzillow.forc.t_1 + dzillow.forc.t_2 + hp.ret.t_2 + hp.ret.t_1,
                 TrainData2008.2)
ar.forc.hp.pred <- predict(ar.forc.hp, PredData2008.3)
final.output <- final.output %>%
  .[, ar.forc.hp.pred := ar.forc.hp.pred] %>%
  .[, ar.forc.hp.sq.error := (dzillow.forc.2008.3 - ar.forc.hp.pred) ^ 2]



## -- MSE for non-CA -- ##

##for linear ar model
mse.ar <- final.output[substr(fips.code, 1, 2) != "06", mean(ar.sq.error)]
msa.ar.forc.hp <- final.output[substr(fips.code, 1, 2) != "06", mean(ar.forc.hp.sq.error)]
##for the full model
mse.full <- final.output[substr(fips.code, 1, 2) != "06", mean(full.lm.sq.error)]
##for random forest
mse.rf <- final.output[substr(fips.code, 1, 2) != "06", mean(rf.sq.error)]

print("percentage decrease in MSE from using a random forest relative to the AR(2)")
print((mse.rf - mse.ar) / mse.ar)
print("percentage decrease in MSE from using a random forest relative to a full lm model")
print((mse.rf - mse.full) / mse.full)
print("percentage decrease in MSE from using a random forest relative to the ARX(2,2) with Forc and HPA")
print((mse.rf - msa.ar.forc.hp) / msa.ar.forc.hp)

## -- Save the prediction for the counties  -- ##

if (!dir.exists("RdsFiles")) dir.create("RdsFiles/")
saveRDS(final.output, "RdsFiles/10-dforc_2008Q3_predictions.rds")

zillow.pred <- copy(final.output) %>%
  .[, .(fips.code, forc.rf.pred = rf.pred)] %>%
  setkey(fips.code)

##Add the predictions to the county data
county.panel <- readRDS("../../Data/_Data_Final_/30-county_panel.rds")
if ("forc.rf.pred" %in% names(county.panel)) {
  county.panel <- county.panel[, forc.rf.pred := NULL]
}
county.panel <- setkey(county.panel, fips.code) %>%
  merge(zillow.pred, all.x = TRUE)
saveRDS(county.panel, "../../Data/_Data_Final_/30-county_panel.rds")

##Add the predictions to the zip data
cw <- fread("data-raw/missouri_databridge_county_to_zip_2010.csv",
            skip = 1) %>%
  .[, .(fips.code = sprintf("%05.f", county),
        zip = sprintf("%05.f", `ZIP Census Tabulation Area`),
        cw.housing.units = `Total HUs, 2010 census`)] %>%
  .[, .(zip, fips.code, cw.housing.units)] %>%
  setkey(fips.code)

zip.panel <- readRDS("../../Data/_Data_Final_/40-zip_panel.rds")
if ("forc.rf.pred" %in% names(zip.panel)) {
  zip.panel <- zip.panel[, forc.rf.pred := NULL]
}

zillow.pred.zip <- merge(zillow.pred, cw,
                     all = TRUE, allow.cartesian = TRUE) %>%
  .[!is.na(forc.rf.pred)] %>%
  .[, w.weights := cw.housing.units / sum(cw.housing.units), by = zip] %>%
  .[, .(forc.rf.pred = sum(forc.rf.pred * w.weights)), by = zip]

zip.panel <- zip.panel %>%
  setkey(zip) %>%
  merge(zillow.pred.zip, all.x = TRUE) %>%
  .[complete.cases(.)]

saveRDS(zip.panel, "../../Data/_Data_Final_/40-zip_panel.rds")



## --- Plot Variable Importance --- ##

##For information on %IncMSE seee
## https://stats.stackexchange.com/a/284859/12053
## https://stackoverflow.com/a/27920414/1317443
## https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm

##Based on https://stackoverflow.com/a/6364905/1317443
simpleCap <- function(x) {
  x <- tolower(x)
  s <- strsplit(x, "\\.")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=".")
}
## varImpPlot(rfModel)

##data.table with variable importance
DT.rf.imp <- rfModel$importance %>%
  as.data.table(keep.rownames = TRUE) %>%
  setnames("%IncMSE", "PercIncMSE") %>%
  .[order(PercIncMSE)] %>%
  .[, label := rn] %>%
  .[, label := sapply(label, simpleCap)] %>%
  ##Get the labels for t-1
  .[, label := gsub(".T_1", "$_{t-1}$", label)] %>%
  .[, label := gsub(".T_2", "$_{t-2}$", label)] %>%
  .[, label := gsub("_hp", " $\\\\times$ Hp", label)] %>%
  .[, label := gsub(".Sq", "$^2$", label)] %>%
  .[, label := gsub("Dzillow", "$\\\\Delta$Zillow", label)] %>%
  ##Delete two dollar signs right next to each other
  .[, label := gsub("\\$\\$", "", label)] %>%
  .[, label := gsub("\\.", "", label)] %>%
  ##Update a few more for the years
  .[label == "NonOccRate", label := "NonOccRate2005"] %>%
  .[label == "Incomeperhousehold", label := "IncomePerHousehold2007"] %>%
  .[label == "MaxUiBen2007", label := "MaxUIBenefits2007"]


##The plot
if (!dir.exists("PlotsFinal")) dic.create("PlotsFinal")
pdf("PlotsFinal/10-RF_variable_importance.pdf",
    width = 9, height = 10.3)
DT.rf.imp[, dotchart(PercIncMSE, labels = TeX(label),
                     main = "Random Forest Variable Importance -- %IncMSE",
                     cex = 1.1, yaxs = "i")]
dev.off()

