#install packages
install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)

# SECTION I
#define target folder
sheet_location = "E:\\Documents\\Rstudio\\Statistical Learning and Data Mining\\Datasets\\companies\\"

#define target data
sheets = list.files(sheet_location)

#define count of target data
len = length(sheets)

#import company data files
for(i in 1:len) {
  assign(paste0("comp", i),
         read.csv(paste0(sheet_location, sheets[i])))
}

#create vector count of days, t
t = c(1:nrow(comp1))

t = as.numeric(t)

#combine company data
comps = mget(paste0("comp", 1:len))

#isolate closing price and add t column
comps = lapply(comps, function(x) cbind(subset(x, select = Close), t))

#rename closing price to "s"
comps = lapply(comps, function(x) setNames(x, c("s", "t")))
list2env(comps,.GlobalEnv)

# SECTION II
#define function to create vector of percent change relative to previous day, y
get.y.fxn = function(comp) {
  t = 1:nrow(comp)
  y.t = c()
  y.t[1] = 0
  
  for (i in 2:length(t)) {
    y.t[i] = round((comp$s[i] - comp$s[i-1])/comp$s[i-1], 4)*100
  }
  
  comp$y = y.t
  return(comp)
}

#apply previously defined function
comps = lapply(comps, function(x) get.y.fxn(x))
list2env(comps,.GlobalEnv)

# SECTION III
#define function to create vector of closing prices for preceding 15 days, v
get.v.fxn = function(comp) {
  comp.1 = comp$y[1:nrow(comp)]
  comp.1 = comp.1[1:(nrow(comp)-15)]
  comp.2 = comp$y[2:nrow(comp)]
  comp.2 = comp.2[1:(nrow(comp)-15)]
  comp.3 = comp$y[3:nrow(comp)]
  comp.3 = comp.3[1:(nrow(comp)-15)]
  comp.4 = comp$y[4:nrow(comp)]
  comp.4 = comp.4[1:(nrow(comp)-15)]
  comp.5 = comp$y[5:nrow(comp)]
  comp.5 = comp.5[1:(nrow(comp)-15)]
  comp.6 = comp$y[6:nrow(comp)]
  comp.6 = comp.6[1:(nrow(comp)-15)]
  comp.7 = comp$y[7:nrow(comp)]
  comp.7 = comp.7[1:(nrow(comp)-15)]
  comp.8 = comp$y[8:nrow(comp)]
  comp.8 = comp.8[1:(nrow(comp)-15)]
  comp.9 = comp$y[9:nrow(comp)]
  comp.9 = comp.9[1:(nrow(comp)-15)]
  comp.10 = comp$y[10:nrow(comp)]
  comp.10 = comp.10[1:(nrow(comp)-15)]
  comp.11 = comp$y[11:nrow(comp)]
  comp.11 = comp.11[1:(nrow(comp)-15)]
  comp.12 = comp$y[12:nrow(comp)]
  comp.12 = comp.12[1:(nrow(comp)-15)]
  comp.13 = comp$y[13:nrow(comp)]
  comp.13 = comp.13[1:(nrow(comp)-15)]
  comp.14 = comp$y[14:nrow(comp)]
  comp.14 = comp.14[1:(nrow(comp)-15)]
  comp.15 = comp$y[15:nrow(comp)]
  comp.15 = comp.15[1:(nrow(comp)-15)]
  
  v = cbind(comp.1,comp.2,comp.3,comp.4,comp.5,
                 comp.6,comp.7,comp.8,comp.9,comp.10,
                 comp.11,comp.12,comp.13,comp.14,comp.15)
  
  return(v)
}

#apply previously defined function to create comp.v vectors
comp1.v = get.v.fxn(comp1)
comp2.v = get.v.fxn(comp2)
comp3.v = get.v.fxn(comp3)
comp4.v = get.v.fxn(comp4)
comp5.v = get.v.fxn(comp5)
comp6.v = get.v.fxn(comp6)
comp7.v = get.v.fxn(comp7)
comp8.v = get.v.fxn(comp8)
comp9.v = get.v.fxn(comp9)
comp10.v = get.v.fxn(comp10)
comp11.v = get.v.fxn(comp11)
comp12.v = get.v.fxn(comp12)
comp13.v = get.v.fxn(comp13)
comp14.v = get.v.fxn(comp14)
comp15.v = get.v.fxn(comp15)
comp16.v = get.v.fxn(comp16)
comp17.v = get.v.fxn(comp17)
comp18.v = get.v.fxn(comp18)
comp19.v = get.v.fxn(comp19)
comp20.v = get.v.fxn(comp20)

#combine comp.v vectors
comp.v = cbind(comp1.v,comp2.v,comp3.v,comp4.v,comp5.v,
               comp6.v,comp7.v,comp8.v,comp9.v,comp10.v,
               comp11.v,comp12.v,comp13.v,comp14.v,comp15.v,
               comp16.v,comp17.v,comp18.v,comp19.v,comp20.v)

# SECTION IV
#calculate high and low returns
r.t = c()
comp20.labels = c()
for (i in 1:nrow(comp20)){
  r.t[i] = round((comp20$s[i+1] - comp20$s[i])/comp20$s[i],4)*100
  if (is.na(r.t[i])) {
    comp20.labels[i] = NA
  } else if (r.t[i] >= 0.6) {
    comp20.labels[i] = "HIGH"
  } else {comp20.labels[i] = "LOW"}
}

comp20.labels = comp20.labels[-1:-15]

#display percentage of high and low returns
cat("Percentage of High Returns:",round(length(which(comp20.labels == "HIGH"))/length(comp20.labels),4)*100)
cat("Percentage of Low Returns:",round(length(which(comp20.labels == "LOW"))/length(comp20.labels),4)*100)

# SECTION V
#create correlation matrix and compute eigenvalues and eigenvectors
CORR = cor(comp.v)
CORR.ev = eigen(CORR)
L = CORR.ev$values
W = CORR.ev$vectors
k = c(1:300)

plot(k,L, xlab="# of Eigenvalue", ylab="Eigenvalue")

#verify sum of eigenvalues
cat("Sum of Eigenvalues:", sum(L))

#calculate number of eigenvalues that capture 95% of data variance
cumulative_variance_percent=cumsum(CORR.ev$values)/300
threshold = 0.95
number_components=min(k[(cumulative_variance_percent>threshold)])
plot(k,cumulative_variance_percent, xlab="# of Eigenvalue", ylab="PEV",
     main="Percentage of Explained Variance v. Eigenvalue")
cat(number_components, "components capture 95% of variance.")

redW = W[,1:number_components]
redL = L[1:number_components]

Z = comp.v%*%redW

# SECTION VI
#define train/test proportions
n = dim(Z)[1]
set.seed(123)
idx = sample(1:n,floor(n*0.85))

#create train and test sets
trainset = Z[idx,]
testset = Z[-idx,]

#train truth
trainlabel = comp20.labels[idx]
trainset = as.data.frame(trainset)
trainset = cbind(trainlabel, trainset)

#test truth
testlabel = comp20.labels[-idx]
testset = as.data.frame(testset)
testset = cbind(testlabel, testset)

#rebalance with cloning
#train HIGH v LOW
Htrain = trainset[(trainset[,1]=="HIGH"),]
n = dim(Htrain)[1]
set.seed(123)
Htrain = Htrain[sample(1:n, floor(n*0.4)),]
train.clone = trainset
train.clone = rbind(train.clone, Htrain)

train.clone = na.omit(train.clone)

#test HIGH v LOW
Htest = testset[(testset[,1]=="HIGH"),]
n = dim(Htest)[1]
set.seed(123)
Htest = Htest[sample(1:n, floor(n*0.2)),]
test.clone = testset
test.clone = rbind(test.clone, Htest)

test.clone = na.omit(test.clone)

#check balance
cat("Percent Train High:",round(length(which(train.clone[,1] == "HIGH"))/length(train.clone[,1]),4)*100)
cat("Percent Train Low:",round(length(which(train.clone[,1] == "LOW"))/length(train.clone[,1]),4)*100)

cat("Percent Test High:",round(length(which(test.clone[,1] == "HIGH"))/length(test.clone[,1]),4)*100)
cat("Percent Test Low:",round(length(which(test.clone[,1] == "LOW"))/length(test.clone[,1]),4)*100)

Btrainlabel = as.factor(train.clone[,1])
train.clone = train.clone[,-1]
train.clone = cbind(Btrainlabel, train.clone)

Btestlabel = as.factor(test.clone[,1])
test.clone = test.clone[,-1]
test.clone = cbind(Btestlabel, test.clone)

# SECTION VII
costval = c(.001, .01, .1, .25, .5, .75, 1, 1.5, 2.5, 5)
svm.test.acc = c()
svm.train.acc = c()
svm.ratio = c()
svm.high.acc = c()
svm.low.acc = c()
svm.v.count = c()
svm.perc.support = c()

for (i in costval) {
  svm = svm(Btrainlabel ~ ., data = train.clone, kernel = "linear",
      cost = i, scale = FALSE)
  predtrain = predict(svm, train.clone)
  predtest = predict(svm, test.clone)
  svm.v.count = append(svm.v.count, svm$tot.nSV)
  svm.train.acc = append(svm.train.acc, sum(predtrain == Btrainlabel)/length(Btrainlabel))
  svm.test.acc = append(svm.test.acc, sum(predtest == Btestlabel)/length(Btestlabel))
  svm.high.acc = append(svm.high.acc, sum(Btestlabel == "HIGH" & predtest == "HIGH")/sum(Btestlabel == "HIGH"))
  svm.low.acc = append(svm.low.acc, sum(Btestlabel == "LOW" & predtest == "LOW")/sum(Btestlabel == "LOW"))
}

svm.ratio = svm.test.acc/svm.train.acc
svm.perc.support = svm.v.count/length(Btrainlabel)

svm_stats = data.frame(costval, svm.train.acc, svm.test.acc, svm.ratio, svm.perc.support)

plot(costval, svm.test.acc, "l", xlab = "cost", ylab = "percent", col = "green", main = "Linear SVM")
abline(costval, svm.train.acc, col = "red")
abline(costval, svm.perc.support, col = "blue")

# SECTION VIII
svm.opt = svm(Btrainlabel ~ ., data = train.clone, kernel = "linear",
          cost = 0.25, scale = FALSE)

predtrain.opt = predict(svm.opt, train.clone)
predtest.opt = predict(svm.opt, test.clone)

conf.train.opt = confusionMatrix(predtrain.opt, Btrainlabel)
conf.test.opt = confusionMatrix(predtest.opt, Btestlabel)

conf.train.opt
conf.test.opt

# SECTION IX
hil.data = cbind(comp20.labels, Z)
hil.data = as.data.frame(hil.data)
hil.high = subset(hil.data, hil.data$comp20.labels == "HIGH")
hil.low = subset(hil.data, hil.data$comp20.labels == "LOW")
idx = sample(1:nrow(hil.high),floor(nrow(hil.high)*0.5))
hil.high = hil.high[idx,]
hil.low = hil.low[idx,]
hil.high = hil.high[,-1]
hil.low = hil.low[,-1]
hil.high = na.omit(hil.high)
hil.low = na.omit(hil.low)
hil.dif = matrix(0, nrow = 204, ncol = 212)
for (i in 1:nrow(hil.high)) {
  for (j in 1:ncol(hil.high)) {
    hil.dif[i,j] = as.numeric(hil.high[i,j]) - as.numeric(hil.low[i,j])
  }
}

#gamma range of 0.125-0.5, values from 0 to 5 were tested
gamma = .25
hil.dif = abs(hil.dif)
hil.dif = hil.dif^2
hil.dif = -1*gamma*hil.dif
hil.dif = exp(hil.dif)
hil = (2-(2*hil.dif))^.5

#gamma = .125
svm.test.acc.rad1 = c()
svm.train.acc.rad1 = c()
svm.high.acc.rad1 = c()
svm.low.acc.rad1 = c()
svm.ratio.rad1 = c()
svm.v.count.rad1 = c()
svm.perc.support.rad1 = c()

for (i in costval) {
  svm.rad1 = svm(Btrainlabel ~ ., data = train.clone, kernel = "radial",
            cost = i, scale = FALSE, gamma = .125)
  predtrain.rad1 = predict(svm.rad1, train.clone)
  predtest.rad1 = predict(svm.rad1, test.clone)
  svm.train.acc.rad1 = append(svm.train.acc.rad1, sum(predtrain.rad1 == Btrainlabel)/length(Btrainlabel))
  svm.test.acc.rad1 = append(svm.test.acc.rad1, sum(predtest.rad1 == Btestlabel)/length(Btestlabel))
  svm.high.acc.rad1 = append(svm.high.acc.rad1, sum(Btestlabel == "HIGH" & predtest.rad1 == "HIGH")/sum(Btestlabel == "HIGH"))
  svm.low.acc.rad1 = append(svm.low.acc.rad1, sum(Btestlabel == "LOW" & predtest.rad1 == "LOW")/sum(Btestlabel == "LOW"))
  svm.v.count.rad1 = append(svm.v.count.rad1, svm.rad1$tot.nSV)
}

svm.ratio.rad1 = svm.test.acc.rad1/svm.train.acc.rad1
svm.perc.support.rad1 = svm.v.count.rad1/length(Btrainlabel)

svm_stats.rad1 = data.frame(costval, svm.train.acc.rad1, svm.test.acc.rad1, svm.perc.support.rad1)

#gamma = .25
svm.test.acc.rad2 = c()
svm.train.acc.rad2 = c()
svm.high.acc.rad2 = c()
svm.low.acc.rad2 = c()
svm.ratio.rad2 = c()
svm.v.count.rad2 = c()
svm.perc.support.rad2 = c()

for (i in costval) {
  svm.rad2 = svm(Btrainlabel ~ ., data = train.clone, kernel = "radial",
                 cost = i, scale = FALSE, gamma = .25)
  predtrain.rad2 = predict(svm.rad2, train.clone)
  predtest.rad2 = predict(svm.rad2, test.clone)
  svm.train.acc.rad2 = append(svm.train.acc.rad2, sum(predtrain.rad2 == Btrainlabel)/length(Btrainlabel))
  svm.test.acc.rad2 = append(svm.test.acc.rad2, sum(predtest.rad2 == Btestlabel)/length(Btestlabel))
  svm.high.acc.rad2 = append(svm.high.acc.rad2, sum(Btestlabel == "HIGH" & predtest.rad2 == "HIGH")/sum(Btestlabel == "HIGH"))
  svm.low.acc.rad2 = append(svm.low.acc.rad2, sum(Btestlabel == "LOW" & predtest.rad2 == "LOW")/sum(Btestlabel == "LOW"))
  svm.v.count.rad2 = append(svm.v.count.rad2, svm.rad2$tot.nSV)
}

svm.ratio.rad2 = svm.test.acc.rad2/svm.train.acc.rad2
svm.perc.support.rad2 = svm.v.count.rad2/length(Btrainlabel)

svm_stats.rad2 = data.frame(costval, svm.train.acc.rad2, svm.test.acc.rad2, svm.perc.support.rad2)

#gamma = .375
svm.test.acc.rad3 = c()
svm.train.acc.rad3 = c()
svm.high.acc.rad3 = c()
svm.low.acc.rad3 = c()
svm.ratio.rad3 = c()
svm.v.count.rad3 = c()
svm.perc.support.rad3 = c()

for (i in costval) {
  svm.rad3 = svm(Btrainlabel ~ ., data = train.clone, kernel = "radial",
                 cost = i, scale = FALSE, gamma = .375)
  predtrain.rad3 = predict(svm.rad3, train.clone)
  predtest.rad3 = predict(svm.rad3, test.clone)
  svm.train.acc.rad3 = append(svm.train.acc.rad3, sum(predtrain.rad3 == Btrainlabel)/length(Btrainlabel))
  svm.test.acc.rad3 = append(svm.test.acc.rad3, sum(predtest.rad3 == Btestlabel)/length(Btestlabel))
  svm.high.acc.rad3 = append(svm.high.acc.rad3, sum(Btestlabel == "HIGH" & predtest.rad3 == "HIGH")/sum(Btestlabel == "HIGH"))
  svm.low.acc.rad3 = append(svm.low.acc.rad3, sum(Btestlabel == "LOW" & predtest.rad3 == "LOW")/sum(Btestlabel == "LOW"))
  svm.v.count.rad3 = append(svm.v.count.rad3, svm.rad3$tot.nSV)
}

svm.ratio.rad3 = svm.test.acc.rad3/svm.train.acc.rad3
svm.perc.support.rad3 = svm.v.count.rad3/length(Btrainlabel)

svm_stats.rad3 = data.frame(costval, svm.train.acc.rad3, svm.test.acc.rad3, svm.perc.support.rad3)

#gamma = .5
svm.test.acc.rad4 = c()
svm.train.acc.rad4 = c()
svm.high.acc.rad4 = c()
svm.low.acc.rad4 = c()
svm.ratio.rad4 = c()
svm.v.count.rad4 = c()
svm.perc.support.rad4 = c()

for (i in costval) {
  svm.rad4 = svm(Btrainlabel ~ ., data = train.clone, kernel = "radial",
                 cost = i, scale = FALSE, gamma = .5)
  predtrain.rad4 = predict(svm.rad4, train.clone)
  predtest.rad4 = predict(svm.rad4, test.clone)
  svm.train.acc.rad4 = append(svm.train.acc.rad4, sum(predtrain.rad4 == Btrainlabel)/length(Btrainlabel))
  svm.test.acc.rad4 = append(svm.test.acc.rad4, sum(predtest.rad4 == Btestlabel)/length(Btestlabel))
  svm.high.acc.rad4 = append(svm.high.acc.rad4, sum(Btestlabel == "HIGH" & predtest.rad4 == "HIGH")/sum(Btestlabel == "HIGH"))
  svm.low.acc.rad4 = append(svm.low.acc.rad4, sum(Btestlabel == "LOW" & predtest.rad4 == "LOW")/sum(Btestlabel == "LOW"))
  svm.v.count.rad4 = append(svm.v.count.rad4, svm.rad4$tot.nSV)
}

svm.ratio.rad4 = svm.test.acc.rad4/svm.train.acc.rad4
svm.perc.support.rad4 = svm.v.count.rad4/length(Btrainlabel)

svm_stats.rad4 = data.frame(costval, svm.train.acc.rad4, svm.test.acc.rad4, svm.perc.support.rad4)

# SECTION X
color.class = ifelse(comp20.labels == "HIGH", "red", "blue")

#plot closing cost v day
plot(comp20$t[1:300], comp20$s[1:300], xlab = "day", ylab = "closing price", col = color.class[1:300], main = "Closing Price, Day 1 to 300")
plot(comp20$t[301:600], comp20$s[301:600], xlab = "day", ylab = "closing price", col = color.class[301:600], main = "Closing Price, Day 301 to 600")
plot(comp20$t[601:900], comp20$s[601:900], xlab = "day", ylab = "closing price", col = color.class[601:900], main = "Closing Price, Day 601 to 900")
plot(comp20$t[901:1243], comp20$s[901:1243], xlab = "day", ylab = "closing price", col = color.class[901:1243], main = "Closing Price, Day 901 to 1243")

#plot percent change v day
plot(comp20$t[1:300], comp20$y[1:300], xlab = "day", ylab = "percent change", col = color.class[1:300], main = "Percent Change, Day 1 to 300")
plot(comp20$t[301:600], comp20$y[301:600], xlab = "day", ylab = "percent change", col = color.class[301:600], main = "Percent Change, Day 301 to 600")
plot(comp20$t[601:900], comp20$y[601:900], xlab = "day", ylab = "percent change", col = color.class[601:900], main = "Percent Change, Day 601 to 900")
plot(comp20$t[901:1243], comp20$y[901:1243], xlab = "day", ylab = "percent change", col = color.class[901:1243], main = "Percent Change, Day 901 to 1243")