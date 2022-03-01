library(copent)
library(ggplot2)

adult = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",header = FALSE, col.names = c("age","workclass","fnlwgt","edu","edunum","marital","occupation","relationship","race","sex","capital-gain","capital-loss","hpw","native","income"),sep = ",", strip.white = TRUE)

income = 0
income[which(adult$income=="<=50K")] = 1
income[which(adult$income==">50K")] = 2
sex = 0
sex[which(adult$sex=="Male")] = 1
sex[which(adult$sex=="Female")] = 2

x11(width = 10, height = 5)
ggplot(adult, aes(x = edunum, color = sex)) + geom_density(alpha = 0.6) + xlab("Education")

k = 50
n = 500 # number of the used samples
ci1 = 0 # ci btw income, education given sex
ci2 = 0 # ci btw income, sex given education
for (i in 1:k){
  i1 = income[1:n] + 0.000001 * runif(n)
  s1 = sex[1:n] + 0.000001 * runif(n)
  edu1 = adult$edunum[1:n] + 0.000001 * runif(n)
  ci1 = ci1 + ci(i1,edu1,s1)
  ci2 = ci2 + ci(i1,s1,edu1)
}
ci = c(ci1,ci2)
ci = ci / k

x11()
names(ci) = c("Sex2Edu","Edu2Sex")
min1 = min(ci)
if (min1 > 0) {min1 = 0.003}
xci1 = barplot(ci, col = "grey", border = "grey", ylab = "CI", ylim = c(min1-0.003, max(ci)+0.003))
text(cex = 1, x = xci1, y = ci+ sign(ci) * 0.0015, labels = round(ci,digits = 4), col = "red")
