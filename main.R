library(shiny)
runApp('S:/Tools/Excel Planner/Modeling/Scotts Prediction',host="0.0.0.0",port=5050)





#install.packages('rsconnect')
library(rsconnect)
library(RcppArmadillo)
rsconnect::setAccountInfo(name='zihaozhangap', token='CCC977CDDB18ED55C68DBA2E664C2F6E', secret='/PqGH9VGjl0pqvOh6uOIHmQl3JB+lzGluzAv1Gez')
rsconnect::deployApp("S:/Tools/Excel Planner/Modeling/Scotts Prediction")


