#number of frames or plots
frames <- 50

# function for creating file name with leading zeros
# makes it easier to process them sequentially
rename <- function(x){
  if (x < 10) {
    return(name <- paste('000',i,'plot.png',sep=''))
  }
  if (x < 100 && i >= 10) {
    return(name <- paste('00',i,'plot.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste('0', i,'plot.png', sep=''))
  }
}

#loop through plots
for(i in 1:frames){
  name <- rename(i)
  
  #saves the plot as a .png file in the working directory
  png(name)
  sd <- 10
  n  <- 10000
  factor <- i * 2
  m  <- 50 + factor
  x  <- rnorm(n, m, sd)
  hist(x,
       xlim=c(0,200),
       ylim=c(0,2000),
       main = paste('Histogram of rnorm() n = ', n, ' mean = ', m, ' sd = ', sd),
  )
  dev.off()
}

#run ImageMagick
my_command<-'cd C:\\'
system(my_command)
my_command <- 'convert -delay 3 *.png animation.gif'
system(my_command)
timepoint<-as.Date(c('2014-03-05','2014-04-10','2014-04-21','2014-08-02',
                     '2014-09-27','2014-10-20','2014-11-1'))
events<-c('人大二次会议','宜信8亿坏账风波','银监会"四条红线"','杨晓军上海会议','王岩岫十大原则'
          ,'P2P归银监会管','P2P倒闭潮')