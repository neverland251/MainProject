conzone_sep[1]

highway_name <- c("경부","서해안","호남","중부내륙","영동")
death_2017 <- c(37,23,23,25,24)
death_2016 <- c(53,28,14,29,15)
death_2015 <- c(58,14,20,20,17)
death_2014 <- c(51,25,27,26,13)
death_2013 <- c(58,33,20,21,24)
death_2012 <- c(70,25,14,23,34)
death_data <- t(data.frame(death_2017,death_2016,death_2015,death_2014,death_2013,death_2012))
colnames(death_data) <- highway_name

