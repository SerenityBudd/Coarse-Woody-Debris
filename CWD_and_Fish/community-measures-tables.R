source("CWD_and_Fish/all_fish_t-test.R")
source("CWD_and_Fish/fish_t-test_4.R")
source("CWD_and_Fish/fish_t-test_8.R")
source("CWD_and_Fish/fish_t-test_13.R")
source("CWD_and_Fish/div_pool_4.R")
source("CWD_and_Fish/div_pool_8.R")
source("CWD_and_Fish/div_pool_13.R")
source("CWD_and_Fish/div_all_pools.R")


richnesstable <-  cbind(
rbind(cbind(rich4stats[1,], rich4stats[2,]),
      cbind(rich8stats[1,], rich8stats[2,]),
      cbind(rich13stats[1,], rich13stats[2,]),
      cbind(richstats[1,], richstats[2,]))[,-c(1,5)],   

rbind(c(t.rich4$conf.int, t.rich4$statistic, t.rich4$parameter, t.rich4$p.value),
      c(t.rich8$conf.int, t.rich8$statistic, t.rich8$parameter, t.rich8$p.value),
      c(t.rich13$conf.int, t.rich13$statistic, t.rich13$parameter, t.rich13$p.value),
      c(t.rich$conf.int, t.rich$statistic, t.rich$parameter, t.rich$p.value)))

#write.csv(richnesstable, "data/richnesstable.csv")
 
abundancetable <-  cbind(
  rbind(cbind(abund4stats[1,], abund4stats[2,]),
        cbind(abund8stats[1,], abund8stats[2,]),
        cbind(abund13stats[1,], abund13stats[2,]),
        cbind(abundstats[1,], abundstats[2,]))[,-c(1,5)],   
  
  rbind(c(t.abund4$conf.int, t.abund4$statistic, t.abund4$parameter, t.abund4$p.value),
        c(t.abund8$conf.int, t.abund8$statistic, t.abund8$parameter, t.abund8$p.value),
        c(t.abund13$conf.int, t.abund13$statistic, t.abund13$parameter, t.abund13$p.value),
        c(t.abund$conf.int, t.abund$statistic, t.abund$parameter, t.abund$p.value)))

#write.csv(abundancetable, "data/abundancetable.csv")

diversitytable <-  cbind(
  rbind(cbind(div4stats[1,], div4stats[2,]),
        cbind(div8stats[1,], div8stats[2,]),
        cbind(div13stats[1,], div13stats[2,]),
        cbind(divstats[1,], divstats[2,]))[,-c(1,5)],   
  
  rbind(c(t.div4$conf.int, t.div4$statistic, t.div4$parameter, t.div4$p.value),
        c(t.div8$conf.int, t.div8$statistic, t.div8$parameter, t.div8$p.value),
        c(t.div13$conf.int, t.div13$statistic, t.div13$parameter, t.div13$p.value),
        c(t.div$conf.int, t.div$statistic, t.div$parameter, t.div$p.value)))

#write.csv(diversitytable, "data/diversitytable.csv")
