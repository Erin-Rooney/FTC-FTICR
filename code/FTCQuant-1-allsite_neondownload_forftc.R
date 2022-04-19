# A Possinger, E Rooney
# 7 23 2021
# load all sites neon temp data for ftc transect manuscript

# load packages

source("code/0-method-packages.R")


#load NEON data files

fall1 = loadByProduct(dpID="DP1.00041.001",
                      site=c("TOOL", "HEAL", "BONA", "BARR"),
                      package="basic",
                      startdate="2017-09",
                      enddate="2017-11",
                      nCores=5,
                      avg=30)

fall2 = loadByProduct(dpID="DP1.00041.001",
                      site=c("TOOL", "HEAL", "BONA", "BARR"),
                      package="basic",
                      startdate="2018-09",
                      enddate="2018-11",
                      nCores=5,
                      avg=30)

fall3 = loadByProduct(dpID="DP1.00041.001",
                      site=c("TOOL", "HEAL", "BONA", "BARR"),
                      package="basic",
                      startdate="2019-09",
                      enddate="2019-11",
                      nCores=5,
                      avg=30)

winter1 = loadByProduct(dpID="DP1.00041.001",
                        site=c("TOOL", "HEAL", "BONA", "BARR"),
                        package="basic",
                        startdate="2017-12",
                        enddate="2018-02",
                        nCores=5,
                        avg=30)

winter2 = loadByProduct(dpID="DP1.00041.001",
                        site=c("TOOL", "HEAL", "BONA", "BARR"),
                        package="basic",
                        startdate="2018-12",
                        enddate="2019-02",
                        nCores=5,
                        avg=30)

winter3 = loadByProduct(dpID="DP1.00041.001",
                        site=c("TOOL", "HEAL", "BONA", "BARR"),
                        package="basic",
                        startdate="2019-12",
                        enddate="2020-02",
                        nCores=5,
                        avg=30)

spring1 = loadByProduct(dpID="DP1.00041.001",
                        site=c("TOOL", "HEAL", "BONA", "BARR"),
                        package="basic",
                        startdate="2018-03",
                        enddate="2018-05",
                        nCores=5,
                        avg=30)

spring2 = loadByProduct(dpID="DP1.00041.001",
                        site=c("TOOL", "HEAL", "BONA", "BARR"),
                        package="basic",
                        startdate="2019-03",
                        enddate="2019-05",
                        nCores=5,
                        avg=30)

spring3 = loadByProduct(dpID="DP1.00041.001",
                        site=c("TOOL", "HEAL", "BONA", "BARR"),
                        package="basic",
                        startdate="2020-03",
                        enddate="2020-05",
                        nCores=5,
                        avg=30)

summer1 = loadByProduct(dpID="DP1.00041.001",
                        site=c("TOOL", "HEAL", "BONA", "BARR"),
                        package="basic",
                        startdate="2018-06",
                        enddate="2018-08",
                        nCores=5,
                        avg=30)

summer2 = loadByProduct(dpID="DP1.00041.001",
                        site=c("TOOL", "HEAL", "BONA", "BARR"),
                        package="basic",
                        startdate="2019-06",
                        enddate="2019-08",
                        nCores=5,
                        avg=30)

summer3 = loadByProduct(dpID="DP1.00041.001",
                        site=c("TOOL", "HEAL", "BONA", "BARR"),
                        package="basic",
                        startdate="2020-06",
                        enddate="2020-08",
                        nCores=5,
                        avg=30)

save(fall1, file="raw/fall1.RData")
save(fall2, file="raw/fall2.RData")
save(fall3, file="raw/fall3.RData")
save(winter1, file="raw/winter1.RData")
save(winter2, file="raw/winter2.RData")
save(winter3, file="raw/winter3.RData")
save(spring1, file="raw/spring1.RData")
save(spring2, file="raw/spring2.RData")
save(spring3, file="raw/spring3.RData")
save(summer1, file="raw/summer1.RData")
save(summer2, file="raw/summer2.RData")
save(summer3, file="raw/summer3.RData")

