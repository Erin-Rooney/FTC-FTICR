#xrd diffractogram patterns
# EC Rooney
# Feb 9 2022

source("code/0-method-packages.R")
library(rxylib)
library(magrittr)
library(utils)


H12433data = read_xyData("processed/H1-24-33.xy")
H13350data = read_xyData("processed/H1-33-50.xy")
H15060data = read_xyData("processed/H1-50-60.xy")
H22834data = read_xyData("processed/H2-28-34.xy")
H23447data = read_xyData("processed/H2-34-47.xy")
H24768data = read_xyData("processed/H2-47-68.xy")
H34050data = read_xyData("processed/H3-40-50.xy")
H33038data = read_xyData("processed/H3-30-38.xy")
T14060data = read_xyData("processed/T1-40-60.xy")
T16067data = read_xyData("processed/T1-60-67.xy")
T22838data = read_xyData("processed/T2-28-38.xy")
T23844data = read_xyData("processed/T2-38-44.xy")
T24458data = read_xyData("processed/T2-44-58.xy")
T33541data = read_xyData("processed/T3-35-41.xy")
T34150data = read_xyData("processed/T3-41-50.xy")
T35058data = read_xyData("processed/T3-50-58.xy")

"H1-24-33dataframe" = data.frame(H12433data[["dataset"]][[1]][["data_block"]])
"H1-33-50dataframe" = data.frame(H13350data[["dataset"]][[1]][["data_block"]])
"H1-50-60dataframe" = data.frame(H15060data[["dataset"]][[1]][["data_block"]])
"H2-28-34dataframe" = data.frame(H22834data[["dataset"]][[1]][["data_block"]])
"H2-34-47dataframe" = data.frame(H23447data[["dataset"]][[1]][["data_block"]])
"H2-47-68dataframe" = data.frame(H24768data[["dataset"]][[1]][["data_block"]])
"H3-40-50dataframe" = data.frame(H34050data[["dataset"]][[1]][["data_block"]])
"H3-30-38dataframe" = data.frame(H33038data[["dataset"]][[1]][["data_block"]])
"T1-40-60dataframe" = data.frame(T14060data[["dataset"]][[1]][["data_block"]])
"T1-60-67dataframe" = data.frame(T16067data[["dataset"]][[1]][["data_block"]])
"T2-28-38dataframe" = data.frame(T22838data[["dataset"]][[1]][["data_block"]])
"T2-38-44dataframe" = data.frame(T23844data[["dataset"]][[1]][["data_block"]])
"T2-44-58dataframe" = data.frame(T24458data[["dataset"]][[1]][["data_block"]])
"T3-35-41dataframe" = data.frame(T33541data[["dataset"]][[1]][["data_block"]])
"T3-41-50dataframe" = data.frame(T34150data[["dataset"]][[1]][["data_block"]])
"T3-50-58dataframe" = data.frame(T35058data[["dataset"]][[1]][["data_block"]])


