
### construct basic package dataset

corpus <- usnews[500:1499, 1:5]
write.csv2(corpus, file = "inst/extdata/corpus.csv", row.names = FALSE)

