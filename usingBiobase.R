m <- read.csv("microarray.csv")

library(Biobase)

cols.with.data <- grep('AVG_Signal', colnames(m))

data.matrix <- as.matrix(m[, cols.with.data])
rownames(data.matrix) <- m$PROBE_ID
colnames(data.matrix) <- sub(".AVG_Signal", "", colnames(data.matrix))

features <- AnnotatedDataFrame(m[,c("SYMBOL", "ILMN_GENE", "CHROMOSOME", "DEFINITION",
                                    "SYNONYMS", "REFSEQ_ID", "UNIGENE_ID", "ENTREZ_GENE_ID", "GI")])
rownames(features) <- m$PROBE_ID

es <- ExpressionSet(assayData = data.matrix, featureData=features)
MAplot(es)

library(affyPLM)
es <- normalize(es)

library(limma)

strsplit2(grep('AVG',colnames(m),value = T),"\\.")[,1]

exps <- as.factor(strsplit2(grep('AVG',colnames(m),value = T),"\\.")[,1])
design <- model.matrix(~exps+0)
contrasts <- makeContrasts(X32GyHeLa-KontrolHeLa, X16GyC4I-KontrolC4I, X32GyHeLa-X16GyC4I, levels=design)
fit <- lmFit(es, design)
fit2 <- contrasts.fit(fit, contrasts)
fit2 <- eBayes(fit2)
topTable(fit2, coef=1, adjust="BH")
results <- decideTests(fit2)
vennDiagram(results)
MAplot(es, ref=7)
