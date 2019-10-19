data(wine)
X.quanti <- PCAmixdata::splitmix(wine)$X.quanti
X.quali <- PCAmixdata::splitmix(wine)$X.quali
tree <- hclustvar(t(X.quanti))
plot(tree)
tree <- as.phylo(tree)
trait <- X.quanti$Fruity
names(trait) <- tree$tip.label
plot(contMap(tree,trait))
