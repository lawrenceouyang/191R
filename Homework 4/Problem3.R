# Problem 3a:
jpeg('pointCloud.jpeg', width = 480, height = 480, units = "px")
plot(A[1:250,1], A[1:250,2], xlim=c(-7,7), ylim=c(-7,7), pch=16, col="red", xlab = 'X', ylab = 'Y', main = "Point Cloud");
par(new=T);
plot(A[251:500,1], A[251:500,2], xlim=c(-7,7), ylim=c(-7,7), pch=17, col="blue", xlab = 'X', ylab = 'Y', main = "Point Cloud");
par(new=F);
dev.off()

# Problem 3b:
myPCA = prcomp(A, center = TRUE, scale. = TRUE, retx = TRUE);
print(summary(myPCA));
jpeg('PCApoints.jpeg', width = 480, height = 480, units = "px")
plot(myPCA$x[1:250,1], myPCA$x[1:250,2], xlim=c(-2,2), ylim=c(-2,2), pch=16, col="red", xlab = 'PC1', ylab = 'PC2', main = "PCA Points");
par(new=T);
plot(myPCA$x[251:500,1], myPCA$x[251:500,2], xlim=c(-2,2), ylim=c(-2,2), pch=17, col="blue", xlab = 'PC1', ylab = 'PC2', main = "PCA Points");
par(new=F);
dev.off()

# Problem 3c:
DIST = as.matrix(dist(A, method = "euclidean", diag = FALSE, upper = TRUE, p = 2));
W = exp(-DIST^2/1); #(.75,1)
D = diag(rowSums(W));
invD = solve(D);
L = invD %*% W;

# Obtain square root of D
eigD = eigen(D);
sqrtD = eigD$vectors %*% diag(sqrt(eigD$values)) %*% t(eigD$vectors);

S = sqrtD %*% L %*% solve(sqrtD);
eigS = eigen(S);

jpeg('DiffusionMap1.jpeg', width = 480, height = 480, units = "px")
plot(eigS$vectors[1:250,2], eigS$vectors[1:250,3], xlim=c(-0.1, 0.1), ylim=c(-0.05, 0.15), pch=16, col="red", xlab = 'X', ylab = 'Y', main = "Diffusion Map 1");
par(new=T);
plot(eigS$vectors[251:500,2], eigS$vectors[251:500,3], xlim=c(-0.1, 0.1), ylim=c(-0.05, 0.15), pch=17, col="blue", xlab = 'X', ylab = 'Y', main = "Diffusion Map 1");
par(new=F);
dev.off()


