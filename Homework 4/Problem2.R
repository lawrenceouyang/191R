EnglandTeams = read.csv("England_2009_2010_TeamNames.csv", header = FALSE);
EnglandTeamScores = read.csv("England_2009_2010_Scores.csv", header = FALSE);

n = dim(EnglandTeamScores)[1];
S_match = (matrix(20, nrow=n, ncol=n) + as.matrix(EnglandTeamScores) %*% as.matrix(t(EnglandTeamScores)))/2;

D = diag(rowSums(S_match));

Laplacian = D - S_match;

E = eigen(Laplacian);
Ranking = E$vectors[,n-1];
Ranked = data.frame(Ranking, row.names = EnglandTeams[,1]);
Ranked[order(-Ranked$Ranking), , drop = FALSE];