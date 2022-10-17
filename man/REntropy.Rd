\name{REntropy}
\alias{REntropy}
\title{REntropy}
\usage{
REntropy(postclassprob)
}
\description{
The function compute relative entropy of a classification proposed by Celeux & Soromenho (1996) as following :
\deqn{E N(\widehat{\tau})=-\sum_{i=1}^N \sum_{k=1}^K \widehat{\tau}_{i k} \log \hat{\tau}_{i k}}

This package were design to be use with lcmm package (Proust-Lima et al., 2015).
}

\arguments{
\item{postclassprob}{ Data frame containing posterior classification probabilities. The package extracte automatically colums with "prob" or "Prob" or "PROB" in there names.}
}
\examples{

df0<-data.frame("Prob1" = c(0.5,0.5,0.5,0.5),
                "PROB2" = c(0.5,0.5,0.5,0.5))

REntropy(df0)


df1<-data.frame("Prob1" = c(1,1,0,0),
                "PROB2" = c(0,0,1,1))

REntropy(df1)
}
\references{
Celeux, G., & Soromenho, G. (1996). An entropy criterion for assessing the number of clusters in a mixture model. Journal of Classification, 13, 195–212.

Proust-Lima, C., Philipps, V., & Liquet, B. (2015). Estimation of Extended Mixed Models Using Latent Classes and Latent Processes : The R Package lcmm. Journal of Statistical Software, 78. https://doi.org/10.18637/jss.v078.i02
}


\author{
Simon Pineau
}
