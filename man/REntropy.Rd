\name{REntropy}
\alias{REntropy}
\title{REntropy}
\usage{
REntropy(df)
}
\description{
This package is design to be use mith lcmm package. The function compute relative entropy of a classification proposed by Celeux & Soromenho (1996) as following :
$\deqn{E N(\widehat{\tau})=-\sum_{i=1}^N \sum_{k=1}^K \widehat{\tau}_{i k} \log \hat{\tau}_{i k}}$
}

\arguments{
\item{postclassprob}{ Data frame containing posterior classification probabilities. The package extracte automatically colums with "prob" or "Prob" or "PROB" in there names. }
}
\references{
Celeux, G., & Soromenho, G. (1996). An entropy criterion for assessing the number of clusters in a mixture model. Journal of Classification, 13, 195–212.
}
