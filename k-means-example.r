require("class")
require("datasets")
data("iris")

rnum <- sample(rep(1:150))
iris <- (iris[rnum, ])

kmeans1 <- function(dados, k) {
    euc_dist <- function(x1, x2) sum((x1 - x2)^2)

    rotulo <- 1:k

    rownames(dados)[nrow(dados)] <- 1

    for (i in 1:nrow(dados)) {
        rownames(dados)[i] <- sample(rotulo, 1)
    }

    centroids <- colMeans(dados[rownames(dados) == 1, ])

    for (j in 2:k) {
        centroids <- rbind(centroids, colMeans(dados[rownames(dados) == j, ]))
    }

    rownames(centroids) <- 1:k

    for (i in 1:nrow(dados)) {
        distancias <- NULL

        for (j in 1:k) {
            distancias[j] <- euc_dist(dados[i, ], centroids[j, ])
        }

        names(distancias) <- 1:k

        rownames(dados)[i] <- as.numeric(
            names(distancias[distancias == min(distancias)])
        )

        centroids <- colMeans(dados[rownames(dados) == 1, ])

        for (z in 2:k) {
            centroids <- rbind(
                centroids,
                colMeans(dados[rownames(dados) == z, ])
            )
        }
    }

    return(list(
        centroides = centroids,
        grupo1 = dados[rownames(dados) == 1, ],
        grupo2 = dados[rownames(dados) == 2, ],
        grupo3 = dados[rownames(dados) == 3, ],
        cluster = as.numeric(rownames(dados))
    ))
}

ans <- kmeans1(iris, 2)

print(ans)