library(igraph)
library(dplyr)

rm(list = ls(all = TRUE))
options(stringsAsFactors = F)

addWeightToAdjMatrix <- function(ii) {
    i <- as.character(ii[["x"]])
    j <- as.character(ii[["y"]])
    adj.matrix[i, j] <<- adj.matrix[i, j] + 1
}

addProductsToMatrix <- function(productIds) {
    product.ids <- as.character(unlist(strsplit(productIds, ",")))
    product.ids <- merge(product.ids, product.ids)
    apply(product.ids, 1, addWeightToAdjMatrix)
}

getGraphFromOrdersAdjm <- function(data.orders) {
    products <- data.frame(ProductId = unique(data.orders$ProductId))
    data.orders <- merge(data.orders, products, by = "ProductId")
    
    adj.matrix <<- as.data.frame(matrix(nr = nrow(products), nc = nrow(products)))
    adj.matrix[is.na(adj.matrix)] <<- 0
    colnames(adj.matrix) <<- products$ProductId
    rownames(adj.matrix) <<- products$ProductId

    a <- data.orders %>% group_by(OrderId) %>% summarize(n = paste(ProductId, collapse = ","))
    lapply(a$n, addProductsToMatrix)
    g <- graph_from_adjacency_matrix(as.matrix(adj.matrix), mode = "undirected", weighted = TRUE)
    g <- simplify(g)
    V(g)$name <- products$ProductId

    return(g)
}

### example data - 3 orders with 4 products
data.orders <- data.frame(OrderId = character(), ProductId = character())
data.orders[1,] <- c("Order.1", "Product.1")
data.orders[2,] <- c("Order.1", "Product.2")
data.orders[3,] <- c("Order.2", "Product.1")
data.orders[4,] <- c("Order.2", "Product.3")
data.orders[5,] <- c("Order.2", "Product.4")
data.orders[6,] <- c("Order.3", "Product.1")
data.orders[7,] <- c("Order.3", "Product.2")

### get products graph
g <- getGraphFromOrdersAdjm(data.orders)
### visualize graph
tkplot(g, vertex.color = "light blue")

### test converting graph to adjacency matrix
adjm <- as.matrix(as_adjacency_matrix(g, attr = "weight"))
