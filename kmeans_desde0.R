library(ggplot2)

# permitirá repetir el proceso de cálculo de centroides y asignación a clúster en la función principal

calculo_con_centroides <- function(datos, indices, k, input) {
    
    # obtención de centroides
    centroides <- matrix(0, length(indices), ncol(datos))
    for (i in 1:length(indices)) {
        centroides[i, ] <- colMeans(datos[input == i, ])
    }
    colnames(centroides) <- colnames(datos)
    
    # cálculo de nuevas distancias
    nuevos_datos <- rbind(datos, centroides)
    
    distancias <- dist(nuevos_datos, method = 'euclidean')
    distancias <- as.matrix(distancias)
    
    # nueva asignación a clústers
    indices_centroides <- tail(1:nrow(distancias), k)
    distancias <- distancias[, indices_centroides]
    
    resultado <- rep(0, nrow(distancias))
    for (i in 1:nrow(distancias)) {
        resultado[i] <- which(distancias[i,] == min(distancias[i, ]))
    }
    
    # resultado
    names(resultado) <- rownames(datos)
    resultado <- resultado[1:nrow(datos)]
    return(resultado)
}

# función kmeans

kmeans <- function(datos, k, grafica = F) {
    
    #elegimos al azar k casos
    k_casos <- sample(1:nrow(datos), k)
    
    #calculamos la distancia euclidea entre estos y el resto
    distancias <- dist(datos, method = 'euclidean')
    distancias <- as.matrix(distancias)
    distancias <- distancias[, k_casos]
    
    # cálculo de pertenencia a grupo
    res <- rep(0, nrow(distancias))
    for (i in 1:nrow(distancias)) {
        res[i] <- which(distancias[i, ] == min(distancias[i, ]))
    }
    
    names(res) <- rownames(datos)
    
    # repetición del proceso hasta convergencia
    x <- T
    tries <- 0
    
    while (x == T) {
        
        old_res <- res
        res <- calculo_con_centroides(datos, indices = k_casos, k, input = res)
        
        if (identical(res, old_res)) {tries <- tries + 1}
        if (tries == 4) {x <- F}
        
    }
    
    # resultados
    
    if (grafica == T) {
        
        if (ncol(datos) == 2) {
            
            nombres_var <- colnames(datos)
            
            grafico <- ggplot(data = datos, 
                              aes(get(nombres_var[1]), get(nombres_var[2]),
                              colour = as.factor(res))) + geom_jitter()
            
            print(grafico + xlab(nombres_var[1]) + ylab(nombres_var[2]) + labs(colour = 'Cluster'))
            
        } else {
            
            warning('Solo se puede mostrar gráfica si el número de variables es 2')
        }
        
    } 
    
    return(res)
    
}

# pruebas

kmeans(iris[, 3:4], k = 3, grafica = T)

kmeans(USArrests[, 1:4], k = 4, grafica = F)


