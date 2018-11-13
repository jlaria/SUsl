#' Visualize the kNN classifiers
#'
#' Plots the kNN decision boundary for 2D problems.
#'
#' @param train matrix or data.frame with 2 columns
#' @param cl vector of training classes
#' @param k number of neighbors
#' @param validate matrix or data.frame with 2 columns
#' @param cl_validate vector of classes
#' @param prob_size whether to adjust the size of points depending on the estimated probabilities
#' @return a ggplot object
knn_viz = function(train, cl, k = 7, validate=NULL, cl_validate=NULL, prob_size = T){

  test <- expand.grid(x=seq(min(c(train[,1], validate[,1]))-0.1,
                            max(c(train[,1], validate[,1]))+0.1,
                            length.out = 100),
                      y=seq(min(c(train[,2], validate[,2]))-0.1,
                            max(c(train[,2], validate[,2]))+0.1,
                            length.out = 100))

  classif <- class::knn(train, test, cl, k, prob=TRUE)
  prob <- attr(classif, "prob")

  data.contour = data.frame(
    x = rep(test$x, length(levels(cl))),
    y = rep(test$y, length(levels(cl))),
    group = rep(levels(cl), each = nrow(test)),
    class = rep(classif, length(levels(cl)))
  )
  data.contour$h = (data.contour$group == data.contour$class) + 0

  p = ggplot2::ggplot()+ ggplot2::scale_size(range=c(0.01, 1))
  if(!prob_size){
    p = p + ggplot2::geom_point(ggplot2::aes(x = test[,1], y = test[,2], color = classif), size = 0.01)
  }else{
    p = p + ggplot2::geom_point(ggplot2::aes(x = test[,1], y = test[,2], color = classif, size = prob))
  }
  p = p+ ggplot2::geom_contour(ggplot2::aes(x=x, y=y, z=h, group=group, color=group),
                   bins=1,
                   data=data.contour)

  names.x = ifelse(is.null(colnames(train)), "x", colnames(train)[1])
  names.y = ifelse(is.null(colnames(train)), "y", colnames(train)[2])

  if(!is.null(validate)){
    p = p +
      ggplot2::geom_point(ggplot2::aes(x = validate[,1], y = validate[,2], color = cl_validate), size = 2)+
      ggplot2::geom_point(ggplot2::aes(x = validate[,1], y = validate[,2]), size = 2, shape = 1)+
      ggplot2::labs(x = names.x,
           y = names.y,
           color = "Class",
           size = "Prob.",
           title = "k-NN decision boundary",
           subtitle = paste0("k = ", k, " (testing set)"))
  }else{
    p = p +
      ggplot2::geom_point(ggplot2::aes(x = train[,1], y = train[,2], color = cl), size = 2)+
      ggplot2::geom_point(ggplot2::aes(x = train[,1], y = train[,2]), size = 2, shape = 1)+
      ggplot2::labs(x = names.x,
           y = names.y,
           color = "Class",
           size = "Prob.",
           title = "k-NN decision boundary",
           subtitle = paste0("k = ", k, " (training set)"))
  }
  return(p)
}

# train <- rbind(iris3[1:25,1:2,1],
#                iris3[1:25,1:2,2],
#                iris3[1:25,1:2,3])
# train = data.frame(train)
# colnames(train) = c("x", "y")
# cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
#
# knn_viz(train, cl, k = 20)
#
# pc = prcomp(iris[,-5], scale = T)
# train = data.frame(
#   x = pc$x[,1],
#   y = pc$x[,2]
# )
# cl = iris$Species
#
# p = knn_viz(train, cl, k = 12)
# p + labs(x = "PC1", y = "PC2")
