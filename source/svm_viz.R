#' Visualize the SVM classifiers
#'
#' Plots the SVM decision boundary for 2D problems.
#'
#' @param train matrix or data.frame with 2 columns
#' @param cl vector of training classes
#' @param kernel the kernel. See \code{?e1071::svm}
#' @return a ggplot object
svm_viz = function(train, cl, npoints = 100, kernel = "linear", ...){

  test <- expand.grid(x=seq(min(train[,1]-0.1), max(train[,1]+0.1),
                            length.out = npoints),
                      y=seq(min(train[,2]-0.1), max(train[,2]+0.1),
                            length.out = npoints))
  classifier=e1071::svm(train, cl, kernel=kernel, ...)
  classif = predict(classifier, newdata=test)

  data.contour = data.frame(
    x = rep(test$x, length(levels(cl))),
    y = rep(test$y, length(levels(cl))),
    group = rep(levels(cl), each = nrow(test)),
    class = rep(classif, length(levels(cl)))
  )
  data.contour$h = (data.contour$group == data.contour$class) + 0

  ggplot2::ggplot()+
    ggplot2::geom_point(aes(x = test$x, y = test$y, color = classif), size=0.01)+
    ggplot2::geom_contour(aes(x=x, y=y, z=h, group=group, color=group),
                 bins=1,
                 data=data.contour)+
    ggplot2::geom_point(aes(x = train[,1], y = train[,2], color = cl), size = 2)+
    ggplot2::geom_point(aes(x = train[,1], y = train[,2]), size = 2, shape = 1)+
    ggplot2::labs(x = "x1",
         y = "x2",
         color = "Class",
         title = "SVM decision boundary",
         subtitle = paste("Kernel:",kernel))+
    ggplot2::theme_classic()
}
