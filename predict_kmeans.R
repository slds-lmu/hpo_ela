predict.kmeans = function(object, newdata, method = c("centers", "classes")) {
  method = match.arg(method)
  centers = object$centers
  ss_by_center = apply(centers, 1, function(x) {
    colSums((t(newdata) - x) ^ 2)
  })
  best_clusters = apply(ss_by_center, 1, which.min)
  
  if (method == "centers") {
    centers[best_clusters, ]
  } else {
    best_clusters
  }
}

