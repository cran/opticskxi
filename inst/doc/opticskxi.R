### R code from vignette source 'opticskxi.Rnw'

###################################################
### code chunk number 1: opticskxi.Rnw:61-62
###################################################
  options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: shape_ds3
###################################################
  library('opticskxi')

  data('multishapes')
  dbscan_shapes <- dbscan::dbscan(multishapes[1:2], eps = 0.15)
  gg_shapes <- cbind(multishapes[1:2], Clusters = dbscan_shapes$cluster) %>%
    ggpairs(group = 'Clusters')

  data('DS3', package = 'dbscan')
  dbscan_ds3 <- dbscan::dbscan(DS3, minPts = 25, eps = 12)
  gg_ds3 <- cbind(DS3, Clusters = dbscan_ds3$cluster) %>%
    ggpairs(group = 'Clusters')

  cowplot::plot_grid(gg_shapes, gg_ds3, nrow = 2,
    labels = c('(a)', '(b)'), label_x = 0.9)


###################################################
### code chunk number 3: multi_gauss
###################################################
  n <- 1e3
  set.seed(0)
  multi_gauss <- cbind.data.frame(
    x = c(rnorm(n / 2, -3), rnorm(n / 4, 3), rnorm(n / 4, 3, .2)),
    y = c(rnorm(n * .75), rnorm(n / 8, 1, .2), rnorm(n / 8, -1, .2)))

  dbscan_gauss <- dbscan::dbscan(multi_gauss, minPts = 30, eps = .5)
  gg_mgauss <- cbind(multi_gauss, Clusters = dbscan_gauss$cluster) %>%
    ggpairs(group = 'Clusters')
  gg_mgauss_small <- dbscan::dbscan(multi_gauss, minPts = 30, eps = .2) %$%
    cbind(multi_gauss, Clusters = cluster) %>% ggpairs(group = 'Clusters')
  cowplot::plot_grid(gg_mgauss, gg_mgauss_small, nrow = 2,
    labels = c('(a)', '(b)'), label_x = .9)


###################################################
### code chunk number 4: multi_gauss_xi
###################################################
  optics_gauss <- dbscan::optics(multi_gauss, minPts = 30)
  xi_gauss <- dbscan::extractXi(optics_gauss, xi = 0.03)
  ggplot_optics(optics_gauss, groups = xi_gauss$cluster)


###################################################
### code chunk number 5: multi_gauss_kxi
###################################################
  kxi_gauss <- opticskxi(optics_gauss, n_xi = 4, pts = 100)
  ggplot_optics(optics_gauss, groups = kxi_gauss)


###################################################
### code chunk number 6: kxi_shapes_ds3
###################################################
  gg_shapes_optics <- dbscan::optics(multishapes[1:2]) %>%
    ggplot_optics(groups = opticskxi(., n_xi = 5, pts = 30))
  gg_ds3_optics <- dbscan::optics(DS3, minPts = 25) %>%
    ggplot_optics(groups = opticskxi(., n_xi = 6, pts = 100))
  cowplot::plot_grid(gg_shapes_optics, gg_ds3_optics, nrow = 2,
    labels = c('(a)', '(b)'), label_x = .9)


###################################################
### code chunk number 7: hla_metrics
###################################################
  data('hla')
  m_hla <- hla[-c(1:2)] %>% scale
  df_params_hla <- expand.grid(n_xi = 3:5, pts = c(20, 30, 40),
    dist = c('manhattan', 'euclidean', 'abscorrelation', 'abspearson'))
  df_kxi_hla <- opticskxi_pipeline(m_hla, df_params_hla)
  ggplot_kxi_metrics(df_kxi_hla, n = 8)


###################################################
### code chunk number 8: hla_profiles
###################################################
  gtable_kxi_profiles(df_kxi_hla) %>% plot


###################################################
### code chunk number 9: opticskxi.Rnw:244-249
###################################################
  best_kxi_hla <- get_best_kxi(df_kxi_hla, rank = 2)
  clusters_hla <- best_kxi_hla$clusters

  hla$id %<>% `levels<-`(c('Controls', 'Sch. patients'))
  residuals_table(clusters_hla, hla$id) %>% print_table('HLA')


###################################################
### code chunk number 10: hla_dimred
###################################################
  fortify_pca(m_hla, sup_vars = data.frame(Clusters = clusters_hla)) %>%
    ggpairs('Clusters', ellipses = TRUE, variables = TRUE)


###################################################
### code chunk number 11: crohn_metrics
###################################################
  data('crohn')
  m_crohn <- crohn[-c(1:6)] %>% scale
  df_params_crohn <- expand.grid(n_xi = 3:5, dim_red = c('PCA', 'ICA'),
    dist = c('euclidean', 'abscorrelation', 'abspearson'),
    pts = c(30, 40, 50), n_dimred_comp = c(4, 6, 8))
  df_kxi_crohn <- opticskxi_pipeline(m_crohn, df_params_crohn)
  ggplot_kxi_metrics(df_kxi_crohn)


###################################################
### code chunk number 12: crohn_profiles
###################################################
  gtable_kxi_profiles(df_kxi_crohn) %>% plot


###################################################
### code chunk number 13: opticskxi.Rnw:299-304
###################################################
  best_kxi_crohn <- get_best_kxi(df_kxi_crohn, rank = 1)
  clusters_crohn <- best_kxi_crohn$clusters

  crohn$crohn %<>% factor %>% `levels<-`(c('Controls', 'Crohn patients'))
  residuals_table(clusters_crohn, crohn$crohn) %>% print_table('Crohn')


###################################################
### code chunk number 14: crohn_dimred
###################################################
  ica <- fortify_ica(m_crohn, n.comp = 4,
    sup_vars = data.frame(Clusters = clusters_crohn))
  ggpairs(ica, 'Clusters', axes = 1:4, ellipses = TRUE, level = .75) %>%
    plot


###################################################
### code chunk number 15: crohn_dimred_23
###################################################
  ggpairs(ica, 'Clusters', axes = 2:3, ellipses = TRUE, variables = TRUE,
    n_vars = 3)


