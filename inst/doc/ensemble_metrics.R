### R code from vignette source 'ensemble_metrics.Rnw'

###################################################
### code chunk number 1: ensemble_metrics.Rnw:51-52
###################################################
  options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: silwidth
###################################################

  library('opticskxi')
  data('m_psych_embeds')
  set.seed(0)

  df_params = expand.grid(n_xi = 8:15, pts = c(15, 20, 25, 30),
                          dist = "cosine", dim_red = "ICA",
                          n_dimred_comp = c(10, 15, 20, 25))

  df_kxi = opticskxi_pipeline(m_psych_embeds, df_params,
                              metrics_dist = 'cosine',
                              max_size_ratio = 0.15, n_min_clusters = 5,
                              n_cores = 1)

  plot(gtable_kxi_profiles(df_kxi))


###################################################
### code chunk number 3: bwratio
###################################################
  plot(gtable_kxi_profiles(df_kxi, metric = 'bw.ratio'))


###################################################
### code chunk number 4: dunn
###################################################
  plot(gtable_kxi_profiles(df_kxi, metric = 'dunn'))


###################################################
### code chunk number 5: metricsvals
###################################################
  plot(ggplot_kxi_metrics(df_kxi, n = 15,
                          metric = c("avg.silwidth", "bw.ratio", "dunn")))


###################################################
### code chunk number 6: ensemble_metrics.Rnw:194-196
###################################################
  ensemble_metrics(n_top = 50, df_params = df_kxi)[[1]] %>%
    print_vignette_table('Ensemble')


###################################################
### code chunk number 7: ensemblemetrics
###################################################

  df_ensemble_kxi = ensemble_models(df_kxi, n_models = 4,
                                    model_subsample = c(0.1, 0.2, 0.5))
  
  plot(gtable_kxi_profiles(df_ensemble_kxi))


###################################################
### code chunk number 8: ica
###################################################

  clusters = df_ensemble_kxi$clusters[[1]]

  set.seed(1)
  df_ica = fortify_ica(m_psych_embeds, n.comp = 4,
                       sup_vars = data.frame(Clusters = clusters))

  ggpairs(df_ica, 'Clusters', ellipses = TRUE, axes = 1:4) %>%
    grid::grid.draw()


###################################################
### code chunk number 9: icadunn
###################################################

  best_kxi <- get_best_kxi(df_kxi, metric = 'dunn')

  set.seed(1)
  fortify_ica(m_psych_embeds, n.comp = 4,
              sup_vars = data.frame(Clusters = best_kxi$clusters)) %>%
    ggpairs('Clusters', ellipses = TRUE, axes = 1:4) %>%
    grid::grid.draw()


