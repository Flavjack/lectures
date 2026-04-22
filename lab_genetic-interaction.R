# -------------------------------------------------------------------------
# cape: A package for the combined analysis of pleiotropy and epistasis ---
# -------------------------------------------------------------------------

# lecture info ------------------------------------------------------------

browseURL("https://cran.r-project.org/web/packages/cape/vignettes/cape.html")
browseURL("https://doi.org/10.1371/journal.pcbi.1003270")

#> instalar y cargar los paquetes

if (!require("cape")) install.packages("cape")

#> Experimento

# El experimento se realizó para identificar los loci de rasgos cuantitativos (QTLs) 
# para la obesidad y otros factores de riesgo de diabetes tipo II 
# en un retrocruzamiento recíproco de ratones no obesos no diabéticos (NON/Lt ratones)
# y propensos a la diabetes Nueva Zelanda (NZO/HILt ratones) (Reifsnyder, 2000).

#> variables medidas a las 24 semanas

# (1) peso corporal (g) ~ body weight (g) = BW_24
# (2) niveles de insulina (ng/mL) ~  insulin levels (ng/mL) = INS_24
# (3) logaritmo de los niveles de plasma en la glucosa (mg/dL) ~ 
# log of plasma glucose levels (mg/dL) = log_GLU_24
# (4) Madre con peso normal ~ mother of each mouse was normal weight = "pgm" 

#> cargar los datos

# browseURL("https://raw.githubusercontent.com/TheJacksonLaboratory/cape/master/tests/testthat/testdata/demo_qtl_data/NON_NZO_Reifsnyder_pgm_CAPE_num.csv")

cross <- read_population(
  paste0("https://raw.githubusercontent.com/TheJacksonLaboratory/cape/"
         , "master/tests/testthat/testdata/demo_qtl_data/"
         , "NON_NZO_Reifsnyder_pgm_CAPE_num.csv"))

#> Individuos = 208 
#> Genome size = 2.5
#> Precio = 10$

cross_obj <- cape2mpp(cross)
obesity_cross <- cross_obj$data_obj
obesity_geno <- cross_obj$geno_obj$geno

#> revision de los supuestos estadisticos (dist. normal)

hist_pheno(obesity_cross
           , pheno_which = c("BW_24", "INS_24", "log_GLU_24"))

qnorm_pheno(obesity_cross
            , pheno_which = c("BW_24", "INS_24", "log_GLU_24"))

#> no presenta una distribución normal => normalizar variables

obesity_cross <- norm_pheno(obesity_cross, mean_center = TRUE)

hist_pheno(obesity_cross
            , pheno_which = c("BW_24", "INS_24", "log_GLU_24"))

#> variables normalizadas

#> correlación entre las variables

plot_pheno_cor(
  obesity_cross
  , pheno_which = c("BW_24", "INS_24", "log_GLU_24")
  , color_by = "pgm"
  , group_labels = c("Non-obese", "Obese")
  )

#> importar parámetros del modelo

param_file <- "https://raw.githubusercontent.com/TheJacksonLaboratory/cape/master/demo/demo_qtl/0_NON_NZO.parameters_0.yml"

# browseURL(param_file)

#> ejecutar modelo

set.seed(2021)

final_cross <- run_cape(obesity_cross
                        , obesity_geno
                        , results_file = "NON_NZO"
                        , p_or_q = 0.05
                        , verbose = FALSE
                        , param_file = param_file
                        , results_path = ".")

#> resultados

plot_svd(final_cross)

#> se selecionaran las dos primeras dimensiones (ET)

plot_variant_influences(final_cross, show_alleles = F)

##> Chr. 1 suprime el efecto fenotipico de los marcadores
# en el Chr. 10 & 12 (color azul)

##> El efecto principal del Chr. 1, incrementa lo niveles
# de los marcadores de los 3 rasgos (color marron)

plot_network(final_cross)

##> Pleiotropia: Chr. 7 -> Chr. 11 & Chr. 14

##> Epistasis: Chr. 2 -> Chr. 15 | Chr. 12

#> revisar interacción: Chr2 => Chr 15

plot_effects(data_obj = final_cross
             , geno_obj = obesity_geno
             , marker1 = "D2Mit120_B"
             , marker2 = "D15Mit72_B"
             , marker1_label = "Chr2"
             , marker2_label = "Chr15"
             , plot_type = "b"
             , error_bars = "se")

##> Marcador Chr. 2 vs. Marcador Chr. 15
# El marcador del Chr. 2 tiene un efecto aditivo



# -------------------------------------------------------------------------
# grid figures ------------------------------------------------------------
# -------------------------------------------------------------------------

library(tidyverse)
library(cowplot)

p1 <- \(){
  plot_pheno_cor(
    obesity_cross
    , pheno_which = c("BW_24", "INS_24", "log_GLU_24")
    , color_by = "pgm"
    , group_labels = c("Non-obese", "Obese")
  )
}

p2 <- \(){ plot_network(final_cross) }

p3 <- \(){ 

  plot_effects(data_obj = final_cross
               , geno_obj = obesity_geno
               , marker1 = "D2Mit120_B"
               , marker2 = "D15Mit72_B"
               , marker1_label = "Chr2"
               , marker2_label = "Chr15"
               , plot_type = "b"
               , error_bars = "se")
} 
  

list(p1, p2, p3) %>% 
  plot_grid(plotlist = ., nrow = 1, rel_widths = c(1, 1.5, 1)) %>% 
  ggsave2("outputs/genetic-interaction.png", plot = .
          , width = 45, height = 15, units = "cm")
  
