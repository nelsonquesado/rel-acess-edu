# Nelson de O. Quesado Filho

# limpeza do environment
rm(list = ls()); gc()

# bibliotecas utilizadas
library(tidyverse); library(tidylog); library(dagitty); library(ggdag)

# definindo diretorio
rstudioapi::getActiveDocumentContext()$path %>% 
  dirname %>% 
  setwd

# diagrama causal
dag <- dagitty('dag{
        horas.trab [pos = "0, 0"]
        renda [pos = "0, 1"]
        acessibilidade [exposure, pos = "1, 1"]
        desempenho [outcome, pos = "1, 2"]
        pcd [pos = "2, 0"]
        genero [pos = "2, 1"]
        qualidade.ensino [pos = "1, 3"]
        horas.trab <- renda
        horas.trab -> acessibilidade
        horas.trab -> desempenho
        acessibilidade <- renda
        desempenho <- renda
        qualidade.ensino <- renda
        acessibilidade <- pcd
        acessibilidade <- genero
        acessibilidade -> desempenho
        desempenho <- genero
        desempenho <- pcd
        desempenho <- qualidade.ensino
        }')

plot(dag)

# plot do dag
dag %>%
  ggdag(text = FALSE, stylized = TRUE, node_size = 15) +
  geom_dag_label_repel(aes(label = name, fill = name), color = 'white', vjust = .2, size = 5) +
  geom_dag_edges() +
  labs(title = 'Hipótese de Relação Causal A Priori', subtitle = 'Diagrama Causal') +
  theme_dag() +
  theme(legend.position="none", strip.text = element_blank())

ggsave('figs/dag1.png', scale = 1.1, dpi = 'retina')

# plot da relacao de interesse
dag %>%
  ggdag_status(text = FALSE, stylized = TRUE, node_size = 10) +
  geom_dag_label_repel(aes(label = name, fill = name), color = 'white', vjust = .2, size = 5) +
  geom_dag_edges() +
  labs(title = 'Hipótese de Relação Causal A Priori', subtitle = 'Diagrama Causal', color = "Relação de Interesse") +
  theme_dag() +
  theme(legend.position = "bottom")
ggsave('figs/dag2.png', scale = 1.1, dpi = 'retina', width = 8, height = 6)

# analise de independências condicionais
s.deps <- impliedConditionalIndependencies(dag, type = 'all.pairs')
impliedConditionalIndependencies(dag, type = 'missing.edge')
impliedConditionalIndependencies(dag, type = 'basis.set')
impliedConditionalIndependencies(dag, type = 'basis.set') %>% length
length(s.deps)

# dags equivalentes
ggdag_equivalent_dags(dag, node_size = 0.5, text = FALSE) +
  theme_dag_gray() +
  theme(strip.text = element_blank(), axis.title = element_blank(), axis.text = element_blank())

# caminhos causais
paths <- paths(dag)
paths
paths %>% .$open %>% sum

# plot dos caminhos causais
ggdag_paths(dag, text = TRUE, stylized = TRUE, shadow = TRUE, node_size = 10, text_col = 'black', text_size = 6) +
  #geom_dag_label_repel(aes(label = name, fill = name), color = 'white', label.size = .05) +
  theme_linedraw() +
  theme(legend.position = 'none', strip.text = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
ggsave('figs/dag3.png', dpi = 'retina', width = 16, height = 8)

# controles necessários
adjustmentSets(dag)

dag %>%
  ggdag_adjustment_set() +
  theme_linedraw() +
  labs(title = 'Caminhos Causais Abertos') +
  theme(strip.text = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
