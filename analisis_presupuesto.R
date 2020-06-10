##----------------------------------------------------------------
## Presupuesto de ciencia 1980-2017
## Autor: German Gonzalez
## Fecha 10/6/20
##----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(ggthemes)
library(grid)
library(ggpubr)

options(bitmapType ="cairo")

##---------------------- Cargamos los datos -----------------------------


presupuesto <- read_excel("gasto_publico_nacional_1980-2017.xls", sheet=2)
glimpse(presupuesto)

##---------------------- Grafico -----------------------------

## Colores
fills <- c("#ab88ba","#db6161","#4f4fff","#ff9d9d","#acadea","#2d73c4","#69aabd","#ffd500")

## Orden de los gobiernos
orden <- c("Última dictadura militar", "Alfonsín", "Menem", 
                       "De la Rúa", "Duhalde", "Kirchner", "CFK", "Macri")
presidentes <- data.frame(presidencia=orden[-1], anios=c(1984, 1990, 2000, 2002, 2003, 2008, 2015))

                       
## Seleccionamos los datos de ciencia y universidad
cyu <- presupuesto %>%
 filter(`FINALIDAD  /  FUNCION` %in% c("II.1.3. Ciencia y técnica", "II.1.2. Educación superior y universitaria"))  %>%
 pivot_longer(`1980`:`2017`,names_to = "Año", values_to = "Presupuesto") %>%
 rename(funcion=`FINALIDAD  /  FUNCION`) %>%
 mutate(`Año` = as.numeric(`Año`)) %>%
 mutate(Presidente = factor(rep(c(rep("Última dictadura militar", 4), rep("Alfonsín", 6),
                       rep("Menem", 10), rep("De la Rúa", 2), "Duhalde", 
                       rep("Kirchner", 5), rep("CFK", 8), rep("Macri", 2)), 2),
                       levels=orden))

## Ciencia                       
cyt <- cyu %>% filter(funcion == "II.1.3. Ciencia y técnica")
prom_cyt <- mean(cyt$Presupuesto)

## Presidencias
pres <- tribble(~Presidente, ~n,
            "Última dictadura militar",   4,  
            "Alfonsín",                   6,  
            "Menem",                     10.5,
            "De la Rúa",                  2.2,  
            "Duhalde",                    1,  
            "Kirchner",                   5,  
            "CFK",                        7.5,  
            "Macri",                      2.25)  
pres <- pres %>% mutate(Presidente = factor(Presidente, levels=orden))

p0 <-  ggplot(pres, aes(x=1, y=n, fill=forcats::fct_rev(Presidente))) +
     geom_col() +
     geom_text(aes(label = Presidente),
             position = position_stack(vjust = 0.5)) +
     scale_y_continuous(expand = c(0, 0)) +        
     coord_flip() +
     scale_fill_manual(values=fills) +
     theme_void() +
     theme(legend.position="none")

## Grafico ciencia 
p1 <- ggplot(cyt, aes(x=`Año`, y=Presupuesto, group=1)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=prom_cyt, linetype="dashed", color = "red") +
  geom_vline(data=presidentes, aes(xintercept = anios-0.25), color="black",linetype="dashed") +
  coord_cartesian(xlim=c(1980,2017)) +
  scale_x_continuous(breaks=1980:2017, expand = c(0, 0)) +
  theme_few() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.background = element_rect(colour = "gray90", fill = "gray70")) 
   

fig1 <- ggarrange(p0, p1, heights = c(1, 20), ncol = 1, nrow = 2, align="v")
          
annotate_figure(fig1, top = text_grob("\nPresupuesto función Ciencia y Técnica como porcentaje del PBI (1980-2017)\n", size = 14),
               bottom = text_grob("Gráfico realizado por Germán González (@germangeler).    \n Fuente de los datos: Gasto Público Consolidado por finalidad y función - Ministerio de Economía    \n", hjust = 1, x = 1, face = "italic", size = 10))
ggsave("presupuesto_ciencia.png", dpi=96, height=7, width=16)
              
## Universidad                       
uni <- cyu %>% filter(funcion == "II.1.2. Educación superior y universitaria")
prom_uni <- mean(uni$Presupuesto)

## Graficamos 
p2 <- ggplot(uni, aes(x=`Año`, y=Presupuesto, group=1)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=prom_uni, linetype="dashed", color = "red") +
  geom_vline(data=presidentes, aes(xintercept = anios-0.25), color="black",linetype="dashed") +
  coord_cartesian(xlim=c(1980,2017)) +
  scale_x_continuous(breaks=1980:2017, expand = c(0, 0)) +
  theme_few() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.background = element_rect(colour = "gray90", fill = "gray70"))

fig2 <- ggarrange(p0, p2, heights = c(1, 20), ncol = 1, nrow = 2, align="v")
          
annotate_figure(fig2, top = text_grob("\nPresupuesto función Educación superior y universitaria como porcentaje del PBI (1980-2017)\n", size = 14), bottom = text_grob("Gráfico realizado por Germán González (@germangeler).\n Fuente de los datos: Gasto Público Consolidado por finalidad y función - Ministerio de Economía\n", hjust = 1, x = 1, face = "italic", size = 10))

ggsave("presupuesto_univ.png", dpi=96, height=7, width=16)

