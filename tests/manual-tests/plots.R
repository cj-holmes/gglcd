library(tidyverse)

n <- 1000

tibble(x=runif(n), y=runif(n)) %>%
  arrange(y) %>%
  mutate(angle = seq(45, -45, l=n),
         lab = "Splay") %>%
  bind_rows(tibble(x=runif(n), y=runif(n)) %>%
              arrange(y) %>%
              mutate(angle = seq(45, 135, l=n),
                     lab = "Bend")) %>%
  ggplot()+
  geom_lc(aes(x, y, angle=angle, length=0.04, aspect=3))+
  coord_fixed()+
  facet_wrap(~lab)


gglcd::lcd(0, 90, 1000, lc_length = 0.05, surface_b = 0.1, surface_t = 0.05,
           lc_shape = "elipse",
           lc_col="red", lc_fill = "gold",
           bg_col="purple", bg_fill="lightblue",
           surface_b_col="white",
           surface_b_fill= "black",
           surface_t_col = "green",
           surface_t_fill = "pink",

           bg_lwd = 3,
           lc_lwd = 1,
           surface_b_lwd = 3,
           surface_t_lwd = 3)


n <- 1

tibble(x=runif(n), y=runif(n)) %>%
  arrange(y) %>%
  mutate(angle = seq(45, -45, l=n),
         lab = "Splay") %>%
  ggplot()+
  geom_lc(aes(x, y, angle=angle, length=0.04, aspect=3), lc_shape = "elipse", elipse_res = 35)+
  coord_fixed()




