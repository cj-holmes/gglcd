library(gglcd)
pdf('data-raw/hex-logo/out.pdf', 6, 6)

cols <- viridis::plasma(3)

lcd(0, 360, n_mol = 1000, return_df = T, seed=1) %>%
  ggplot(aes(x, y, angle=angle, length=length, width=width))+
  geom_lc(aes(fill=angle))+
  coord_fixed()+
  scale_fill_viridis_c(option="plasma")+
  theme_void()

dev.off()
