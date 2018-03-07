#### Habitat within bays comparison 
> dunnTest(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity"),], method="bonferroni")
Dunn (1964) Kruskal-Wallis multiple comparison
  p-values adjusted with the Bonferroni method.

   Comparison           Z       P.unadj         P.adj
1   CIB - FBB  -98.599635  0.000000e+00  0.000000e+00
2   CIB - FBE -118.394533  0.000000e+00  0.000000e+00
3   FBB - FBE   -7.271337  3.559455e-13  5.339182e-12 <-- DIFF. Bare < Eel
4   CIB - PGE  -12.786036  1.962308e-37  2.943463e-36
5   FBB - PGE   86.186092  0.000000e+00  0.000000e+00
6   FBE - PGE  104.168426  0.000000e+00  0.000000e+00
7   CIB - WBB    2.569323  1.018974e-02  1.528461e-01 <-- NOT Diff ... weird 
8   FBB - WBB   84.253896  0.000000e+00  0.000000e+00
9   FBE - WBB   97.270503  0.000000e+00  0.000000e+00
10  PGE - WBB   12.803448  1.568300e-37  2.352450e-36
11  CIB - WBE  -45.812260  0.000000e+00  0.000000e+00
12  FBB - WBE   57.629437  0.000000e+00  0.000000e+00
13  FBE - WBE   72.539143  0.000000e+00  0.000000e+00
14  PGE - WBE  -32.470247 2.805623e-231 4.208434e-230
15  WBB - WBE  -39.172696  0.000000e+00  0.000000e+00 <-- Very diff, Bare < Eel

#### Bay comparison 
> dunnTest(value ~ Site, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity"),], method="bonferroni")
Dunn (1964) Kruskal-Wallis multiple comparison
  p-values adjusted with the Bonferroni method.

  Comparison          Z       P.unadj         P.adj
1    CI - FB -128.92645  0.000000e+00  0.000000e+00
2    CI - PG  -12.78604  1.962308e-37  1.177385e-36
3    FB - PG  112.67875  0.000000e+00  0.000000e+00
4    CI - WB  -32.84662 1.272728e-236 7.636368e-236
5    FB - WB  106.24162  0.000000e+00  0.000000e+00
6    PG - WB  -18.45357  4.881301e-76  2.928780e-75

#### Regional comparison 
> dunnTest(value ~ Region, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity"),], method="bonferroni")
Dunn (1964) Kruskal-Wallis multiple comparison
  p-values adjusted with the Bonferroni method.

     Comparison        Z P.unadj P.adj
1 North - South 95.31271       0     0