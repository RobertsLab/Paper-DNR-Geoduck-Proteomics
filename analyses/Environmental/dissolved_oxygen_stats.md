#### Habitat within bays comparison
> kruskal.test(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO"),])

	Kruskal-Wallis rank sum test

data:  value by variable
Kruskal-Wallis chi-squared = 5965, df = 7, p-value < 2.2e-16

> dunnTest(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO"),], method="bonferroni")
Dunn (1964) Kruskal-Wallis multiple comparison
  p-values adjusted with the Bonferroni method.

   Comparison          Z       P.unadj         P.adj
1   CIB - CIE -31.195022 1.244658e-213 3.485043e-212 <--- Different, Bare < Eelgrass 
2   CIB - FBB -12.311326  7.871927e-35  2.204139e-33
3   CIE - FBB  18.579742  4.687572e-77  1.312520e-75
4   CIB - FBE  -8.116770  4.787560e-16  1.340517e-14
5   CIE - FBE  16.434223  1.088101e-60  3.046684e-59
6   FBB - FBE   1.648504  9.924924e-02  1.000000e+00 <--- Not Different 
7   CIB - PGB -41.696900  0.000000e+00  0.000000e+00
8   CIE - PGB -10.690054  1.133049e-26  3.172536e-25
9   FBB - PGB -29.058468 1.202824e-185 3.367907e-184
10  FBE - PGB -24.823459 5.004435e-136 1.401242e-134
11  CIB - PGE -26.067689 8.479115e-150 2.374152e-148
12  CIE - PGE   4.424894  9.648968e-06  2.701711e-04
13  FBB - PGE -13.786597  3.069006e-43  8.593217e-42
14  FBE - PGE -12.685516  7.114421e-37  1.992038e-35
15  PGB - PGE  14.854308  6.523804e-50  1.826665e-48 <--- Different, Bare > Eelgrass
16  CIB - WBB  12.089626  1.198313e-33  3.355276e-32
17  CIE - WBB  42.716322  0.000000e+00  0.000000e+00
18  FBB - WBB  24.064875 5.832993e-128 1.633238e-126
19  FBE - WBB  17.583815  3.277606e-69  9.177297e-68
20  PGB - WBB  52.966777  0.000000e+00  0.000000e+00
21  PGE - WBB  37.436775 9.859360e-307 2.760621e-305
22  CIB - WBE  17.834793  3.794515e-71  1.062464e-69
23  CIE - WBE  48.458382  0.000000e+00  0.000000e+00
24  FBB - WBE  29.753990 1.539843e-194 4.311560e-193
25  FBE - WBE  22.122667 1.912811e-108 5.355872e-107
26  PGB - WBE  58.673907  0.000000e+00  0.000000e+00
27  PGE - WBE  43.052668  0.000000e+00  0.000000e+00 
28  WBB - WBE   5.643770  1.663661e-08  4.658250e-07 <--- Less different, Bare > Eelgrass

#### Bay comparison
> dunnTest(value ~ Site, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO"),], method="bonferroni")
Dunn (1964) Kruskal-Wallis multiple comparison
  p-values adjusted with the Bonferroni method.

  Comparison          Z       P.unadj         P.adj
1    CI - FB   4.801911  1.571585e-06  9.429510e-06
2    CI - PG -26.542989 3.094339e-155 1.856604e-154
3    FB - PG -28.955638 2.383177e-184 1.429906e-183
4    CI - WB  42.677285  0.000000e+00  0.000000e+00
5    FB - WB  34.181226 4.596852e-256 2.758111e-255
6    PG - WB  68.163579  0.000000e+00  0.000000e+00

#### Regional comparison
> dunnTest(value ~ Region, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO"),], method="bonferroni")
Dunn (1964) Kruskal-Wallis multiple comparison
  p-values adjusted with the Bonferroni method.

     Comparison        Z P.unadj P.adj
1 North - South 45.57309       0     0