### Temperature Data Analysis 
<--- indicates temperature is not significantly different between sites

> dunnTest(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature"),], method="bonferroni")
Dunn (1964) Kruskal-Wallis multiple comparison
  p-values adjusted with the Bonferroni method.

   Comparison           Z       P.unadj         P.adj
1   CIB - CIE  -3.5846288  3.375580e-04  9.451624e-03 <--- Not really diff.
2   CIB - FBB  40.3837357  0.000000e+00  0.000000e+00
3   CIE - FBB  43.9683644  0.000000e+00  0.000000e+00
4   CIB - FBE  39.2037367  0.000000e+00  0.000000e+00
5   CIE - FBE  42.7883654  0.000000e+00  0.000000e+00
6   FBB - FBE  -1.1799990  2.380006e-01  1.000000e+00 <--- Not different
7   CIB - PGB  39.9325099  0.000000e+00  0.000000e+00
8   CIE - PGB  43.4986133  0.000000e+00  0.000000e+00
9   FBB - PGB  -0.2425229  8.083750e-01  1.000000e+00 <--- Not different
10  FBE - PGB   0.9313779  3.516581e-01  1.000000e+00 <--- Not different
11  CIB - PGE  36.0397116 1.998862e-284 5.596815e-283
12  CIE - PGE  39.6058150  0.000000e+00  0.000000e+00
13  FBB - PGE  -4.1353212  3.544586e-05  9.924840e-04 
14  FBE - PGE  -2.9614204  3.062236e-03  8.574262e-02 <--- Not different
15  PGB - PGE  -3.8728861  1.075541e-04  3.011514e-03 <--- Not different
16  CIB - WBB -56.0965519  0.000000e+00  0.000000e+00
17  CIE - WBB -52.4591935  0.000000e+00  0.000000e+00
18  FBB - WBB -97.0743297  0.000000e+00  0.000000e+00
19  FBE - WBB -95.8769730  0.000000e+00  0.000000e+00
20  PGB - WBB -96.3118480  0.000000e+00  0.000000e+00
21  PGE - WBB -92.3623901  0.000000e+00  0.000000e+00
22  CIB - WBE -58.2300920  0.000000e+00  0.000000e+00
23  CIE - WBE -54.5927336  0.000000e+00  0.000000e+00
24  FBB - WBE -99.2078697  0.000000e+00  0.000000e+00
25  FBE - WBE -98.0105131  0.000000e+00  0.000000e+00
26  PGB - WBE -98.4340377  0.000000e+00  0.000000e+00
27  PGE - WBE -94.4845799  0.000000e+00  0.000000e+00
28  WBB - WBE  -2.1658757  3.032069e-02  8.489793e-01 <--- Not different

#### Combine data for habitats within bays, then re-run analysis
> kruskal.test(value ~ Site, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature"),])

Kruskal-Wallis rank sum test

data:  value by Site
Kruskal-Wallis chi-squared = 25288, df = 3, p-value < 2.2e-16

> dunnTest(value ~ Site, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature"),], method="bonferroni")
Dunn (1964) Kruskal-Wallis multiple comparison
  p-values adjusted with the Bonferroni method.

  Comparison          Z    P.unadj    P.adj
1    CI - FB   58.81156 0.00000000 0.000000
2    CI - PG   56.24209 0.00000000 0.000000
3    FB - PG   -2.26553 0.02348017 0.140881 <--- 
4    CI - WB  -78.26914 0.00000000 0.000000
5    FB - WB -137.94582 0.00000000 0.000000
6    PG - WB -134.91345 0.00000000 0.000000

#### Region comparison

> dunnTest(value ~ Region, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature"),], method="bonferroni")
Dunn (1964) Kruskal-Wallis multiple comparison
  p-values adjusted with the Bonferroni method.

     Comparison         Z P.unadj P.adj
1 North - South -138.4075       0     0
> 