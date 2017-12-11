

# A plotting R script produced by the REVIGO server at http://revigo.irb.hr/
# If you found REVIGO useful in your work, please cite the following reference:
# Supek F et al. "REVIGO summarizes and visualizes long lists of Gene Ontology
# terms" PLoS ONE 2011. doi:10.1371/journal.pone.0021800


# --------------------------------------------------------------------------
# If you don't have the ggplot2 package installed, uncomment the following line:
# install.packages( "ggplot2" );
library( ggplot2 );
# --------------------------------------------------------------------------
# If you don't have the scales package installed, uncomment the following line:
# install.packages( "scales" );
library( scales );


# --------------------------------------------------------------------------
# Here is your data from REVIGO. Scroll down for plot configuration options.

revigo.names <- c("term_ID","description","frequency_%","plot_X","plot_Y","plot_size","log10_p_value","uniqueness","dispensability");
revigo.data <- rbind(c("GO:0000003","reproduction", 0.769, 0.427,-0.291, 4.994,-2.3042,1.000,0.000),
c("GO:0008152","metabolic process",75.387, 0.294,-0.476, 6.986,-6.4547,0.998,0.000),
c("GO:0008380","RNA splicing", 0.413, 0.184,-3.394, 4.725,-18.4078,0.875,0.000),
c("GO:0009629","response to gravity", 0.006,-2.477, 3.268, 2.909,-1.2021,0.951,0.000),
c("GO:0040011","locomotion", 0.997, 0.092,-0.127, 5.107,-3.4498,0.994,0.000),
c("GO:0051028","mRNA transport", 0.075,-1.429, 5.324, 3.986,-8.6364,0.847,0.000),
c("GO:0098609","cell-cell adhesion", 0.251,-0.433,-0.383, 4.507,-14.8962,0.949,0.000),
c("GO:0006807","nitrogen compound metabolic process",38.744, 0.310,-0.688, 6.696,-1.0661,0.985,0.022),
c("GO:0061077","chaperone-mediated protein folding", 0.043,-0.109,-0.529, 3.743,-2.8176,0.957,0.025),
c("GO:0016032","viral process", 0.131, 0.260,-1.273, 4.226,-3.3316,0.920,0.027),
c("GO:0007018","microtubule-based movement", 0.287, 2.491,-2.725, 4.567,-7.9245,0.776,0.029),
c("GO:0006457","protein folding", 0.903, 0.042,-0.016, 5.064,-13.5513,0.957,0.032),
c("GO:0002119","nematode larval development", 0.015, 1.373, 1.302, 3.295,-3.3706,0.836,0.056),
c("GO:0005975","carbohydrate metabolic process", 5.260, 0.032,-0.047, 5.829,-2.6428,0.961,0.068),
c("GO:0010388","cullin deneddylation", 0.000,-0.089,-2.641, 1.580,-1.7416,0.935,0.077),
c("GO:0045197","establishment or maintenance of epithelial cell apical/basal polarity", 0.008, 0.290,-2.307, 3.035,-2.6116,0.877,0.123),
c("GO:0000281","mitotic cytokinesis", 0.070, 1.989,-4.022, 3.953,-2.1383,0.859,0.143),
c("GO:0007163","establishment or maintenance of cell polarity", 0.099, 1.438,-3.503, 4.102,-1.3150,0.858,0.146),
c("GO:0097120","receptor localization to synapse", 0.004,-1.167, 4.755, 2.730,-4.0635,0.870,0.159),
c("GO:1901998","toxin transport", 0.008,-1.373, 4.090, 3.026,-5.1904,0.923,0.172),
c("GO:0045454","cell redox homeostasis", 0.861,-2.066,-1.421, 5.043,-6.3458,0.749,0.176),
c("GO:0012501","programmed cell death", 0.430, 2.080,-4.203, 4.742,-1.9268,0.837,0.183),
c("GO:0006099","tricarboxylic acid cycle", 0.469, 0.931,-5.962, 4.780,-6.2596,0.775,0.185),
c("GO:0030030","cell projection organization", 0.608, 6.587,-1.317, 4.892,-3.0128,0.727,0.190),
c("GO:0006081","cellular aldehyde metabolic process", 0.753, 1.971,-4.140, 4.985,-1.3085,0.829,0.194),
c("GO:0015991","ATP hydrolysis coupled proton transport", 0.163,-1.885, 6.669, 4.321,-3.7773,0.822,0.210),
c("GO:0007229","integrin-mediated signaling pathway", 0.056,-4.050, 4.236, 3.860,-3.5817,0.810,0.221),
c("GO:0010501","RNA secondary structure unwinding", 0.025, 0.218,-2.324, 3.507,-4.2366,0.915,0.224),
c("GO:0031047","gene silencing by RNA", 0.089,-1.899,-2.144, 4.056,-1.3461,0.766,0.225),
c("GO:0055114","oxidation-reduction process",15.060,-1.125,-6.156, 6.286,-1.9419,0.867,0.232),
c("GO:0051983","regulation of chromosome segregation", 0.098,-2.310,-1.020, 4.098,-1.0433,0.811,0.232),
c("GO:0006413","translational initiation", 0.518,-0.361,-5.449, 4.823,-12.1605,0.852,0.243),
c("GO:0071364","cellular response to epidermal growth factor stimulus", 0.005,-2.649, 3.757, 2.842,-2.0464,0.913,0.245),
c("GO:0016192","vesicle-mediated transport", 1.085,-1.690, 5.284, 5.144,-3.6716,0.896,0.261),
c("GO:0006897","endocytosis", 0.235,-1.883, 5.829, 4.480,-3.2306,0.876,0.271),
c("GO:0050790","regulation of catalytic activity", 1.575,-4.229, 2.666, 5.306,-3.1349,0.888,0.283),
c("GO:0006094","gluconeogenesis", 0.262,-1.110,-6.274, 4.527,-2.7998,0.837,0.283),
c("GO:0006730","one-carbon metabolic process", 0.328, 0.982,-6.110, 4.625,-1.9276,0.815,0.289),
c("GO:0034976","response to endoplasmic reticulum stress", 0.100,-3.084, 4.096, 4.106,-2.8010,0.908,0.295),
c("GO:0000413","protein peptidyl-prolyl isomerization", 0.393,-0.215,-3.774, 4.703,-3.2418,0.902,0.309),
c("GO:0032060","bleb assembly", 0.003, 7.292, 0.925, 2.511,-3.1355,0.758,0.310),
c("GO:0006107","oxaloacetate metabolic process", 0.015,-0.656,-6.052, 3.287,-2.6586,0.834,0.313),
c("GO:0051603","proteolysis involved in cellular protein catabolic process", 0.759,-0.499,-4.192, 4.988,-6.5751,0.860,0.329),
c("GO:0046034","ATP metabolic process", 1.263, 0.845,-6.418, 5.210,-3.8356,0.723,0.329),
c("GO:0000972","transcription-dependent tethering of RNA polymerase II gene DNA at nuclear periphery", 0.006, 7.867, 1.832, 2.921,-1.6814,0.825,0.330),
c("GO:0001778","plasma membrane repair", 0.002, 5.854, 2.828, 2.389,-1.7416,0.757,0.331),
c("GO:0045954","positive regulation of natural killer cell mediated cytotoxicity", 0.004,-3.541, 4.491, 2.704,-2.6818,0.806,0.345),
c("GO:0006396","RNA processing", 3.210, 0.048,-4.089, 5.615,-1.0130,0.872,0.346),
c("GO:0007266","Rho protein signal transduction", 0.137,-4.220, 4.467, 4.246,-1.4343,0.818,0.360),
c("GO:0034332","adherens junction organization", 0.023, 7.666,-0.371, 3.464,-1.1191,0.777,0.361),
c("GO:0016559","peroxisome fission", 0.023, 7.670, 1.747, 3.469,-2.2513,0.812,0.361),
c("GO:0051592","response to calcium ion", 0.018,-2.022, 2.983, 3.357,-2.0177,0.942,0.366),
c("GO:0071896","protein localization to adherens junction", 0.001,-1.294, 5.645, 2.238,-1.6814,0.886,0.368),
c("GO:0042769","DNA damage response, detection of DNA damage", 0.001,-1.960, 2.665, 2.161,-1.0882,0.927,0.373),
c("GO:0030212","hyaluronan metabolic process", 0.008,-1.219,-5.494, 3.006,-1.4241,0.914,0.374),
c("GO:1904851","positive regulation of establishment of protein localization to telomere", 0.002,-2.371, 5.643, 2.410,-2.1931,0.763,0.380),
c("GO:0007629","flight behavior", 0.001, 0.551, 1.470, 2.255,-2.1761,0.872,0.394),
c("GO:0006851","mitochondrial calcium ion transport", 0.005,-1.733, 5.082, 2.818,-2.1931,0.916,0.407),
c("GO:0006048","UDP-N-acetylglucosamine biosynthetic process", 0.025,-1.250,-4.964, 3.502,-2.6586,0.886,0.408),
c("GO:0050885","neuromuscular process controlling balance", 0.012, 0.817, 1.517, 3.188,-1.7190,0.915,0.410),
c("GO:0035999","tetrahydrofolate interconversion", 0.129, 0.257,-7.180, 4.217,-2.6888,0.779,0.418),
c("GO:0002181","cytoplasmic translation", 0.064,-0.281,-5.415, 3.915,-3.7375,0.872,0.429),
c("GO:0019344","cysteine biosynthetic process", 0.131, 0.164,-6.796, 4.225,-2.1931,0.774,0.430),
c("GO:0001731","formation of translation preinitiation complex", 0.070, 6.421,-2.240, 3.954,-8.8697,0.703,0.432),
c("GO:0016319","mushroom body development", 0.004, 1.851, 1.695, 2.701,-2.0308,0.834,0.433),
c("GO:0032968","positive regulation of transcription elongation from RNA polymerase II promoter", 0.029,-3.494,-0.851, 3.567,-1.4343,0.800,0.435),
c("GO:0006552","leucine catabolic process", 0.003,-0.970,-6.470, 2.632,-1.8285,0.808,0.436),
c("GO:0006002","fructose 6-phosphate metabolic process", 0.060,-1.009,-3.427, 3.885,-1.3085,0.907,0.444),
c("GO:0045214","sarcomere organization", 0.013, 6.545, 0.638, 3.220,-4.4828,0.639,0.444),
c("GO:0097119","postsynaptic density protein 95 clustering", 0.001, 5.627, 4.177, 2.243,-1.3085,0.678,0.449),
c("GO:1903608","protein localization to cytoplasmic stress granule", 0.002,-1.515, 6.193, 2.292,-1.0433,0.857,0.463),
c("GO:0051103","DNA ligation involved in DNA repair", 0.039,-2.137, 1.439, 3.703,-1.1191,0.866,0.463),
c("GO:0001736","establishment of planar polarity", 0.019, 1.551, 1.174, 3.379,-1.6700,0.833,0.468),
c("GO:0010510","regulation of acetyl-CoA biosynthetic process from pyruvate", 0.001,-3.175,-4.900, 2.121,-1.2021,0.788,0.479),
c("GO:0003374","dynamin family protein polymerization involved in mitochondrial fission", 0.002, 7.766, 1.026, 2.449,-2.6586,0.753,0.485),
c("GO:0043044","ATP-dependent chromatin remodeling", 0.046, 7.721, 1.773, 3.770,-1.7459,0.811,0.489),
c("GO:0007010","cytoskeleton organization", 0.786, 7.596, 1.680, 5.004,-2.4031,0.768,0.490),
c("GO:0098792","xenophagy", 0.002,-1.747, 2.787, 2.398,-1.2921,0.918,0.490),
c("GO:0019886","antigen processing and presentation of exogenous peptide antigen via MHC class II", 0.002,-1.076, 0.761, 2.498,-2.3566,0.973,0.490),
c("GO:0010827","regulation of glucose transport", 0.013,-2.424, 5.072, 3.235,-1.2496,0.774,0.495),
c("GO:0051292","nuclear pore complex assembly", 0.005, 8.144, 1.852, 2.780,-1.6606,0.800,0.506),
c("GO:0015813","L-glutamate transport", 0.023,-1.246, 5.019, 3.466,-1.4241,0.833,0.509),
c("GO:0051291","protein heterooligomerization", 0.013, 7.994, 1.877, 3.206,-1.5008,0.807,0.509),
c("GO:0006105","succinate metabolic process", 0.015,-0.666,-6.035, 3.274,-1.7416,0.834,0.515),
c("GO:0006997","nucleus organization", 0.052, 7.487, 1.656, 3.821,-1.5633,0.805,0.516),
c("GO:0006268","DNA unwinding involved in DNA replication", 0.058, 7.568,-0.700, 3.873,-2.0308,0.753,0.522),
c("GO:0006103","2-oxoglutarate metabolic process", 0.020,-0.573,-6.094, 3.413,-2.1378,0.831,0.525),
c("GO:0050821","protein stabilization", 0.045,-3.804, 2.125, 3.763,-2.9108,0.892,0.525),
c("GO:0044351","macropinocytosis", 0.002,-1.024, 3.312, 2.340,-2.0103,0.900,0.525),
c("GO:0045104","intermediate filament cytoskeleton organization", 0.009, 7.483, 0.279, 3.083,-1.4241,0.740,0.528),
c("GO:0006417","regulation of translation", 0.692,-2.909,-3.287, 4.948,-6.7620,0.767,0.528),
c("GO:1990966","ATP generation from poly-ADP-D-ribose", 0.001,-1.205,-6.364, 2.086,-1.3085,0.819,0.531),
c("GO:0000463","maturation of LSU-rRNA from tricistronic rRNA transcript (SSU-rRNA, 5.8S rRNA, LSU-rRNA)", 0.037, 7.034,-0.199, 3.676,-2.3290,0.760,0.535),
c("GO:0009792","embryo development ending in birth or egg hatching", 0.155, 1.547, 1.427, 4.298,-1.9803,0.809,0.537),
c("GO:0072499","photoreceptor cell axon guidance", 0.001, 4.636, 3.168, 2.248,-1.6814,0.654,0.538),
c("GO:0071404","cellular response to low-density lipoprotein particle stimulus", 0.003,-2.568, 3.604, 2.530,-1.0433,0.916,0.540),
c("GO:0021675","nerve development", 0.016, 1.934, 1.721, 3.320,-1.0433,0.825,0.542),
c("GO:1903543","positive regulation of exosomal secretion", 0.003, 4.081, 4.449, 2.576,-2.6586,0.650,0.543),
c("GO:0008340","determination of adult lifespan", 0.020, 1.396, 1.322, 3.413,-1.8086,0.834,0.547),
c("GO:0006635","fatty acid beta-oxidation", 0.080,-0.010,-6.209, 4.011,-6.1198,0.778,0.551),
c("GO:1900034","regulation of cellular response to heat", 0.004,-3.568, 3.819, 2.670,-2.2811,0.865,0.557),
c("GO:2000969","positive regulation of alpha-amino-3-hydroxy-5-methyl-4-isoxazole propionate selective glutamate receptor activity", 0.001,-3.458, 5.715, 1.875,-1.3085,0.754,0.558),
c("GO:0016322","neuron remodeling", 0.005, 2.025,-0.396, 2.814,-1.0882,0.776,0.560),
c("GO:0071709","membrane assembly", 0.095, 7.403,-0.685, 4.086,-1.2021,0.719,0.561),
c("GO:0006172","ADP biosynthetic process", 0.008,-1.050,-7.215, 3.028,-1.3085,0.791,0.564),
c("GO:0045647","negative regulation of erythrocyte differentiation", 0.002,-1.781, 1.896, 2.348,-1.0433,0.713,0.565),
c("GO:0038083","peptidyl-tyrosine autophosphorylation", 0.011,-0.703,-4.193, 3.157,-1.0033,0.900,0.566),
c("GO:0006418","tRNA aminoacylation for protein translation", 1.099, 1.076,-6.064, 5.149,-1.4343,0.715,0.570),
c("GO:0007023","post-chaperonin tubulin folding pathway", 0.020,-0.154,-0.300, 3.404,-2.1931,0.959,0.573),
c("GO:0043051","regulation of pharyngeal pumping", 0.002,-2.346, 1.409, 2.328,-1.2021,0.921,0.574),
c("GO:0070050","neuron cellular homeostasis", 0.003,-2.401, 0.325, 2.640,-1.1792,0.822,0.575),
c("GO:0018279","protein N-linked glycosylation via asparagine", 0.015,-0.076,-6.203, 3.284,-1.5018,0.806,0.577),
c("GO:0071526","semaphorin-plexin signaling pathway", 0.019,-3.346, 3.356, 3.381,-1.5822,0.821,0.578),
c("GO:0051639","actin filament network formation", 0.003, 7.192, 0.648, 2.549,-1.3085,0.744,0.581),
c("GO:0051489","regulation of filopodium assembly", 0.009, 6.283, 1.808, 3.043,-1.3104,0.693,0.582),
c("GO:0006886","intracellular protein transport", 1.199,-2.114, 6.590, 5.187,-4.9547,0.790,0.584),
c("GO:0002576","platelet degranulation", 0.003,-1.036, 3.087, 2.519,-1.2496,0.795,0.586),
c("GO:0045053","protein retention in Golgi apparatus", 0.007,-2.591, 3.926, 2.949,-1.8965,0.701,0.586),
c("GO:0006337","nucleosome disassembly", 0.010, 7.852, 0.171, 3.096,-1.2496,0.765,0.588),
c("GO:0007173","epidermal growth factor receptor signaling pathway", 0.025,-3.812, 3.920, 3.505,-1.6111,0.815,0.589),
c("GO:0060285","cilium-dependent cell motility", 0.006,-0.654, 2.374, 2.912,-1.3104,0.795,0.591),
c("GO:0051764","actin crosslink formation", 0.004, 8.064, 0.560, 2.714,-1.7416,0.739,0.592),
c("GO:0051560","mitochondrial calcium ion homeostasis", 0.006,-2.727, 0.241, 2.891,-2.6586,0.814,0.598),
c("GO:0007339","binding of sperm to zona pellucida", 0.007, 0.253,-2.849, 2.957,-2.3290,0.848,0.600),
c("GO:0061512","protein localization to cilium", 0.010,-1.202, 4.752, 3.095,-3.2314,0.843,0.601),
c("GO:0007224","smoothened signaling pathway", 0.038,-4.033, 4.197, 3.683,-1.1773,0.815,0.605),
c("GO:0006631","fatty acid metabolic process", 0.878, 1.143,-5.884, 5.052,-1.2779,0.776,0.607),
c("GO:0051014","actin filament severing", 0.011, 2.607,-2.197, 3.139,-2.1761,0.832,0.608),
c("GO:0009083","branched-chain amino acid catabolic process", 0.034,-0.519,-7.129, 3.636,-3.1385,0.783,0.610),
c("GO:0034394","protein localization to cell surface", 0.013,-1.862, 6.520, 3.207,-1.1945,0.848,0.612),
c("GO:0000059","protein import into nucleus, docking", 0.013, 5.334, 4.753, 3.216,-4.2411,0.646,0.613),
c("GO:0070814","hydrogen sulfide biosynthetic process", 0.058,-0.555,-2.389, 3.875,-1.7416,0.926,0.616),
c("GO:0034454","microtubule anchoring at centrosome", 0.003, 7.577, 0.746, 2.624,-1.0433,0.737,0.617),
c("GO:0071407","cellular response to organic cyclic compound", 0.172,-2.728, 3.842, 4.343,-1.2904,0.897,0.618),
c("GO:0042026","protein refolding", 0.069,-0.121,-0.748, 3.949,-2.6586,0.956,0.621),
c("GO:0032091","negative regulation of protein binding", 0.017,-2.701, 1.656, 3.335,-1.6187,0.913,0.621),
c("GO:0060294","cilium movement involved in cell motility", 0.004, 0.085, 2.725, 2.695,-2.2513,0.777,0.623),
c("GO:0010457","centriole-centriole cohesion", 0.004, 7.454, 0.670, 2.697,-1.3085,0.735,0.623),
c("GO:0006397","mRNA processing", 0.561, 0.184,-3.469, 4.857,-17.1612,0.853,0.624),
c("GO:0030593","neutrophil chemotaxis", 0.015,-2.529, 3.838, 3.272,-1.2496,0.729,0.625),
c("GO:0038095","Fc-epsilon receptor signaling pathway", 0.003,-3.380, 3.354, 2.544,-1.1783,0.823,0.629),
c("GO:0060414","aorta smooth muscle tissue morphogenesis", 0.001, 1.969, 1.545, 1.959,-2.1761,0.842,0.629),
c("GO:0030317","flagellated sperm motility", 0.017,-0.812, 2.377, 3.333,-1.9888,0.784,0.631),
c("GO:0045616","regulation of keratinocyte differentiation", 0.006,-0.727, 1.165, 2.869,-1.0433,0.739,0.631),
c("GO:0021670","lateral ventricle development", 0.003, 1.840, 1.678, 2.531,-1.4241,0.837,0.633),
c("GO:0019388","galactose catabolic process", 0.005,-1.607,-6.366, 2.817,-1.0433,0.855,0.634),
c("GO:0000266","mitochondrial fission", 0.022, 8.172, 1.892, 3.441,-1.6247,0.810,0.636),
c("GO:0008053","mitochondrial fusion", 0.017, 7.678, 0.047, 3.338,-1.0882,0.725,0.637),
c("GO:0044331","cell-cell adhesion mediated by cadherin", 0.003,-0.398,-0.382, 2.642,-1.3085,0.903,0.637),
c("GO:0055088","lipid homeostasis", 0.041,-4.029, 2.158, 3.717,-2.2467,0.888,0.641),
c("GO:0016339","calcium-dependent cell-cell adhesion via plasma membrane cell adhesion molecules", 0.004,-0.491,-0.637, 2.708,-2.0136,0.956,0.643),
c("GO:0016575","histone deacetylation", 0.048, 7.117,-0.109, 3.787,-1.1809,0.771,0.649),
c("GO:0051016","barbed-end actin filament capping", 0.022, 6.017, 1.513, 3.455,-4.1141,0.620,0.651),
c("GO:0021517","ventral spinal cord development", 0.012, 1.670, 1.650, 3.176,-1.1792,0.825,0.651),
c("GO:1990090","cellular response to nerve growth factor stimulus", 0.004,-2.624, 3.761, 2.694,-1.0661,0.914,0.651),
c("GO:0006890","retrograde vesicle-mediated transport, Golgi to ER", 0.047,-1.703, 5.460, 3.777,-1.8187,0.886,0.652),
c("GO:0006909","phagocytosis", 0.051,-0.975, 4.496, 3.813,-2.1145,0.811,0.656),
c("GO:0045887","positive regulation of synaptic growth at neuromuscular junction", 0.002, 5.142, 3.246, 2.384,-1.0433,0.631,0.657),
c("GO:0006120","mitochondrial electron transport, NADH to ubiquinone", 0.019,-0.655,-6.751, 3.386,-2.1931,0.777,0.657),
c("GO:0021785","branchiomotor neuron axon guidance", 0.002, 4.613, 3.115, 2.318,-1.1191,0.651,0.658),
c("GO:0018105","peptidyl-serine phosphorylation", 0.087,-0.859,-4.792, 4.049,-2.1816,0.887,0.658),
c("GO:0008544","epidermis development", 0.058, 2.214, 1.479, 3.871,-1.1945,0.881,0.660),
c("GO:0007269","neurotransmitter secretion", 0.043,-3.591, 3.687, 3.739,-2.4011,0.672,0.660),
c("GO:0006471","protein ADP-ribosylation", 0.019,-0.125,-5.293, 3.391,-1.3150,0.807,0.661),
c("GO:0006098","pentose-phosphate shunt", 0.287, 0.403,-6.466, 4.566,-2.3290,0.751,0.663),
c("GO:0009617","response to bacterium", 0.145,-2.504, 3.711, 4.271,-1.2496,0.933,0.667),
c("GO:1904871","positive regulation of protein localization to Cajal body", 0.002,-2.345, 5.662, 2.364,-2.6818,0.762,0.668),
c("GO:0031532","actin cytoskeleton reorganization", 0.028, 7.378,-0.249, 3.555,-1.5086,0.715,0.669),
c("GO:0021591","ventricular system development", 0.007, 1.750, 1.670, 2.943,-1.0726,0.829,0.670),
c("GO:0032880","regulation of protein localization", 0.219,-3.525, 6.159, 4.449,-2.4970,0.760,0.672),
c("GO:0000904","cell morphogenesis involved in differentiation", 0.188, 6.379,-0.440, 4.382,-3.1959,0.664,0.674),
c("GO:0019243","methylglyoxal catabolic process to D-lactate via S-lactoyl-glutathione", 0.021,-0.817,-7.165, 3.421,-1.0433,0.798,0.676),
c("GO:0008652","cellular amino acid biosynthetic process", 2.932, 1.251,-6.407, 5.575,-1.3150,0.733,0.677),
c("GO:0007077","mitotic nuclear envelope disassembly", 0.000, 7.489, 1.248, 1.771,-1.0882,0.776,0.679),
c("GO:0000244","spliceosomal tri-snRNP complex assembly", 0.010, 6.878, 0.169, 3.118,-1.0882,0.735,0.680),
c("GO:0033962","cytoplasmic mRNA processing body assembly", 0.011, 7.842, 1.775, 3.145,-1.6863,0.789,0.683),
c("GO:0051561","positive regulation of mitochondrial calcium ion concentration", 0.002,-2.571, 0.548, 2.444,-2.1761,0.824,0.684),
c("GO:0000381","regulation of alternative mRNA splicing, via spliceosome", 0.017,-2.871,-0.617, 3.344,-5.6716,0.811,0.692),
c("GO:0042073","intraciliary transport", 0.019, 5.645, 3.639, 3.387,-3.4622,0.574,0.692),
c("GO:0033627","cell adhesion mediated by integrin", 0.013,-0.397,-0.350, 3.231,-1.6606,0.956,0.695));

one.data <- data.frame(revigo.data);
names(one.data) <- revigo.names;
one.data <- one.data [(one.data$plot_X != "null" & one.data$plot_Y != "null"), ];
one.data$plot_X <- as.numeric( as.character(one.data$plot_X) );
one.data$plot_Y <- as.numeric( as.character(one.data$plot_Y) );
one.data$plot_size <- as.numeric( as.character(one.data$plot_size) );
one.data$log10_p_value <- as.numeric( as.character(one.data$log10_p_value) );
one.data$frequency <- as.numeric( as.character(one.data$frequency) );
one.data$uniqueness <- as.numeric( as.character(one.data$uniqueness) );
one.data$dispensability <- as.numeric( as.character(one.data$dispensability) );
#head(one.data);


# --------------------------------------------------------------------------
# Names of the axes, sizes of the numbers and letters, names of the columns,
# etc. can be changed below

p1 <- ggplot( data = one.data );
p1 <- p1 + geom_point( aes( plot_X, plot_Y, colour = log10_p_value, size = plot_size), alpha = I(0.6) ) + scale_size_area();
p1 <- p1 + scale_colour_gradientn( colours = c("blue", "green", "yellow", "red"), limits = c( min(one.data$log10_p_value), 0) );
p1 <- p1 + geom_point( aes(plot_X, plot_Y, size = plot_size), shape = 21, fill = "transparent", colour = I (alpha ("black", 0.6) )) + scale_size_area();
p1 <- p1 + scale_size( range=c(5, 30)) + theme_bw(); # + scale_fill_gradientn(colours = heat_hcl(7), limits = c(-300, 0) );
ex <- one.data [ one.data$dispensability < 0.15, ]; 
p1 <- p1 + geom_text( data = ex, aes(plot_X, plot_Y, label = description), colour = I(alpha("black", 0.85)), size = 3 );
p1 <- p1 + labs (y = "semantic space x", x = "semantic space y");
p1 <- p1 + theme(legend.key = element_blank()) ;
one.x_range = max(one.data$plot_X) - min(one.data$plot_X);
one.y_range = max(one.data$plot_Y) - min(one.data$plot_Y);
p1 <- p1 + xlim(min(one.data$plot_X)-one.x_range/10,max(one.data$plot_X)+one.x_range/10);
p1 <- p1 + ylim(min(one.data$plot_Y)-one.y_range/10,max(one.data$plot_Y)+one.y_range/10);



# --------------------------------------------------------------------------
# Output the plot to screen

p1;

# Uncomment the line below to also save the plot to a file.
# The file type depends on the extension (default=pdf).

# ggsave("C:/Users/path_to_your_file/revigo-plot.pdf");
