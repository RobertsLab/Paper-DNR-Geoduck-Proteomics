

# A treemap R script produced by the REVIGO server at http://revigo.irb.hr/
# If you found REVIGO useful in your work, please cite the following reference:
# Supek F et al. "REVIGO summarizes and visualizes long lists of Gene Ontology
# terms" PLoS ONE 2011. doi:10.1371/journal.pone.0021800

# author: Anton Kratz <anton.kratz@gmail.com>, RIKEN Omics Science Center, Functional Genomics Technology Team, Japan
# created: Fri, Nov 02, 2012  7:25:52 PM
# last change: Fri, Nov 09, 2012  3:20:01 PM

# -----------------------------------------------------------------------------
# If you don't have the treemap package installed, uncomment the following line:
# install.packages( "treemap" );
library(treemap) 								# treemap package by Martijn Tennekes

# Set the working directory if necessary
# setwd("C:/Users/username/workingdir");

# --------------------------------------------------------------------------
# Here is your data from REVIGO. Scroll down for plot configuration options.

revigo.names <- c("term_ID","description","freqInDbPercent","abslog10pvalue","uniqueness","dispensability","representative");
revigo.data <- rbind(c("GO:0000003","reproduction",0.769,2.3042,1.000,0.000,"reproduction"),
c("GO:0008152","metabolic process",75.387,6.4547,0.998,0.000,"metabolism"),
c("GO:0008380","RNA splicing",0.413,18.4078,0.875,0.000,"RNA splicing"),
c("GO:0008544","epidermis development",0.058,1.1945,0.881,0.660,"RNA splicing"),
c("GO:0060414","aorta smooth muscle tissue morphogenesis",0.001,2.1761,0.842,0.629,"RNA splicing"),
c("GO:0051764","actin crosslink formation",0.004,1.7416,0.739,0.592,"RNA splicing"),
c("GO:0018279","protein N-linked glycosylation via asparagine",0.015,1.5018,0.806,0.577,"RNA splicing"),
c("GO:0006337","nucleosome disassembly",0.010,1.2496,0.765,0.588,"RNA splicing"),
c("GO:0035999","tetrahydrofolate interconversion",0.129,2.6888,0.779,0.418,"RNA splicing"),
c("GO:0051291","protein heterooligomerization",0.013,1.5008,0.807,0.509,"RNA splicing"),
c("GO:0051292","nuclear pore complex assembly",0.005,1.6606,0.800,0.506,"RNA splicing"),
c("GO:0009083","branched-chain amino acid catabolic process",0.034,3.1385,0.783,0.610,"RNA splicing"),
c("GO:0000413","protein peptidyl-prolyl isomerization",0.393,3.2418,0.902,0.309,"RNA splicing"),
c("GO:0071709","membrane assembly",0.095,1.2021,0.719,0.561,"RNA splicing"),
c("GO:0000381","regulation of alternative mRNA splicing, via spliceosome",0.017,5.6716,0.811,0.692,"RNA splicing"),
c("GO:0008652","cellular amino acid biosynthetic process",2.932,1.3150,0.733,0.677,"RNA splicing"),
c("GO:0003374","dynamin family protein polymerization involved in mitochondrial fission",0.002,2.6586,0.753,0.485,"RNA splicing"),
c("GO:0000904","cell morphogenesis involved in differentiation",0.188,3.1959,0.664,0.674,"RNA splicing"),
c("GO:0043044","ATP-dependent chromatin remodeling",0.046,1.7459,0.811,0.489,"RNA splicing"),
c("GO:0010501","RNA secondary structure unwinding",0.025,4.2366,0.915,0.224,"RNA splicing"),
c("GO:0033962","cytoplasmic mRNA processing body assembly",0.011,1.6863,0.789,0.683,"RNA splicing"),
c("GO:0000463","maturation of LSU-rRNA from tricistronic rRNA transcript (SSU-rRNA, 5.8S rRNA, LSU-rRNA)",0.037,2.3290,0.760,0.535,"RNA splicing"),
c("GO:0031532","actin cytoskeleton reorganization",0.028,1.5086,0.715,0.669,"RNA splicing"),
c("GO:0002181","cytoplasmic translation",0.064,3.7375,0.872,0.429,"RNA splicing"),
c("GO:0018105","peptidyl-serine phosphorylation",0.087,2.1816,0.887,0.658,"RNA splicing"),
c("GO:0006552","leucine catabolic process",0.003,1.8285,0.808,0.436,"RNA splicing"),
c("GO:0045104","intermediate filament cytoskeleton organization",0.009,1.4241,0.740,0.528,"RNA splicing"),
c("GO:0007077","mitotic nuclear envelope disassembly",0.000,1.0882,0.776,0.679,"RNA splicing"),
c("GO:0051014","actin filament severing",0.011,2.1761,0.832,0.608,"RNA splicing"),
c("GO:0051016","barbed-end actin filament capping",0.022,4.1141,0.620,0.651,"RNA splicing"),
c("GO:0016575","histone deacetylation",0.048,1.1809,0.771,0.649,"RNA splicing"),
c("GO:0045616","regulation of keratinocyte differentiation",0.006,1.0433,0.739,0.631,"RNA splicing"),
c("GO:0006631","fatty acid metabolic process",0.878,1.2779,0.776,0.607,"RNA splicing"),
c("GO:0006635","fatty acid beta-oxidation",0.080,6.1198,0.778,0.551,"RNA splicing"),
c("GO:0006417","regulation of translation",0.692,6.7620,0.767,0.528,"RNA splicing"),
c("GO:0006418","tRNA aminoacylation for protein translation",1.099,1.4343,0.715,0.570,"RNA splicing"),
c("GO:0006413","translational initiation",0.518,12.1605,0.852,0.243,"RNA splicing"),
c("GO:0006396","RNA processing",3.210,1.0130,0.872,0.346,"RNA splicing"),
c("GO:0006397","mRNA processing",0.561,17.1612,0.853,0.624,"RNA splicing"),
c("GO:0001736","establishment of planar polarity",0.019,1.6700,0.833,0.468,"RNA splicing"),
c("GO:0051603","proteolysis involved in cellular protein catabolic process",0.759,6.5751,0.860,0.329,"RNA splicing"),
c("GO:0038083","peptidyl-tyrosine autophosphorylation",0.011,1.0033,0.900,0.566,"RNA splicing"),
c("GO:0001731","formation of translation preinitiation complex",0.070,8.8697,0.703,0.432,"RNA splicing"),
c("GO:0006471","protein ADP-ribosylation",0.019,1.3150,0.807,0.661,"RNA splicing"),
c("GO:0045214","sarcomere organization",0.013,4.4828,0.639,0.444,"RNA splicing"),
c("GO:0000244","spliceosomal tri-snRNP complex assembly",0.010,1.0882,0.735,0.680,"RNA splicing"),
c("GO:0051639","actin filament network formation",0.003,1.3085,0.744,0.581,"RNA splicing"),
c("GO:0009629","response to gravity",0.006,1.2021,0.951,0.000,"response to gravity"),
c("GO:0040011","locomotion",0.997,3.4498,0.994,0.000,"locomotion"),
c("GO:0051028","mRNA transport",0.075,8.6364,0.847,0.000,"mRNA transport"),
c("GO:0015991","ATP hydrolysis coupled proton transport",0.163,3.7773,0.822,0.210,"mRNA transport"),
c("GO:0034394","protein localization to cell surface",0.013,1.1945,0.848,0.612,"mRNA transport"),
c("GO:0006851","mitochondrial calcium ion transport",0.005,2.1931,0.916,0.407,"mRNA transport"),
c("GO:0071896","protein localization to adherens junction",0.001,1.6814,0.886,0.368,"mRNA transport"),
c("GO:0032880","regulation of protein localization",0.219,2.4970,0.760,0.672,"mRNA transport"),
c("GO:0006897","endocytosis",0.235,3.2306,0.876,0.271,"mRNA transport"),
c("GO:0006886","intracellular protein transport",1.199,4.9547,0.790,0.584,"mRNA transport"),
c("GO:0006890","retrograde vesicle-mediated transport, Golgi to ER",0.047,1.8187,0.886,0.652,"mRNA transport"),
c("GO:0007269","neurotransmitter secretion",0.043,2.4011,0.672,0.660,"mRNA transport"),
c("GO:0016192","vesicle-mediated transport",1.085,3.6716,0.896,0.261,"mRNA transport"),
c("GO:1904871","positive regulation of protein localization to Cajal body",0.002,2.6818,0.762,0.668,"mRNA transport"),
c("GO:1901998","toxin transport",0.008,5.1904,0.923,0.172,"mRNA transport"),
c("GO:2000969","positive regulation of alpha-amino-3-hydroxy-5-methyl-4-isoxazole propionate selective glutamate receptor activity",0.001,1.3085,0.754,0.558,"mRNA transport"),
c("GO:0097120","receptor localization to synapse",0.004,4.0635,0.870,0.159,"mRNA transport"),
c("GO:0015813","L-glutamate transport",0.023,1.4241,0.833,0.509,"mRNA transport"),
c("GO:0000059","protein import into nucleus, docking",0.013,4.2411,0.646,0.613,"mRNA transport"),
c("GO:0045053","protein retention in Golgi apparatus",0.007,1.8965,0.701,0.586,"mRNA transport"),
c("GO:0061512","protein localization to cilium",0.010,3.2314,0.843,0.601,"mRNA transport"),
c("GO:0002576","platelet degranulation",0.003,1.2496,0.795,0.586,"mRNA transport"),
c("GO:1903543","positive regulation of exosomal secretion",0.003,2.6586,0.650,0.543,"mRNA transport"),
c("GO:0006909","phagocytosis",0.051,2.1145,0.811,0.656,"mRNA transport"),
c("GO:0044351","macropinocytosis",0.002,2.0103,0.900,0.525,"mRNA transport"),
c("GO:0098609","cell-cell adhesion",0.251,14.8962,0.949,0.000,"cell-cell adhesion"),
c("GO:0016339","calcium-dependent cell-cell adhesion via plasma membrane cell adhesion molecules",0.004,2.0136,0.956,0.643,"cell-cell adhesion"),
c("GO:0033627","cell adhesion mediated by integrin",0.013,1.6606,0.956,0.695,"cell-cell adhesion"),
c("GO:0044331","cell-cell adhesion mediated by cadherin",0.003,1.3085,0.903,0.637,"cell-cell adhesion"),
c("GO:0006807","nitrogen compound metabolic process",38.744,1.0661,0.985,0.022,"nitrogen compound metabolism"),
c("GO:0061077","chaperone-mediated protein folding",0.043,2.8176,0.957,0.025,"chaperone-mediated protein folding"),
c("GO:0042026","protein refolding",0.069,2.6586,0.956,0.621,"chaperone-mediated protein folding"),
c("GO:0007023","post-chaperonin tubulin folding pathway",0.020,2.1931,0.959,0.573,"chaperone-mediated protein folding"),
c("GO:0016032","viral process",0.131,3.3316,0.920,0.027,"viral process"),
c("GO:0007339","binding of sperm to zona pellucida",0.007,2.3290,0.848,0.600,"viral process"),
c("GO:0007018","microtubule-based movement",0.287,7.9245,0.776,0.029,"microtubule-based movement"),
c("GO:0000281","mitotic cytokinesis",0.070,2.1383,0.859,0.143,"microtubule-based movement"),
c("GO:0055088","lipid homeostasis",0.041,2.2467,0.888,0.641,"microtubule-based movement"),
c("GO:1903608","protein localization to cytoplasmic stress granule",0.002,1.0433,0.857,0.463,"microtubule-based movement"),
c("GO:0034976","response to endoplasmic reticulum stress",0.100,2.8010,0.908,0.295,"microtubule-based movement"),
c("GO:0006268","DNA unwinding involved in DNA replication",0.058,2.0308,0.753,0.522,"microtubule-based movement"),
c("GO:0000266","mitochondrial fission",0.022,1.6247,0.810,0.636,"microtubule-based movement"),
c("GO:0016322","neuron remodeling",0.005,1.0882,0.776,0.560,"microtubule-based movement"),
c("GO:0034332","adherens junction organization",0.023,1.1191,0.777,0.361,"microtubule-based movement"),
c("GO:0019243","methylglyoxal catabolic process to D-lactate via S-lactoyl-glutathione",0.021,1.0433,0.798,0.676,"microtubule-based movement"),
c("GO:0050821","protein stabilization",0.045,2.9108,0.892,0.525,"microtubule-based movement"),
c("GO:0045887","positive regulation of synaptic growth at neuromuscular junction",0.002,1.0433,0.631,0.657,"microtubule-based movement"),
c("GO:0055114","oxidation-reduction process",15.060,1.9419,0.867,0.232,"microtubule-based movement"),
c("GO:0042073","intraciliary transport",0.019,3.4622,0.574,0.692,"microtubule-based movement"),
c("GO:1990090","cellular response to nerve growth factor stimulus",0.004,1.0661,0.914,0.651,"microtubule-based movement"),
c("GO:0007266","Rho protein signal transduction",0.137,1.4343,0.818,0.360,"microtubule-based movement"),
c("GO:0050790","regulation of catalytic activity",1.575,3.1349,0.888,0.283,"microtubule-based movement"),
c("GO:0006172","ADP biosynthetic process",0.008,1.3085,0.791,0.564,"microtubule-based movement"),
c("GO:0071364","cellular response to epidermal growth factor stimulus",0.005,2.0464,0.913,0.245,"microtubule-based movement"),
c("GO:0000972","transcription-dependent tethering of RNA polymerase II gene DNA at nuclear periphery",0.006,1.6814,0.825,0.330,"microtubule-based movement"),
c("GO:0019388","galactose catabolic process",0.005,1.0433,0.855,0.634,"microtubule-based movement"),
c("GO:0030212","hyaluronan metabolic process",0.008,1.4241,0.914,0.374,"microtubule-based movement"),
c("GO:0012501","programmed cell death",0.430,1.9268,0.837,0.183,"microtubule-based movement"),
c("GO:0045954","positive regulation of natural killer cell mediated cytotoxicity",0.004,2.6818,0.806,0.345,"microtubule-based movement"),
c("GO:0007229","integrin-mediated signaling pathway",0.056,3.5817,0.810,0.221,"microtubule-based movement"),
c("GO:0007224","smoothened signaling pathway",0.038,1.1773,0.815,0.605,"microtubule-based movement"),
c("GO:0071404","cellular response to low-density lipoprotein particle stimulus",0.003,1.0433,0.916,0.540,"microtubule-based movement"),
c("GO:0009617","response to bacterium",0.145,1.2496,0.933,0.667,"microtubule-based movement"),
c("GO:0071407","cellular response to organic cyclic compound",0.172,1.2904,0.897,0.618,"microtubule-based movement"),
c("GO:0032968","positive regulation of transcription elongation from RNA polymerase II promoter",0.029,1.4343,0.800,0.435,"microtubule-based movement"),
c("GO:0070814","hydrogen sulfide biosynthetic process",0.058,1.7416,0.926,0.616,"microtubule-based movement"),
c("GO:0008053","mitochondrial fusion",0.017,1.0882,0.725,0.637,"microtubule-based movement"),
c("GO:0019886","antigen processing and presentation of exogenous peptide antigen via MHC class II",0.002,2.3566,0.973,0.490,"microtubule-based movement"),
c("GO:0019344","cysteine biosynthetic process",0.131,2.1931,0.774,0.430,"microtubule-based movement"),
c("GO:1900034","regulation of cellular response to heat",0.004,2.2811,0.865,0.557,"microtubule-based movement"),
c("GO:0006730","one-carbon metabolic process",0.328,1.9276,0.815,0.289,"microtubule-based movement"),
c("GO:0034454","microtubule anchoring at centrosome",0.003,1.0433,0.737,0.617,"microtubule-based movement"),
c("GO:0045454","cell redox homeostasis",0.861,6.3458,0.749,0.176,"microtubule-based movement"),
c("GO:0030317","flagellated sperm motility",0.017,1.9888,0.784,0.631,"microtubule-based movement"),
c("GO:0007163","establishment or maintenance of cell polarity",0.099,1.3150,0.858,0.146,"microtubule-based movement"),
c("GO:0010510","regulation of acetyl-CoA biosynthetic process from pyruvate",0.001,1.2021,0.788,0.479,"microtubule-based movement"),
c("GO:0046034","ATP metabolic process",1.263,3.8356,0.723,0.329,"microtubule-based movement"),
c("GO:0007173","epidermal growth factor receptor signaling pathway",0.025,1.6111,0.815,0.589,"microtubule-based movement"),
c("GO:0045647","negative regulation of erythrocyte differentiation",0.002,1.0433,0.713,0.565,"microtubule-based movement"),
c("GO:0031047","gene silencing by RNA",0.089,1.3461,0.766,0.225,"microtubule-based movement"),
c("GO:0006048","UDP-N-acetylglucosamine biosynthetic process",0.025,2.6586,0.886,0.408,"microtubule-based movement"),
c("GO:0051489","regulation of filopodium assembly",0.009,1.3104,0.693,0.582,"microtubule-based movement"),
c("GO:0016559","peroxisome fission",0.023,2.2513,0.812,0.361,"microtubule-based movement"),
c("GO:0010457","centriole-centriole cohesion",0.004,1.3085,0.735,0.623,"microtubule-based movement"),
c("GO:1904851","positive regulation of establishment of protein localization to telomere",0.002,2.1931,0.763,0.380,"microtubule-based movement"),
c("GO:0097119","postsynaptic density protein 95 clustering",0.001,1.3085,0.678,0.449,"microtubule-based movement"),
c("GO:0051561","positive regulation of mitochondrial calcium ion concentration",0.002,2.1761,0.824,0.684,"microtubule-based movement"),
c("GO:0006120","mitochondrial electron transport, NADH to ubiquinone",0.019,2.1931,0.777,0.657,"microtubule-based movement"),
c("GO:0051560","mitochondrial calcium ion homeostasis",0.006,2.6586,0.814,0.598,"microtubule-based movement"),
c("GO:0051983","regulation of chromosome segregation",0.098,1.0433,0.811,0.232,"microtubule-based movement"),
c("GO:0006107","oxaloacetate metabolic process",0.015,2.6586,0.834,0.313,"microtubule-based movement"),
c("GO:0032060","bleb assembly",0.003,3.1355,0.758,0.310,"microtubule-based movement"),
c("GO:0006105","succinate metabolic process",0.015,1.7416,0.834,0.515,"microtubule-based movement"),
c("GO:1990966","ATP generation from poly-ADP-D-ribose",0.001,1.3085,0.819,0.531,"microtubule-based movement"),
c("GO:0006103","2-oxoglutarate metabolic process",0.020,2.1378,0.831,0.525,"microtubule-based movement"),
c("GO:0006098","pentose-phosphate shunt",0.287,2.3290,0.751,0.663,"microtubule-based movement"),
c("GO:0006099","tricarboxylic acid cycle",0.469,6.2596,0.775,0.185,"microtubule-based movement"),
c("GO:0006094","gluconeogenesis",0.262,2.7998,0.837,0.283,"microtubule-based movement"),
c("GO:0006081","cellular aldehyde metabolic process",0.753,1.3085,0.829,0.194,"microtubule-based movement"),
c("GO:0032091","negative regulation of protein binding",0.017,1.6187,0.913,0.621,"microtubule-based movement"),
c("GO:0001778","plasma membrane repair",0.002,1.7416,0.757,0.331,"microtubule-based movement"),
c("GO:0060294","cilium movement involved in cell motility",0.004,2.2513,0.777,0.623,"microtubule-based movement"),
c("GO:0007010","cytoskeleton organization",0.786,2.4031,0.768,0.490,"microtubule-based movement"),
c("GO:0060285","cilium-dependent cell motility",0.006,1.3104,0.795,0.591,"microtubule-based movement"),
c("GO:0006997","nucleus organization",0.052,1.5633,0.805,0.516,"microtubule-based movement"),
c("GO:0051592","response to calcium ion",0.018,2.0177,0.942,0.366,"microtubule-based movement"),
c("GO:0038095","Fc-epsilon receptor signaling pathway",0.003,1.1783,0.823,0.629,"microtubule-based movement"),
c("GO:0070050","neuron cellular homeostasis",0.003,1.1792,0.822,0.575,"microtubule-based movement"),
c("GO:0010827","regulation of glucose transport",0.013,1.2496,0.774,0.495,"microtubule-based movement"),
c("GO:0051103","DNA ligation involved in DNA repair",0.039,1.1191,0.866,0.463,"microtubule-based movement"),
c("GO:0098792","xenophagy",0.002,1.2921,0.918,0.490,"microtubule-based movement"),
c("GO:0042769","DNA damage response, detection of DNA damage",0.001,1.0882,0.927,0.373,"microtubule-based movement"),
c("GO:0006002","fructose 6-phosphate metabolic process",0.060,1.3085,0.907,0.444,"microtubule-based movement"),
c("GO:0072499","photoreceptor cell axon guidance",0.001,1.6814,0.654,0.538,"microtubule-based movement"),
c("GO:0071526","semaphorin-plexin signaling pathway",0.019,1.5822,0.821,0.578,"microtubule-based movement"),
c("GO:0030030","cell projection organization",0.608,3.0128,0.727,0.190,"microtubule-based movement"),
c("GO:0045197","establishment or maintenance of epithelial cell apical/basal polarity",0.008,2.6116,0.877,0.123,"microtubule-based movement"),
c("GO:0021785","branchiomotor neuron axon guidance",0.002,1.1191,0.651,0.658,"microtubule-based movement"),
c("GO:0030593","neutrophil chemotaxis",0.015,1.2496,0.729,0.625,"microtubule-based movement"),
c("GO:0006457","protein folding",0.903,13.5513,0.957,0.032,"protein folding"),
c("GO:0002119","nematode larval development",0.015,3.3706,0.836,0.056,"nematode larval development"),
c("GO:0050885","neuromuscular process controlling balance",0.012,1.7190,0.915,0.410,"nematode larval development"),
c("GO:0016319","mushroom body development",0.004,2.0308,0.834,0.433,"nematode larval development"),
c("GO:0021675","nerve development",0.016,1.0433,0.825,0.542,"nematode larval development"),
c("GO:0021670","lateral ventricle development",0.003,1.4241,0.837,0.633,"nematode larval development"),
c("GO:0021591","ventricular system development",0.007,1.0726,0.829,0.670,"nematode larval development"),
c("GO:0021517","ventral spinal cord development",0.012,1.1792,0.825,0.651,"nematode larval development"),
c("GO:0043051","regulation of pharyngeal pumping",0.002,1.2021,0.921,0.574,"nematode larval development"),
c("GO:0007629","flight behavior",0.001,2.1761,0.872,0.394,"nematode larval development"),
c("GO:0009792","embryo development ending in birth or egg hatching",0.155,1.9803,0.809,0.537,"nematode larval development"),
c("GO:0008340","determination of adult lifespan",0.020,1.8086,0.834,0.547,"nematode larval development"),
c("GO:0005975","carbohydrate metabolic process",5.260,2.6428,0.961,0.068,"carbohydrate metabolism"),
c("GO:0010388","cullin deneddylation",0.000,1.7416,0.935,0.077,"cullin deneddylation"));

stuff <- data.frame(revigo.data);
names(stuff) <- revigo.names;

stuff$abslog10pvalue <- as.numeric( as.character(stuff$abslog10pvalue) );
stuff$freqInDbPercent <- as.numeric( as.character(stuff$freqInDbPercent) );
stuff$uniqueness <- as.numeric( as.character(stuff$uniqueness) );
stuff$dispensability <- as.numeric( as.character(stuff$dispensability) );

# by default, outputs to a PDF file
pdf( file="revigo_treemap.pdf", width=16, height=9 ) # width and height are in inches

# check the tmPlot command documentation for all possible parameters - there are a lot more
tmPlot(
	stuff,
	index = c("representative","description"),
	vSize = "abslog10pvalue",
	type = "categorical",
	vColor = "representative",
	title = "REVIGO Gene Ontology treemap",
	inflate.labels = FALSE,      # set this to TRUE for space-filling group labels - good for posters
	lowerbound.cex.labels = 0,   # try to draw as many labels as possible (still, some small squares may not get a label)
	bg.labels = "#CCCCCCAA",     # define background color of group labels
												       # "#CCCCCC00" is fully transparent, "#CCCCCCAA" is semi-transparent grey, NA is opaque
	position.legend = "none"
)

dev.off()
