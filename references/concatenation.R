#R script to concatenate multiple aligned sequences in fasta files #into a single fasta file
#requires the seqinr package
#list to store concatenated aligned sequences
#Author: T.F.Khang
#Last update: 18 September 2016

concat <- function(seq1, seq2){

both <- sort(union ( names(seq1), names(seq2) ))
seqjoin <- vector("list", length(both))
names(seqjoin) <- both

for(i in 1:length(both)){
	#find the index of the matching sequence in the other fasta file
	idmatch_seq2 <- which(names(seq2) == both[i])
	idmatch_seq1 <- which(names(seq1) == both[i])

	if(length(idmatch_seq2) > 0 & length(idmatch_seq1) > 0){
	#join the two aligned sequences of the same species
	seqjoin[[i]] <- c(seq2[[idmatch_seq2]],seq1[[idmatch_seq1]])

	}
	else if(length(idmatch_seq2) > 0 & length(idmatch_seq1) == 0){
	seqjoin[[i]] <- c(seq2[[idmatch_seq2]], rep("-",length(seq1[[1]])))
	
	}

	else if(length(idmatch_seq2) == 0 & length(idmatch_seq1) > 0){
	seqjoin[[i]] <- c(rep("-",length(seq2[[1]])), seq1[[idmatch_seq1]])
	}

	}

#coerce into a SeqFastadna object, so that write.fasta can be applied
seqjoin <- lapply(seqjoin, as.SeqFastadna)

return(seqjoin)

}

#Example:
##set directory to the one containing the aligned fasta files
#ITS <- read.fasta(file="ITS.fas")
#COI <- read.fasta(file="COI.fas")
#rRNA28S <- read.fasta(file="rRNA28S.fas")
#
#B <- concat(rRNA28S, ITS)
#B <- concat(B, COI)
#write.fasta(B, names(B), file="Dugesia_concat.fasta")

