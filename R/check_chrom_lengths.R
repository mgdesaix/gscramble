############ function for checking chromosome lengths ###################
###### Compares the last position on each chromosome (found in meta data file)
###### to the chromosome length (found in recombination map chrom_len)

#' check that the chromosome lengths are acceptable given recombination rates
#'
#' Needs more documenting.
#' @export
check_chrom_lengths<-function(meta,rec){
  Flag<-FALSE
  chrom_list<-unique(meta$chrom)

  for(i in 1:length(chrom_list)){

    meta_last_pos<-max(meta$pos[which(meta$chrom == chrom_list[i])])
    chrom_length<-rec$chrom_len[which(rec$chrom == chrom_list[i])[1]]

    if(chrom_length < meta_last_pos){
      print(paste("Length of chrom ", chrom_list[i]," from recombination map has to be equal to or exceed the final position from the meta file" ))
      Flag <- TRUE
    }
    if(chrom_length >= meta_last_pos){
      print(paste("The length of chromosome ", chrom_list[i], " is formatted properly"))
    }

  }
  if(Flag == TRUE){stop("Exit function check_chrom_lengths")}
}
