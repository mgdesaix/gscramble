#' check that the chromosome lengths are acceptable given recombination rates
#'
#' For gscramble to work properly, all variant positions on a chromosome (found in the meta data file)
#' must be equal to or less than the total chromosome length found in the recombination map. In other words,
#' the variant positions must be within the the total length of each chromosome. The check_chrom_lengths()
#' function checks that variant positions on each chromosome do not exceed the total chromosome length from the
#' recombination map. Input files for the function include 1) the meta data file which contains 3 columns (the chromosome ID
#' as a character, the position of the variant as a numeric, and the name of the variant as a character) and
#' 2) the recombination map which contains 5 columns (the chromosome ID as a character, the total length of the chromosome
#' as a numeric, the starting position of the recombination bin as a numeric, the last position of the recombination bin as
#' a numeric, and the recombination probability for the given bin as a numeric).
#'
#' @param meta a tibble with meta data for the genotype data. It must consist of the columns (chrom, pos, variant_id).
#' @param rec a tibble with recombination map information for your study species.It must contain 5 columns (chrom, chrom_len, start_pos, end_pos, rec_prob).
#' @return This function will return a message for each chromosome stating whether the chromosome lengths are accurate
#' @export
#' @examples
#' #The example uses the built in datasets M_meta and RecRates to use the check_chrom_lengths() function
#'
#' check_chrom_lengths(M_meta,RecRates)
#'
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
  if(Flag == TRUE)stop("Exit function check_chrom_lengths")
}
