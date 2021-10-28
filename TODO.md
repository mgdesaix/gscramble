## TODO


### Coding stuff

- [x] `check_chrom_lengths()` function (see tutorial vignette line 162)
- [x] A Function to check the validity of the GSPs.
- [x] Figure out if recombination rates between adjacent markers 
will work within the existing framework.  (We could do that, but 
it would take a lot longer.  We will just leave it in large
bins)
- [x] Check out `xover()` and deal with what happens when a new breakpoint occurs exactly
at an existing one, and be explicit in the documentation about whether breaks occur
"to the left" or "to the right" of the breakpoint.  

#### Functions To Document:

- [ ] big-wrapper()
- [ ] check_chrom_lengths()
- [ ] check_gsp_for_validity_and_saturation()
- [ ] check_pedigree_for_inbreeding()
- [ ] computeQs_from_segments()
- [ ] create_GSP()
- [ ] data()
- [ ] drop-segs-down-gsp()
- [ ] full_analysis()
- [ ] gsp2dot()
- [ ] import()
- [ ] make_subscript_matrix()
- [ ] mat_scramble()
- [ ] perm_gs_by_pops()
- [ ] plot_simulated_chromosome_segments()
- [ ] prep_gsp_for_hap_dropping()
- [ ] rearrange_genos()
- [ ] recomb_point()
- [ ] seg2tib()
- [ ] seg_haps_through_gsp()
- [ ] segregate()
- [ ] sim_level_founder_haplos()
- [ ] tidy_up_sampled_haplos()
- [ ] xover()

### In the paper

- [ ] A concise mathematical description of the components of a GSP, and the constraints between them.


### For thinking

- [x] Can we come up with a framework for specifying the GSP from 
requests by the user for the desired end results (i.e. number of 
individuals of different hybrid categories.We should be able to figure this out given the constraints.

