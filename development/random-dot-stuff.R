


no_loop <- tribble(
  ~Kid, ~Pa, ~Ma,
  "W", "A", "B",
  "X", "A", "C",
  "Y", "D", "B"
)

ped2dot(
  no_loop,
  ObsNodes = unique(unlist(no_loop)),
  pfactorNodeStyle = "invis",
  pfactorEdgeStyle = "invis",
  ShowLabelNodes = unique(unlist(no_loop))
  )


with_loop <- tribble(
  ~Kid, ~Pa, ~Ma,
  "W", "A", "B",
  "X", "A", "C",
  "Y", "D", "B",
  "Z", "D", "C"
)

ped2dot(
  with_loop,
  ObsNodes = unique(unlist(with_loop)),
  pfactorNodeStyle = "invis",
  pfactorEdgeStyle = "invis",
  ShowLabelNodes = unique(unlist(with_loop)),
  outf = "no-loop-orig"
  )
