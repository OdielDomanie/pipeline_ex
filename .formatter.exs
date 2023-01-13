# Used by "mix format"
export_locals_without_parens = [
  ~>: 2,
  ~>>: 2,
  merge: 1,
  merge: 2,
  switch: 2,
  parallel: 2,
  parallel: 3,
  split: 2,
  only: 3
]

[
  inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
  locals_without_parens: export_locals_without_parens,
  export: [locals_without_parens: export_locals_without_parens]
]
