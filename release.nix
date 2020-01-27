# Used for building and testing on build servers like Hydra
{
  inherit ((import ./.).lazy-lambda-calculus.components)
    exes
    library
    tests
    ;
}
