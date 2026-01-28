package common.rich.path.ref.io

// A witness type to restrict certain types to this package only, since using sealed means all
// class would have to be in the same file.
private sealed trait PackageWitness
private object PackageWitness extends PackageWitness
