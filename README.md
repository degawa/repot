# repot
Abstract data types designed for the repository pattern for reading configuration files in Fortran

The repository pattern is a design pattern that makes an object persistent in a repository and reconstructs it from the repository. I understand that the repository pattern abstracts files or databases and encapsulates operations on them.

Based on that understanding, this and [another repository](https://github.com/degawa/repot_examples.git) provide a practical example of the repository pattern. This repository defines user-defined types for reading content from a repository, an abstracted file. The intended use is to extend one of the abstract data types depending on a problem.

- `base_repository_atype` can read scalar values of
    - 4-byte integer number
    - 8-type real number
- `repository_atype` can read scalar values of
    - 4-byte integer number
    - 8-type real number
    - logical value
    - string
- `detailed_repository_atype` can read scalar values and arrays of
    - 4-byte integer number
    - 8-type real number
    - logical value
    - string

Of course, using these types is unnecessary, and it is more efficient to define user-defined types according to the data format required by the target problem.

`repository_constructor_arguments_type` is used to obtain the file format and name for constructing the user-defined repository types.

## Getting started
### Requirements
- Modern Fortran compiler
- [Fortran Package Manager](https://github.com/fortran-lang/fpm) (fpm) 0.7.0 alpha
- [test-drive](https://github.com/fortran-lang/test-drive) 0.4.0

### Reference as a fpm project's dependency
To use (extend abstract types provided by repot) in your fpm project, add the following to the fpm.toml.

```TOML
[dependencies]
repot = {git = "https://github.com/degawa/repot.git"}
```

### Reference from your project
Add the following `use` statement to modules extending abstract types.

```Fortran
use :: repot
```
