# Code Design

## Brief sketch

General architecture/idea from previous project

- Will work very hard to make less bloated though, no unnecessary features

Although there are a few style differences:

- More generics / autoderiving
- Language extensions - in alphabetical order
- Imports - Treescript in alphabetical order, then others in alphabetical order
- Qualified imports always have shortest non-conflicting qualifiers, no special differences for "main" imported class (e.g `Text` in `Data.Text`)
- Try to put small functions above big functions in modules - still order is datatypes, classes, instances, and functions
- Modules can be bigger, but they'll still be split when too big (overall module structure still shouldn't matter much) - generally 100 lines isOK, 200 or 300 in some cases OK too
