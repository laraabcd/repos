i want to see how such compiler is implemented with a dag.

but not sure if this is how the dag would be implemented.

in the dragon book, the authors teach that a dag is created just like an
ast but without allocating exisiting nodes.

the way i did it here its not exactly like that but good enough to do
dfs and topological sort so might be enough to do some basic
optimizations.