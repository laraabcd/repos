# Differential Geometry library

Differential geometry is a fascinating subject.

Currently, trying to build a differential geometry library, based on Sage Manifolds, powerful enough to do some general relativity computations.

# API
Assuming everything is correct, this library is powerful enough to say something about the `Schwarzschild` spacetime. 

Still need to simplify stuff and make it easier to use.

This library can, with some limitations, carry out most of the sage manifolds tutorial here.

https://nbviewer.org/github/sagemanifolds/SageManifolds/blob/master/Notebooks/SM_tutorial.ipynb

and pretty much carry this out 

Schwarzchild metric:

https://nbviewer.org/github/sagemanifolds/SageManifolds/blob/master/Notebooks/SM_basic_Schwarzschild.ipynb

And 

psuedo riemann manifolds on the schwarzchild metric

(except for the geodesics)

https://nbviewer.org/github/egourgoulhon/SageMathTour/blob/master/Notebooks/demo_pseudo_Riemannian_Schwarzschild.ipynb

And i can, it seems like, define n dimensional spheres since thats just

two charts defined on positive and negative numbers. and 

a function:

f: B^n -> R

f(u) = sqrt (1 - (abs u))^2

and 
maps that determine the graph coordinates of the n-sphere

p^+-: intersection U^+- S^n -> B^n

and since i have some primitives for smooth manifolds including pseudo-riemann manifolds i can do stuff on this.
