pdrt-sandbox (with sdrt)
============

Synopsis
--------
[(Projective) Discourse Representation
Theory](http://hbrouwer.github.io/pdrt-sandbox/)

Description
-----------

This is an implementation of the formal framework of Projective Discourse
Representation Theory (Venhuizen et al. 2013; 2014), which is an extension
of standard Discourse Representation Theory (Kamp 1981; Kamp & Reyle 1993)
with projection pointers.

The implementation includes a translation from PDRT to DRT and First-order
Logic, composition via different types of merge, and unresolved structures
based on Montague Semantics, defined as Haskell functions.

Furthermore, this implementation includes a preliminary version of Segmented Discourse Representation Theory (Asher & Lascarides 2003). This theory, SDRT for short, is an extension of DRT adding a rhetorical structure on top of DRT formalizations. This allows to make predictions about felicitous attachment of new content to the discourse and about correct binding of discourse referents. 

References
----------

* H. Kamp. *A theory of truth and semantic representation*, 1981.

* H. Kamp and U. Reyle. *From discourse to logic: Introduction to
  model-theoretic semantics of natural language, formal logic and Discourse
  Representation Theory*, 1993.

* N. J. Venhuizen, J. Bos and H. Brouwer. *Parsimonious semantic
  representations with projection pointers*, 2013.
                     
* N. J. Venhuizen, J. Bos, P. Hendriks and H. Brouwer. *How and why
  conventional implicatures project*, 2014.

* N. Asher and A. Lascarides. *Logics of Conversation*, Cambridge University Press, 2003

License
-------

The library is available under the [Apache License, Version
2.0](http://www.apache.org/licenses/LICENSE-2.0.html).
