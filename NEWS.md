# ss3diags 2.0.0 

* ss3iags is now installed as an R-package. Package documentation is upatded to reflect these chenges. 
  * A simple, cod-like, Stock Synthesis model, simulated via ss3sim, replaces Pacific North Hake (`pac.hke`) and North Atlantic Shortfin Mako Shark (`natl.sma`) example datasets.
* SSplotRetro: fixed bug so that the shading in uncertainty area shows up when xlims are specified.
* SSplotJABBAres: added 'con' option to subplots argument so conditional age-at-length data can be plotted. Also added a seas argument so user can specify if data should be combined across seasons within a year or kept separate. 
* SScompsTA1.8: added a seas argument so users can specify if data should be combined across seasons within a year or kept separate.
* SSplot functions (SSplotModelComp, SSplotEnsemble, SSplotHCxval, SSplotJABBAres, SSplotRetro): Marked `plot`, `png`, `pdf`, `print`, and `new` as deprecated. They will be defunct in a future version.
* Added a `NEWS.md` file to track changes to the package.

# ss3diags 1.0.8

* Fixed MASE

# ss3diags 1.0.7

* Bug fixes

# ss3diags 1.0.6

* Improved SSdiagsMCMC

# ss3diags 1.0.5

* Updated SSdeltaMVLN

# ss3diags 1.0.4 

* Reference: authors changes 

# ss3diags 1.0.3

* Added annF_ quantaties

# ss3diags 1.0.2

* Added aut

# ss3diags 1.0.1

* Added SSplotH
