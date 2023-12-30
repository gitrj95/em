                 ______________________________________

                  EM: A MINIMALIST EMACS CONFIGURATION

                         Ramakrishnan Jayaraman
                 ______________________________________


Table of Contents
_________________

1. Configure
.. 1. Elisp
.. 2. Automatic theme selection
2. Install


*em* is a lightweight Emacs configuration that strives to still be
feature-rich.

The configuration aims to satisfy some criteria:
- modern: should have rich aesthetic appeal, especially in GUI Emacs.
- fast: after installation, boots should be easily under 1 second.
- close-to-core: stick as close to core Emacs as possible.
- unobtrusive: keep visual noise away.
- semantically consistent: there's only one interface/method for doing
  something, /e.g./ there is one mode of completion via the minibuffer.


1 Configure
===========

1.1 Elisp
~~~~~~~~~

  if you want to exercise your elisp-fu, just add whatever `.el' or
  `.elc' files to `etc/'.


1.2 Automatic theme selection
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Themes are automatically set on sunrise and sunset. Set
  `calendar-latitude' and `calendar-longitude' in `early-init.el' for
  accurate timings.


2 Install
=========

  Inspect the `Makefile'. The implementation is trivial.
