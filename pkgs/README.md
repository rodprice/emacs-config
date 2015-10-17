These are directions for setting up and using a local package archive
that I posted in emacs.stackexchange.com, 15 Oct 2015. See
http://emacs.stackexchange.com/questions/17401/how-can-i-set-up-an-elpa-server/17408#17408

--

I set up a local package archive (Emacs 24.5) using `package.el` by
first creating two directories, `pkgs` and `local`. (The names don't
matter.) The files comprising your packages go in `pkgs`, and `local`
will end up holding your archive. If you want to make an archive
available to others, you can expose `local` using a web server, but
the process is the same as building a local archive.

I put the following in my `init.el` file:

    (require 'package)
    (setq package-enable-at-startup nil)

    (require 'package-x)
    (defvar local-archive
      (expand-file-name "local/" user-emacs-directory)
      "Location of the package archive.")
    (setq package-archive-upload-base local-archive)
    (add-to-list 'package-archives `("local" . ,local-archive) t)

    (package-initialize)

I put my archive in my `.emacs.d` directory (hence the
`user-emacs-directory` in the `expand-file-name` form) but you can put
it anywhere you like.

Once this is evaluated, execute `M-x package-upload-file` and enter
the file name of your package to be installed. This will generate a
new package in `local`. You will see three new files in there,
`archive-contents`, `yourpackagename-version.el`, and
`yourpackagename-readme.txt`. Now you should be able to do `M-x
package-list-packages` and see your package listed. With luck, they'll
be at the top marked "new". You can install the package as usual, and
you will see it show up in your `.emacs.d/elpa` directory just like
any other package.

This should work out of the box for Emacs 24. I'm not sure about
earlier versions. Your users can access your package in the usual way
by including

    (require 'package)
    (setq package-enable-at-startup nil)
    (add-to-list 'package-archives 
      '("archive" . "http://yourdomain.com/path-to-local-dir"))
    (package-initialize)

in their `init.el` file.

For information on how to write a package, see
[Preparing Lisp code for distribution][1] in the GNU Emacs Lisp
manual.


  [1]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging.html#Packaging


--

These are thoughts on handling versioning issues in Emacs packaging
system, posted on emacs.stackexchange.com, 16 October 2015. See
http://emacs.stackexchange.com/questions/604/problems-installing-packages-via-elpa-that-require-cl-lib-1-0/17435#17435

Emacs' `package.el` *always* downloads the highest versioned package
it can find, and I don't know a way to stop it from doing so, in
general. You can, however, specify which repository your package comes
from, using the `package-pinned-packages` variable, available through
the customization menu for Emacs >= 24.4. I use it to "pin" packages I
use to MELPA Stable rather than MELPA, for example. If you don't, and
you update, say, `zenburn-theme`, you will always get `zenburn-theme`
version 20151003.2346 (or whatever version is most current) in MELPA
rather than version 2.2 (today's most current) from MELPA Stable. (I
have both archives in my `package-archives` list.) This way I at least
can make Emacs stick to something a package author has marked as a
stable version, not the latest commit he or she has made to Github.

This helps with versioning to a small degree, but doesn't really
answer your question, since you are trying to keep `package.el` from
downloading anything that will shadow your built-in
package. Unfortunately, `package-pinned-packages` will only let you
specify a repository listed in `package-archives`. Emacs' "built-in"
package archive isn't listed there, at least on my Emacs 24.5, so that
isn't an option for your situation. (Emacs maintainers: how about
putting a "built-in" archive in there by default?)

You say you have a function for batch install of all packages, so
presumably you don't mind writing a little lisp code. Emacs 24.5 (and
maybe earlier) `package.el` has a function `package-built-in-p` which
will let you test whether a package is built in or not. From the
documentation,

    Return true if PACKAGE is built-in to Emacs. Optional arg MIN-VERSION, 
    if non-nil, should be a version list specifying the minimum acceptable
    version.

This lets you specify a minimum acceptable version, but not a maximum
acceptable version, or even (I wish) a particular version. You can use
it to determine whether a package in your batch list is built-in or
not, and exclude it from your list if it is. Or you could just exclude
it from your list and uninstall the version you already have in your
`elpa` directory, as a previous poster suggested.

You could set up your own `local` package archive, put your preferred
version in there, and pin your package to `local`. This way you get
total control over the versioning. See
[How can I set up an Elpa server?][1] for directions. Now that I think
about it, this may be a good idea for many reasons...

EDIT: Also have a look at `package-load-list` variable. It lets you
load a particular version of a package, if you already have it
installed. See the Emacs manual [47.2 Package Installation][2] for
more detail.

[1]: http://emacs.stackexchange.com/questions/17401/how-can-i-set-up-an-elpa-server/17408#17408
[2]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html#Package-Installation
