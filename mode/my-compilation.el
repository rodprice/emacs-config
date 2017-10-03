;;; my-compilation.el --- Better compilation regexps  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Rodney Price

;; Author: Rodney Price <rod@kobe>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixed compilation buffer regexps

(defvar my-compilation-error-regexp-alist
  '(
    absoft
    ada
    aix
    ant
    bash
    borland
    python-tracebacks-and-caml
    comma
    cucumber
    edg-1
    epc
    iar
    ibm
    java
    jikes-file
    maven
    jikes-line
    gcc-include
    ruby-Test::Unit
    gnu
    lcc
    makepp
    mips-1
    mips-2
    msft
    omake
    oracle
    perl
    php
    rxp
    sparc-pascal-file
    sparc-pascal-line
    sparc-pascal-example
    sun
    sun-ada
    watcom
    4bsd
    gcov-file
    gcov-header
    gcov-nomark
    gcov-called-line
    gcov-never-called
    perl--Pod::Checker
    perl--Test
    perl--Test2
    perl--Test::Harness
    weblint
    )
  )


;; Straight from compile.c
(defvar my-compilation-error-regexp-alist-alist
  '((absoft
     "^\\(?:[Ee]rror on \\|[Ww]arning on\\( \\)\\)?[Ll]ine[ \t]+\\([0-9]+\\)[ \t]+\
of[ \t]+\"?\\([a-zA-Z]?:?[^\":\n]+\\)\"?:" 3 2 nil (1))

    (ada
     "\\(warning: .*\\)? at \\([^ \n]+\\):\\([0-9]+\\)$" 2 3 nil (1))

    (aix
     " in line \\([0-9]+\\) of file \\([^ \n]+[^. \n]\\)\\.? " 2 1)

    (ant
     "^[ \t]*\\[[^] \n]+\\][ \t]*\\([^: \n]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\):\\)?\
\\( warning\\)?" 1 (2 . 4) (3 . 5) (6))

    (bash
     "^\\([^: \n\t]+\\): line \\([0-9]+\\):" 1 2)

    (borland
     "^\\(?:Error\\|Warnin\\(g\\)\\) \\(?:[FEW][0-9]+ \\)?\
\\([a-zA-Z]?:?[^:( \t\n]+\\)\
 \\([0-9]+\\)\\(?:[) \t]\\|:[^0-9\n]\\)" 2 3 nil (1))

    (python-tracebacks-and-caml
     "^[ \t]*File \\(\"?\\)\\([^,\" \n\t<>]+\\)\\1, lines? \\([0-9]+\\)-?\\([0-9]+\\)?\\(?:$\\|,\
\\(?: characters? \\([0-9]+\\)-?\\([0-9]+\\)?:\\)?\\([ \n]Warning\\(?: [0-9]+\\)?:\\)?\\)"
     2 (3 . 4) (5 . 6) (7))

    (comma
     "^\"\\([^,\" \n\t]+\\)\", line \\([0-9]+\\)\
\\(?:[(. pos]+\\([0-9]+\\))?\\)?[:.,; (-]\\( warning:\\|[-0-9 ]*(W)\\)?" 1 2 3 (4))

    (cucumber
     "\\(?:^cucumber\\(?: -p [^[:space:]]+\\)?\\|#\\)\
\\(?: \\)\\([^\(].*\\):\\([1-9][0-9]*\\)" 1 2)

    (msft
     ;; Must be before edg-1, so that MSVC's longer messages are
     ;; considered before EDG.
     ;; The message may be a "warning", "error", or "fatal error" with
     ;; an error code, or "see declaration of" without an error code.
     "^ *\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) ?\
: \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
     2 3 nil (4))

    (edg-1
     "^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
     1 2 nil (3 . 4))
    (edg-2
     "at line \\([0-9]+\\) of \"\\([^ \n]+\\)\"$"
     2 1 nil 0)

    (epc
     "^Error [0-9]+ at (\\([0-9]+\\):\\([^)\n]+\\))" 2 1)

    (ftnchek
     "\\(^Warning .*\\)? line[ \n]\\([0-9]+\\)[ \n]\\(?:col \\([0-9]+\\)[ \n]\\)?file \\([^ :;\n]+\\)"
     4 2 3 (1))

    (iar
     "^\"\\(.*\\)\",\\([0-9]+\\)\\s-+\\(?:Error\\|Warnin\\(g\\)\\)\\[[0-9]+\\]:"
     1 2 nil (3))

    (ibm
     "^\\([^( \n\t]+\\)(\\([0-9]+\\):\\([0-9]+\\)) :\
 \\(?:warnin\\(g\\)\\|informationa\\(l\\)\\)?" 1 2 3 (4 . 5))

    ;; fixme: should be `mips'
    (irix
     "^[-[:alnum:]_/ ]+: \\(?:\\(?:[sS]evere\\|[eE]rror\\|[wW]arnin\\(g\\)\\|[iI]nf\\(o\\)\\)[0-9 ]*: \\)?\
\\([^,\" \n\t]+\\)\\(?:, line\\|:\\) \\([0-9]+\\):" 3 4 nil (1 . 2))

    (java
     "^\\(?:[ \t]+at \\|==[0-9]+== +\\(?:at\\|b\\(y\\)\\)\\).+(\\([^()\n]+\\):\\([0-9]+\\))$" 2 3 nil (1))

    (jikes-file
     "^\\(?:Found\\|Issued\\) .* compiling \"\\(.+\\)\":$" 1 nil nil 0)


    ;; This used to be pathologically slow on long lines (Bug#3441),
    ;; due to matching filenames via \\(.*?\\).  This might be faster.
    (maven
     ;; Maven is a popular free software build tool for Java.
     "\\([^ \n]\\(?:[^\n :]\\| [^-/\n]\\|:[^ \n]\\)*?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\] " 1 2 3)

    (jikes-line
     "^ *\\([0-9]+\\)\\.[ \t]+.*\n +\\(<-*>\n\\*\\*\\* \\(?:Error\\|Warnin\\(g\\)\\)\\)"
     nil 1 nil 2 0
     (2 (compilation-face '(3))))

    (gcc-include
     "^\\(?:In file included \\|                 \\|\t\\)from \
\\([0-9]*[^0-9\n]\\(?:[^\n :]\\| [^-/\n]\\|:[^ \n]\\)*?\\):\
\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\(?:\\(:\\)\\|\\(,\\|$\\)\\)?"
     1 2 3 (4 . 5))

    (ruby-Test::Unit
     "^[\t ]*\\[\\([^\(].*\\):\\([1-9][0-9]*\\)\\(\\]\\)?:in " 1 2)

    (gnu
     ;; The first line matches the program name for

     ;;     PROGRAM:SOURCE-FILE-NAME:LINENO: MESSAGE

     ;; format, which is used for non-interactive programs other than
     ;; compilers (e.g. the "jade:" entry in compilation.txt).

     ;; This first line makes things ambiguous with output such as
     ;; "foo:344:50:blabla" since the "foo" part can match this first
     ;; line (in which case the file name as "344").  To avoid this,
     ;; the second line disallows filenames exclusively composed of
     ;; digits.

     ;; Similarly, we get lots of false positives with messages including
     ;; times of the form "HH:MM:SS" where MM is taken as a line number, so
     ;; the last line tries to rule out message where the info after the
     ;; line number starts with "SS".  --Stef

     ;; The core of the regexp is the one with *?.  It says that a file name
     ;; can be composed of any non-newline char, but it also rules out some
     ;; valid but unlikely cases, such as a trailing space or a space
     ;; followed by a -, or a colon followed by a space.

     ;; The "in \\|from " exception was added to handle messages from Ruby.
     "^\\(?:[[:alpha:]][-[:alnum:].]+: ?\\|[ \t]+\\(?:in \\|from \\)\\)?\
\\([0-9]*[^0-9\n]\\(?:[^\n :]\\| [^-/\n]\\|:[^ \n]\\)*?\\): ?\
\\([0-9]+\\)\\(?:-\\(?4:[0-9]+\\)\\(?:\\.\\(?5:[0-9]+\\)\\)?\
\\|[.:]\\(?3:[0-9]+\\)\\(?:-\\(?:\\(?4:[0-9]+\\)\\.\\)?\\(?5:[0-9]+\\)\\)?\\)?:\
\\(?: *\\(\\(?:Future\\|Runtime\\)?[Ww]arning\\|W:\\)\\|\
 *\\([Ii]nfo\\(?:\\>\\|rmationa?l?\\)\\|I:\\|\\[ skipping .+ \\]\\|\
\\(?:instantiated\\|required\\) from\\|[Nn]ote\\)\\|\
 *[Ee]rror\\|[0-9]?\\(?:[^0-9\n]\\|$\\)\\|[0-9][0-9][0-9]\\)"
     1 (2 . 4) (3 . 5) (6 . 7))

    (lcc
     "^\\(?:E\\|\\(W\\)\\), \\([^(\n]+\\)(\\([0-9]+\\),[ \t]*\\([0-9]+\\)"
     2 3 4 (1))

    (makepp
     "^makepp\\(?:\\(?:: warning\\(:\\).*?\\|\\(: Scanning\\|: [LR]e?l?oading makefile\\|: Imported\\|log:.*?\\) \\|: .*?\\)\
`\\(\\(\\S +?\\)\\(?::\\([0-9]+\\)\\)?\\)['(]\\)"
     4 5 nil (1 . 2) 3
     (0 (progn (save-match-data
                 (compilation-parse-errors
                  (match-end 0) (line-end-position)
                  `("`\\(\\(\\S +?\\)\\(?::\\([0-9]+\\)\\)?\\)['(]"
                    2 3 nil
                    ,(cond ((match-end 1) 1) ((match-end 2) 0) (t 2))
                    1)))
               (end-of-line)
               nil)))

    ;; Should be lint-1, lint-2 (SysV lint)
    (mips-1
     " (\\([0-9]+\\)) in \\([^ \n]+\\)" 2 1)
    (mips-2
     " in \\([^()\n ]+\\)(\\([0-9]+\\))$" 1 2)

    (msft
     ;; The message may be a "warning", "error", or "fatal error" with
     ;; an error code, or "see declaration of" without an error code.
     "^ *\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \
: \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
     2 3 nil (4))

    (omake
     ;; "omake -P" reports "file foo changed"
     ;; (useful if you do "cvs up" and want to see what has changed)
     "omake: file \\(.*\\) changed" 1 nil nil nil nil
     ;; FIXME-omake: This tries to prevent reusing pre-existing markers
     ;; for subsequent messages, since those messages's line numbers
     ;; are about another version of the file.
     (0 (progn (compilation--flush-file-structure (match-string 1))
               nil)))

    (oracle
     "^\\(?:Semantic error\\|Error\\|PCC-[0-9]+:\\).* line \\([0-9]+\\)\
\\(?:\\(?:,\\| at\\)? column \\([0-9]+\\)\\)?\
\\(?:,\\| in\\| of\\)? file \\(.*?\\):?$"
     3 1 2)

    ;; "during global destruction": This comes out under "use
    ;; warnings" in recent perl when breaking circular references
    ;; during program or thread exit.
    (perl
     " at \\([^ \n]+\\) line \\([0-9]+\\)\\(?:[,.]\\|$\\| \
during global destruction\\.$\\)" 1 2)

    (php
     "\\(?:Parse\\|Fatal\\) error: \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)"
     2 3 nil nil)

    (rxp
     "^\\(?:Error\\|Warnin\\(g\\)\\):.*\n.* line \\([0-9]+\\) char\
 \\([0-9]+\\) of file://\\(.+\\)"
     4 2 3 (1))

    (sparc-pascal-file
     "^\\w\\w\\w \\w\\w\\w +[0-3]?[0-9] +[0-2][0-9]:[0-5][0-9]:[0-5][0-9]\
 [12][09][0-9][0-9] +\\(.*\\):$"
     1 nil nil 0)
    (sparc-pascal-line
     "^\\(\\(?:E\\|\\(w\\)\\) +[0-9]+\\) line \\([0-9]+\\) -  "
     nil 3 nil (2) nil (1 (compilation-face '(2))))
    (sparc-pascal-example
     "^ +\\([0-9]+\\) +.*\n\\(\\(?:e\\|\\(w\\)\\) [0-9]+\\)-+"
     nil 1 nil (3) nil (2 (compilation-face '(3))))

    (sun
     ": \\(?:ERROR\\|WARNIN\\(G\\)\\|REMAR\\(K\\)\\) \\(?:[[:alnum:] ]+, \\)?\
File = \\(.+\\), Line = \\([0-9]+\\)\\(?:, Column = \\([0-9]+\\)\\)?"
     3 4 5 (1 . 2))

    (sun-ada
     "^\\([^, \n\t]+\\), line \\([0-9]+\\), char \\([0-9]+\\)[:., \(-]" 1 2 3)

    (watcom
     "^[ \t]*\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)): ?\
\\(?:\\(Error! E[0-9]+\\)\\|\\(Warning! W[0-9]+\\)\\):"
     1 2 nil (4))

    (4bsd
     "\\(?:^\\|::  \\|\\S ( \\)\\(/[^ \n\t()]+\\)(\\([0-9]+\\))\
\\(?:: \\(warning:\\)?\\|$\\| ),\\)" 1 2 nil (3))

    (gcov-file
     "^ *-: *\\(0\\):Source:\\(.+\\)$"
     2 1 nil 0 nil)
    (gcov-header
     "^ *-: *\\(0\\):\\(?:Object\\|Graph\\|Data\\|Runs\\|Programs\\):.+$"
     nil 1 nil 0 nil)
    ;; Underlines over all lines of gcov output are too uncomfortable to read.
    ;; However, hyperlinks embedded in the lines are useful.
    ;; So I put default face on the lines; and then put
    ;; compilation-*-face by manually to eliminate the underlines.
    ;; The hyperlinks are still effective.
    (gcov-nomark
     "^ *-: *\\([1-9]\\|[0-9]\\{2,\\}\\):.*$"
     nil 1 nil 0 nil
     (0 'default)
     (1 compilation-line-face))
    (gcov-called-line
     "^ *\\([0-9]+\\): *\\([0-9]+\\):.*$"
     nil 2 nil 0 nil
     (0 'default)
     (1 compilation-info-face) (2 compilation-line-face))
    (gcov-never-called
     "^ *\\(#####\\): *\\([0-9]+\\):.*$"
     nil 2 nil 2 nil
     (0 'default)
     (1 compilation-error-face) (2 compilation-line-face))

    (perl--Pod::Checker
     ;; podchecker error messages, per Pod::Checker.
     ;; The style is from the Pod::Checker::poderror() function, eg.
     ;; *** ERROR: Spurious text after =cut at line 193 in file foo.pm
     ;;
     ;; Plus end_pod() can give "at line EOF" instead of a
     ;; number, so for that match "on line N" which is the
     ;; originating spot, eg.
     ;; *** ERROR: =over on line 37 without closing =back at line EOF in file bar.pm
     ;;
     ;; Plus command() can give both "on line N" and "at line N";
     ;; the latter is desired and is matched because the .* is
     ;; greedy.
     ;; *** ERROR: =over on line 1 without closing =back (at head1) at line 3 in file x.pod
     ;;
     "^\\*\\*\\* \\(?:ERROR\\|\\(WARNING\\)\\).* \\(?:at\\|on\\) line \
\\([0-9]+\\) \\(?:.* \\)?in file \\([^ \t\n]+\\)"
     3 2 nil (1))
    (perl--Test
     ;; perl Test module error messages.
     ;; Style per the ok() function "$context", eg.
     ;; # Failed test 1 in foo.t at line 6
     ;;
     "^# Failed test [0-9]+ in \\([^ \t\r\n]+\\) at line \\([0-9]+\\)"
     1 2)
    (perl--Test2
     ;; Or when comparing got/want values, with a "fail #n" if repeated
     ;; # Test 2 got: "xx" (t-compilation-perl-2.t at line 10)
     ;; # Test 3 got: "xx" (t-compilation-perl-2.t at line 10 fail #2)
     ;;
     ;; And under Test::Harness they're preceded by progress stuff with
     ;; \r and "NOK",
     ;; ... NOK 1# Test 1 got: "1234" (t/foo.t at line 46)
     ;;
     "^\\(.*NOK.*\\)?# Test [0-9]+ got:.* (\\([^ \t\r\n]+\\) at line \
\\([0-9]+\\)\\( fail #[0-9]+\\)?)"
     2 3)
    (perl--Test::Harness
     ;; perl Test::Harness output, eg.
     ;; NOK 1# Test 1 got: "1234" (t/foo.t at line 46)
     ;;
     ;; Test::Harness is slightly designed for tty output, since
     ;; it prints CRs to overwrite progress messages, but if you
     ;; run it in with M-x compile this pattern can at least step
     ;; through the failures.
     ;;
     "^.*NOK.* \\([^ \t\r\n]+\\) at line \\([0-9]+\\)"
     1 2)
    (weblint
     ;; The style comes from HTML::Lint::Error::as_string(), eg.
     ;; index.html (13:1) Unknown element <fdjsk>
     ;;
     ;; The pattern only matches filenames without spaces, since that
     ;; should be usual and should help reduce the chance of a false
     ;; match of a message from some unrelated program.
     ;;
     ;; This message style is quite close to the "ibm" entry which is
     ;; for IBM C, though that ibm bit doesn't put a space after the
     ;; filename.
     ;;
     "^\\([^ \t\r\n(]+\\) (\\([0-9]+\\):\\([0-9]+\\)) "
     1 2 3)
    )
  "Alist of values for `compilation-error-regexp-alist'.")

(require 'compile)
(defun use-my-compilation-error-regexps ()
  (setq compilation-error-regexp-alist my-compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist-alist my-compilation-error-regexp-alist-alist))


(provide 'my-compilation)
;;; my-compilation.el ends here
