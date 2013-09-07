#About HH-Web#

HH-Web is the beginnings of a web application framework written in Lisp designed to streamline development of modern web applications.

See the LICENSE file for the license covering all code within the HH-Web directory and any subdirectories.  
Basically, it's the MIT license, so you are free to do what you want, provided you reproduce the original copyright.

##Platforms##

The primary platform for HH-Web development is SBCL x64 on Mac OS X Lion (latest SBCL rev tested is 1.0.57); other Common Lisp implementations that appear to work correctly are CCL 1.8 x86, LispWorks 6.01, and ABCL 1.0.1.  HH-Web has also been tested with SBCL 1.0.57 x64 on Ubuntu 11.04.

Platforms with known issues include CCL x64 (problem with threads) and CMUCL (which does not like named-readtables).

##Features##

In its present form (as of June 2012), there are 4 principal features in HH-Web:

  * **Composable widgets**: HH-Web  let's the developer write a web page using conventional  Lisp s-expression syntax, complete with properly generated CSS & Javascript.  More importantly, commonly used fragments of HTML/CSS/Javascript can be bundled as a *tag* (just like HTML tags such as `p`, `div`, `h1`, etc.), and the corresponding HTML/CSS/Javascript for the expected fragment will be generated instead.  Using custom tags means that most of the development of a new site involves identifying the common elements that make up the site's pages, creating custom tags (with parameters or "attributes", when necessary) as needed, and then authoring the pages as templates using both HTML tags and the site-specific custom tags.

  * **Regex-based URL dispatching**: Inspired by Django's (and Rails') use of regex dispatching, HH-Web implements a similar technique, but in Lisp.  Most importantly, if the regex contains "variables" (e.g., named registers, in regex parlance), then symbols will be interned for those variables and exposed as parameters to the URL's handler.  Thus, components of an URL's path will automatically be extracted and made available as arguments to the handling code.

  * **Dynamic code reloading**: HH-Web automatically monitors specific files for new URL dispatching rules, page template changes, and all taglibraries. Changes in the underlying code is by default automatically reflected the next time the page is refreshed in the browser.  The intent is to provide the equivalent usability of other web authoring environments such as PHP or ASP: change the code for a page, and reflect that immediately in the browser.

  * **Integrated caching**: HH-Web's support for caching is evolving, but in its current form it permits marking caching guidelines for specific URLs.  Specifically, caching causes HH-Web to preseve any generated content (from templates and tags, typically) in an in-memory cache for later use.  If a request for a page in its map appear again, then HH-Web will serve the content from the cache, rather than invoking code to regenerate the content fresh.  

##Getting Started##

As an aide to starting new projects that use HH-Web to structure web application development, you can load the HH-Web system into the REPL using [Quicklisp](http://quicklisp.org) (included as of 7/3/2012--thanks [Zach](http://xach.com)!):

    (ql:quickload :hh-web)

then execute the following expression:

    (hh-web:make-skeleton :for :acme :in #p"~/lisp/")
  
Substitute your own project name for *acme* in the above expression.  Note that the argument to the **:for** keyword must be a symbol or string, and the argument to the **:in** keyword must be a directory path (e.g., it must end in "/"), although that directory should not exist,  `make-skeleton` will not overwrite any files or directories that already present--it will raise an error instead.  In the above example, the package for your new application will appear as a subdirectory of the one you provided, thus it will appear in `~/lisp/acme/`.

If you use [Quicklisp](http://quicklisp.org) and the "~/lisp/" directory is registered with [ASDF](http://common-lisp.net/project/asdf/) as a [source directory](http://common-lisp.net/project/asdf/asdf.html#Configuring-ASDF), then you can then run the following at your REPL to start your newly-created web application:

    (ql:quickload :acme)
    (acme:start-httpd-server)

Now just point your browser at [http://localhost:8000](http://localhost:8000), and you should see a boilerplate homepage.

You can stop your application at any time by running the following:

    (acme:stop-httpd-server)

Additionally, the generated skeleton of a project also creates a single script called `quickrun.lisp` that lets you load and run your site in one step.  For example, with SBCL:

    sbcl --load ~/lisp/acme/quickrun.lisp
    
Here's the directory structure created for your project:

    /acme
      |
      +-- acme.asd			# system declaration
      +-- locales				# directory containing files to support i18n and l10n
      +-- logs.lisp			# helpers for logging using log5
      +-- package.lisp		# package declaration
      +-- quickrun.lisp	# script for loading and running the site
      +-- server.lisp		# runs your site with hunchentoot
      +-- taglibraries		# directory for your tag libraries
      +-- templates			# directory for your templates
      +-- templates.lisp	# file for declaring which templates to use
      +-- urls.lisp			# file containing the regex rules for dispatching requests
      
To develop your site, edit the following files as needed, and refresh your browser to see changes take effect immediately:

* urls.lisp for your request dispatch rules
* templates.lisp for the declaring what templates you wish to use
* any file in the taglibraries folder for designing your tag or widget libraries
* any file in the templates folder for creating your own page templates

The generated skeleton also demonstrates one mechanism for integrating [Bootstrap](http://getbootstrap.com) 
and [jQuery](http://jquery.com) into a site, so that the resources of both libraries are always available.
Note that the chosen method here may or may not be suitable for your scenario: the skeleton generates
links to CDNs that host these libraries to ease other web sites.

# Learning More

For more information on how HH-Web works and how to make use of it for your application, see the documentation on the [wiki](https://github.com/hargettp/hh-web/wiki).
