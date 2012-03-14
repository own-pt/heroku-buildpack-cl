Heroku Buildpack for Common Lisp
================================

A Buildpack that allows you to deploy Common Lisp applications on the Heroku infrastructure.

Original work by Mike Travers, mt@hyperphor.com

## Changes 
* Support for SBCL and Hunchentoot.

> Example app at https://github.com/jsmpereira/heroku-cl-example

* Implementation choice via env variables.

> You need this first: http://devcenter.heroku.com/articles/labs-user-env-compile.
It will allow the config vars to be present at build time.

> Then you can do 
```heroku config:add CL_IMPL={sbcl|cc}```

### Notes

* To avoid trouble with SBCL source encoding use:
```heroku config:add LANG=en_US.UTF-8```

* Actually only SBCL and Hunchentoot work right now, but I'm working
on adding back the CCL and AllegroServe from Mike Travers's original fork.

* I plan on using the same method for specifying web server. Something like:
```heroku config:add CL_WEBSERVER={hunchentoot|portableaserve}```

José Santos, jsmpereira@gmail.com

######I've kept the original readme for reference.

## Status
* Working to first approximation.
* For a minimal example of use, see [the example application](https://github.com/mtravers/heroku-cl-example).
* For a more complex example, see [the WuWei demo site](http://warm-sky-3012.herokuapp.com/) and [source](https://github.com/mtravers/wuwei).

## Notes
* The scripts bin/test-compile and bin/test-run simulate as far as possible the Heroku build and run environments on your local machine.

## Todos
* parameterizing/forking for other Lisp implementations and web servers.
* support for Heroku's database infrastructure.

## Credits
* Heroku and their new [Buildpack-capable stack](http://devcenter.heroku.com/articles/buildpacks)
* [QuickLisp](http://www.quicklisp.org/) library manager 
* [OpenMCL](http://trac.clozure.com/ccl) aka Clozure CL 
* [Portable AllegroServe](http://portableaserve.sourceforge.net/)

Mike Travers, mt@hyperphor.com



