# Tool to convert [Sphinx](http://www.sphinx-doc.org/) docs to [Paradox](https://github.com/lightbend/paradox)

It uses [Laika](https://github.com/planet42/Laika) to parse reStructured
text and render it as Paradox
[markdown](https://developer.lightbend.com/docs/paradox/latest/).

Run it through sbt to convert a site:
```
$ sbt
sbt> ~run <path-to-sphinx-site> <path-to-paradox-site>
```

Manual steps/problems needed to be dealth with before the script can be run:
 
 * `.. :includecode` statements needs to have a blank line after
   * regex search replace with `(.. includecode:: .*)\n[^\n]` `\n[^\n\s]`
 * Paragraphs cannot end with `:` 
   * regex search replace with `^(\w.*):+$` `\1`
 * Unsupported directives found:
    * `.. highlightlang::`
    * `.. tabularcolumns::`
    * `.. highlight::`
 

See also the `scripts` directory for utilities to help convert and clean up
docs.

Examples of projects where it has been used:

 - [Akka HTTP](https://github.com/akka/akka-http) -
   [akka/akka-http#313](https://github.com/akka/akka-http/pull/313)
 - [SSL Config](https://github.com/typesafehub/ssl-config) -
   [typesafehub/ssl-config#51](https://github.com/typesafehub/ssl-config/pull/51)
