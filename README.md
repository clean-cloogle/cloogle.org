# cloogle

A Clean hoogle clone. Use at your own risk. Live version available
[here](http://cloogle.org/).

### Current features
- Search for function/operator/class member names.
- Search for function types.

### How to setup

- The frontend heavily depends on [VanillaJS](http://vanilla-js.com/) so you
	should have a webbrowser that supports it.

- Add `env/envs.linux64` to your `$CLEAN_HOME/etc/IDEEnvs`.

- Run `make`. This builds all necessary binaries and runs `builddb`, which
	creates a file `types.db` which holds the internal database of functions and
	their types. If you add new libraries later on, you need to rerun `builddb`.

- You can then run the Clean backend with:

		$ ./CloogleServer 31215 < types.db

  Alternatively, use `serve` as a wrapper. It will restart the server on
  crashes, and log to both stdout and cloogle.log:

    $ ./serve

	In this example, the server uses port 31215. You need to use the same
	settings in `api.php`.

	Leave the `CloogleServer` running. When a HTTP request for `api.php` is made,
	that PHP script will communicate with the Clean backend server.

	You may want to consider running the backend server in a sandbox or with
	limited permissions.

### Api specification for developers
`api.php` should be called with a `GET` request where the `str` variable
contains the search string. You may also add `mod` (a comma-separated list of
modules to search in) and `page` (for pagination: 0 for the first *n* results,
1 for the next *n*, etc.).

The api will return a JSON formatted data structure containing the following
fields:

- `return`

	Return code:

	* `0`: success
	* `127`: no results
	* `128`: ununderstandable input (usually shouldn't happen)
	* `129`: function name too long
	* `150`: the Clean backend could not be reached
	* `151`: invalid request type (should use GET)
	* `152`: no input (GET variable `str` should be set to the search string)

- `msg`

	A human friendly message representing the return code.

- `data`

	An array of search results. A result has the following fields:

	* `library`
	* `filename`
	* `func`: the function name and type as a string
	* `unifier`: a list of two lists of type assignments that represents the
		unification of the searched type to the found type (only when `unify` is
		not the empty string)
	* `cls`: if the function is a class member, this contains `cls_name` and
		`cls_vars` and represents the class it was found in
	* `modul`: the module the result was found in (not a typo)
	* `distance`: the distance measure we use to sort the results (lower is
		better)

- `more_available`

	If there are more results that can be found using pagination, this will be
	the number of results that have a higher distance than the last result sent.

### Talking with the Clean backend directly
`CloogleServer` is a TCP server listening on port 31215 (typically). Send a
JSON request with the following fields:

* `unify`, the type to search for as a string (or the empty string)
* `name`, the name of the function to search for (or the empty string)
* `modules`, a list of names of modules to search in (*optional*)
* `page`: 0 for the first *n* results, 1 for the next *n*, etc. (*optional*)

The Clean backend will return a JSON string, similar to the output of the PHP
script described above. The error codes above 150 are specific to the script
and cannot be returned by the Clean backend.

### Live statistics
There is a possibility to set up a web page that shows live statistics.
Currently, only the last few searches are shown. For this, you need to have
`nodejs` installed. Then do:

    $ cd stats
    $ npm install

And to run:

    $ node server.js ../cloogle.log

This starts a WebSocket server on port 31216. You can navigate to `/stats` to
view the statistics. This page will receive live updates.

### Todo in order of importance

- Search on type definitions
- Also grab possible comments above the function signature
- Search for instances of classes
- DoS prevention (both on PHP and on Clean level)
- Statistics: show requests / minute

### Authors

Maintainers:

- [dopefishh](https://github.com/dopefishh)
- [Camil Staps](https://camilstaps.nl)

Contributors:

- [KDercksen](https://github.com/KDercksen) (searching on module; help text)

### Licence

```
The MIT License (MIT)

Copyright (c) <year> <copyright holders>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
