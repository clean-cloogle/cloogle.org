# cloogle

A Clean hoogle clone. Use at your own risk. Live version available
[here](http://cloogle.org/).

### Current features
- Search for function/operator/class names.
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

	In this example, the server uses port 31215. You need to use the same
	settings in `api.php`.

	Leave the `CloogleServer` running. When a HTTP request for `api.php` is made,
	that PHP script will communicate with the Clean backend server.

	You may want to consider running the backend server in a sandbox or with
	limited permissions.

### Api specification for developers
`api.php` should be called with a `GET` request where the `str` variable
contains the search string. The api will return a JSON formatted data structure
containing the following fields:

- `return`

	Return code:

	* `0`: success
	* `1`: invalid request type (should use GET)
	* `2`: no input (GET variable `str` should be set to the search string)
	* `3`: the Clean backend could not be reached
	* `4`: ununderstandable input (usually shouldn't happen)

- `msg`

	A human friendly message representing the return code.

- `data`

	An array of search results. Every items contains the following fields:
	`library`, `filename`, `modul` (not a typo), `func` and `distance`
	representing the name of the library, filename, the module name, the matched
	function signature and some loosely defined distance to the search string.

### Todo in order of importance

- Search on type definitions
- Also grab possible comments above the function signature
- Search for instances of classes

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
