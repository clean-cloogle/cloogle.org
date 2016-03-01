# cloogle

A clean hoogle clone. Use at your own risk. Live version available
[here](http://martlubbers.net/cloogle)

### Current features
- Search for function/operator/class names.

### How to setup

- The frontend heavily depends on [VanillaJS](http://vanilla-js.com/) so you
  should have a webbrowsers that supports it.
- Put a folder containing `StdEnv` in a directory called `stdenv` in the same
  directory as the code. You can also change the `STDENV_PATH` variable which
	is set in `api.php` if you want it loaded from somewhere else.

### Api specification for developers
`api.php` should be called with a `GET` request where the `str` variable
contains the search string. The api will return a JSON formatted datastructure
containing the following fields

- `return`

	Return code, `0` for success, `1` for wrongly called api, `127` for no
	results.
- `msg`

	A human friendly message representing the return code.
- `data`

	An array of search results. Every items contains the following fields:
	`library`, `filename`, `module`, `func` and `distance` representing the name
	of the library, filename, the module name, the matched function signature and
	the levenshtein distance.

### Todo in order of importance

- Search on type definitions
- Search for function signatures
- Also grab possible comments above the function signature
- Search also in `clean-platform`
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
