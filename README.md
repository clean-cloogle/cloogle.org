# Cloogle

A Clean hoogle clone: search for functions, types, and classes from the Clean
standard libraries.

Use any of the available frontends:

- Web app at [cloogle.org](https://cloogle.org/).
- The `!cloogle` bang on DuckDuckGo.
- [@CloogleBot](https://telegram.me/CloogleBot) on Telegram (see
	[camilstaps/CloogleBot](https://github.com/camilstaps/CloogleBot)).
- [KDercksen/cloogle-cli](https://github.com/KDercksen/cloogle-cli), a command
	line interface to the api.
- The `:Cloogle` command in
	[camilstaps/vim-clean](https://github.com/camilstaps/vim-clean).
- An email to `query@cloogle.org` with the query in the subject (see
	[this gist](https://gist.github.com/camilstaps/bf719319d90d974329ee7e4ad08f1aa2)
	for the implementation).

---

### Readme contents

- [Setup](#setup)
- [API specification of the PHP wrapper](#api-specification-for-developers)
- [API specification of the Clean backend](#talking-with-the-clean-backend-directly)
- [Statistics](#statistics)
- [Authors](#authors)
- [Copyright &amp; License](#copyright--license)

---

## Setup

After installing
[docker-compose](https://www.docker.com/products/docker-compose) run the
following command:

```bash
docker-compose up
```

Your Cloogle server now runs at port `31215` on your local machine.
The web frontend is available at port `80`, live statistics at port `31216`.

If you intend to run this on a server that has port 80 occupied already, you
can use nginx as a proxy. Change `80:80` to `31280:80` in `docker-compose.yml`
and use the following nginx config:

```nginx
server {
	listen [::]:80;
	server_name cloogle.org;

	location / {
		proxy_pass http://127.0.0.1:31280;
		proxy_set_header Host $host;
		proxy_set_header X-Forwarded-For $remote_addr;
	}
}
```

## HTTP API specification
`api.php` should be called with a `GET` request where the `str` variable
contains the search string. You may also add `mod` (a comma-separated list of
modules to search in) and `page` (for pagination: 0 for the first *n* results,
1 for the next *n*, etc.).

The API will return a JSON formatted data structure containing the following
fields:

- `return`

	Return code:

	* `0`: success
	* `1`: cache hit, thus success
	* `127`: no results
	* `128`: ununderstandable input (usually shouldn't happen)
	* `129`: invalid name field
	* `130`: couldn't parse unify field as a type
	* `150`: the Clean backend could not be reached
	* `151`: invalid request type (should use GET)
	* `152`: no input (GET variable `str` should be set to the search string)
	* `153`: the Clean backend timed out

- `msg`

	A human friendly message representing the return code.

- `data`

	An array of search results. A result is an array of three elements. The first
	determines the kind of result. It may be `FunctionResult`, `TypeResult`,
	`ClassResult` or `MacroResult`. The second contains general data, in
	particular the following fields:

	* `library`
	* `modul`: the module the result was found in (not a typo)
	* `filename`: the filename of the definition module
	* `dcl_line`: the line where the definition is found
	* `distance`: the distance measure we use to sort the results (lower is
		better)

	The third element of the array contains data specific to the kind of result.
	It is easiest to look in `CloogleServer.icl` at the types
	`FunctionResultExtras`, `TypeResultExtras`, `ClassResultExtras` and
	`MacroResultExtras` to get an idea of the fields that may be returned.

- `more_available`

	If there are more results that can be found using pagination, this will be
	the number of results that have a higher distance than the last result sent.
	If there are no more results, this field *may* be zero or may not be present.

- `suggestions`

	If there are similar searches that may return more results, this will be an
	array of two-tuples with the alternative search (which has the same fields as
	a request) and the number of results.

## TCP API Specification
`CloogleServer` is a TCP server listening on port 31215 (typically). Send a
JSON request with at least one of the following fields:

* `unify`, the type to search for as a string.
* `name`, the name of the function to search for.
* `className`, the name of the class to search for.
* `typeName`, the name of the type to search for.
* `libraries`, a list of two elements:
	* A list of names of libraries to search in
	* A boolean, whether language builtins should be searched or not.
* `modules`, a list of names of modules to search in.
* `page`: 0 for the first *n* results, 1 for the next *n*, etc.

All fields are optional. If `className` is present, `unify` and `name` will be
ignored. If `typeName` is present (and `className` is not), `unify` and `name`
will be ignored.

The Clean backend will return a JSON string, similar to the output of the PHP
script described above. The error codes above 150 are specific to the script
and cannot be returned by the Clean backend.

## Authors

Maintainers:

- [dopefishh](https://github.com/dopefishh)
- [Camil Staps](https://camilstaps.nl)

Contributors:

- [KDercksen](https://github.com/KDercksen) (searching on module; help text)

## Copyright &amp; License

Copyright &copy; Mart Lubbers and Camil Staps.
Licensed under MIT; See the `LICENSE` file.
