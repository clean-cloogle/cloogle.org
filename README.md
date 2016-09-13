# Cloogle

A Clean hoogle clone: search for functions, types, and classes from the Clean
standard libraries.

Use any of the available frontends:

- Web app at [cloogle.org](http://cloogle.org/).
- The `!cloogle` bang on DuckDuckGo.
- [@CloogleBot](https://telegram.me/CloogleBot) on Telegram (see
	[camilstaps/CloogleBot](https://github.com/camilstaps/CloogleBot)).
- [KDercksen/cloogle-cli](https://github.com/KDercksen/cloogle-cli), a command
	line interface to the api.
- The `:Cloogle` command in
	[camilstaps/vim-clean](https://github.com/camilstaps/vim-clean).

---

### Readme contents

- [Setup](#setup)
- [Setup using Docker](#setup-using-docker)
- [API specification of the PHP wrapper](#api-specification-for-developers)
- [API specification of the Clean backend](#talking-with-the-clean-backend-directly)
- [Live statistics](#live-statistics)
- [Authors](#authors)
- [Copyright &amp; License](#copyright--license)

---

## Setup

### Frontend

- The frontend heavily depends on [VanillaJS](http://vanilla-js.com/) so you
	should have a browser that supports it.

### Backend

```bash
$ cd backend
$ cat env/envs.linux64 >> "$CLEAN_HOME/etc/IDEEnvs"
$ make
```

You have now built the necessary binaries and created `types.db`, which holds
the internal database.

You can now run the CloogleServer with:

```bash
$ ./CloogleServer 31215 < types.db
```

Alternatively, use `serve` as a wrapper. It will restart the server on
crashes, and log to both stdout and cloogle.log:

```bash
$ ./serve
```

In this example, the server uses port 31215. You need to use the same settings
in `frontend/api.php`.

Leave the `CloogleServer` running.

Install a web server with PHP support to handle requests for the `frontend`
directory. When an HTTP request for `api.php` is made, that PHP script will
communicate with the Clean backend server.

### Live statistics
The live version's statistics page is at
[cloogle.org/stats](http://cloogle.org/stats).

There is a possibility to set up a web page that shows live statistics.
Currently, only the last few searches are shown. For this, you need to have
`nodejs` installed. Then do:

```bash
$ cd frontend/stats
$ npm install
```

And to run:

```bash
$ node server.js ../../backend/cloogle.log
```

This starts a WebSocket server on port 31216. You can navigate to `/stats` to
view the statistics. This page will receive live updates.

## Setup using Docker

This is an experimental feature. There are two `Dockerfile`s available: one for
the Cloogle server and one for the live statistics. We need them to share a
`cloogle.log`. Also, both use `--net=host` to easily open their ports. In a
production environment, you should not do this, but instead use `-p` and set up
forwarding rules.

### Cloogle server

```bash
$ cd backend
$ sudo touch /var/log/cloogle.log
$ docker build -t cloogle .
$ docker run -d --net=host --name=cloogle \
	-v /var/log/cloogle.log:/usr/src/cloogle/cloogle.log \
	cloogle
```

### Live statistics

```bash
$ cd frontend/stats
$ docker build -t cloogle-stats .
$ docker run -d --net=host --name=cloogle-stats \
	-v /var/log/cloogle.log:/var/log/cloogle.log \
	-v /path/to/cert.pem:/srv/ssl/cert.pem \
	-v /path/to/key.pem:/srv/ssl/key.pem \
	cloogle-stats
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
	* `127`: no results
	* `128`: ununderstandable input (usually shouldn't happen)
	* `129`: invalid name field
	* `130`: couldn't parse unify field as a type
	* `150`: the Clean backend could not be reached
	* `151`: invalid request type (should use GET)
	* `152`: no input (GET variable `str` should be set to the search string)

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
