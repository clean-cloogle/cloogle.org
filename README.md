# Cloogle [![][travis badge]][travis]

The [Clean][] language search engine. Cloogle lets you search for functions,
types, classes and modules from Clean libraries. It also has documentation for
language features, common compiler errors and ABC instructions. The web app is
available at [cloogle.org][].

Cloogle was inspired by [Hoogle][]. As of June 27, 2018, Cloogle indexes
100&times; less lines of code than Hoogle (25 thousand vs. 2.3 million). This
allows Cloogle to implement more advanced features, such as `using` queries to
find usages.

---

**Readme contents**

- [Frontends](#frontends)
- [Auxiliary tools](#auxiliary-tools)
- [Interfacing with Cloogle](#interfacing-with-cloogle)
- [Preparing a library for indexing](#preparing-a-library-for-indexing)
- [Local setup](#local-setup)
- [Authors, copyright &amp; license](#authors-copyright--license)

---

## Frontends
These frontends to Cloogle are currently known to us:

- The web app at [cloogle.org][].
- The `!cloogle` bang on DuckDuckGo.
- The `:Cloogle` command or `<LocalLeader>c`in [vim-clean][].
- An email to `query@cloogle.org` with the query in the subject
  (see [cloogle-mail][]).
- The `!query` command of the IRC bot `clooglebot` which often resides on the
  `#cloogle` and `#cleanlang` channels on [freenode][] (see [clean-irc][]).

Old frontends:

- [@CloogleBot][] on Telegram (see [CloogleBot][]).
- [cloogle-cli][], a command line interface to the API.

## Auxiliary tools
Several tools used in Cloogle are available as separate libraries from
[github.com/clean-cloogle](https://github.com/clean-cloogle). Additionally,
these tools can be helpful:

**[Cloogle-tags][cloogle-tags]:**
This program lets you index local Clean code in *tagfiles* that can be used by
your text editor.

**Library browser:**
The frontend includes a library browser to browse through all known Clean
libraries. The browser can be accessed through
[cloogle.org/src](https://cloogle.org/src).

**Documentation browser:**
There is also an HTML version of the Clean Language Report, available at
[cloogle.org/doc](https://cloogle.org/doc).

**Logging:**
A websocket server on port 31216 provides the realtime [cloogle.org][] log.

**Statistics:**
On [cloogle.org/stats/live.html](https://cloogle.org/stats/live.html), a
realtime usage chart is shown. For longterm statistics you can see
[cloogle.org/stats/longterm.html](https://cloogle.org/stats/longterm.html).

## Interfacing with Cloogle
**TCP API**  
`CloogleServer` is a TCP server listening on port 31215 (typically). Send a
JSON request in the format described by the `Request` type in [Cloogle.dcl][],
followed by a newline character. The response is in the format described by the
`Response` type. The connection is kept alive for some time to allow further
requests.

To interface with Cloogle, it is recommended that you use the HTTP API rather
than the TCP API.

For a Clean interface to Cloogle, it is suggested that you use [libcloogle][].

For a Python interface to Cloogle, you can use [cloogle.py][].

**HTTP API**  
The HTTP API is a simple wrapper around the TCP API, with a more stable API.
`api.php` should be called with a `GET` request where the `str` parameter
contains the search string. You may also add `mod` (a comma-separated list of
modules to search in) and `page` (for pagination: 0 for the first *n* results,
1 for the next *n*, etc.). For the query syntax in the `str` parameter, see the
'How to use' guidelines on [cloogle.org][].

The API returns the same JSON as the TCP API, but may include additional
results when searched for Clean error messages (for example, `stack overflow`).
These error messages are indexed by the frontend rather than the backend.
Additionally, the HTTP API may give return codes above 150, which are not used
by the TCP API. For the meaning of the return codes, see [Cloogle.icl][].

## Preparing a library for indexing
Your library will be most easily accessible when:

- Functions, type definitions, classes and modules are documented. For details
  on the documentation format, see the README of the [Cloogle][] submodule.
- Macros have types using the `@type` documentation field.

Having documentation is not a strict requirement, however.
To add your library to the index, follow the steps in
[CONTRIBUTING.md](/CONTRIBUTING.md#3-i-just-want-to-add-library-x) and create a
pull request.

## Local setup
After installing [docker-compose][] run the following commands:

```bash
touch cloogle.log
sudo docker-compose up
```

Your Cloogle server now runs at port `31215` on your local machine.
The web frontend is available at port `80`, live statistics at port `31216`.

If you intend to run this on a server that has port 80 occupied already, you
can use nginx or apache2 as a proxy. Change `80:80` to `31280:80` in
`docker-compose.yml` and use the following nginx config:

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

Or the following apache2 virtualhost (be sure to enable `mod_proxy`).

```ApacheConf
<VirtualHost *:80>
	ServerName cloogle.org

	ProxyRequests off
	ProxyPass / http://localhost:31280/
	ProxyPassReverse / http://localhost:31280/
</VirtualHost>
```

## Authors, copyright &amp; license
Copyright &copy; 2016-present Mart Lubbers and Camil Staps.
Licensed under MIT; See the [LICENSE](/LICENSE) file.

Maintainers:

- [dopefishh](https://github.com/dopefishh)
- [Camil Staps](https://camilstaps.nl)

Contributors:

- [ErinvanderVeen](https://github.com/ErinvanderVeen) (logo and UI design)
- [KDercksen](https://github.com/KDercksen) (searching on module; help text)
- [SteffenMichels](https://github.com/SteffenMichels) (ABC documentation)

[cloogle.org]: https://cloogle.org
[Cloogle]: https://github.com/clean-cloogle/Cloogle
[libcloogle]: https://github.com/clean-cloogle/libcloogle
[Cloogle.dcl]: https://github.com/clean-cloogle/libcloogle/blob/master/Cloogle.dcl
[Cloogle.icl]: https://github.com/clean-cloogle/libcloogle/blob/master/Cloogle.icl
[CloogleBot]: https://github.com/clean-cloogle/CloogleBot
[@CloogleBot]: https://telegram.me/CloogleBot
[cloogle-tags]: https://github.com/clean-cloogle/cloogle-tags
[cloogle-cli]: https://github.com/clean-cloogle/cloogle-cli
[cloogle-mail]: https://github.com/clean-cloogle/cloogle-mail
[clean-irc]: https://github.com/clean-cloogle/clean-irc
[cloogle.py]: https://github.com/clean-cloogle/cloogle.py

[Clean]: http://clean.cs.ru.nl
[vim-clean]: https://github.com/camilstaps/vim-clean

[docker-compose]: https://www.docker.com/products/docker-compose
[Hoogle]: https://github.com/ndmitchell/hoogle
[travis badge]: https://api.travis-ci.org/clean-cloogle/cloogle.org.svg?branch=master
[travis]: https://travis-ci.org/clean-cloogle/cloogle.org
[freenode]: https://freenode.net
