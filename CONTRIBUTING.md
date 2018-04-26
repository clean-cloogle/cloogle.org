# CONTRIBUTING

Thank you for taking the time to contribute to Cloogle. Please read this before
contributing to avoid wasting your or our time.

---

1. [Bug reports](#1-bug-reports)
2. [Submitting patches](#2-submitting-patches)
3. [I just want to add library X](#3-i-just-want-to-add-library-x)
4. [I just want to add a new user agent to the statistics](#4-i-just-want-to-add-a-new-user-agent-to-the-statistics)

---

## 1. Bug reports

We use GitHub issues for bug tracking.

First, check if your bug has been reported already. If not, open a new GitHub
issue. Make sure to include the following:

- Steps to reproduce
- Actual outcome
- Expected outcome
- How you're accessing Cloogle (through the frontend or directly to Clean)

**If you have found a security vulnerability**, please do **not** open a GitHub
issue. Instead, contact one of the maintainers (see the list in `README.md`).
We will take immediate action.

## 2. Submitting patches

### Which repository to patch

- [libcloogle][] describes the Cloogle API as a set of types. Because many
  applications make assumptions about this API, one must be reluctant with
  changing this. However, sometimes it is necessary. See
  [`CONTRIBUTING.md`](https://github.com/clean-cloogle/libcloogle/blob/master/CONTRIBUTING.md)
  in that repository for more details.
- [Cloogle][] contains the core functionality for indexing and searching. It
  relies on [CleanPrettyPrint][] to pretty-print the abstract syntax tree of
  Clean, and on [CleanTypeUnifier][] to unify types in type search.
- This repository ([cloogle.org](https://github.com/clean-cloogle/cloogle.org))
  is only a wrapper around [Cloogle][]. It contains functionality for TCP,
  caching, the web frontend and statistics.

### General

- Fork the repo, make a topic branch and when done submit a pull request.
- If your PR fixes some issue, be sure to mention it in the commit message
  (e.g. `Fix #15`).
- Keep PRs as focused as possible. If you intend to fix multiple, independent
  things, open multiple, separate PRs.
- If you intend to make large changes, it's probably best to open an issue and
  discuss with the maintainers first.
- If necessary, edit `README.md` as well.

### Code style

- Use tabs instead of spaces.
- Try to keep lines under 80 chars (except for HTML).
- When editing the frontend, do not use external frameworks (jQuery,
  bootstrap, etc.). We strive for minimality and elegance.

### Other stuff

- Add yourself to the authors list in `README.md`.

## 3. I just want to add library X
If the library is not well-known it is advised to first open an issue to see
whether it is suitable to be indexed by cloogle.

To add a library you have to modify [`libs.json`](/libs.json). This file is a
JSON record with three collections of Clean libraries. Usually, you should add
the new library to the `Miscellaneous` collection. Please keep the alphabetic
order intact.

The newly added item may contain the following fields:

- `name` (**required**): a human-readable name.
- `fetch_url` (**required**): choose one of:
  - `["Git", "<URL>"]` where `<URL>` points to a public git repository;
  - `["SVN", "<URL>"]` where `<URL>` points to a public Subversion repository;
  - `["CleanDistribution", "<NAME>"]`, if the library is distributed in Clean
    nightlies as `<NAME>`.
- `path`: the path from the root of the repository to the files that should be
  indexed.
- `info_url`: a URL to an informative page about the library.
- `pattern_exclude`: a pattern (see below) for files to exclude.
- `pattern_app`: a pattern (see below) for modules that should be marked as
  apps rather than libraries.
- `pattern_core`: a pattern (see below) for modules that are part of the core
  of the library.

Patterns are JSON lists of simple patterns. A simple pattern is one of:

- `["PWildcard"]`, to match everything;
- `["PStartsWith",<S>]`, to match paths starting with `<S>`;
- `["PNot",<P>]`, to negate the simple pattern `<P>`.

## 4. I just want to add a new user agent to the statistics
If you have created a new Cloogle client, please give it a specific user agent
and add it to the `$user_agents` array in `frontend/stats/ajax/conf.php`. That
way, it will show up in the statistics on
https://cloogle.org/stats/longterm.html. There are no strict naming
conventions, but you can have a look at the other user agents for inspiration.

Each entry has a key, which is the name of the client (e.g. 'vim-clean'). The
value is an array with a required `pattern`, which is used in SQL `LIKE`
queries so you can use `%` as in `%Linux%` - in most cases, the pattern will be
equal to the name. The optional `url` is a link to where the client may be used
or downloaded.

[Cloogle]: https://github.com/clean-cloogle/Cloogle
[libcloogle]: https://github.com/clean-cloogle/libcloogle
[CleanPrettyPrint]: https://github.com/clean-cloogle/CleanPrettyPrint
[CleanTypeUnifier]: https://github.com/clean-cloogle/CleanTypeUnifier
