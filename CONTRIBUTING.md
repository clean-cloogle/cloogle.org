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

### General

- Fork the repo, make a topic branch and when done submit a pull request
- If your PR fixes some issue, be sure to mention it in the commit message
  (e.g. `Fix #15`)
- Keep PRs as focused as possible. If you intend to fix multiple, independent
  things, open multiple, separate PRs.
- If you intend to make large changes, it's probably best to open an issue and
  discuss with the maintainers first.
- If necessary, edit `README.md` as well.

### Code style

- Use tabs instead of spaces
- Keep lines under 80 chars (except for HTML).
- When editing the frontend, do not use external frameworks (jQuery,
  bootstrap, etc.). We strive for minimality and elegance.

### Other stuff

- Add yourself to the authors list in `README.md`

## 3. I just want to add library X
If the library is not well known it is advised to first open an issue to see
whether it is suitable to be indexed by cloogle.

To add a library you have to add it in to the following places:

- `backend/builddb.icl`

	Add your library to the `zero` instance of `CLI` to get it indexed.
- `backend/Dockerfile`

	In this file you have to add the download of the library. E.g. add a line
	to get the files to the docker (e.g. with subversion, git, mercurial etc.)
	to `/opt/clean/lib`.
- `frontend/Dockerfile`

	In this file do the same as in the backend to make the files through the
	web frontend.
- `frontend/index.html`

	Add your library to the checkboxes in the miscellaneous column.

## 4. I just want to add a new user agent to the statistics
If you have created a new Cloogle client, please give it a specific user agent
and add it to the `$user_agents` array in `frontend/stats/conf.php`. That way,
it will show up in the statistics on https://cloogle.org/stats/longterm.html.
There are no strict naming conventions, but you can have a look at the other
user agents for inspiration.

Each entry has a key, which is the name of the client (e.g. 'vim-clean'). The
value is an array with a required `pattern`, which is used in SQL `LIKE`
queries so you can use `%` as in `%Linux%` - in most cases, the pattern will be
equal to the name. The optional `url` is a link to where the client may be used
or downloaded.
