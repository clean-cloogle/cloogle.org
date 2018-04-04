<!DOCTYPE html>
<html>
<head>
	<!-- clean-cloogle/cloogle.org {{{COMMIT}}} -->
	<title>Cloogle</title>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	<meta name="viewport" content="width=device-width, initial-scale=1"/>
	<meta name="description" content="Cloogle is the unofficial Clean language search engine"/>
	<meta name="keywords" content="Clean,Clean language,Concurrent Clean,search,functions,search engine,programming language,clean platform,iTasks,cloogle,hoogle"/>
	<script src="common.js" type="text/javascript" defer="defer"></script>
	<script src="api.js" type="text/javascript" defer="defer"></script>
	<script src="clean.js/clean.js" type="text/javascript" defer="defer"></script>
	<link rel="stylesheet" href="common.css" type="text/css"/>
	<link rel="stylesheet" href="clean.js/clean.css" type="text/css"/>
	<link rel="stylesheet" href="frontend.css" type="text/css"/>
</head>
<body>
	<div id="header">
		<div id="logo">
			<a href="https://github.com/clean-cloogle/cloogle.org">
				<img src="logo.png" alt="follow link for the sourcecode" />
			</a>
		</div>
		<div id="search">
			<form id="search-form" action="#">
				<input type="text" id="search-str" placeholder="fix :: (a -> a) -> a" />
				<input type="submit" value="Search" /><br/>
				<ul id="searchlinks">
					<li onclick="toggleAdvanced();this.classList.toggle('active');">Advanced</li>
					<li class="separator">|</li>
					<li onclick="toggleById('helptext');this.classList.toggle('active');">How to use</li>
					<li class="separator">|</li>
					<li onclick="toggleById('contributetext');this.classList.toggle('active');">Contribute</li>
					<li class="separator">|</li>
					<li onclick="shareButtonClick()" id="share-button">Share</li>
				</ul>
			</form>
		</div>
		<input id="share-link" type="text"/>
		<div id="advanced" class="tooltip">
			<div>
				<label><input type="checkbox" id="include-builtins" checked="checked"/> Include language builtins</label><br/>
				<label><input type="checkbox" id="include-core"/> Include library core modules</label><br/>
				<label><input type="checkbox" id="include-apps"/> Include apps</label><br/>
				<table>
					<?php
						function make_group_id($name) {
							return 'libs-' . str_replace(' ', '-', $name);
						}

						$groups = json_decode(file_get_contents('/var/libs.json'), true);

						echo '<tr>';
						foreach (array_keys($groups) as $group) {
							echo '<th><a title="Toggle selection" href="javascript:toggleLibSelection(\'' . make_group_id($group) . '\')">' . $group . '</a></th>';
						}
						echo '</tr>';

						echo '<tr>';
						foreach ($groups as $group => $libs) {
							echo '<td id="' . make_group_id($group) . '">';
							foreach ($libs as $lib) {
								echo '<label><input type="checkbox" class="search-libs" checked="checked" value="' . $lib['name'] . '"/> ' . $lib['name'];
								if (isset($lib['pattern_app']) && $lib['pattern_app'] == [['PWildcard']]) {
									echo ' (<abbr title="Some modules in this library require that \'include apps\' is turned on.">app</abbr>)';
								}
								echo '</label><br/>';
							}
							echo '</td>';
						}
						echo '</tr>';
					?>
				</table>
			</div>
		</div>
		<div id="helptext" class="tooltip">
			<div>
				<p>Cloogle is the unofficial <a href="http://clean.cs.ru.nl">Clean</a> language search engine.</p>
				<p><b>The following search strings are recognised:</b></p>
				<table>
					<tr>
						<td class='code'>hd</td>
						<td class='description'>Functions with a name like <code>hd</code></td>
					</tr>
					<tr>
						<td class='code'>:: a [a] -&gt; a</td>
						<td class='description'>Functions with a type unifiable with <code>a [a] -&gt; a</code></td>
					</tr>
					<tr>
						<td class='code'>hd :: [a] -&gt; a</td>
						<td class='description'>A combination of the above</td>
					</tr>
					<tr>
						<td class='code'>:: A.a: [a] -&gt; a</td>
						<td class='description'>Type search, where <code>a</code> cannot be unified.</td>
					</tr>
					<tr>
						<td class='code'>type Maybe</td>
						<td class='description'>The type definition of <code>Maybe x</code></td>
					</tr>
					<tr>
						<td class='code'>class Text</td>
						<td class='description'>The class definition of <code>Text</code></td>
					</tr>
					<tr>
						<td class='code'>using Maybe, ==</td>
						<td class='description'>Anything that uses <code>Maybe</code> <em>and</em> <code>==</code></td>
					</tr>
				</table>
				<p><b>Besides this web app, there are other frontends available:</b></p>
				<ul>
					<li>The <code>!cloogle</code> bang on <a href="https://duckduckgo.com/">DuckDuckGo</a></li>
					<li><a href="https://telegram.me/CloogleBot">@CloogleBot</a> on Telegram</li>
					<li><a href="https://github.com/clean-cloogle/cloogle-cli">cloogle-cli</a>, a command line application</li>
					<li>The <code>:Cloogle</code> command in Vim, if you use <a href="https://github.com/camilstaps/vim-clean">vim-clean</a></li>
					<li>An email to <a href="mailto:query@cloogle.org">query@cloogle.org</a>, with the query in the subject</li>
				</ul>
				<p>You can also browse the <a href="src">standard libraries</a> and the <a href="doc">documentation</a>.</p>
			</div>
		</div>
		<div id="contributetext" class="tooltip">
			<div>
				<p>
					Development takes place on <a href="https://github.com/clean-cloogle/cloogle.org">GitHub</a>.
					Please read the <a href="https://github.com/clean-cloogle/cloogle.org/blob/master/CONTRIBUTING.md">contributing guidelines</a> first.
				</p>
				<p>
					For bug reports, open an issue in the <a href="https://github.com/clean-cloogle/cloogle.org/issues">issue tracker</a>.
				</p>
			</div>
		</div>
	</div>
	<div id="search-results"></div>
</body>
</html>
