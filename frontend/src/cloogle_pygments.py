import urllib
import sys
import pygments
import pygments.lexers
import pygments.formatters
from pygments.token import Literal, Name, Operator

_escape_html_table = {
    ord('&'): u'&amp;',
    ord('<'): u'&lt;',
    ord('>'): u'&gt;',
    ord('"'): u'&quot;',
    ord("'"): u'&#39;',
}

CLEAN_SYNTAX_TOKENS = ['=', '=:', ':==', '|', '->', '(', ')', ':', '::', '::!',
        '..', '_', '\\', '.', '#', '#!', '!', '\\\\', '<-', '<-:', '<-|', '&',
        '|*|', '|*->*|', '|*->*->*|']


# This is blatantly stolen from the HTML formatter
# Original code has a BSD licence
class CloogleHtmlFormatter(pygments.formatters.HtmlFormatter):
    def _format_lines(self, tokensource):
        """
        Just format the tokens, without any wrapping tags.
        Yield individual lines.
        """
        nocls = self.noclasses
        lsep = self.lineseparator
        # for <span style=""> lookup only
        getcls = self.ttype2class.get
        c2s = self.class2style
        escape_table = _escape_html_table
        tagsfile = self.tagsfile

        lspan = ''
        line = []
        for ttype, value in tokensource:
            if nocls:
                cclass = getcls(ttype)
                while cclass is None:
                    ttype = ttype.parent
                    cclass = getcls(ttype)
                cspan = cclass and '<span style="%s">' % c2s[cclass][0] or ''
            else:
                cls = self._get_css_classes(ttype)
                cspan = cls and '<span class="%s">' % cls or ''

            safe_value = value.translate(escape_table)
            if ttype in [Name.Class, Name, Operator, Literal] \
                    and value not in CLEAN_SYNTAX_TOKENS:
                value = u'<a href="https://cloogle.org/#%s">%s</a>' % (
                    urllib.parse.quote(value), safe_value)
            else:
                value = safe_value
            parts = value.split('\n')

            if tagsfile and ttype in Name:
                filename, linenumber = self._lookup_ctag(value)
                if linenumber:
                    base, filename = os.path.split(filename)
                    if base:
                        base += '/'
                    filename, extension = os.path.splitext(filename)
                    url = self.tagurlformat % {'path': base, 'fname': filename,
                                               'fext': extension}
                    parts[0] = "<a href=\"%s#%s-%d\">%s" % \
                        (url, self.lineanchors, linenumber, parts[0])
                    parts[-1] = parts[-1] + "</a>"

            # for all but the last line
            for part in parts[:-1]:
                if line:
                    if lspan != cspan:
                        line.extend(((lspan and '</span>'), cspan, part,
                                     (cspan and '</span>'), lsep))
                    else:  # both are the same
                        line.extend((part, (lspan and '</span>'), lsep))
                    yield 1, ''.join(line)
                    line = []
                elif part:
                    yield 1, ''.join(
                        (cspan, part, (cspan and '</span></a>'), lsep))
                else:
                    yield 1, lsep
            # for the last line
            if line and parts[-1]:
                if lspan != cspan:
                    line.extend(((lspan and '</span>'), cspan, parts[-1]))
                    lspan = cspan
                else:
                    line.append(parts[-1])
            elif parts[-1]:
                line = [cspan, parts[-1]]
                lspan = cspan
            # else we neither have to open a new span nor set lspan

        if line:
            line.extend(((lspan and '</span>'), lsep))
            yield 1, ''.join(line)


print(pygments.highlight(
    ''.join(sys.stdin),
    pygments.lexers.get_lexer_by_name('clean'),
    CloogleHtmlFormatter(
        full=True,
        linenos=True,
        linespans='line',
        encoding='iso8859',
        hl_lines=[] if len(sys.argv) == 0 else [int(a) for a in sys.argv[1:]],
        cssfile='view.css',
        noclobber_cssfile=True,
        )).decode('utf-8'))
