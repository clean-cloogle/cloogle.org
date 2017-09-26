<?php
define('CLEANHOME', '/opt/clean');

$dom = new DOMDocument;
$dom->loadHTMLFile(CLEANHOME . '/doc/CleanLangRep/CleanRep.2.2_1.htm');

function readContentsLevel($dom, $level) {

}

function isToc($class) {
	return preg_match('/^MsoToc\d$/', $class) === 1;
}

function cleanToc($name) {
	$name = preg_replace(['/[\r\x80-\xff]+/', '/\n/'], ['', ' '], $name);
	$matches = [];
	if (preg_match_all('/^([\d\.]+)?\s*(.+?)\s+[iv\d]+$/', $name, $matches) === 1)
		return [
			'index' => $matches[1][0],
			'title' => preg_replace('/Chapter \d+\s+/', '', $matches[2][0]),
		];
	else
		return null;
}

$toc = [];
foreach ($dom->getElementsByTagName('p') as $p) {
	if (isToc($p->getAttribute('class'))) {
		$elem = cleanToc($p->textContent);
		if ($elem != null) {
			$elem['level'] = (int) str_replace('MsoToc', '', $p->getAttribute('class'));
			$elem['link'] = str_replace('#', ';jump=', $p->getElementsByTagName('a')[0]->getAttribute('href'));
			$toc[] = $elem;
		}
	}
}

class Toc {
	protected $title = '';
	protected $link = '';
	protected $ref = null;
	protected $children = [];

	public static function factory($level, $elems) {
		$toc = new Toc;
		while (count($elems) > 0) {
			$elem = array_shift($elems);
			if ($elem['level'] <= $level) {
				$deeper = [];
				while ($elems[0]['level'] > $level)
					$deeper[] = array_shift($elems);
				$child = Toc::factory($level + 1, $deeper);
				$child->title = $elem['title'];
				$child->link = $elem['link'];
				$toc->children[] = $child;
			}
		}
		return $toc;
	}

	public function printTree() {
		echo '<div class="browser togglee">';
		foreach ($this->children as $child) {
			if ($child->isLeaf()) {
				echo '<div class="browser-item module" id="doc-' . $child->link .
						'" data-name="' . $child->link . '">' .
						$child->title . '</span></div>';
			} else {
				echo '<div class="browser-item directory toggle-container" id="doc-' . $child->link . '">' .
						'<span class="toggler" onclick="toggle(this)">' .
							'<span class="toggle-icon">&#x229e</span>' .
							'<span class="title">' . $child->title . '</span></span>';
				$child->printTree();
				echo '</div>';
			}
		}
		echo '</div>';
	}

	public function isLeaf() {
		return count($this->children) == 0;
	}
}

$toc = Toc::factory(1, $toc);
$toc->printTree();
