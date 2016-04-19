<?php
error_reporting(E_ALL);
ini_set('display_errors', '1');

define('PRE_IDENT', '[\w~@#$%^?!+\-*<>\/|&=:`]+');
define('PRE_MODULE', 
	"/\s*(?:definition\s*|system\s*|implementation\s*)module\s+([\w.]+)\s*[\n;]/");
define('PRE_FUNC', 
  '/^(?:\\/\\/)?\s*(?:instance|class)?\s*\(?(' . PRE_IDENT . ')\)?\s*(?:infix[lr]?\s+\d\s*(?:\\/\\/)?)?(?:\s+a\s+)?::.*$/mi');

function search_doc(&$r, $name, $libraries, $searchmodules){
	foreach($libraries as $library => $librarypath){
		$files = glob($librarypath . "*.dcl", GLOB_NOSORT | GLOB_MARK);
		foreach($files as $filepath) {
			if(mb_substr($filepath, -1) !== DIRECTORY_SEPARATOR){
				$path_segments = explode(DIRECTORY_SEPARATOR, $filepath);
				$filename = end($path_segments);
				$contents = file_get_contents($filepath);
				$module = preg_match(PRE_MODULE, $contents, $modules) == 1 ?
					$modules[1] : NULL;
				if(count($searchmodules) > 0 && !in_array($module, $searchmodules)){
					continue;
				}
				print_r($searchmodules);
				printf($module);
				if(preg_match_all(PRE_FUNC, $contents, $funcs) !== false){
					for($i=0; $i<count($funcs[1]); $i++){
						$funcname = $funcs[1][$i];
						$funcsig = $funcs[0][$i];
						$lowername = strtolower($name);
						$lowerfuncname = strtolower($funcname);
						if(strstr($lowerfuncname, $lowername) !== FALSE){
							$score = -100+levenshtein($lowername, $lowerfuncname);
						} else {
							$score = levenshtein($lowername, $lowerfuncname);
						}
						if($score < 3){
							array_push($r, array(
								"library" => $library,
								"filename" => $filename,
								"func" => str_replace("\n", "", $funcsig),
								"module" => $module,
								"distance" => $score));
						}
					}
				}
			}
		}		
	}
	return "Success";
}

function sort_results(&$r, $by='distance'){
	usort($r, function($a, $b) use ($by) { return $a[$by] > $b[$by]; });
}

if($_SERVER['REQUEST_METHOD'] !== 'GET'){
	echo json_encode(array(
		"return" => 1,
		"data" => array(),
		"msg" => "Can only be accessed by GET request"));
} else if(!isset($_GET['str'])){
	echo json_encode(array(
		"return" => 2,
		"data" => array(),
		"msg" => "GET variable 'str' should be set"));
} else {
	$libraries = array(
		'ArgEnv' => './ArgEnv/',
		'Directory' => './Directory/',
		'Dynamics' => './Dynamics/',
		'Generics' => './Generics/',
		'MersenneTwister' => './MersenneTwister/',
		'StdEnv' => './StdEnv/',
		'StdLib' => './StdLib/',
		'TCPIP' => './TCPIP/',
		'cleanplatform:OS-Linux-64' =>'./clean-platform/OS-Linux-64/',
		'cleanplatform:OS-Linux-64/System' =>'./clean-platform/OS-Linux-64/System/',
		'cleanplatform:OS-Linux-64/Database' =>'./clean-platform/OS-Linux-64/Database/',
		'cleanplatform:OS-Linux-64/Database/SQL' =>'./clean-platform/OS-Linux-64/Database/SQL/',
		'cleanplatform:OS-Mac' =>'./clean-platform/OS-Mac/',
		'cleanplatform:OS-Mac/System' =>'./clean-platform/OS-Mac/System/',
		'cleanplatform:OS-Mac/Database' =>'./clean-platform/OS-Mac/Database/',
		'cleanplatform:OS-Mac/Database/SQL' =>'./clean-platform/OS-Mac/Database/SQL/',
		'cleanplatform:OS-Mac/Network' =>'./clean-platform/OS-Mac/Network/',
		'cleanplatform:OS-Linux-32' =>'./clean-platform/OS-Linux-32/',
		'cleanplatform:OS-Linux-32/System' =>'./clean-platform/OS-Linux-32/System/',
		'cleanplatform:OS-Windows-64' =>'./clean-platform/OS-Windows-64/',
		'cleanplatform:OS-Windows-64/System' =>'./clean-platform/OS-Windows-64/System/',
		'cleanplatform:OS-Linux' =>'./clean-platform/OS-Linux/',
		'cleanplatform:OS-Linux/System' =>'./clean-platform/OS-Linux/System/',
		'cleanplatform:OS-Linux/Network' =>'./clean-platform/OS-Linux/Network/',
		'cleanplatform:OS-Posix' =>'./clean-platform/OS-Posix/',
		'cleanplatform:OS-Posix/System' =>'./clean-platform/OS-Posix/System/',
		'cleanplatform:OS-Posix/Network' =>'./clean-platform/OS-Posix/Network/',
		'cleanplatform:OS-Posix/DataSources' =>'./clean-platform/OS-Posix/DataSources/',
		'cleanplatform:OS-Windows-32' =>'./clean-platform/OS-Windows-32/',
		'cleanplatform:OS-Windows-32/System' =>'./clean-platform/OS-Windows-32/System/',
		'cleanplatform:OS-Windows' =>'./clean-platform/OS-Windows/',
		'cleanplatform:OS-Windows/System' =>'./clean-platform/OS-Windows/System/',
		'cleanplatform:OS-Windows/Database' =>'./clean-platform/OS-Windows/Database/',
		'cleanplatform:OS-Windows/Database/SQL' =>'./clean-platform/OS-Windows/Database/SQL/',
		'cleanplatform:OS-Windows/Network' =>'./clean-platform/OS-Windows/Network/',
		'cleanplatform:OS-Windows/DataSources' =>'./clean-platform/OS-Windows/DataSources/',
		'cleanplatform:OS-Windows/Data' =>'./clean-platform/OS-Windows/Data/',
		'cleanplatform:OS-Independent' =>'./clean-platform/OS-Independent/',
		'cleanplatform:OS-Independent/Math' =>'./clean-platform/OS-Independent/Math/',
		'cleanplatform:OS-Independent/System' =>'./clean-platform/OS-Independent/System/',
		'cleanplatform:OS-Independent/Crypto' =>'./clean-platform/OS-Independent/Crypto/',
		'cleanplatform:OS-Independent/Crypto/Hash' =>'./clean-platform/OS-Independent/Crypto/Hash/',
		'cleanplatform:OS-Independent/Control' =>'./clean-platform/OS-Independent/Control/',
		'cleanplatform:OS-Independent/Control/Monad' =>'./clean-platform/OS-Independent/Control/Monad/',
		'cleanplatform:OS-Independent/GUI' =>'./clean-platform/OS-Independent/GUI/',
		'cleanplatform:OS-Independent/Deprecated' =>'./clean-platform/OS-Independent/Deprecated/',
		'cleanplatform:OS-Independent/Deprecated/StdLib' =>'./clean-platform/OS-Independent/Deprecated/StdLib/',
		'cleanplatform:OS-Independent/Database' =>'./clean-platform/OS-Independent/Database/',
		'cleanplatform:OS-Independent/Database/SQL' =>'./clean-platform/OS-Independent/Database/SQL/',
		'cleanplatform:OS-Independent/Text' =>'./clean-platform/OS-Independent/Text/',
		'cleanplatform:OS-Independent/Text/Unicode' =>'./clean-platform/OS-Independent/Text/Unicode/',
		'cleanplatform:OS-Independent/Text/Unicode/Encodings' =>'./clean-platform/OS-Independent/Text/Unicode/Encodings/',
		'cleanplatform:OS-Independent/Text/Encodings' =>'./clean-platform/OS-Independent/Text/Encodings/',
		'cleanplatform:OS-Independent/Text/Parsers' =>'./clean-platform/OS-Independent/Text/Parsers/',
		'cleanplatform:OS-Independent/Text/Parsers/Test' =>'./clean-platform/OS-Independent/Text/Parsers/Test/',
		'cleanplatform:OS-Independent/Text/Parsers/MetarDemo' =>'./clean-platform/OS-Independent/Text/Parsers/MetarDemo/',
		'cleanplatform:OS-Independent/Internet' =>'./clean-platform/OS-Independent/Internet/',
		'cleanplatform:OS-Independent/Internet/HTTP' =>'./clean-platform/OS-Independent/Internet/HTTP/',
		'cleanplatform:OS-Independent/Network' =>'./clean-platform/OS-Independent/Network/',
		'cleanplatform:OS-Independent/Test' =>'./clean-platform/OS-Independent/Test/',
		'cleanplatform:OS-Independent/Data' =>'./clean-platform/OS-Independent/Data/',
		'cleanplatform:OS-Independent/Data/Functor' =>'./clean-platform/OS-Independent/Data/Functor/',
		'cleanplatform:OS-Independent/Data/IntMap' =>'./clean-platform/OS-Independent/Data/IntMap/',
		'cleanplatform:OS-Independent/Data/Encoding' =>'./clean-platform/OS-Independent/Data/Encoding/',
		'cleanplatform:OS-Independent/Graphics' =>'./clean-platform/OS-Independent/Graphics/',
		'cleanplatform:OS-Independent/Graphics/Scalable' =>'./clean-platform/OS-Independent/Graphics/Scalable/');

	$res = array();
	$modules = isset($_GET['mod']) ? explode(',', $_GET['mod']) : array();
	$msg = search_doc($res, $_GET['str'], $libraries, $modules);
	sort_results($res);
	if(!$res){
		echo json_encode(array(
			"return" => 127,
			"data" => array(),
			"msg" => "Nothing found..."));
	} else {
		echo json_encode(array(
			"return" => 0,
			"data" => $res,
			"msg" => $msg));
	}
}
?>
