<?php
error_reporting(E_ALL);
ini_set('display_errors', '1');

define('STDENV_PATH', './stdenv/');
define('PRE_MODULE', 
	"/\s*(?:definition\s*|system\s*|implementation\s*)module\s+(\S+)\s*[\n;]/");
define('PRE_FUNC', 
	"/\s+%s\s+::\s+.*[\n;]/i");

function search_doc(&$r, $name){
	$files = glob(STDENV_PATH . "*.dcl", GLOB_NOSORT | GLOB_MARK);
	foreach($files as $filepath) {
		if(mb_substr($filepath, -1) !== DIRECTORY_SEPARATOR){
			$filename = end(explode(DIRECTORY_SEPARATOR, $filepath));
			$contents = file_get_contents($filepath);
			$module = preg_match(PRE_MODULE, $contents, $modules) == 1 ?
				$modules[1] : NULL;
			if(preg_match(sprintf(PRE_FUNC, $name), $contents, $funcs) == 1){
				foreach($funcs as $func){
					array_push($r, array(
						"filename" => $filename,
						"func" => $func,
						"module" => $module));
				}
			}
		}
	}
	return "Success";
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
	$res = array();
	$msg = search_doc($res, $_GET['str']);
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
