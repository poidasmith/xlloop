<?php

XLLoop_Handler();

function RandTest()
{
	return "Hello there " . rand();
}

function XLLoop_Handler()
{
	try {
		$input = file_get_contents('php://input');
		$value = json_decode($input);
		$argc = count($value->args);
		$args = array();
		for($i = 0; $i < $argc; $i++) {
			$args[$i] = XLLoop_decode($value->args[$i]);
		}
		$function = new ReflectionFunction($value->name);
		$result = $function->invokeArgs($args);
		$enc = XLLoop_encode($result);
		print json_encode($enc);
	} catch (Exception $e) {
		print json_encode(XLLoop_encode("#" . $e->getMessage()));
	}
}

class XLLoop_XLError
{
	public $code;
	
	public function __construct($code) {
		$this->code = $code;
	}
}

class XLLoop_XLArray
{
	public $rows;
	public $cols;
	public $array;
	
	public function __construct($rows, $cols, $array) {
		$this->rows = $rows;
		$this->cols = $cols;
		$this->array = &$array;
	}
}

class XLLoop_XLSRef
{
	public $colFirst;
	public $colLast;
	public $rowFirst;
	public $rowLast;
	
	public function __construct($colFirst, $colLast, $rowFirst, $rowLast) {
		$this->colFirst = $colFirst;
		$this->colLast = $colLast;
		$this->rowFirst = $rowFirst;
		$this->rowLast = $rowLast;
	}
}

function XLLoop_decode($json)
{
	switch($json->type) {
	case 1: //  double
		return $json->num;
	case 2: // string
		return $json->str;
	case 3: // boolean
		return $json->bool;
	case 4: // error
		return new XLLoop_XLError($json->error);
	case 5: // multi
		$length = count($json->array);
		$value = array();
		for($i = 0; $i < $length; $i++) {
			$value[$i] = XLLoop_decode($json->array[$i]);
		}
		return new XLLoop_XLArray($json->rows, $json->cols, $value);	
	case 6: // missing
	case 7: // nil
		return NULL;
	case 8: // int
		return $json->int;
	case 9: // sref
		return new XLLoop_XLSref($json->colFirst, $json->colLast, $json->rowFirst, $json->rowLast);
	}
	
	throw new Exception("Unknown XLoper type $json->type");
}

function XLLoop_encode($value)
{
	if(is_string($value)) {
		return array("type" => 2, "str" => $value);
	} else if(is_numeric($value)) {
		return array("type" => 1, "num" => $value);
	} else if(is_bool($value)) {
		return array("type" => 3, "bool" => $value);
	} else {
		return XLLoop_encode("#Unknown object");
	}
}
?>
