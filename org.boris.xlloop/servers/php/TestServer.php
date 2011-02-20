<?php

require "XLLoop.php";

function ArgsTest($arg)
{
	return $arg;
}

function RandTest()
{
	$len = rand(1) * 50 + 2;
	return "=rand(" . $len . ")";
}

function Random()
{
	return rand();
}

function GetClass($arg)
{
	return get_class($arg);
}

function GetArgType($arg)
{
	return gettype($arg);
}

function Sum()
{
	$numargs = func_num_args();
	$result = 0;
	for($i = 0; $i < $numargs; $i++) {
		$arg = func_get_arg($i);
		if(is_numeric($arg))
			$result += $arg;
		else if(get_class($arg) == "XLLoop_XLArray") {
			$c = count($arg->array);
			for($j = 0; $j < $c; $j++) {
				$result += Sum($arg->array[$j]);
			}
		}
	}
	return $result;
}

XLLoop_Reflection_Handler();

?>
