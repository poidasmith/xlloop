--[[ ***************************************************************************
* 
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
******************************************************************************]]

-- Provides general CSV utilities

local io = require 'io'

local function read(file, separator, quote_char)
	local sep = quote_char or ','
	local header = {}
	local table = {}
	for line in io.lines(file) do
		
	end
	return table
end

local function write(array, separator, quote_char)
	
end

local function headers(file, separator, is_quoted)
end

local function column_count(file, separator, is_quoted)
	return 
end

local function row_count(file)
end
