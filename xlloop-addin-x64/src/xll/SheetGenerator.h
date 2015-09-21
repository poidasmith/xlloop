/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef SHEET_GENERATOR_H
#define SHEET_GENERATOR_H

#include "windows.h"
#include "BinaryProtocol.h"

class SheetGenerator  {
public:
	static void Register(LPXLOPER xllName, Protocol* protocol, dictionary* ini);
	static void Execute(Protocol* protocol, const char* name);
};

#endif // SHEET_GENERATOR_H