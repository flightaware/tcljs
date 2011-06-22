/*
 * tcljs.c --
 *
 *	This file implements a Tcl interface to JavaScript (SpiderMonkey)
 *
 * Copyright (c) Jos Decoster
 * Copyright (c) 2009 by FlightAware, All Rights Reserved
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include <tcl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "jsapi.h"
#include "jsstr.h"
#include "jsfun.h"

enum TclJsReturnType { TJR_BOOLEAN, TJR_DOUBLE, TJR_INT, TJR_STRING };

typedef struct TclJsClass {
    JSRuntime *rt;
    JSContext *cx;
    JSObject *glob;
    JSClass global_class;
} TclJsClass;

typedef struct TclJsFunction {
    char* tclFuncName;
    char* jsFuncName;
    enum TclJsReturnType returnType;
} TclJsFunction;

struct TclJsPrivateContextData {
    int error;
    Tcl_Interp *interp;
    Tcl_Obj *errorMessageObj;
    Tcl_Obj *errorFileName;
    Tcl_Obj *errorLineNumber;
    int branchCallbackCount;
    Tcl_HashTable functionHashTable;
    Tcl_Obj *callbackScript;
};

static int tclJsCnt = 0;

/*
 *----------------------------------------------------------------------
 *
 * tclJsErrorHandler --
 *
 *	Implements the error handler used when evaluating JS scripts
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Generate JS-error message
 *
 *----------------------------------------------------------------------
 */
static void
tclJsErrorHandler(
    JSContext *cx,
    const char *errorMessage,
    JSErrorReport *errorReport
    )
{
    struct TclJsPrivateContextData *private = (struct TclJsPrivateContextData *)
	JS_GetContextPrivate(cx);
    private->error = 1;
    private->errorMessageObj = Tcl_NewStringObj(errorMessage, -1);
    private->errorLineNumber = Tcl_NewIntObj(errorReport->lineno);
    if (errorReport->filename)
	private->errorFileName = Tcl_NewStringObj(errorReport->filename, -1);
    else
	private->errorFileName = NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * tclJsTclCmd --
 *
 *	Implements the calling of Tcl-functions from JS scripts
 *
 * Results:
 *	
 *
 * Side effects:
 *	
 *
 *----------------------------------------------------------------------
 */
static JSBool
tclJsTclCmd(
    JSContext *cx,
    JSObject *obj,
    uintN argc,
    jsval *argv,
    jsval *rval
    )
{
    int i, len, intVal;
    double doubleVal;
    JSFunction *jsFunc;
    const char *funcName;
    Tcl_Obj *sobj, *eobj;
    JSString *str;
    const Tcl_UniChar *ucResult;
    Tcl_HashEntry *interpEntry;
    TclJsFunction *tclJsFunction;
    struct TclJsPrivateContextData *private = (struct TclJsPrivateContextData *)
	JS_GetContextPrivate(cx);
    
    /* get function name */
    jsFunc = JS_ValueToFunction(cx, argv[-2]);
    if (jsFunc == NULL) {
	JS_ReportError(cx, "Could not get JSFunction object for Tcl command "
		       "called from JavaScript");
	return JS_FALSE;
    }
    funcName = JS_GetFunctionName(jsFunc);

    /* look for function with same name in associated interpreter */
    interpEntry = Tcl_FindHashEntry(&private->functionHashTable,
				    (ClientData)jsFunc);
    if (interpEntry == NULL) {
	JS_ReportError(cx, "Error when evaluating Tcl command '%s"
		       "' called from JavaScript: could not find Tcl "
		       "interpreter", funcName);
	return JS_FALSE;
    }
    tclJsFunction = (TclJsFunction*)Tcl_GetHashValue(interpEntry);
    
    /* create Tcl command */
    sobj = Tcl_NewListObj(0, NULL);
    Tcl_ListObjAppendElement(private->interp, sobj,
			     Tcl_NewStringObj(tclJsFunction->tclFuncName, -1));
    for(i = 0; i < (int)argc; i++) {
	str = JS_ValueToString(cx, argv[i]);
	Tcl_ListObjAppendElement(
	    private->interp, sobj,
	    Tcl_NewUnicodeObj(JS_GetStringChars(str), JS_GetStringLength(str)));
    }

    /* execute the Tcl command */
    if (Tcl_EvalObjEx(private->interp, sobj,
		      TCL_EVAL_GLOBAL) != TCL_OK) {
	eobj = Tcl_GetObjResult(private->interp);
	if (eobj == NULL) {
	    JS_ReportError(cx, "Error when evaluating Tcl command '%s"
			   "' called from JavaScript as '%s': unknown error",
			   tclJsFunction->tclFuncName, funcName);
	    return JS_FALSE;
	}
	else {
	    JS_ReportError(cx, "Error when evaluating Tcl command '%s"
			   "' called from JavaScript as '%s': %s",
			   tclJsFunction->tclFuncName, funcName,
			   Tcl_GetStringFromObj(eobj, 0));
	    return JS_FALSE;
	}
    }
    eobj = Tcl_GetObjResult(private->interp);
    switch (tclJsFunction->returnType) {
    case TJR_BOOLEAN:
	if (Tcl_GetBooleanFromObj(private->interp, eobj, &intVal) !=
	    TCL_OK) {
	    JS_ReportError(cx, "Error when evaluating Tcl command '%s"
			   "' called from JavaScript as '%s': "
			   "Could not convert result to boolean: %s",
			   tclJsFunction->tclFuncName, funcName,
			   Tcl_GetStringFromObj(eobj, 0));
	    return JS_FALSE;
	}
	*rval = BOOLEAN_TO_JSVAL(intVal);
	break;
    case TJR_DOUBLE:
	if (Tcl_GetDoubleFromObj(private->interp, eobj, &doubleVal) !=
	    TCL_OK) {
	    JS_ReportError(cx, "Error when evaluating Tcl command '%s"
			   "' called from JavaScript as '%s': "
			   "Could not convert result to double: %s",
			   tclJsFunction->tclFuncName, funcName,
			   Tcl_GetStringFromObj(eobj, 0));
	    return JS_FALSE;
	}
	*rval = INT_TO_JSVAL(doubleVal);
	break;
    case TJR_INT:
	if (Tcl_GetIntFromObj(private->interp, eobj, &intVal) !=
	    TCL_OK) {
	    JS_ReportError(cx, "Error when evaluating Tcl command '%s"
			   "' called from JavaScript as '%s': "
			   "Could not convert result to int: %s",
			   tclJsFunction->tclFuncName, funcName,
			   Tcl_GetStringFromObj(eobj, 0));
	    return JS_FALSE;
	}
	*rval = INT_TO_JSVAL(intVal);
	break;
    case TJR_STRING:
	ucResult = Tcl_GetUnicodeFromObj(eobj, &len);
	str = JS_NewUCStringCopyN(cx, ucResult, len);
	*rval = STRING_TO_JSVAL(str);
	break;
    }
    return JS_TRUE;
}

/*
 *----------------------------------------------------------------------
 *
 * tclJsInterpCmd --
 *
 *	Implements the new Tcl JS-script object command.
 *
 * Results:
 *	Plain Tcl result
 *
 * Side effects:
 *	Evaluates the specified scripts or destroys the JS-interp
 *
 *----------------------------------------------------------------------
 */
static int
tclJsInterpCmd(
    ClientData clientData,	/* Not used. */
    Tcl_Interp *interp,		/* Current interpreter */
    int objc,			/* Number of arguments */
    Tcl_Obj *const objv[]	/* Argument strings */
    )
{
    JSFunction *jsFunc;
    JSString *str;
    TclJsClass *jsClass = (TclJsClass*)clientData;
    TclJsFunction *tclJsFunction;
    Tcl_HashEntry *interpEntry;
    Tcl_HashSearch searchPtr;
    const Tcl_UniChar *ucScript;
    const char *jsFuncName;
    const char *script;
    const char *tclFuncName;
    int index, len, new, typeIndex;
    jsval rval;
    struct TclJsPrivateContextData *private = (struct TclJsPrivateContextData *)
	JS_GetContextPrivate(jsClass->cx);
    
    /* define subcommands */
    static const char *subCommands[] = {
	"eval" , "uceval", "destroy", "function", "callback", NULL
    };
    enum SubCmds {
	TCLJS_EVAL, TCLJS_UCEVAL, TCLJS_DESTROY, TCLJS_FUNCTION, TCLJS_CALLBACK
    };
    
    /* define JS return types */
    static const char *jsReturnTypes[] = {
	"boolean", "double", "int", "string", NULL
    };

    /* check arguments */
    if (objc < 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "subcommand ...");
	return TCL_ERROR;
    }
    
    if (Tcl_GetIndexFromObj(interp, objv[1], subCommands, "subcommand", 0,
			    &index) != TCL_OK)
	return TCL_ERROR;

    /* process specified command */
    switch ((enum SubCmds) index) {
    case TCLJS_EVAL:
	if (objc != 3) {
	    Tcl_WrongNumArgs(interp, 2, objv, "script");
	    return TCL_ERROR;
	}
	private->error = 0;	
	script = Tcl_GetStringFromObj(objv[2], &len);
	if (!JS_EvaluateScript(jsClass->cx, jsClass->glob, script, len,
			       "<file>", 0, &rval)) {
	    Tcl_Obj* obj = Tcl_NewStringObj("Error during evaluation: ", -1);
	    Tcl_AppendObjToObj(obj, private->errorMessageObj);
	    Tcl_SetObjResult(interp, obj);	
	    return TCL_ERROR;
	}
	str = JS_ValueToString(jsClass->cx, rval);
	Tcl_SetObjResult(interp, Tcl_NewStringObj(JS_GetStringBytes(str), JS_GetStringLength(str)));
	break;
    case TCLJS_UCEVAL:
	if (objc != 3) {
	    Tcl_WrongNumArgs(interp, 2, objv, "script");
	    return TCL_ERROR;
	}
	private->error = 0;	
	ucScript = Tcl_GetUnicodeFromObj(objv[2], &len);
	if (!JS_EvaluateUCScript(jsClass->cx, jsClass->glob, ucScript, len,
			       "<file>", 0, &rval)) {
	    Tcl_Obj* obj = Tcl_NewStringObj("Error during evaluation: ", -1);
	    Tcl_AppendObjToObj(obj, private->errorMessageObj);
	    Tcl_SetObjResult(interp, obj);	
	    return TCL_ERROR;
	}
	str = JS_ValueToString(jsClass->cx, rval);
	Tcl_SetObjResult(interp, Tcl_NewUnicodeObj(JS_GetStringChars(str), JS_GetStringLength(str)));
	break;
    case TCLJS_DESTROY:
	if (objc != 2) {
	    Tcl_WrongNumArgs(interp, 2, objv, "");
	    return TCL_ERROR;
	}
	script = Tcl_GetStringFromObj(objv[1], &len);
	Tcl_DeleteCommand(interp, script);
	break;
    case TCLJS_FUNCTION:
	if (objc < 4 || objc > 5) {
	    Tcl_WrongNumArgs(interp, 2, objv,
			     "tclName returnType "
			     "?javascriptName?");
	    return TCL_ERROR;
	}
	private->error = 0;	
	/* get function name in tcl and in JS */
	tclFuncName = Tcl_GetStringFromObj(objv[2], NULL);
	if (objc == 5)
	    jsFuncName = Tcl_GetStringFromObj(objv[4], NULL);
	else
	    jsFuncName = tclFuncName;
	/* get return type */
	if (Tcl_GetIndexFromObj(interp, objv[3], jsReturnTypes, "returnType",
				0, &typeIndex) != TCL_OK)
	    return TCL_ERROR;
	/* check if function with same JS name already exists */
	interpEntry = Tcl_FirstHashEntry(&private->functionHashTable, &searchPtr);
	while(interpEntry != NULL) {
	    tclJsFunction = (TclJsFunction*)Tcl_GetHashValue(interpEntry);
	    if (!strcmp(jsFuncName, tclJsFunction->jsFuncName)) {
		/* same JS function found, remove it */
		Tcl_DeleteHashEntry(interpEntry);
		ckfree(tclJsFunction->tclFuncName);
		ckfree(tclJsFunction->jsFuncName);
		ckfree((char*)tclJsFunction);
		break;
	    }
	    interpEntry = Tcl_NextHashEntry(&searchPtr);
	}
	/* create function in JS interpreter */
	jsFunc = JS_DefineFunction(jsClass->cx, jsClass->glob, jsFuncName,
				   tclJsTclCmd, 0, 0);
	if (jsFunc == NULL) {
	    Tcl_Obj* obj = Tcl_NewStringObj("Error creating function: ",-1);
	    Tcl_AppendObjToObj(obj, private->errorMessageObj);
	    Tcl_SetObjResult(interp, obj);	
	    return TCL_ERROR;	    
	}
	/* add function to hash */
	tclJsFunction = (TclJsFunction*)ckalloc(sizeof(TclJsFunction));
	tclJsFunction->tclFuncName = ckalloc(strlen(tclFuncName)+1);
	strcpy(tclJsFunction->tclFuncName, tclFuncName);
	tclJsFunction->jsFuncName = ckalloc(strlen(jsFuncName)+1);
	tclJsFunction->returnType = (enum TclJsReturnType)typeIndex;
	strcpy(tclJsFunction->jsFuncName, jsFuncName);
	interpEntry = Tcl_CreateHashEntry(
	    &private->functionHashTable, (const char*)jsFunc, &new);
	Tcl_SetHashValue(interpEntry, (ClientData)tclJsFunction);
	break;
    case TCLJS_CALLBACK:
	if (objc != 3) {
	    Tcl_WrongNumArgs(interp, 2, objv, "script");
	    return TCL_ERROR;
	}
	if (private->callbackScript != NULL)
	    Tcl_DecrRefCount(private->callbackScript);
	Tcl_IncrRefCount(objv[2]);
	private->callbackScript = objv[2];
	break;
    }
    
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * tclJsDeleteInterp --
 *
 *	Delete command associated with interp-commands
 *
 * Results:
 *	
 *
 * Side effects:
 *	Deletes the JS-interp
 *
 *----------------------------------------------------------------------
 */
static void
tclJsDeleteInterp(
    ClientData clientData
    )
{
    Tcl_HashEntry *interpEntry;
    TclJsClass *jsClass = (TclJsClass*)clientData;
    Tcl_HashSearch searchPtr;
    TclJsFunction *tclJsFunc;
    struct TclJsPrivateContextData *private = (struct TclJsPrivateContextData *)
	JS_GetContextPrivate(jsClass->cx);
    
    /* empty hashtable */
    interpEntry = Tcl_FirstHashEntry(&private->functionHashTable, &searchPtr);
    while(interpEntry != NULL) {
	tclJsFunc = (TclJsFunction*)Tcl_GetHashValue(interpEntry);
	Tcl_DeleteHashEntry(interpEntry);
	ckfree(tclJsFunc->tclFuncName);
	ckfree(tclJsFunc->jsFuncName);
	ckfree((char*)tclJsFunc);
	interpEntry = Tcl_NextHashEntry(&searchPtr);
    }
    Tcl_DeleteHashTable(&private->functionHashTable);
    
    /* TBD: delete js context data? */
    if (private->callbackScript != NULL)
	Tcl_DecrRefCount(private->callbackScript);
    
    /* destroy JS interpreter */
    JS_DestroyContext(jsClass->cx);
    JS_DestroyRuntime(jsClass->rt);
    ckfree((char*)jsClass);
}

/*
 *----------------------------------------------------------------------
 *
 * tclJsBranchCallback --
 *
 *    javascript branch callback handler
 *
 * Results:
 *    periodically runs JS_MaybeGC and eventually some other stuff
 *
 * NB - if needed, implement code to raise an exception here, like to
 *      abort a long-running script
 *
 *    - add callback to tcl here (e.g. to process event loop)
 *
 *----------------------------------------------------------------------
 */
JSBool tclJsBranchCallback (JSContext *cx, JSScript *script)
{
    struct TclJsPrivateContextData *private = (struct TclJsPrivateContextData *)
	JS_GetContextPrivate (cx);

    if (private->callbackScript != NULL) {
	int code = Tcl_GlobalEvalObj(private->interp, private->callbackScript);
	if (code != TCL_OK) {
	    jsval jserr;
	    Tcl_Obj* tclerr = Tcl_GetObjResult(private->interp);
	    Tcl_Obj* options = Tcl_GetReturnOptions(private->interp, code);
	    Tcl_Obj* key = Tcl_NewStringObj("-errorinfo", -1);
	    Tcl_Obj* stackTrace = 0;
	    int len = 0;
	    JSString *str;
	    const Tcl_UniChar *ucResult;
	    Tcl_IncrRefCount(key);
	    if (Tcl_DictObjGet(NULL, options, key, &stackTrace) == TCL_OK)
		ucResult = Tcl_GetUnicodeFromObj(stackTrace, &len);
	    else
		ucResult = Tcl_GetUnicodeFromObj(tclerr, &len);
	    str = JS_NewUCStringCopyN(cx, ucResult, len);
	    jserr = STRING_TO_JSVAL(str);
	    Tcl_DecrRefCount(key);
	    JS_SetPendingException(cx, jserr);
	    return JS_FALSE;
	}
    }
    
    if (++private->branchCallbackCount % 5000 != 0)
        return JS_TRUE;

    JS_MaybeGC(cx);

    return JS_TRUE;
}


/*
 *----------------------------------------------------------------------
 *
 * tclJsInterp --
 *
 *	Implements the new Tcl "js::interp" command.
 *
 * Results:
 *	A handle to the JavaScript interpreter
 *
 * Side effects:
 *	Creates a JavaScript interpreter and interpreter command proc
 *
 *----------------------------------------------------------------------
 */
static int
tclJsInterp(
    ClientData clientData,	/* Not used. */
    Tcl_Interp *interp,		/* Current interpreter */
    int objc,			/* Number of arguments */
    Tcl_Obj *const objv[]	/* Argument strings */
    )
{
    TclJsClass *jsClass;
    char nm[32];
    struct TclJsPrivateContextData *private = 0;
    
    /* check arguments */
    if (objc != 1){
	Tcl_WrongNumArgs(interp, 1, objv, "");
	return TCL_ERROR;
    }

    /* alloc and initialise TclJsClass object */

    jsClass = (TclJsClass*)ckalloc(sizeof(TclJsClass));

    jsClass->rt = NULL;
    jsClass->cx = NULL;
    jsClass->glob = NULL;
    jsClass->global_class.name = "global";
    jsClass->global_class.flags = JSCLASS_GLOBAL_FLAGS;
    jsClass->global_class.addProperty = JS_PropertyStub;
    jsClass->global_class.delProperty = JS_PropertyStub;
    jsClass->global_class.getProperty = JS_PropertyStub;
    jsClass->global_class.setProperty = JS_PropertyStub;
    jsClass->global_class.enumerate = JS_EnumerateStub;
    jsClass->global_class.resolve = JS_ResolveStub;
    jsClass->global_class.convert = JS_ConvertStub;
    jsClass->global_class.finalize = JS_FinalizeStub;
    jsClass->global_class.getObjectOps = NULL;
    jsClass->global_class.checkAccess = NULL;
    jsClass->global_class.call = NULL;
    jsClass->global_class.construct = NULL;
    jsClass->global_class.xdrObject = NULL;
    jsClass->global_class.hasInstance = NULL;
    jsClass->global_class.mark = NULL;
    jsClass->global_class.reserveSlots = NULL;

    /* initialize the JS run time, and return result in rt */
    jsClass->rt = JS_NewRuntime(64L * 1024L * 1024L);
    if (!jsClass->rt) {
	ckfree((char*)jsClass);
	Tcl_SetObjResult(interp, Tcl_NewStringObj(
			     "Could not create new JavaScript runtime", -1));
	return TCL_ERROR;
    }
    
    /* establish a context */
    jsClass->cx = JS_NewContext(jsClass->rt, 8192);
    if (jsClass->cx == NULL) {
	JS_DestroyRuntime(jsClass->rt);
	ckfree((char*)jsClass);
	Tcl_SetObjResult(interp, Tcl_NewStringObj(
			     "Could not create new JavaScript context", -1));
	return TCL_ERROR;
    }

    /* js initialisations */
    JS_SetOptions (jsClass->cx, JSOPTION_VAROBJFIX);

// NB - JSVERSION_LATEST is documented but not defined in spidermonkey source
// as of 1.7
#ifdef JSVERSION_LATEST
    JS_SetVersion(cx, JSVERSION_LATEST);
#else
    JS_SetVersion(jsClass->cx, JSVERSION_1_7);
#endif

    /* set error callback */
    JS_SetErrorReporter (jsClass->cx, tclJsErrorHandler);
    
    /* add a branch callback */
    JS_SetBranchCallback(jsClass->cx, tclJsBranchCallback);

    /* alloc and initialise js private context data */
    private = (struct TclJsPrivateContextData *)
	ckalloc (sizeof(struct TclJsPrivateContextData));
    private->error = 0;
    private->interp = interp;
    private->errorMessageObj = NULL;
    private->errorFileName = NULL;
    private->errorLineNumber = NULL;
    private->branchCallbackCount = 0;
    private->callbackScript = NULL;
    Tcl_InitHashTable(&private->functionHashTable, TCL_ONE_WORD_KEYS);
    JS_SetContextPrivate (jsClass->cx, private);
    
    /* create the global object here */
    jsClass->glob = JS_NewObject(jsClass->cx, &(jsClass->global_class),
				 NULL, NULL);

    if (jsClass->glob == NULL) {
	JS_DestroyContext(jsClass->cx);
	JS_DestroyRuntime(jsClass->rt);
	ckfree((char*)jsClass);
	Tcl_SetObjResult(interp, Tcl_NewStringObj(
			 "Could not create new JavaScript global object", -1));
	return TCL_ERROR;
    }

    /* initialize the built-in JS objects and the global object */
    if (!JS_InitStandardClasses(jsClass->cx, jsClass->glob)) {
	JS_DestroyContext(jsClass->cx);
	JS_DestroyRuntime(jsClass->rt);
	ckfree((char*)jsClass);
	Tcl_SetObjResult(interp, Tcl_NewStringObj(
			 "Could not initialise JavaScript standard classes", -1));
	return TCL_ERROR;
    }

    /* create name of newly create JS-interp */
    sprintf(nm, "::js::jsip%d", tclJsCnt++);

    /* create the JS-interp command for the newly create JS-interp */
    Tcl_CreateObjCommand(interp, nm, tclJsInterpCmd, (ClientData)jsClass,
			 tclJsDeleteInterp);

    /* return name of JS-interp */
    Tcl_SetObjResult(interp, Tcl_NewStringObj(nm, -1));

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcljs_Init --
 *
 *	Initialize the new package.  The string "Sample" in the
 *	function name must match the PACKAGE declaration at the top of
 *	configure.in.
 *
 * Results:
 *	A standard Tcl result
 *
 * Side effects:
 *	The TclJs package is created.
 *	The new command "js::init" is added to the Tcl interpreter.
 *
 *----------------------------------------------------------------------
 */

DLLEXPORT int
Tcljs_Init(Tcl_Interp *interp)
{
    /*
     * This may work with 8.0, but we are using strictly stubs here,
     * which requires 8.1.
     */
    if (Tcl_InitStubs(interp, "8.1", 0) == NULL) {
	return TCL_ERROR;
    }
    if (Tcl_PkgRequire(interp, "Tcl", "8.1", 0) == NULL) {
	return TCL_ERROR;
    }
    if (Tcl_PkgProvide(interp, "TclJs", PACKAGE_VERSION) != TCL_OK) {
	return TCL_ERROR;
    }
    Tcl_CreateObjCommand(interp, "::js::interp", (Tcl_ObjCmdProc *) tclJsInterp,
			 (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL);
    return TCL_OK;
}
