//
// WebSocket/Erlang interface
//
//

// operations:
//     {rsync,IRef,Request}  -> {reply,IRef,Reply}
//     {nsync,IRef,Request}  -> {noreply,IRef}
//     {async,IRef,Request}  -> void
//
// request:
//     {new, Class, Arguments}
//        -> {object,ID}
//
//     {newf, ParamNames, Body}
//        -> {function,ID}
//
//     {delete, ID}
//        -> ok | {error,Reason}
//
//     {call, Object, method, This, Arguments}
//        -> {ok,Value} | {error,Reason}
//
//     {call, Function, This, Arguments}
//        -> {ok,Value} | {error,Reason}
//
//     {get, Object, AttrIndex} 
//        -> {ok,Value} | {error,Reason}
//     {set, Object, AttrIndex, Value}
//        -> {ok,Value} | {error,Reason}
//
// Reverse operations:
//     {start, Mod, Fun, Args} 
//     {call,  Mod, Fun, Args} -> Value
//     {cast,  Mod, Fun, Args}
//
//     {notify, ID, Data}
//     {info,  String}
//
//  Values:
//     number => integer|float
//     string => list of chars
//     atom   => string
//     boolean => true|false
//     array  => list of values
//     function => {function, ID}
//     object   => {object,ID}
//

function WseClass() {
    this.ws          = undefined;
    this.state       = "closed";

    this.oid         = 1;
    this.objects     = new Array();

    this.iref        = 1;
    this.requests    = new Array();
    this.replies     = new Array();

    this.OkTag       = Ei.atom("ok");
    this.ErrorTag    = Ei.atom("error");
    this.ObjectTag   = Ei.atom("object");
    this.FunctionTag = Ei.atom("function");
    this.ReplyTag    = Ei.atom("reply");
    this.NoReplyTag  = Ei.atom("noreply");
};

// Fixme: check binaryType for the wanted encoding ?!
WseClass.prototype.encode = function(Obj) {
//    return Base64.encode(Ei.encode(Obj));
    return Ei.encode(Obj);
};

// Fixme: handle base64 string & Blob and ArrayBuffer!
WseClass.prototype.decode = function(Data) {
//   return Ei.decode(Base64.decode(Data));
    return Ei.decode(Data, 0);
};

WseClass.prototype.open = function(url) {
    var wse = this;  // save WebSocket closure

    if ("WebSocket" in window) {
	this.state = "connecting";
	this.ws = new WebSocket(url);
	this.ws.binaryType = "arraybuffer";

	this.ws.onopen = function() {
            var info = Ei.tuple(Ei.atom("info"),"connected");
	    wse.state = "open";
            this.send(wse.encode(info));
	    for (ref in wse.requests) {
		var cmd = wse.requests[ref];
		if (cmd !== undefined)
		    this.send(wse.encode(cmd));
	    }
	    wse.requests = new Array();
	};
	
	this.ws.onmessage = function(evt) {
            var Request = wse.decode(evt.data);
            var val = wse.dispatch(Request);
            if (val != undefined)
		this.send(wse.encode(val));
	};

	this.ws.onclose = function() {
	    wse.state = "closed";
	    wse.ws = undefined;
	};
	return true;
    }
    return false;
};

//
// Remove all children (DOM util)
//
WseClass.prototype.removeChildren = function(Cell) {
    if (Cell.hasChildNodes()) {
	while(Cell.childNodes.length >= 1)
	    Cell.removeChild(Cell.firstChild);
    }
}

//
// Decode javascript object into BERT rpc values
//
WseClass.prototype.encode_value = function(Obj) {
    switch(typeof(Obj)) {
    case "number": return Obj;
    case "string": return Obj;
    case "boolean":
	if (Obj) 
	    return Ei.atom("true");
	return Ei.atom("false");
    case "object":
	// {object, window}    - the current window object
	// {object, document}  - the current document object
	// {object, id}        - DOM object with id field
	// {object, num}       - Stored in objects array
	if (Obj == window.self)
	    return Ei.tuple(this.ObjectTag,Ei.atom("window"));
	if (Obj == window.document) 
	    return Ei.tuple(this.ObjectTag,Ei.atom("document"));
	if (('id' in Obj) && Obj.id) {
	    if (Obj == document.getElementById(Obj.id))
		return Ei.tuple(this.ObjectTag,Obj.id);
	}
	if (!('wsekey' in Obj)) {
	    Obj.wsekey = this.oid++;
	    this.objects[Obj.wsekey] = Obj;
	}
	return Ei.tuple(this.ObjectTag,Obj.wsekey);
    case "function":
	if (!('wsekey' in Obj)) {
	    Obj.wsekey = this.oid++;
	    this.objects[Obj.wsekey] = Obj;
	}
	return Ei.tuple(this.FunctionTag,Obj.wsekey);
    case "undefined":
	return Ei.atom("undefined");
    }
};

//
// Decode BERT rpc values into javascript objects
// {object, window} => window.self
// {object, document} => window.document
// {object, id}       => window.document.getElelementById(id)
// {object, num}      => objects[num]
// {function,num}     => objects[num]
// [H1,H2...Hn]       => Array
//
WseClass.prototype.decode_value = function(Obj) {
    switch(typeof(Obj)) {
    case "number":  return Obj;
    case "string":  return Obj;
    case "boolean": return Obj;
    case "object":
	if (Ei.isAtom(Obj))
	    return Obj.value;
	else if (Ei.isTuple(Obj)) {
	    var elem = Obj.value;
	    if ((elem.length==2) && Ei.eqAtom(elem[0],"object")) {
		if (Ei.eqAtom(elem[1], "window"))
		    return window.self;
		else if (Ei.eqAtom(elem[1],"document"))
		    return window.document;
		else if (typeof(elem[1]) == "number")
		    return this.objects[elem[1]];
		else if (typeof(elem[1]) == "string")
		    return window.document.getElementById(elem[1]);
	    }
	    if ((elem.length==2) && Ei.eqAtom(elem[0],"function")) {
		if (typeof(elem[1]) == "number")
		    return this.objects[elem[1]];
		else {
		    console.debug("object " + this.objects);
		}
	    }
	    return undefined;
	}
	else if (Ei.isArray(Obj)) {
	    var i;
	    var arr = new Array();
	    for (i = 0; i < Obj.length; i++)
		arr[i] = this.decode_value(Obj[i]);
	    return arr;
	}
	return undefined;
    default:
	console.debug("unhandled object "+ Obj);
	return Obj;
    }	
};
//
// Decode ehtml to DOM 
// Elem =  {Tag,Attributes,Children}
//       | {Tag,Attributes}
// Tag is EiAtom
// Attributes is Array of Tuple(Atom,Value) or Atom
// optional Children is Array of Elem
// Return top level DOM element
//
WseClass.prototype.decode_ehtml = function (Obj) {
    var element = undefined;

    if (typeof(Obj) == "string") {
	element = document.createTextNode(Obj);
    }
    else if (typeof(Obj) == "number") {
	element = document.createTextNode(Obj.toString());	
    }
    else if (Ei.isArray(Obj)) {
	element = document.createDocumentFragment();
	for (i = 0; i < Obj.length; i++) {
	    var child = this.decode_ehtml(Obj[i]);
	    if (child != undefined)
		element.appendChild(child);
	}
    }
    else if (Ei.isTuple(Obj)) {
	var argv = Obj.value;

	if ((argv.length == 0) || !Ei.isAtom(argv[0]))
	    return undefined;

	element = document.createElement(argv[0].value);

	if ((argv.length > 1) && Ei.isArray(argv[1])) {
	    var attrs = argv[1];
	    var i;
	    for (i = 0; i < attrs.length; i++) {
		if (Ei.isTuple(attrs[i])) {
		    var key = this.decode_value(attrs[i].value[0]);
		    var value;
		    if (attrs[0].length > 1)
			value = this.decode_value(attrs[i].value[1]);
		    else
			value = true;
		    element.setAttribute(key, value);
		}
	    }
	    if ((argv.length > 2) && Ei.isArray(argv[2])) {
		var children = argv[2];
		for (i = 0; i < children.length; i++) {
		    var child = this.decode_ehtml(children[i]);
		    if (child != undefined)
			element.appendChild(child);
		}
	    }
	}
    }
    return element;
};

//
// Dispatch remote operations
//
WseClass.prototype.dispatch = function (Request) {
    var iref;
    var value;
    var r;
    var t;

    if (Ei.isTupleSize(Request, 3)) {
	var argv = Request.value;
	if (Ei.eqAtom(argv[0],      "rsync"))
	    iref = argv[1];
	else if (Ei.eqAtom(argv[0], "nsync"))
	    iref = -argv[1];
	else if (Ei.eqAtom(argv[0], "async"))
	    iref = 0;
	else if (Ei.eqAtom(argv[0], "reply")) {
	    var fn;
	    iref = argv[1];
	    value = argv[2];
	    fn = this.replies[iref];
	    if (fn != undefined) {
		delete this.replies[iref];
		fn(value);
	    }
	    return undefined;
	}
	else
	    return undefined;  // signal protocol error
	r = argv[2];
    }
    else
	return undefined;

    if (Ei.isTuple(r)) {
	var argv = r.value;
	if ((argv.length == 3) && Ei.eqAtom(argv[0],"send")) {
	    var Cell = document.getElementById(argv[1]);
	    // console.debug("SEND");
	    if (typeof(argv[2]) == "string") {
		Cell.innerHTML = Ei.pp(argv[2]);
	    }
	    else if (Ei.isTuple(argv[2]) || Ei.isArray(argv[2])) {
		var elem = this.decode_ehtml(argv[2]);
		this.removeChildren(Cell);
		Cell.appendChild(elem);
		// parentNode.replaceChild(elem, obj);
	    }
	    value = this.OkTag;  // FIXME
	}
	else if ((argv.length == 3) && Ei.eqAtom(argv[0],"new")) {
	    // console.debug("NEW_OBJECT");
	    var obj = new Object();
	    var fn  = window[this.decode_value(argv[1])];
	    fn.apply(obj, this.decode_value(argv[2]));
	    obj.__proto__ = fn.prototype;
	    value = this.encode_value(obj);
	}
	else if ((argv.length == 3) && Ei.eqAtom(argv[0],"newf")) {
	    // console.debug("NEW_FUNCTION");
	    var fn = new Function(argv[1],argv[2]);
	    // console.debug("function = "+fn);
	    value = this.encode_value(fn);
	}
	else if ((argv.length == 4) && Ei.eqAtom(argv[0],"call")) {
	    var fn   = this.decode_value(argv[1]);
	    var objb = this.decode_value(argv[2]);
	    var args = this.decode_value(argv[3]);
	    var val;
	    var vale;
	    val = window[fn].apply(objb, args);
	    vale = this.encode_value(val);
	    console.debug("call/3=" + Ei.pp(argv[1]) + "," + Ei.pp(argv[2]) + "," + Ei.pp(argv[3]));
	    value = Ei.tuple(this.OkTag, vale);
	}
	else if ((argv.length == 5) && Ei.eqAtom(argv[0],"call")) {
	    var obja = this.decode_value(argv[1]);
	    var meth = this.decode_value(argv[2]);
	    var objb = this.decode_value(argv[3]);
	    var args = this.decode_value(argv[4]);
	    var val;
	    var vale;
	    val  = (obja[meth]).apply(objb, args);
	    vale = this.encode_value(val);
	    console.debug("call/4=" + Ei.pp(argv[1]) + "," + Ei.pp(argv[2]) + "," + Ei.pp(argv[3]) + "," + Ei.pp(argv[4]));
	    value = Ei.tuple(this.OkTag, vale);
	}
	else if ((argv.length == 3) && Ei.eqAtom(argv[0],"get")) {
	    var obj   = this.decode_value(argv[1]);
	    var attr  = this.decode_value(argv[2]);
	    // var val   = obj[attr];
	    var val   = obj.getAttribute(attr);
	    console.debug(argv[1]+".get: "+attr+"="+val);
	    value = Ei.tuple(this.OkTag, this.encode_value(val));
	}
	else if ((argv.length == 4) && Ei.eqAtom(argv[0],"set")) {
	    var obj   = this.decode_value(argv[1]);
	    var attr  = this.decode_value(argv[2]);
	    var val   = this.decode_value(argv[3]);
	    console.debug(argv[1]+".set: "+attr+"="+argv[3]+"("+val+")");
	    obj.setAttribute(attr, val);
	    // obj[attr] = val;
	    value = this.OkTag;
	}
	else if ((argv.length === 2) && Ei.eqAtom(argv[0],"delete")) {
	    if (this.objects[argv[1]] != 'undefined') {
		delete this.objects[argv[1]];
		value = this.OkTag;
	    }
	}
    }
    if (iref == 0) {
	// console.debug("ival=0");
	return undefined;
    }
    else if (iref > 0) {
	if (value == undefined) {
	    value = Ei.tuple(this.ErrorTag, Ei.atom("badarg"));
	}
	t = Ei.tuple(this.ReplyTag,iref,value);
    }
    else {
	t = Ei.tuple(this.NoReplyTag,-iref);
    }
    // console.debug("t = " + Ei.pp(t));
    return t;
};

//
// Start remote controller "program" 
//
WseClass.prototype.start = function (mod,fun,args) {
    var cmd = Ei.tuple(Ei.atom("start"),Ei.atom(mod),Ei.atom(fun),args);
    if (this.state === "open")
	this.ws.send(this.encode(cmd));
    else {
	var ref = this.iref++;
	this.requests[ref] = cmd;
    }
    return true;
};

//
// Call remote function mod:fun(Args)
// execute onreply when reply is returned
//
WseClass.prototype.call = function (mod,fun,args,onreply) {
    var ref = this.iref++;
    var cmd = Ei.tuple(Ei.atom("call"),ref,
	Ei.atom(mod),Ei.atom(fun),args);
    if (this.state == "open") {
	this.ws.send(this.encode(cmd));
	this.replies[ref] = onreply;
	return true;
    }
    else {
	this.requests[ref] = cmd;
	this.replies[ref]  = onreply;
	return true;
    }
    return false;
};
//
// Execute remote function mod:fun(Args)
//
WseClass.prototype.cast = function (mod,fun,args) {
    var ref = this.iref++;
    var cmd = Ei.tuple(Ei.atom("cast"),ref,
	Ei.atom(mod),Ei.atom(fun),args);
    if (this.state == "open") {
	this.ws.send(this.encode(cmd));
	return true;
    }
    else {
	this.requests[ref] = cmd;
	return true;
    }
    return false;
};

//
// Send event 
//
WseClass.prototype.notify = function (ref,data) {
    var cmd = Ei.tuple(Ei.atom("notify"),ref,data);
    if (this.state == "open") {
	this.ws.send(this.encode(cmd));
	return true;
    }
    else {
	this.requests[ref] = cmd;
	return true;
    }
    return false;
};

var Wse = new WseClass();
