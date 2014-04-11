//
// WebSocket/Erlang interface
//
//

function Base64Class() {
    this._keyStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
};

// Base64 encoder
Base64Class.prototype.encode = function (input) {
    var output = "";
    var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
    var i = 0;

    while (i < input.length) {
	chr1 = input.charCodeAt(i++);
	chr2 = input.charCodeAt(i++);
	chr3 = input.charCodeAt(i++);
	    
	enc1 = chr1 >> 2;
	enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
	enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
	enc4 = chr3 & 63;
	    
	if (isNaN(chr2)) {
	    enc3 = enc4 = 64;
	} else if (isNaN(chr3)) {
	    enc4 = 64;
	}
	output = output +
	    this._keyStr.charAt(enc1) + this._keyStr.charAt(enc2) +
	    this._keyStr.charAt(enc3) + this._keyStr.charAt(enc4);
    }
    return output;
};
    
// Base64 decoder
Base64Class.prototype.decode = function (input) {
    var output = "";
    var chr1, chr2, chr3;
    var enc1, enc2, enc3, enc4;
    var i = 0;
	
    input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");
	
    while (i < input.length) {
	enc1 = this._keyStr.indexOf(input.charAt(i++));
	enc2 = this._keyStr.indexOf(input.charAt(i++));
	enc3 = this._keyStr.indexOf(input.charAt(i++));
	enc4 = this._keyStr.indexOf(input.charAt(i++));
	    
	chr1 = (enc1 << 2) | (enc2 >> 4);
	chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
	chr3 = ((enc3 & 3) << 6) | enc4;
	    
	output = output + String.fromCharCode(chr1);
	    
	if (enc3 != 64) {
	    output = output + String.fromCharCode(chr2);
	}
	if (enc4 != 64) {
	    output = output + String.fromCharCode(chr3);
	}
    }
    return output;
};

var Base64 = new Base64Class();

function UTF8Class() {
};

// method for UTF-8 encoding
UTF8Class.prototype.encode = function (string) {
    string = string.replace(/\r\n/g,"\n");
    var utftext = "";
	
    for (var n = 0; n < string.length; n++) {
	var c = string.charCodeAt(n);
	if (c < 128) {
	    utftext += String.fromCharCode(c);
	}
	else if((c > 127) && (c < 2048)) {
	    utftext += String.fromCharCode((c >> 6) | 192);
	    utftext += String.fromCharCode((c & 63) | 128);
	}
	else {
	    utftext += String.fromCharCode((c >> 12) | 224);
	    utftext += String.fromCharCode(((c >> 6) & 63) | 128);
	    utftext += String.fromCharCode((c & 63) | 128);
	}
    }
    return utftext;
};
    
// method for UTF-8 decoding
UTF8Class.prototype.decode = function(utftext) {
    var string = "";
    var i = 0;
    var c = c1 = c2 = 0;
	
    while (i < utftext.length) {
	c = utftext.charCodeAt(i);
	if (c < 128) {
	    string += String.fromCharCode(c);
	    i++;
	}
	else if((c > 191) && (c < 224)) {
	    c2 = utftext.charCodeAt(i+1);
	    string += String.fromCharCode(((c & 31) << 6) | (c2 & 63));
	    i += 2;
	}
	else {
	    c2 = utftext.charCodeAt(i+1);
	    c3 = utftext.charCodeAt(i+2);
	    string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
	    i += 3;
	}
    }
    return string;
};

var UTF8 = new UTF8Class();

function EiClass() {
    this.MAGIC = String.fromCharCode(131);
    this.SMALL_ATOM = String.fromCharCode(115);
    this.ATOM = String.fromCharCode(100);
    this.BINARY = String.fromCharCode(109);
    this.SMALL_INTEGER = String.fromCharCode(97);
    this.INTEGER = String.fromCharCode(98);
    this.SMALL_BIG = String.fromCharCode(110);
    this.LARGE_BIG = String.fromCharCode(111);
    this.FLOAT = String.fromCharCode(99);
    this.NEW_FLOAT = String.fromCharCode(70);
    this.STRING = String.fromCharCode(107);
    this.LIST = String.fromCharCode(108);
    this.SMALL_TUPLE = String.fromCharCode(104);
    this.LARGE_TUPLE = String.fromCharCode(105);
    this.NIL = String.fromCharCode(106);
    this.ZERO = String.fromCharCode(0);
};

function EiAtom(Obj) {
    this.type = "Atom";
    this.value = Obj;
    this.toString = function () {
	return Obj;
    };
};

function EiBinary(Obj) {
    this.type = "Binary";
    this.value = Obj;
    this.toString = function () {
	return "<<\"" + Obj + "\">>";
    };
};

function EiTuple(Arr) {
    var i;
    this.type   = "Tuple";
    this.length = Arr.length;
    this.value  = Arr;
    this.toString = function () {
	var i, s = "";
	for (i = 0; i < this.value.length; i++) {
	    if (s !== "") {
		s += ", ";
	    }
	    s += this.value[i].toString();
	}
	return "{" + s + "}";
    };
};

// - INTERFACE -

EiClass.prototype.isAtom = function (Obj) {
    return (Obj.type === "Atom");
};

EiClass.prototype.eqAtom = function (Obj, name) {
    return (Obj.type === "Atom") && (Obj.value === name);
};

EiClass.prototype.isArray = function(Obj) {
    return (typeof(Obj) == "object") &&
	(Obj.constructor.toString().indexOf("Array") !== -1);
};

EiClass.prototype.isTuple = function(Obj) {
    return (typeof(Obj) == "object") &&	(Obj.type == "Tuple");
};

EiClass.prototype.isTupleSize = function(Obj,n) {
    return (typeof(Obj) == "object") &&	(Obj.type == "Tuple") &&
	(Obj.length == n);
};

EiClass.prototype.byte_size = function (Obj) {
    return this.byte_size_inner(Obj);
};

EiClass.prototype.encode = function (Obj) {
    var data, sz;
    data = this.MAGIC + this.encode_inner(Obj);
    sz = 1+this.byte_size(Obj);
    if (data.length != sz) {
	console.debug("encode size is different from byte size!");
	console.debug("data.length = " + data.length);
	console.debug("obj.byte_size = " + sz);
	console.debug("obj = " + Ei.pp(Obj));
    }
    return data;
};

EiClass.prototype.decode = function (S) {
    if (S[0] !== this.MAGIC) {
	throw ("badmagic");
    }
    var Obj = this.decode_inner(S.substring(1));
    if (Obj.rest !== "") {
	throw ("badarg");
    }
    return Obj.term;
};

EiClass.prototype.atom = function (Obj) {
    return new EiAtom(Obj);
};

EiClass.prototype.binary = function (Obj) {
    return new EiBinary(Obj);
};

EiClass.prototype.tuple = function () {
    return new EiTuple([].splice.call(arguments,0));
};

// - EXTERNAL SIZE Calculation
// Preparation for Typed Array usage!
//
EiClass.prototype.byte_size_inner = function (Obj) {
    var func = 'byte_size_' + typeof(Obj);
    return this[func](Obj);
}

EiClass.prototype.byte_size_string = function (Obj) {
    return 1+2+Obj.length;
}

EiClass.prototype.byte_size_boolean = function (Obj) {
    if (Obj) return 1+1+4; // small_atom(true)
    else return 1+1+5; // small_atom(false)
}

EiClass.prototype.byte_size_number = function (Obj) {
    if (Obj % 1 === 0)  // hack to chek for integer
	return this.byte_size_integer(Obj);
    else 
	return this.byte_size_float(Obj);
}

EiClass.prototype.byte_size_integer = function (Obj) {
    if ((Obj >= 0) && (Obj < 256))
	return 1+1;  // small_integer (uint8)
    else if ((Obj >= -134217728) && (Obj <= 134217727)) 
	return 1+4;  // integer (int32)
    return this.byte_size_bignum(Obj);
}

EiClass.prototype.byte_size_bignum = function (Obj) {
    return 1+1+1+4;  // bigint will never (not until js support 64 bit)
}

EiClass.prototype.byte_size_float = function (Obj) {
    return 1+8;  // new_float ext
}

EiClass.prototype.byte_size_object = function (Obj) {
    if (Obj.type === "Atom")   return this.byte_size_atom(Obj);
    if (Obj.type === "Binary") return this.byte_size_binary(Obj);
    if (Obj.type === "Tuple")  return this.byte_size_tuple(Obj);
    if (Obj.constructor.toString().indexOf("Array") !== -1)
	return this.byte_size_array(Obj);
    return this.byte_size_associative_array(Obj);
}

EiClass.prototype.byte_size_atom = function (Obj) {
//    if (Obj.value.length < 256)
//	return 1+1+Obj.value.length;
//    else
	return 1+2+Obj.value.length;
}

EiClass.prototype.byte_size_binary = function (Obj) {
    return 1+4+Obj.value.length;
}

EiClass.prototype.byte_size_tuple = function (Obj) {
    var sum = 1;  // tag
    var i;
    if (Obj.length < 256)
	sum += 1;
    else 
	sum += 4;
    for (i = 0; i < Obj.length; i++)
	sum += this.byte_size_inner(Obj.value[i]);
    return sum;
}

EiClass.prototype.byte_size_array = function (Obj) {
    var sum = 1+4;  // tag+length-bytes
    var i;
    for (i = 0; i < Obj.length; i++)
	sum += this.byte_size_inner(Obj[i]);
    sum +=1; // nil byte
    return sum; 
}

EiClass.prototype.byte_size_associative_array = function (Obj) {
    var sum = 1+4;  // list+length-bytes
    var key;
    for (key in Obj) {
	if (Obj.hasOwnProperty(key)) {
	    var klen = key.length;
	    if (klen < 256) 
		sum += 1+1+klen;
	    else
		sum += 1+2+klen;
	    sum += this.byte_size_inner(Obj[key]);
	    sum += 1+1;  // small_tuple (2)
	}
    }
    sum += 1;  // nil byte
    return sum;
}

// - ENCODING -

EiClass.prototype.encode_inner = function (Obj) {
    var func = 'encode_' + typeof(Obj);
    // console.debug("inner_func = " + func);
    return this[func](Obj);
};

EiClass.prototype.encode_string = function (Obj) {
    return this.STRING + this.uint16_to_bytes(Obj.length) + Obj;
};

EiClass.prototype.encode_boolean = function (Obj) {
    if (Obj) {
	return this.encode_inner(this.atom("true"));
    }
    else {
	return this.encode_inner(this.atom("false"));
    }
};

EiClass.prototype.encode_number = function (Obj) {
    var s, isInteger = (Obj % 1 === 0);

    // Handle floats...
    if (!isInteger) {
	return this.encode_float(Obj);
    }
    
    // Small int...
    if (isInteger && (Obj >= 0) && (Obj < 256)) {
	return this.SMALL_INTEGER + this.uint8_to_bytes(Obj);
    }

    // 4 byte int... uint32! will take care of the encoding
    if (isInteger && (Obj >= -134217728) && (Obj <= 134217727)) {
	return this.INTEGER + this.uint32_to_bytes(Obj);
    }

    // Bignum...
    s = this.bignum_to_bytes(Obj);
    if (s.length < 256) {
	return this.SMALL_BIG + this.uint8_to_bytes(s.length - 1) + s;
    } else {
	return this.LARGE_BIG + this.uint32_to_bytes(s.length - 1) + s;
    }
};

EiClass.prototype.encode_float = function (Obj) {
    var buffer = new Uint8Array(8);
    var dv = new DataView(buffer);
    dv.setFloat64(0, Obj, false);  // store float as big endian 64 
    return this.NEW_FLOAT +
	String.fromCharCode(buffer[0]) +
	String.fromCharCode(buffer[1]) +
	String.fromCharCode(buffer[2]) +
	String.fromCharCode(buffer[3]) +
	String.fromCharCode(buffer[4]) +
	String.fromCharCode(buffer[5]) +
	String.fromCharCode(buffer[6]) +
	String.fromCharCode(buffer[7]);
};

EiClass.prototype.encode_object = function (Obj) {
    // Check if it's an atom, binary, or tuple...
    if (Obj.type === "Atom") {
	return this.encode_atom(Obj);
    }
    if (Obj.type === "Binary") {
	return this.encode_binary(Obj);
    }
    if (Obj.type === "Tuple") {
	return this.encode_tuple(Obj);
    }

    // Check if it's an array...
    if (Obj.constructor.toString().indexOf("Array") !== -1) {
	return this.encode_array(Obj);
    }

    // Treat the object as an associative array...
    return this.encode_associative_array(Obj);
};

EiClass.prototype.encode_atom = function (Obj) {
    var L = Obj.value.length;
//    if (L < 256)
//	return this.SMALL_ATOM + this.uint8_to_bytes(L) + Obj.value;
//    else
	return this.ATOM + this.uint16_to_bytes(L) + Obj.value;
};

EiClass.prototype.encode_binary = function (Obj) {
    return this.BINARY + this.uint32_to_bytes(Obj.value.length) + Obj.value;
};

EiClass.prototype.encode_tuple = function (Obj) {
    var i, s = "";
    var L = Obj.length;
    if (L < 256) {
	s += this.SMALL_TUPLE + this.uint8_to_bytes(L);
    } else {
	s += this.LARGE_TUPLE + this.uint32_to_bytes(L);
    }
    for (i = 0; i < Obj.length; i++) {
	s += this.encode_inner(Obj.value[i]);
    }
    return s;
};

EiClass.prototype.encode_array = function (Obj) {
    var i, s = this.LIST + this.uint32_to_bytes(Obj.length);
    for (i = 0; i < Obj.length; i++) {
	s += this.encode_inner(Obj[i]);
    }
    s += this.NIL;
    return s;
};

EiClass.prototype.encode_associative_array = function (Obj) {
    var key, Arr = [];
    for (key in Obj) {
	if (Obj.hasOwnProperty(key)) {
	    Arr.push(this.tuple(this.atom(key), Obj[key]));
	}
    }
    return this.encode_array(Arr);
};

// - DECODING -

EiClass.prototype.decode_inner = function (S) {
    var Type = S[0];
    S = S.substring(1);
    // console.debug("decode_inner: "+Type);
    switch (Type) {
    case this.SMALL_ATOM:
	return this.decode_small_atom(S);
    case this.ATOM:
	return this.decode_atom(S);
    case this.BINARY:
	return this.decode_binary(S);
    case this.SMALL_INTEGER:
	return this.decode_small_integer(S);
    case this.INTEGER:
	return this.decode_integer(S);
    case this.SMALL_BIG:
	return this.decode_small_big(S);
    case this.LARGE_BIG:
	return this.decode_large_big(S);
    case this.FLOAT:
	return this.decode_float(S);
    case this.NEW_FLOAT:
	return this.decode_new_float(S);
    case this.STRING:
	return this.decode_string(S);
    case this.LIST:
	return this.decode_list(S);
    case this.SMALL_TUPLE:
	return this.decode_small_tuple(S);
    case this.LARGE_TUPLE:
	return this.decode_large_tuple(S);
    case this.NIL:
	return this.decode_nil(S);
    default:
	throw ("Unexpected BERT type: " + Type);
    }
};

EiClass.prototype.read_uint8 = function (S) {
    return S.charCodeAt(0);
};

EiClass.prototype.read_uint16 = function (S) {
    var X0 = S.charCodeAt(0);
    var X1 = S.charCodeAt(1);
    return (X0<<8)+X1;
};

EiClass.prototype.read_uint32 = function (S) {
    var X0 = S.charCodeAt(0);
    var X1 = S.charCodeAt(1);
    var X2 = S.charCodeAt(2);
    var X3 = S.charCodeAt(3);
    return (X0<<24)+(X1<<16)+(X2<<8)+X3;
};

EiClass.prototype.read_int32 = function (S) {
    var X = this.read_uint32(S);
    if (X >= 0x80000000)
	X = -(~X+1);
    return X;
};

EiClass.prototype.decode_atom = function (S) {
    var Size, Value;
    Size = this.read_uint16(S);
    S = S.substring(2);
    Value = S.substring(0, Size);
    if (Value === "true") {
	Value = true;
    }
    else if (Value === "false") {
	Value = false;
    }
    return { term: this.atom(Value), rest:  S.substring(Size) };
};

EiClass.prototype.decode_small_atom = function (S) {
    var Size, Value;
    Size = this.decode_uint8(S);
    S = S.substring(1);
    Value = S.substring(0, Size);
    if (Value === "true") {
	Value = true;
    }
    else if (Value === "false") {
	Value = false;
    }
    return { term: this.atom(Value), rest:  S.substring(Size) };
};

EiClass.prototype.decode_binary = function (S) {
    var Size = this.read_uint32(S);
    S = S.substring(4);
    return { term: this.binary(S.substring(0, Size)), 
	     rest: S.substring(Size) };
};

EiClass.prototype.decode_small_integer = function (S) {
    var Value = this.read_uint8(S);
    S = S.substring(1);
    return { term: Value, rest: S };
};

EiClass.prototype.decode_integer = function (S) {
    var Value = this.read_int32(S);
    S = S.substring(4);
    return { term: Value, rest: S };
};

EiClass.prototype.decode_small_big = function (S) {
    var Size, Value;
    Size = this.read_uint8(S);
    S = S.substring(1);
    Value = this.bytes_to_bignum(S, Size);
    return { value : Value, rest: S.substring(Size + 1) };
};

EiClass.prototype.decode_large_big = function (S) {
    var Size, Value;
    Size = this.read_uint32(S);
    S = S.substring(4);
    Value = this.bytes_to_bignum(S, Size);
    return { value : Value, rest: S.substring(Size + 1) };
};

EiClass.prototype.decode_float = function (S) {
    var Size = 31;
    return { term: parseFloat(S.substring(0, Size)), 
	     rest: S.substring(Size) };
};

EiClass.prototype.decode_new_float = function (S) {
    var buffer = new Uint8Array(8);
    var dv;
    for (var i = 0; i < 8; i++) buffer[i] = S[i];
    dv = new DataView(buffer);
    return { term: dv.getFloat64(0, false),
	     rest: S.substring(8) };
};

EiClass.prototype.decode_string = function (S) {
    var Size = this.read_uint16(S);
    S = S.substring(2);
    return { term: S.substring(0, Size), rest:  S.substring(Size) };
};

//
// Special hack for argument lists  ['array',64,65,66,67]
// to separate this in the erlang term_to_binary from a string!
// also make sure [array|T] is encoded as [array,array|T]
//
EiClass.prototype.decode_list = function (S) {
    var Size, i, El, LastChar, Arr = [];
    Size = this.read_uint32(S);
    S = S.substring(4);
    for (i = 0; i < Size; i++) {
	El = this.decode_inner(S);
	if (!((i===0) && this.eqAtom(El.term, "array")))
	    Arr.push(El.term);
	S = El.rest;
    }
    LastChar = S[0];
    if (LastChar !== this.NIL) {
	throw ("List does not end with NIL!");
    }
    S = S.substring(1);
    return { term: Arr, rest: S };
};

EiClass.prototype.decode_small_tuple = function (S) {
    var Size, i, El, Arr = [];
    Size = this.read_uint8(S);
    S = S.substring(1);
    for (i = 0; i < Size; i++) {
	El = this.decode_inner(S);
	Arr.push(El.term);
	S = El.rest;
    }
    return { term: new EiTuple(Arr), rest: S };
};

EiClass.prototype.decode_large_tuple = function (S) {
    var Size, i, El, Arr = [];
    Size = this.read_uint32(S);
    S = S.substring(4);
    for (i = 0; i < Size; i++) {
	El = this.decode_inner(S);
	Arr.push(El.term);
	S = El.rest;
    }
    return { term: new EiTuple(Arr), rest: S };
};

EiClass.prototype.decode_nil = function (S) {
	return { term: [], rest: S };
};

// Fixme use Uint8Array!!

EiClass.prototype.uint8_to_bytes = function (X) {
    return String.fromCharCode(X & 0xff);
}

EiClass.prototype.uint16_to_bytes = function (X) {
    return String.fromCharCode((X >> 8) & 0xff) +
	String.fromCharCode(X & 0xff);
}

EiClass.prototype.uint32_to_bytes = function (X) {
    return String.fromCharCode((X >> 24) & 0xff) +
	String.fromCharCode((X >> 16) & 0xff) +
	String.fromCharCode((X >> 8) & 0xff) +
	String.fromCharCode(X & 0xff);
}

// - UTILITY FUNCTIONS -

// Encode an integer into an Erlang bignum,
// which is a byte of 1 or 0 representing
// whether the number is negative or positive,
// followed by little-endian bytes.
EiClass.prototype.bignum_to_bytes = function (Int) {
    var isNegative, Rem, s = "";
    isNegative = Int < 0;
    if (isNegative) {
	Int *= -1;
	s += String.fromCharCode(1);
    } else {
	s += String.fromCharCode(0);
    }
    
    while (Int !== 0) {
	Rem = Int % 256;
	s += String.fromCharCode(Rem);
	Int = Math.floor(Int / 256);
    }
    return s;
};

// Encode a list of bytes into an Erlang bignum.
EiClass.prototype.bytes_to_bignum = function (S, Count) {
    var isNegative, i, n, Num = 0;
    isNegative = (S.charCodeAt(0) === 1);
    S = S.substring(1);
    for (i = Count - 1; i >= 0; i--) {
	n = S.charCodeAt(i);
	if (Num === 0) {
	    Num = n;
	}
	else {
	    Num = Num * 256 + n;
	}
    }
    if (isNegative) {
	return Num * -1;
    }
    return Num;
};

// Convert an array of bytes into a string.
EiClass.prototype.bytes_to_string = function (Arr) {
    var i, s = "";
    for (i = 0; i < Arr.length; i++) {
	s += String.fromCharCode(Arr[i]);
    }
    return s;
};

// Pretty Print a byte-string in Erlang binary form.
EiClass.prototype.pp_bytes = function (Bin) {
    var i, s = "";
    for (i = 0; i < Bin.length; i++) {
	if (s !== "") {
	    s += ",";
	}
	s += "" + Bin.charCodeAt(i);
    }
    return "<<" + s + ">>";
};

// Pretty Print a JS object in Erlang term form.
EiClass.prototype.pp = function (Obj) {
    return Obj.toString();
};

var Ei = new EiClass();

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

WseClass.prototype.encode = function(Obj) {
    return Base64.encode(Ei.encode(Obj));
//      return Ei.encode(Obj);
};

WseClass.prototype.decode = function(Data) {
   return Ei.decode(Base64.decode(Data));
//    return Ei.decode(Data);
};

WseClass.prototype.open = function(url) {
    var wse = this;  // save WebSocket closure

    if ("WebSocket" in window) {
	this.state = "connecting";
	this.ws = new WebSocket(url);

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
