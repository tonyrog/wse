//---- BEGIN COPYRIGHT -------------------------------------------------------
//
// Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
//
// This software is licensed as described in the file COPYRIGHT, which
// you should have received as part of this distribution. The terms
// are also available at http://www.rogvall.se/docs/copyright.txt.
//
// You may opt to use, copy, modify, merge, publish, distribute and/or sell
// copies of the Software, and permit persons to whom the Software is
// furnished to do so, under the terms of the COPYRIGHT file.
//
// This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
// KIND, either express or implied.
//
//---- END COPYRIGHT ---------------------------------------------------------
//
// Erlang term encode/decode using TypeArray interface
//

function EiClass() {
    this.MAGIC = 131;
    this.SMALL_ATOM = 115;
    this.ATOM = 100;
    this.BINARY = 109;
    this.SMALL_INTEGER = 97;
    this.INTEGER = 98;
    this.SMALL_BIG = 110;
    this.LARGE_BIG = 111;
    this.FLOAT = 99;
    this.NEW_FLOAT = 70;
    this.REFERENCE = 101;
    this.NEW_REFERENCE = 114;
    this.NEWER_REFERENCE = 90;
    this.STRING = 107;
    this.LIST = 108;
    this.SMALL_TUPLE = 104;
    this.LARGE_TUPLE = 105;
    this.NIL = 106;
    this.MAP = 116;
    this.ATOM_UTF8 = 118;        
    this.SMALL_ATOM_UTF8 = 119;    

    // Encoding options
    this.encode_assoc_map = false;      // use map when available
    this.encode_utf8_atom = true;       // encode atoms as utf8
    this.encode_unicode_strings = true; // encode string as 16-bit code points
    this.encode_utf8_strings = false;   // encode string as utf8 bytes

    this.decode_string_utf8 = true;     // decode list as unicode string
    this.decode_binary_utf8 = true;     // decode binary as utf8 string
    this.decode_binary_latin1 = true;   // decode binary as latin1 string
    this.decode_list_string = true;  // try decode list as string
    this.decode_list_utf8 = true;    // try decode list as utf8 string
};

//
// [] can be encoded as:
//   <<MAGIC, NIL>>
//   <<MAGIC, STRING, 0:16>>
//   <<MAGIC, LIST, 0:32, NIL>>
// 
function EiAtom(Obj) {
    this.type = "Atom";
    this.value = Obj;
    this.toString = function () {
	return Obj;
    };
};

// Obj is typed array or string
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

function EiReference(Sys,Refs,Creation) {
    var i;
    this.type   = "Reference";
    this.length = Refs.length;
    this.value  = [Sys,Refs,Creation];
    this.toString = function () {
	var i, s = "", refs=this.value[1];
	for (i = 0; i < refs.length; i++) {
	    if (s !== "") {
		s += ".";
	    }
	    s += refs[i].toString();
	}
	return "#Ref<" + s + ">";
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

EiClass.prototype.isReference = function(Obj) {
    return (typeof(Obj) == "object") &&	(Obj.type == "Reference");
};

EiClass.prototype.isTupleSize = function(Obj,n) {
    return (typeof(Obj) == "object") &&	(Obj.type == "Tuple") &&
	(Obj.length == n);
};

EiClass.prototype.isBinary = function (Obj) {
    return (Obj.type === "Binary");
};

EiClass.prototype.binary_size = function (obj) {
    if (obj.type === "Binary") return obj.value.byteLength;
    return 0;
};

EiClass.prototype.is_latin1_string = function (str) {
    var len = str.length;
    var i = 0;
    while(i < len) {
	var x = str.charCodeAt(i);
	if (x >= 256) return false;
	i++;
    }
    return true;
}

// calculate number of utf8 bytes needed to encode a string
EiClass.prototype.string_utf8_length = function (str) {
    var len = str.length;
    var i = len - 1;
    while(i >= 0) {
	var x = str.charCodeAt(i);
	if ((x > 0x7f) && (x <= 0x7ff))
	    len++;
	else if ((x > 0x7ff) && (x <= 0xffff))
	    len += 2;
	if ((x >= 0xdc00) && (x <= 0xdfff)) i--; //trail surrogate
	i--;
    }
    return len;
}

// write string (16-bit) as utf8 bytes
EiClass.prototype.write_utf8_bytes = function (str,dv,pos,count) {
    var i = 0;
    while(i < count) {
	var x = str.charCodeAt(i);
	if (x < 256)
	    dv.setUint8(pos++, x);
	else if (x < 2048) {
	    dv.setUInt8(pos++, 0xf0 | (x >> 6));
	    dv.setUInt8(pos++, 0x80 | (x & 0x3f));
	}
	else {
	    x &= 0xFFFF; // truncate to 16 bits
	    dv.setUInt8(pos++, 0xe0 | (x >> 12));
	    dv.setUInt8(pos++, 0x80 | ((x >> 6) & 0x3f));
	    dv.setUInt8(pos++, 0x80 | (x & 0x3f));
	}
	i++;
    }
    return pos;
};

// write string (16-bit) as list of utf8 bytes
EiClass.prototype.write_utf8_list = function (str,dv,pos,count) {
    var i = 0;
    while (i < count) {
	x = Obj.charCodeAt(i);
	if (x <= 255) {
	    dv.setUint8(pos++, this.SMALL_INTEGER);
	    dv.setUint8(pos++, x);
	}
	else if (x < 2048) {
	    dv.setUint8(pos++, this.SMALL_INTEGER);
	    dv.setUInt8(pos++, 0xf0 | (x >> 6));
	    dv.setUint8(pos++, this.SMALL_INTEGER);
	    dv.setUInt8(pos++, 0x80 | (x & 0x3f));
	}
	else {
	    x &= 0xFFFF; // truncate to 16 bits
	    dv.setUint8(pos++, this.SMALL_INTEGER);
	    dv.setUInt8(pos++, 0xe0 | (x >> 12));
	    dv.setUint8(pos++, this.SMALL_INTEGER);
	    dv.setUInt8(pos++, 0x80 | ((x >> 6) & 0x3f));
	    dv.setUint8(pos++, this.SMALL_INTEGER);		    
	    dv.setUInt8(pos++, 0x80 | (x & 0x3f));
	}
	i++;
    }
    return pos;
}

EiClass.prototype.write_latin1_bytes = function (str,dv,pos,count) {
    var i = 0;
    while(i < count) {
	var x = str.charCodeAt(i);
	dv.setUint8(pos++, x & 0xff);
	i++;
    }
    return pos;
};

EiClass.prototype.write_list_bytes = function (str,dv,pos,count) {
    var i = 0;
    while(i < count) {
	var x = str.charCodeAt(i);
	dv.setUint8(pos++, this.SMALL_INTEGER);
	dv.setUint8(pos++, x & 0xff);
	i++;
    }
    return pos;
};

EiClass.prototype.write_list_integers = function (str,dv,pos,count) {
    var i = 0;
    while(i < count) {
	var x = str.charCodeAt(i);
	dv.setUint8(pos++, this.INTEGER);
	dv.setInt32(pos+1, x);
	pos += 4;
	i++;
    }
    return pos;
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

EiClass.prototype.reference = function (Sys,Refs,Creation) {
    return new EiReference(Sys,Refs,Creation);
};

// byte_size_xxx
//   calculate the size need to encode the Term 
//   (use byte_size_term to avoid extra magic byte)
//
EiClass.prototype.byte_size = function (Obj) {
    return 1+this.byte_size_term(Obj);
};

EiClass.prototype.byte_size_term = function (Obj) {
    var len, Size;
    switch(typeof(Obj)) {
    case 'string':
	if (this.encode_unicode_strings) {
	    len = Obj.length;
	    if (this.is_latin1_string(Obj)) {
		if (len < 65535)
		    Size = 2+len; // STRING encoded
		else 
		    Size = 4+2*len+1; // LIST encoded (SMALL_INTEGER)
	    }
	    else
		Size = 4+5*len+1; // LIST encoded (INTEGER)
	}
	else if (this.encode_utf8_strings) {
	    len = this.string_utf8_length(Obj); // utf8 length
	    if (len <= 65535) 
		Size = 2+len;      // STRING encoded (SMALL_INTEGER)
	    else
		Size = 4+2*len+1;  // LIST encoded (SMALL_INTEGER)
	}
	else { // encode as (truncated) latin1
	    len = Obj.length;
	    if (len < 65535)
		Size = 2+len; // STRING encoded
	    else
		Size = 4+2*len+1; // LIST encoded (SMALL_INTEGER)
	}
	break;
    case 'boolean':
	if (Obj)
	    Size = 1+4; // "true"
	else
	    Size = 1+5; // "false
	break;
    case 'number':
	if (Obj % 1 === 0) { // then integer
	    if ((Obj >= 0) && (Obj < 256))
		Size = 1;  // small_integer (uint8)
	    else if ((Obj >= -134217728) && (Obj <= 134217727))
		Size = 4;  // integer (int32)
	    else // bignum (32 bit only, can not really be used here
		Size = 1+1+4; // size,sign,byte*4
	}
	else {
	    Size = 8;  // NEW_FLOAT
	}
	break;
    case 'object':
	switch (Obj.type) {
	case 'Atom':
	    Size = this.atom_size(Obj.value); // FIXME utf8_size
	    break;
	case 'Binary':
	    Size = 4 + Obj.value.byteLength;
	    break;
	case 'Tuple':
	    Size = this.byte_size_tuple(Obj);
	    break;
	case 'Reference':
	    Size = this.byte_size_reference(Obj);
	    break;
	default:
	    if (Obj.constructor.toString().indexOf("Array") !== -1)
		Size = this.byte_size_array(Obj);
	    else
		Size = this.byte_size_associative_array(Obj);
	    break;
	}
	break;
    default: 
	throw ("bad object: " + Obj);
    }
    return 1+Size;
}


EiClass.prototype.atom_size = function (name) {
    var len;
    if (this.encode_utf8_atom)
	len = this.string_utf8_length(name);
    else
	len = name.length;
    if (len < 256)
	return len+1;
    else
	return len+2;
}

EiClass.prototype.byte_size_reference = function (Obj) {
    var Sys  = Obj.value[0];
    var Refs = Obj.value[1];
    var Creation = Obj.value[2];
    var SysSize = 1 + this.atom_size(Sys);
    
    if (Refs.length > 1) {
	if (Creation > 3)
	    Size = 2 + SysSize + Refs.length*4 + 4;
	else
	    Size = 2 + SysSize + Refs.length*4 + 1;
    }
    else
	SysSize + 4 + 1;
    return Size;
}

EiClass.prototype.byte_size_tuple = function (Obj) {
    var i;
    var Size = (Obj.length < 256) ? 1 : 4;
    for (i = 0; i < Obj.length; i++)
	Size += this.byte_size_term(Obj.value[i]);
    return Size;
}

EiClass.prototype.byte_size_array = function (Obj) {
    var len = Obj.length;
    if (len == 0)
	return 1; // nil only
    else {
	var i;
	var Size = 4;  // length-bytes
	for (i = 0; i < len; i++)
	    Size += this.byte_size_term(Obj[i]);
	Size +=1; // nil byte
	return Size;
    }
}

EiClass.prototype.byte_size_associative_array = function (Obj) {
    var key, Size = 0, N = 0;

    for (key in Obj) {
	if (Obj.hasOwnProperty(key)) {
	    var len = key.length;
	    klen = (len < 256) ? 1+len : 2+len;
	    Size += 1+klen;  // (small)atom
	    Size += this.byte_size_term(Obj[key]);
	    N++;
	}
    }
    if (N == 0) {
	if (!this.encode_assoc_map)
	    Size = 4;   // map size
	else
	    Size = 0    // nil
    }
    else {
	if (!this.encode_assoc_map)
	    Size += N*2;  // add N small tuples
	Size += 4;  // list or map size
    }
    return Size;
}

// encode_xxx
//  encode the term into Array Buffer

EiClass.prototype.encode = function (Obj) {
    var sz = this.byte_size(Obj);
    var ab = new ArrayBuffer(sz);
    var dv = new DataView(ab);
    dv.setUint8(0, this.MAGIC);
    this.encode_term(Obj, dv, 1);
    return ab;
};

EiClass.prototype.encode_term = function (Obj,dv,pos) {
    var len;
    switch(typeof(Obj)) {
    case 'string':
	if (this.encode_unicode_strings) {
	    len = Obj.length;
	    if (this.is_latin1_string(Obj)) {
		if (len < 65535) {
		    dv.setUint8(pos++, this.STRING);
		    dv.setUint16(pos, len);
		    pos += 2;
		    pos = this.write_latin1_bytes(Obj,dv,pos,len);
		}
		else {
		    dv.setUint8(pos++, this.LIST);
		    dv.setUint32(pos, len, false);
		    pos += 4;
		    pos = this.write_list_bytes(Obj,dv,pos,len);
		    dv.setUint8(pos++, this.NIL);
		}
	    }
	    else {
		dv.setUint8(pos++, this.LIST);
		dv.setUint32(pos, len, false);
		pos += 4;
		pos = this.write_list_integers(Obj,dv,pos,len);
		dv.setUint8(pos++, this.NIL);		
	    }
	}
	else if (this.encode_utf8_strings) {
	    len = this.string_utf8_length(Obj);
	    if (len <= 65535) {
		dv.setUint8(pos++, this.STRING);
		dv.setUint16(pos, len);
		pos += 2;
		pos = this.write_utf8_bytes(Obj,dv,pos,Obj.length);
	    }
	    else {
		var i, l;
		dv.setUint8(pos++, this.LIST);
		dv.setUint32(pos, len);
		pos += 4;
		pos = this.write_utf8_list(Obj,dv,pos,Obj.length);
		dv.setUint8(pos++, this.NIL);
	    }
	}
	else {
	    len = Obj.length;
	    if (len < 65535) {
		dv.setUint8(pos++, this.STRING);
		dv.setUint16(pos, len);
		pos += 2;
		pos = this.write_latin1_bytes(Obj,dv,pos,len);
	    }
	    else {
		dv.setUint8(pos++, this.LIST);
		dv.setUint32(pos, len, false);
		pos += 4;
		pos = this.write_list_bytes(Obj,dv,pos,len);
	    }
	}
	break;

    case 'boolean':
	if (Obj) {
	    if (this.encode_utf8_atom)
		dv.setUint8(pos, this.SMALL_ATOM_UTF8);
	    else
		dv.setUint8(pos, this.SMALL_ATOM);
	    dv.setUint8(pos+1, 4);
	    dv.setUint8(pos+2, 't'.charCodeAt(0));
	    dv.setUint8(pos+3, 'r'.charCodeAt(0));
	    dv.setUint8(pos+4, 'u'.charCodeAt(0));
	    dv.setUint8(pos+5, 'e'.charCodeAt(0));
	    pos += 6;
	}
	else {
	    if (this.encode_utf8_atom)	    
		dv.setUint8(pos, this.SMALL_ATOM_UTF8);
	    else
		dv.setUint8(pos, this.SMALL_ATOM);
	    dv.setUint8(pos+1, 5);
	    dv.setUint8(pos+2, 'f'.charCodeAt(0));
	    dv.setUint8(pos+3, 'a'.charCodeAt(0));
	    dv.setUint8(pos+4, 'l'.charCodeAt(0));
	    dv.setUint8(pos+5, 's'.charCodeAt(0));
	    dv.setUint8(pos+6, 'e'.charCodeAt(0));
	    pos += 7;
	}
	break;

    case 'number':
	if ((Obj % 1 === 0)) {  // integer
	    if ((Obj >= 0) && (Obj < 256)) {
		dv.setUint8(pos, this.SMALL_INTEGER);
		dv.setUint8(pos+1, Obj);
		pos += 2;
	    }
	    else if ((Obj >= -134217728) && (Obj <= 134217727)) {
		dv.setUint8(pos, this.INTEGER);
		dv.setInt32(pos+1, Obj);
		pos += 5;
	    }
	    else if (Obj < 0) {
		dv.setUint8(pos, this.SMALL_BIG);
		dv.setUint8(pos+1, 4);
		dv.setUint8(pos+2, 0);  // negative
		// little endian encoded digits!
		dv.setUint32(pos+3, -Obj, true);  
		pos += 7;
	    }
	    else {
		dv.setUint8(pos, this.SMALL_BIG);
		dv.setUint8(pos+1, 4);
		dv.setUint8(pos+2, 1);
		// little endian encoded digits!
		dv.setUint32(pos+3, Obj, true);
		pos += 7;
	    }
	}
	else {
	    dv.setUint8(pos, this.NEW_FLOAT);
	    dv.setFloat64(pos+1, Obj, false);  // store float as big endian 64
	    pos += 9;
	}
	break;

    case 'object':
	switch(Obj.type) {
	case 'Atom':
	    pos = this.encode_atom_string(Obj.value,dv,pos);
	    break;
	case 'Binary': { // Obj.value MUST be an Uint8Array!
	    var i, len = Obj.value.byteLength;
	    dv.setUint8(pos, this.BINARY);
	    dv.setUint32(pos+1, len, false);
	    pos += 5;
	    for (i = 0; i < len; i++, pos++)
		dv.setUint8(pos, Obj.value[i]);
	    break;
	}
	case 'Reference': {
	    var Sys  = Obj.value[0];
	    var Refs = Obj.value[1];
	    var Creation = Obj.value[2];
	    var i;
	    
	    if (Refs.length > 1) {
		if (Creation > 3) {
		    dv.setUint8(pos, this.NEWER_REFERENCE);
		    pos += 1;
		    dv.setUint16(pos, Refs.length);
		    pos += 2;
		    pos = this.encode_atom_string(Sys,dv,pos);
		    for (i=0; i < Refs.length; i++) {
			dv.setUint32(pos, Refs[i]);
			pos += 4;
		    }
		    dv.setUint32(pos, Creation);
		    pos += 4;
		}
		else {
		    dv.setUint8(pos, this.NEW_REFERENCE);
		    pos += 1;
		    dv.setUint16(pos, Refs.length);
		    pos += 2;
		    pos = this.encode_atom_string(Sys,dv,pos);
		    for (i=0; i < Refs.length; i++) {
			dv.setUint32(pos, Refs[i]);
			pos += 4;
		    }
		    dv.setUint8(pos, Creation);
		    pos += 1;
		}
	    }
	    else {
		dv.setUint8(pos, this.REFERENCE);
		pos += 1;
		pos = this.encode_atom_string(Sys,dv,pos);
		dv.setUint32(pos, Refs[0]);
		pos += 32;
		dv.setUint8(pos, Creation);
		pos += 1;
	    }
	    break;
	}
	case 'Tuple':
	    pos = this.encode_tuple(Obj,dv,pos);
	    break;
	default:
	    if (Obj.constructor.toString().indexOf("Array") !== -1)
		pos = this.encode_array(Obj,dv,pos);
	    else
		pos = this.encode_associative_array(Obj,dv,pos);
	}
	break;

    default: 
	throw ("bad object: " + Obj);
    }
    return pos;
};

EiClass.prototype.encode_atom_string = function (name,dv,pos) {
    var len;

    if (this.encode_utf8_atom) {
	len = this.string_utf8_length(name);
	if (len < 256) {
	    dv.setUint8(pos++, this.SMALL_ATOM_UTF8);
	    dv.setUint8(pos++, len);
	}
	else if (len < 65536) {
	    dv.setUint8(pos++, this.ATOM_UTF8);
	    dv.setUint16(pos, len, false);
	    pos += 2;
	}	
	else
	    throw("bad atom: too big");    
	pos = this.write_utf8_bytes(name,dv,pos,name.length);
    }
    else {  // use latin1 encoding 
	len = name.length;
	if (len < 256) {
	    dv.setUint8(pos++, this.SMALL_ATOM);
	    dv.setUint8(pos++, len);
	}
	else if (len < 65536) {
	    dv.setUint8(pos++, this.ATOM);
	    dv.setUint16(pos, len, false);
	    pos += 2;
	}	
	else
	    throw("bad atom: too big");    
	pos = this.write_latin1_bytes(name,dv,pos,name.length);
    }
    return pos;
}


EiClass.prototype.encode_tuple = function (Obj,dv,pos) {
    var i;
    var N = Obj.length;
    if (N < 256) {
	dv.setUint8(pos, this.SMALL_TUPLE);
	dv.setUint8(pos+1, N);
	pos += 2;
    } else {
	dv.setUint8(pos, this.LARGE_TUPLE);
	dv.setUint32(pos+1, N, false);
	pos += 5;
    }
    for (i = 0; i < N; i++)
	pos = this.encode_term(Obj.value[i],dv,pos);
    return pos;
};

EiClass.prototype.encode_array = function (Obj,dv,pos) {
    var N = Obj.length;
    if (N > 0) {
	dv.setUint8(pos, this.LIST);
	dv.setUint32(pos+1, N, false);
	pos += 5;
	for (var i = 0; i < N; i++)
	    pos = this.encode_term(Obj[i],dv,pos);
    }
    dv.setUint8(pos,this.NIL);
    return pos+1;
};

// count number of keys
EiClass.prototype.assoc_num_keys = function (Obj) {
    if (!Object.keys) {
	var N = 0;
	var key;
	for (key in Obj) {
	    if (Obj.hasOwnProperty(key))
		N++;
	}
	return N;
    }
    return Object.keys(Obj).length;
};


EiClass.prototype.encode_associative_array = function (Obj,dv,pos) {
    var N = this.assoc_num_keys(Obj);
    if (N > 0) {
	var key;
	dv.setUint8(pos, this.encode_assoc_map ? this.MAP : this.LIST);
	dv.setUint32(pos+1,N,false);
	pos += 5
	for (key in Obj) {
	    if (Obj.hasOwnProperty(key)) {
		if (!this.encode_assoc_map) {
		    dv.setUint8(pos++, this.SMALL_TUPLE);
		    dv.setUint8(pos++, 2);
		}
		pos = this.encode_atom_string(key,dv,pos);
		pos = this.encode_term(Obj[key],dv,pos);
	    }
	}
    }
    if (!this.encode_assoc_map)
	dv.setUint8(pos++,this.NIL);
    else if (this.encode_assoc_map && (N == 0)) { // special case for empty map
	dv.setUint8(pos, this.MAP);
	dv.setUint32(pos+1,0,false);
	pos += 5;
    }
    return pos;
};

//
// decode_size_xxx
//
//  Calculate the size of an external encoded term 
//  in the buffer
//

EiClass.prototype.decode_size = function (ab, pos) {
    var dv = new DataView(ab);
    if (dv.getUint8(pos) !== this.MAGIC) {
	throw ("badmagic");
    }
    return 1+this.decode_size_term(dv, pos+1);
};

EiClass.prototype.decode_size_term = function (dv,pos) {
    var Tag = dv.getUint8(pos++);
    var L = 0;

    switch (Tag) {
    case this.NIL: break;
    case this.SMALL_ATOM: L = 1+dv.getUint8(pos); break;
    case this.ATOM:       L = 2+dv.getUint16(pos,false); break;
    case this.SMALL_ATOM_UTF8: L = 1+dv.getUint8(pos); break;
    case this.ATOM_UTF8:       L = 2+dv.getUint16(pos,false); break;
    case this.BINARY:     L = 4+dv.getUint32(pos,false); break;
    case this.SMALL_INTEGER: L = 1; break;
    case this.INTEGER:	L = 4; break;
    case this.SMALL_BIG: L = 1+dv.getUint8(pos); break;
    case this.LARGE_BIG: L = 4+dv.getUint32(pos,false); break;
    case this.FLOAT:	L = 31; break;
    case this.NEW_FLOAT: L = 8; break;
    case this.REFERENCE:
	L = this.decode_size_term(dv,pos)+4+1; break;
    case this.NEW_REFERENCE:
	L = 2+this.decode_size_term(dv,pos+2)+dv.getUint16(pos,false)*4+1;
	break;
    case this.NEWER_REFERENCE:
	L = 2+this.decode_size_term(dv,pos+2)+dv.getUint16(pos,false)*4+4;
	break;
    case this.STRING: L = 2+dv.getUint16(pos,false); break;
    case this.LIST:
	L = this.decode_size_seq(dv,pos+4,dv.getUint32(pos,false)+1,4);
	break;
    case this.SMALL_TUPLE:
	L = this.decode_size_seq(dv,pos+1,dv.getUint8(pos),1); 
	break;
    case this.LARGE_TUPLE: 
	L = this.decode_size_seq(dv,pos+4,dv.getUint32(pos,false),4);
	break;
    case this.MAP:
	L = this.decode_size_seq(dv,pos+4,2*dv.getUint32(pos,false),4);
	break;
    default: throw ("bad tag: " + Tag);
    }
    return L+1;
};

EiClass.prototype.decode_size_seq = function (dv,pos,len,Size) {
    var i;
    for (i = 0; i < len; i++) {
	var k = this.decode_size_term(dv,pos);
	pos += k;
	Size += k;
    }
    return Size;
}

//
//  Decode the Array Buffer
//

EiClass.prototype.decode = function (ab, pos) {
    var dv = new DataView(ab);
    if (dv.getUint8(pos) !== this.MAGIC) {
	throw ("badmagic");
    }
    return this.decode_term(dv, pos+1);
};

EiClass.prototype.decode_term = function (dv,pos) {
    var R,Tag = dv.getUint8(pos++);

    switch (Tag) {
    case this.NIL:
	R = []; break;
    case this.SMALL_ATOM:
	R = this.decode_latin1_atom_bytes(dv,pos+1,dv.getUint8(pos)); break;
    case this.ATOM:
	R = this.decode_latin1_atom_bytes(dv,pos+2,dv.getUint16(pos,false)); break;
    case this.SMALL_ATOM_UTF8:
	R = this.decode_utf8_atom_bytes(dv,pos+1,dv.getUint8(pos)); break;
    case this.ATOM_UTF8:
	R = this.decode_utf8_atom_bytes(dv,pos+2,dv.getUint16(pos,false)); break;
    case this.BINARY:
	R = this.decode_binary(dv,pos); break;
    case this.SMALL_INTEGER:
	R = dv.getUint8(pos); break;
    case this.INTEGER:
	R = dv.getInt32(pos,false); break;
    case this.SMALL_BIG:
	R = this.decode_big_bytes(dv,pos+1,dv.getUint8(pos)); break;
    case this.LARGE_BIG:
	R = this.decode_big_bytes(dv,pos+4,dv.getUint32(pos,false)); break;
    case this.FLOAT:
	R = parseFloat(this.decode_latin1_string(dv,pos,31)); break;
    case this.NEW_FLOAT:
	R = dv.getFloat64(pos, false); break;
    case this.REFERENCE:
	R = this.decode_ref(dv,pos); break;
    case this.NEW_REFERENCE:
	R = this.decode_new_ref(dv,pos); break;
    case this.NEWER_REFERENCE:
	R = this.decode_newer_ref(dv,pos); break;
    case this.STRING:
	R = this.decode_string(dv,pos+2,dv.getUint16(pos,false)); break;
    case this.LIST:
	R = this.decode_list(dv,pos+4,dv.getUint32(pos,false)); break;
    case this.SMALL_TUPLE:
	R = this.decode_tuple(dv,pos+1,dv.getUint8(pos)); break;
    case this.LARGE_TUPLE:
	R = this.decode_tuple(dv,pos+4,dv.getUint32(pos,false)); break;
    case this.MAP:
	R = this.decode_map(dv,pos+4,dv.getUint32(pos,false)); break;
    default:
	throw ("badtag: " + Tag);
    }
    // console.debug("decode_term = " + R);
    return R;
};

EiClass.prototype.decode_utf8_atom_bytes = function (dv,pos,len) {
    var S = this.utf8_to_string(dv,pos,len);
    if (S === "true")
	return true;
    else if (S === "false")
	return false;
    return new this.atom(S);
};


EiClass.prototype.decode_latin1_atom_bytes = function (dv,pos,len) {
    var S = this.latin1_to_string(dv,pos,len);
    if (S === "true")
	return true;
    else if (S === "false")
	return false;
    return new this.atom(S);
};

EiClass.prototype.decode_binary = function (dv,pos) {
    var Size = dv.getUint32(pos, false);
    if (this.decode_binary_utf8) {
	return this.utf8_to_string(dv,pos+4,Size);
    }
    else if (this.decode_binary_latin1) {
	return this.latin1_to_string(dv,pos+4,Size);
    }    
    else {
	var Bin  = new Uint8Array(dv.buffer,pos+4, Size);
	return this.binary(Bin);
    }
};

EiClass.prototype.decode_big_bytes = function (dv,pos,len) {
    var Sign = dv.getUint8(pos++);
    if (len == 4) {
	var Num = dv.getUint32(pos,false);
	if (Sign) return -Num;
	return Num;
    }
    throw ("bad number");
};

// system/atom, ref:18, creation:8
EiClass.prototype.decode_ref = function (dv,pos) {
    var Sys, Creation, Ref;
    
    Sys = this.decode_term(dv,pos);
    pos += this.decode_size_term(dv,pos);
    Ref = dv.getUint32(pos,false);
    Creation = dv.getUint8(pos+4);
    return new EiReference(Sys,[Ref],Creation);
};

// NumRefs:16, system/atom, Ref0:32,..RefN-1:32., creation:8
EiClass.prototype.decode_new_ref = function (dv,pos) {
    var n, Sys, Creation, Refs = [], i;

    n = dv.getUint16(pos,false);
    pos += 2;
    Sys = this.decode_term(dv,pos);
    pos += this.decode_size_term(dv,pos);
    for (i = 0; i < n; i++) {
	var Ref = dv.getUint32(pos,false);
	Refs.push(Ref);
	pos += 4;
    }
    Creation = dv.getUint8(pos);
    return new EiReference(Sys,Refs,Creation);
};

// NumRefs:16, system/atom, Ref0:32,..RefN-1:32., creation:32
EiClass.prototype.decode_newer_ref = function (dv,pos) {
    var n, Sys, Creation, Refs = [], i;

    n = dv.getUint16(pos,false);
    pos += 2;
    Sys = this.decode_term(dv,pos);
    pos += this.decode_size_term(dv,pos);
    for (i = 0; i < n; i++) {
	var Ref = dv.getUint32(pos,false);
	Refs.push(Ref);
	pos += 4;
    }
    Creation = dv.getUint32(pos,false);
    return new EiReference(Sys,Refs,Creation);
};

// check if list of 8-bit / 16-bit integers
EiClass.prototype.is_string = function (dv,pos,len) {
    var i = 0;
    while(i < len) {
	var tag = dv.getUint8(pos++);
	if (tag == this.SMALL_INTEGER) {
	    pos += 1;
	}
	else if (tag == this.INTEGER) {
	    var val = dv.getInt32(pos,false);
	    if ((val < 0) || (val > 65535)) return false;
	    pos += 4;
	}
	else
	    return false;
	i++;
    }
    // must end with NIL
    return dv.getUint8(pos) == this.NIL;
}

// Convert an array of bytes into a string.
// STRING <size:16> <utf8-encoded bytes>

EiClass.prototype.decode_string = function (dv,pos,count) {
    var s = "";

    if (this.decode_string_utf8) {
	while (count > 0) {
	    var c1 = dv.getUint8(pos++);
	    if (c1 < 0x80) {
		// 7-bit code (ascii)
		s += String.fromCharCode(c1);
		count--;
	    }
	    else if ((c1 >= 0xc0) && (c1 < 0xe0)) { // 11-bit code
		var c2 = dv.getUint8(pos++);
		s += String.fromCharCode(((c1 & 0x1f) << 6) | (c2 & 0x3f));
		count -= 2;
	    }
	    else if ((c1 >= 0xe0) && (c1 < 0xf0)) {
		// 16-bit code	    
		var c2 = dv.getUint8(pos++);
		var c3 = dv.getUint8(pos++);
		s += String.fromCharCode(((c1 & 0x0f) << 12) |
					 ((c2 & 0x3f) << 6) | (c3 & 0x3f));
		count -= 3;
	    }
	    else {
		// this is an error, proably latin1
		s += String.fromCharCode(c1);
		count--;
	    }
	}
    }
    else { // latin1
	while (count > 0) {
	    var x = dv.getUint8(pos++);
	    s += String.fromCharCode(x & 0xff);
	    count--;
	}
    }
    return s;
};

//
// FIXME: find better way
// Special hack for argument lists  ['array',64,65,66,67]
// to separate this in the erlang term_to_binary from a string!
// also make sure [array|T] is encoded as [array,array|T]
//

EiClass.prototype.decode_list = function (dv,pos,count) {

    if (this.decode_list_string && this.is_string(dv,pos,count)) {
	var s = "";

	while(count > 0) {
	    var x1;
	    
	    if (dv.getUint8(pos++) == this.SMALL_INTEGER)
	    { x1 = dv.getUint8(pos); pos += 1; }
	    else { x1 = dv.getInt32(pos,false); pos += 4; }
	    count--;
	    
	    if (this.decode_list_utf8) {
		if (x1 < 128) {	// 7-bit code (ascii)
		    s += String.fromCharCode(x1);
		}
		else if ((x1 >= 0xc0) && (x1 < 0xe0)) { // 11-bit code
		    var x2;
		    
		    if (dv.getUint8(pos++) == this.SMALL_INTEGER)
		    { x2 = dv.getUint8(pos); pos += 1; }
		    else { x2 = dv.getInt32(pos,false); pos += 4; }
		    count--;
		    
		    s += String.fromCharCode(((x1 & 0x1f) << 6) | (x2 & 0x3f));
		}
		else if ((x1 >= 0xe0) && (x1 < 0xf0)) {
		    var x2, x3;

		    if (dv.getUint8(pos++) == this.SMALL_INTEGER)
		    { x2 = dv.getUint8(pos); pos += 1; }
		    else { x2 = dv.getInt32(pos,false); pos += 4; }
		    count--;
		    
		    if (dv.getUint8(pos++) == this.SMALL_INTEGER)
		    { x3 = dv.getUint8(pos); pos += 1; }
		    else { x3 = dv.getInt32(pos,false); pos += 4; }
		    count--;

		    s += String.fromCharCode(((x1 & 0x0f) << 12) |
					     ((x2 & 0x3f) << 6) | (x3 & 0x3f));
		}
		else {
		    // 16 bit or bad latin1 char
		    s += String.fromCharCode(x1);
		}
	    }
	    else {
		s += String.fromCharCode(x1);
	    }
	}
	return s;
    }
    else {
	var Arr = [];
	var i;
	// console.debug("decode_list len=" + len);
	for (i = 0; i < count; i++) {
	    var Term = this.decode_term(dv,pos);
	    var k   = this.decode_size_term(dv,pos);
	    //console.debug("term (tag="+dv.getUint8(pos)+"["+ Term+ "] size = "+k);
	    pos += k;
	    if (!((i==0) && this.eqAtom(Term, "array")))
		Arr.push(Term);
	}
	// console.debug("decode_list end");
	if (dv.getUint8(pos) != this.NIL) {  // improper list not allowed
	    throw ("List does not end with NIL! [tag="+dv.getUint8(pos)+"]");
	}
	return Arr;
    }
};

EiClass.prototype.decode_tuple = function (dv,pos,len) {
    var i, Arr = [];
    for (i = 0; i < len; i++) {
	var Term = this.decode_term(dv,pos);
	pos += this.decode_size_term(dv,pos);
	Arr.push(Term);
    }
    return new EiTuple(Arr);
};


EiClass.prototype.decode_map = function (dv,pos,len) {
    var key, value, i, Obj = new Object();
    for (i = 0; i < len; i++) {
	key = this.decode_term(dv,pos);
	pos += this.decode_size_term(dv,pos);
	value = this.decode_term(dv,pos);
	pos += this.decode_size_term(dv,pos);
	Obj[key.value] = value;  // key must be EiAtom! fixme test!
    }
    return Obj;
};

// Convert an array of (latin1) bytes into a string.
EiClass.prototype.latin1_to_string = function (dv,pos,count) {
    var i, s = "";
    for (i = 0; i < count; i++,pos++)
	s += String.fromCharCode(dv.getUint8(pos));
    return s;
};

// Convert an array of utf8 bytes into a unicode string. FIXME!
EiClass.prototype.utf8_to_string = function (dv,pos,count) {
    var s = "";
    while(count > 0) {
	var c1 = dv.getUint8(pos++);
	if (c1 < 128) {
	    // 7-bit code (ascii)
	    s += String.fromCharCode(c1);
	    count--;
	} else if ((c1 > 191) && (c1 < 224)) {
	    // 11-bit code
	    var c2 = dv.getUint8(pos++);
	    s += String.fromCharCode(((c1 & 31) << 6) | (c2 & 63));
	    count -= 2;
	} else if ((c1 > 223) && (c1 < 240)) {
	    // 16-bit code	    
	    var c2 = dv.getUint8(pos++);
	    var c3 = dv.getUint8(pos++);
	    s += String.fromCharCode(((c1 & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
	    count -= 3;	    
	} else {
	    throw ("bad utf8");  // bad for java script! fixme?
	}
    }
    return s;
};

// convert latin1 string to array of bytes
EiClass.prototype.decode_latin1_string = function (dv,pos,count) {
    var i, s = "";
    for (i = 0; i < count; i++,pos++)
	s += String.fromCharCode(dv.getUint8(pos));
    return s;
};


// Convert an array of bytes into a string.
EiClass.prototype.string_to_bytes = function (Obj,dv,pos,count) {
    var i;
    for (i = 0; i < count; i++, pos++)
	dv.setUint8(pos, Obj.charCodeAt(i));
    return pos;
};

// Pretty Print a byte-string in Erlang binary form.
EiClass.prototype.pp_bytes = function (Bin) {
    var i, s = "";
    for (i = 0; i < Bin.length; i++) {
	if (s !== "") {
	    s += ",";
	}
	s += "" + Bin[i];
    }
    return "<<" + s + ">>";
};

// Pretty Print a JS object in Erlang term form.
EiClass.prototype.pp = function (Obj) {
    return Obj.toString();
};

var Ei = new EiClass();
