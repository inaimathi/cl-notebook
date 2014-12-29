CodeMirror.defineMode("commonlisp", function (config) {
    var assumeBody = /^with|^def|^lambda|^do|^prog|^f?let\*?|case$|bind$|a?when$|a?unless$/i;
    var numLiteral = /^(?:[+\-]?(?:\d+|\d*\.\d+)(?:[efd][+\-]?\d+)?|[+\-]?\d+(?:\/[+\-]?\d+)?|#b[+\-]?[01]+|#o[+\-]?[0-7]+|#x[+\-]?[\da-f]+)/;
    var symbol = /[^\s'`,@()\[\]";]/;
    var builtin = /^(if|when|cond|lambda|case|f?let\*?|loop|prog.)$/i;
    var type;

    function readSym(stream) {
	var ch;
	while (ch = stream.next()) {
	    if (ch == "\\") stream.next();
	    else if (!symbol.test(ch)) { stream.backUp(1); break; }
	}
	return stream.current();
    }

    function base(stream, state) {
	if (stream.eatSpace()) {type = "ws"; return null;}
	if (stream.match(numLiteral)) return "number";
	var ch = stream.next();
	if (ch == "\\") ch = stream.next();

	if (ch == '"') return (state.tokenize = inString)(stream, state);
	else if (ch == "(") { type = "open"; return "bracket"; 
	}
	else if (ch == ")" || ch == "]") { type = "close"; return "bracket"; }
	else if (ch == ";") { stream.skipToEnd(); type = "ws"; return "comment"; }
	else if (/['`,@]/.test(ch)) return null;
	else if (ch == "|") {
	    if (stream.skipTo("|")) { stream.next(); return "symbol"; }
	    else { stream.skipToEnd(); return "error"; }
	} else if (ch == "#") {
	    var ch = stream.next();
	    if (ch == "[") { type = "open"; return "bracket"; }
	    else if (/[+\-=\.']/.test(ch)) return null;
	    else if (/\d/.test(ch) && stream.match(/^\d*#/)) return null;
	    else if (ch == "|") return (state.tokenize = inComment)(stream, state);
	    else if (ch == ":") { readSym(stream); return "meta"; }
	    else return "error";
	} else {
	    var name = readSym(stream);
	    if (name == ".") return null;
	    type = "symbol"
	    if (!state.ctx.first) state.ctx.first = name;

	    // highlighting the names of defined terms (the first symbol after a definition form)
	    if (/^def/.test(name)) {
		if (state.ctx.prev && name != "defvar" && name != "defparameter" && name != "defconstant")
		    state.ctx.prev.looking_for_args_p = true;
		return "def";
	    }
	    /////////////////////////////////////////////////////////////////////////////////////

	    if (/^(error|warn)/.test(name)) return "error-related";
	    if (builtin.test(name)) return "builtin";
	    if (name == "nil" || name == "t") return "atom";
	    if (name.charAt(0) == "*" && name.charAt(name.length-1) == "*") return "special-variable";
	    if (name.charAt(0) == ":") return "keyword";
	    if (name.charAt(0) == "&") return "variable-2";
	    return "variable";
	}
    }
    
    function inLocalBody(stream, state) {
	return (state.ctx.prev && state.ctx.prev.prev && state.ctx.prev.prev.prev && state.ctx.prev.prev.prev.local_body_form_p)
    }

    function inString(stream, state) {
	var escaped = false, next;
	while (next = stream.next()) {
	    if (next == '"' && !escaped) { state.tokenize = base; break; }
	    escaped = !escaped && next == "\\";
	}
	return "string";
    }

    function inComment(stream, state) {
	var next, last;
	while (next = stream.next()) {
	    if (next == "#" && last == "|") { state.tokenize = base; break; }
	    last = next;
	}
	type = "ws";
	return "comment";
    }

    return {
	startState: function () {
	    return {ctx: {prev: null, start: 0, indentTo: 0}, tokenize: base};
	},

	token: function (stream, state) {
	    if (stream.sol() && typeof state.ctx.indentTo != "number")
		state.ctx.indentTo = state.ctx.start + 1;

	    type = null;
	    var style = state.tokenize(stream, state);
	    if (type != "ws") {
		if (state.ctx.indentTo == null) {
		    var sym_p = (type == "symbol");
		    var cur = stream.current();
		    
		    // Special case for flet/labels (whose properties should be indented as though body args)
		    if (cur == "flet" | cur == "labels") {
			state.ctx.prev.local_body_form_p = true;
		    }
		    /////////////////////////////////////////////////////////////////////////////////////////

		    if (sym_p && assumeBody.test(cur) | inLocalBody(stream, state))
			state.ctx.indentTo = state.ctx.start + config.indentUnit;
		    else
			state.ctx.indentTo = "next";

		} else if (state.ctx.indentTo == "next") {
		    state.ctx.indentTo = stream.column();
		}
	    }
	    if (type == "open") { 
		state.ctx = { prev: state.ctx, start: stream.column(), indentTo: null, opening: "(" }
		// explicitly labelling arglists for arg-hinting purposes
		if (state.ctx.prev.prev && state.ctx.prev.prev.looking_for_args_p) {
		    state.ctx.prev.prev.looking_for_args_p = false;
		    state.ctx.node_type = "arglist";
		}
		//////////////////////////////////////////////////////////////////////////////////////////////
	    } else if (type == "close") { state.ctx = state.ctx.prev || state.ctx; }
	    return style;
	},

	indent: function (state, _textAfter) {
	    var i = state.ctx.indentTo;
	    return typeof i == "number" ? i : state.ctx.start + 1;
	},

	lineComment: ";;",
	blockCommentStart: "#|",
	blockCommentEnd: "|#"
    };
});

CodeMirror.defineMIME("text/x-common-lisp", "commonlisp");
