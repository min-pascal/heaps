package hxsl;
using hxsl.Ast;

private class Alloc {
	public var t : VecType;
	public var pos : Int;
	public var size : Int;
	public var g : TVar;
	public var v : Null<TVar>;
	public function new(g, t, pos, size) {
		this.g = g;
		this.t = t;
		this.pos = pos;
		this.size = size;
	}
}

enum ARead {
	AIndex( a : Alloc );
	AOffset( a : Alloc, stride : Int, delta : TExpr );
}

class Flatten {

	var globals : Array<TVar>;
	var params : Array<TVar>;
	var outVars : Array<TVar>;
	var varMap : Map<TVar,Alloc>;
	var textureFormats : Array<{ dim : TexDimension, arr : Bool, rw : Int, isDepth : Bool }>;
	public var allocData : Map< TVar, Array<Alloc> >;

	public function new() {
	}

	public function flatten( s : ShaderData, kind : FunctionKind ) : ShaderData {
		globals = [];
		params = [];
		outVars = [];
		textureFormats = [];
		varMap = new Map();
		allocData = new Map();
		for( v in s.vars )
			gatherVar(v);
		var prefix = switch( kind ) {
		case Vertex: "vertex";
		case Fragment: "fragment";
		case Main: "compute";
		default: throw "assert";
		}
		pack(prefix + "Globals", Global, globals, VFloat);
		pack(prefix + "Params", Param, params, VFloat);
		var allVars = globals.concat(params);
		textureFormats.sort(function(t1,t2) {
			if ( t1.rw != t2.rw )
				return t1.rw - t2.rw;
			if ( t1.arr != t2.arr )
				return t1.arr ? 1 : -1;
			return t1.dim.getIndex() - t2.dim.getIndex();
		});
		for( t in textureFormats ) {
			var name = t.dim == T2D ? "" : t.dim.getName().substr(1);
			if( t.rw > 0 ) name = "RW"+name+t.rw;
			if( t.arr ) name += "Array";
			if( t.isDepth ) name += "Depth";  // Add suffix for depth samplers
			var samplerType = if( t.isDepth ) {
				trace('[FLATTEN.packTextures] Creating TSamplerDepth for ${t.dim} isDepth=${t.isDepth}');
				TSamplerDepth(t.dim, t.arr);
			} else if( t.rw == 0 ) {
				TSampler(t.dim, t.arr);
			} else {
				TRWTexture(t.dim, t.arr, t.rw);
			};
			trace('[FLATTEN.packTextures] Packing "${prefix}Textures${name}" with type ${samplerType}');
			packTextures(prefix + "Textures" + name, allVars, samplerType);
		}
		packBuffers("buffers", allVars, Uniform);
		packBuffers("storagebuffers", allVars, Storage);
		packBuffers("rwbuffers", allVars, RW);
		var funs = [for( f in s.funs ) mapFun(f, mapExpr)];
		return {
			name : s.name,
			vars : outVars,
			funs : funs,
		};
	}

	function mapFun( f : TFunction, mapExpr : TExpr -> TExpr ) : TFunction {
		return {
			kind : f.kind,
			ret : f.ret,
			args : f.args,
			ref : f.ref,
			expr : mapExpr(f.expr),
		};
	}

	static var SWIZ = [X,Y,Z,W];

	function mkOp(e:TExpr,by:Int,f:Int->Int->Int,binop,pos) {
		switch( e.e ) {
		case TConst(CInt(i)):
			return { e : TConst(CInt(f(i,by))), t : TInt, p : pos };
		default:
		}
		return { e : TBinop(binop,e,mkInt(by,pos)), t : TInt, p : pos };
	}

	function mkAdd(e,offset,pos) {
		if( offset == 0 )
			return e;
		return mkOp(e,offset,(x,y) -> x + y,OpAdd,pos);
	}

	function mkMult( e : TExpr, by : Int, pos ) {
		if( by == 1 )
			return e;
		return mkOp(e,by,(x,y) -> x * y,OpMult,pos);
	}

	function mapExpr( e : TExpr ) : TExpr {
		inline function getFieldPos(expr, name) {
			var pos = -1;
			switch( expr.t ) {
			case TStruct(vl):
				var cur = 0;
				for( v in vl ) {
					if( v.name == name ) {
						pos = cur;
						break;
					}
					cur += v.type.size();
				}
			default:
			}
			if( pos < 0 ) throw "assert";
			return pos;
		}
		inline function readField(expr, pos, size) {
			var idx = pos >> 2;
			var arr : TExpr = optimize({ e : TArray(expr,{ e : TConst(CInt(idx)), p : e.p, t : TInt }), p : e.p, t : TVec(4,VFloat) });
			if( size == 4 && pos & 3 == 0 )
				return arr;
			var sw = SWIZ.slice(pos&3,(pos&3) + size);
			return { e : TSwiz(arr,sw), t : size == 1 ? TFloat : TVec(size,VFloat), p : e.p }
		}
		e = switch( e.e ) {
		case TVar(v):
			var a = varMap.get(v);
			if( a == null )
				e
			else
				access(a, v.type, e.p, AIndex(a));
		case TArray( { e : TVar(v), p : vp }, eindex):
			var a = varMap.get(v);
			if( a == null || (!v.type.match(TBuffer(_)) && eindex.e.match(TConst(CInt(_)))) )
				e.map(mapExpr);
			else {
				switch( v.type ) {
				case TArray(t, _) if( t.isTexture() ):
					eindex = toInt(mapExpr(eindex));
					access(a, t, vp, AOffset(a,1,eindex));
				case TBuffer(TInt|TFloat,_), TVec(_, VFloat|VInt):
					e.map(mapExpr);
				case TArray(t, _), TBuffer(t, _):
					var stride = varSize4Bytes(t, a.t);
					if( stride == 0 || (v.type.match(TArray(_)) && stride & 3 != 0) ) throw new Error("Dynamic access to an Array which size is not 4 components-aligned is not allowed", e.p);
					stride = (stride + 3) >> 2;
					eindex = toInt(mapExpr(eindex));
					access(a, t, vp, AOffset(a,stride, mkMult(eindex,stride,vp)));
				default:
					throw "assert";
				}
			}
		case TVarDecl(v, init) if( v.type.match(TStruct(_))):
			var size = Math.ceil(v.type.size() / 4);
			var v2 : TVar = {
				id : Tools.allocVarId(),
				name : v.name,
				type : TArray(TVec(4,VFloat),SConst(size)),
				kind : v.kind,
				qualifiers : v.qualifiers
			};
			var a = new Alloc(v2,VFloat,0,0);
			a.v = v;
			varMap.set(v, a);
			{ e : TVarDecl(v2, init == null ? null : mapExpr(init)), t : TVoid, p : e.p }
		case TField(expr, name):
			var pos = getFieldPos(expr, name);
			var expr = mapExpr(expr);
			switch( e.t ) {
			case TFloat:
				readField(expr, pos, 1);
			case TBytes(size):
				{ e : TCall({ e : TGlobal(UnpackSnorm4x8), p : e.p, t : TVec(size, VFloat) }, [
					floatBitsToUint(readField(expr, pos, 1))
				]), t : e.t, p : e.p }
			case TVec(size,VFloat):
				var idx = pos >> 2;
				var idx2 = ((pos + size - 1) >> 2);
				if( idx == idx2 )
					readField(expr, pos, size);
				else {
					var k = (idx2 << 2) - pos;
					var type = switch(size) {
					case 2: Vec2;
					case 3: Vec3;
					case 4: Vec4;
					default: throw "assert";
					}
					{ e : TCall({ e : TGlobal(type), p : e.p, t : TVoid },[
						readField(expr, pos, k),
						readField(expr, pos + k, size - k)
					]), t : e.t, p : e.p }
				}
			case TMat4:
				{ e : TCall({ e : TGlobal(Mat4), p : e.p, t : TVoid },[
					readField(expr, pos, 4),
					readField(expr, pos + 4, 4),
					readField(expr, pos + 8, 4),
					readField(expr, pos + 12, 4),
				]), t : e.t, p : e.p }
			case TMat3x4:
				{ e : TCall({ e : TGlobal(Mat3x4), p : e.p, t : TVoid },[
					readField(expr, pos, 4),
					readField(expr, pos + 4, 4),
					readField(expr, pos + 8, 4),
				]), t : e.t, p : e.p }
			default:
				throw "Unsupported type "+e.t.toString();
			}
		case TBinop(OpAssign, e1, e2) if ( e.t.match(TMat4) && e1.e.match(TField(_,_))):
			switch ( e1 ) {
			case {e : TField(expr, name), t : TMat4}:
				var pos = getFieldPos(expr, name);
				var expr = mapExpr(expr);
				{e : TBlock([
					{e : TBinop(OpAssign, readField(expr, pos, 4), {e : TArray(e2, {e : TConst(CInt(0)), t : TInt, p : null}), t : TVec(4, VFloat), p : null}), t : e.t, p : e.p},
					{e : TBinop(OpAssign, readField(expr, pos + 4, 4), {e : TArray(e2, {e : TConst(CInt(1)), t : TInt, p : null}), t : TVec(4, VFloat), p : null}), t : e.t, p : e.p},
					{e : TBinop(OpAssign, readField(expr, pos + 8, 4), {e : TArray(e2, {e : TConst(CInt(2)), t : TInt, p : null}), t : TVec(4, VFloat), p : null}), t : e.t, p : e.p},
					{e : TBinop(OpAssign, readField(expr, pos + 12, 4), {e : TArray(e2, {e : TConst(CInt(3)), t : TInt, p : null}), t : TVec(4, VFloat), p : null}), t : e.t, p : e.p},
				]), t : e.t, p : e.p}
			default : throw "assert";
			}
		default:
			e.map(mapExpr);
		};
		return optimize(e);
	}

	inline function mkInt(v:Int,pos) {
		return { e : TConst(CInt(v)), t : TInt, p : pos };
	}

	inline function readIndex( a : Alloc, index : Int, pos ) : TExpr {
		var offs = a.t == null ? a.pos : a.pos >> 2;
		return { e : TArray({ e : TVar(a.g), t : a.g.type, p : pos },mkInt(offs+index,pos)), t : TVec(4,a.t), p : pos }
	}

	inline function readOffset( a : Alloc, stride : Int, delta : TExpr, index : Int, pos ) : TExpr {
		var index = switch( a.g.type ) {
			case TBuffer(_,_,_) :
				index;
			default :
				(a.t == null ? a.pos : a.pos >> 2) + index;
		}
		var offset : TExpr = mkAdd(delta,index,pos);
		return { e : TArray({ e : TVar(a.g), t : a.g.type, p : pos }, offset), t : TVec(4,a.t), p:pos };
	}

	function access( a : Alloc, t : Type, pos : Position, acc : ARead ) : TExpr {
		inline function read(index, pos) {
			return switch( acc ) {
			case AIndex(a): readIndex(a, index, pos);
			case AOffset(a, stride, delta): readOffset(a, stride, delta, index, pos);
			}
		}
		switch( t ) {
		case TMat4:
			return { e : TCall( { e : TGlobal(Mat4), t : TFun([]), p : pos }, [
				read(0,pos),
				read(1,pos),
				read(2,pos),
				read(3,pos),
			]), t : TMat4, p : pos }
		case TMat3x4:
			return { e : TCall( { e : TGlobal(Mat3x4), t : TFun([]), p : pos }, [
				read(0,pos),
				read(1,pos),
				read(2,pos),
			]), t : TMat3x4, p : pos }
		case TMat3:
			return { e : TCall( { e : TGlobal(Mat3), t : TFun([]), p : pos } , [access(a, TMat3x4, pos, acc)] ), t : TMat3, p : pos };
		case TArray(t, SConst(len)):
			var stride = Std.int(a.size / len);
			var earr = [for( i in 0...len ) { var a = new Alloc(a.g, a.t, a.pos + stride * i, stride); access(a, t, pos, AIndex(a)); }];
			return { e : TArrayDecl(earr), t : t, p : pos };
		case TBuffer(_):
			return { e : TVar(a.g), t : t, p : pos };
		case TStruct(vl):
			var size = 0;
			for( v in vl )
				size += varSize(v.type, a.t);
			var stride = Math.ceil(size/4);
			var earr = [for( i in 0...stride ) read(i, pos)];
			return { e : TArrayDecl(earr), t : TArray(TVec(4,VFloat),SConst(stride)), p : pos };
		case t if( t.isTexture() ):
			var e = read(0, pos);
			e.t = t;
			return e;
		default:
			var size = varSize(t, a.t);
			if( size > 4 )
				return Error.t("Access not supported for " + t.toString(), null);
			var e = read(0, pos);
			if( size == 4 ) {
				// 0 size array : return vec4(0.)
				if( a.pos == -1 )
					return { e : TCall({ e : TGlobal(Vec4), t : TFun([]), p : pos },[{ e : TConst(CFloat(0)), t : TFloat, p : pos }]), t : TVec(4,VFloat), p : pos };
			} else {
				var sw = [];
				for( i in 0...size )
					sw.push(Tools.SWIZ[i + (a.pos & 3)]);
				e = { e : TSwiz(e, sw), t : t, p : pos };
			}
			switch( t ) {
			case TInt:
				e.t = TFloat;
				e = floatBitsToInt(e);
			case TVec(size,VInt):
				e.t = TVec(size,VFloat);
				e = { e : TCall({ e : TGlobal([IVec2,IVec3,IVec4][size-2]), t : TFun([]), p : pos }, [e]), t : t, p : pos };
			default:
			}
			return e;
		}
	}

	function floatBitsToInt( e : TExpr ) {
		return { e : TCall({ e : TGlobal(FloatBitsToInt), t : TFun([]), p : e.p }, [e]), t : TInt, p : e.p };
	}

	function floatBitsToUint( e : TExpr ) {
		return { e : TCall({ e : TGlobal(FloatBitsToUint), t : TFun([]), p : e.p }, [e]), t : TInt, p : e.p };
	}

	function toInt( e : TExpr ) {
		if( e.t == TInt ) return e;
		return { e : TCall({ e : TGlobal(ToInt), t : TFun([]), p : e.p }, [e]), t : TInt, p : e.p };
	}

	function optimize( e : TExpr ) {
		switch( e.e ) {
		case TCall( { e : TGlobal(Mat3x4) }, [ { e : TCall( { e : TGlobal(Mat4) }, args) } ]):
			var rem = 0;
			var size = 0;
			while( size < 4 ) {
				var t = args[args.length - 1 - rem].t;
				size += varSize(t,VFloat);
				rem++;
			}
			if( size == 4 ) {
				for( i in 0...rem )
					args.pop();
				var emat = switch( e.e ) { case TCall(e, _): e; default: throw "assert"; };
				return { e : TCall(emat, args), t : e.t, p : e.p };
			}
		case TArray( { e : TArrayDecl(el) }, { e : TConst(CInt(i)) } ):
			if( i >= 0 && i < el.length )
				return el[i];
			Error.t("Reading outside array bounds", e.p);
		default:
		}
		return e;
	}

	function packTextures( name : String, vars : Array<TVar>, t : Type ) {
		trace('[FLATTEN.packTextures] === Packing "${name}" with target type ${t} ===');
		trace('[FLATTEN.packTextures] Checking ${vars.length} variables...');
		
		var alloc = new Array<Alloc>();
		var g : TVar = {
			id : Tools.allocVarId(),
			name : name,
			type : t,
			kind : Param,
		};
		var pos = 0;
		var samplers = [];
		for( v in vars ) {
			
			var count = 1;
			if( !v.type.equals(t) ) {
				switch( v.type ) {
				case TChannel(_):
					// Channel matching depends on target type and channel's depth property
					// ONLY do depth-aware routing if we're actually generating depth2d types
					var doDepthRouting = t.match(TSamplerDepth(_,_));
					
					if (doDepthRouting) {
						switch(t) {
						case TSamplerDepth(T2D, false):
							// Depth sampler - only match if this channel is for depth
							var isDepth = false;
							if (v.qualifiers != null && v.hasQualifier(Depth)) {
								isDepth = true;
							} else {
								// Fallback: check name
								var fullPath = v.name;
								var p = v.parent;
								while (p != null) {
									fullPath = p.name + "." + fullPath;
									p = p.parent;
								}
								var n = fullPath.toLowerCase();
								isDepth = n.indexOf("shadow") >= 0 || n.indexOf("depth") >= 0;
							}
							if (!isDepth) {
								continue;  // Skip non-depth channels for depth samplers
							}
						case TSampler(T2D, false):
							// Regular sampler - only match if this channel is NOT for depth
							var isDepth = false;
							if (v.qualifiers != null && v.hasQualifier(Depth)) {
								isDepth = true;
							} else {
								// Fallback: check name
								var fullPath = v.name;
								var p = v.parent;
								while (p != null) {
									fullPath = p.name + "." + fullPath;
									p = p.parent;
								}
								var n = fullPath.toLowerCase();
								isDepth = n.indexOf("shadow") >= 0 || n.indexOf("depth") >= 0;
							}
							if (isDepth) {
								continue;  // Skip depth channels for regular samplers
							}
						default:
						}
					} else {
						// Depth routing disabled - all channels go to regular samplers
					}
				case TArray(t2,SConst(n)) if( t2.equals(t) ):
					count = n;
				default:
					continue;
				}
			}
			var a = new Alloc(g, null, pos, count);
			a.v = v;
			if( v.qualifiers != null )
				for( q in v.qualifiers )
					switch( q ) {
					case Sampler(name):
						for( i in 0...count )
							samplers[pos+i] = name;
					default:
					}
			varMap.set(v, a);
			alloc.push(a);
			pos += count;
		}
		g.type = TArray(t, SConst(pos));
		if( samplers.length > 0 ) {
			for( i in 0...pos )
				if( samplers[i] == null )
					samplers[i] = "";
			if( g.qualifiers == null )
				g.qualifiers = [];
			g.qualifiers.push(Sampler(samplers.join(",")));
		}
		if( alloc.length > 0 ) {
			outVars.push(g);
			allocData.set(g, alloc);
		} else {
		}
		return alloc;
	}

	function packBuffers( name : String, vars : Array<TVar>, kind ) {
		var alloc = new Array<Alloc>();
		var g : TVar = {
			id : Tools.allocVarId(),
			name : name,
			type : TVoid,
			kind : Param,
		};
		for( v in vars )
			switch( v.type ) {
			case TBuffer(t,SConst(size),k) if( kind == k ):
				var stride = Math.ceil(t.size()/4);
				var bt = switch( t ) {
				case TInt|TFloat if( kind.match( Storage|RW|StoragePartial|RWPartial ) ) :
					v.type;
				default:
					// for buffers of complex types, let's perform our own remaping
					// this ensure that there's no difference between buffer layout
					// depending on the platform or compiler
					TBuffer(TVec(4,VFloat),SConst(size * stride),k);
				}
				var vbuf : TVar = {
					id : Tools.allocVarId(),
					name : v.name,
					type : bt,
					kind : Param,
				};
				// register an allocation that is required for filling the buffers
				var a = new Alloc(vbuf, null, alloc.length, 1);
				a.t = VFloat;
				a.v = v;
				alloc.push(a);
				varMap.set(v, a);
				outVars.push(vbuf);
			default:
			}
		g.type = TArray(TBuffer(TVoid,SConst(0),kind),SConst(alloc.length));
		allocData.set(g, alloc);
	}

	function pack( name : String, kind : VarKind, vars : Array<TVar>, t : VecType ) {
		var alloc = new Array<Alloc>(), apos = 0;
		var g : TVar = {
			id : Tools.allocVarId(),
			name : name,
			type : TVec(0,t),
			kind : kind,
		};
	// For Metal compatibility: align all parameters to vec4 (4-component) boundaries
	// This ensures that vec3 parameters don't span multiple vec4 slots when accessed as float4*
	var metalAlignedPacking = true;		for( v in vars ) {
			if( v.type.isTexture() || v.type.match(TBuffer(_)) )
				continue;
			switch( v.type ) {
			case TArray(t,_) if( t.isTexture() ): continue;
			default:
			}
			var size = varSize(v.type, t);
			if( size == 0 ) {
				// 0-size array !
				var a = new Alloc(g, t, -1, size);
				a.v = v;
				varMap.set(v, a);
				continue;
			}
			
			// Metal-aligned packing: ensure each parameter starts at a vec4 boundary
			if( metalAlignedPacking ) {
				// Align apos to next vec4 boundary (multiple of 4)
				if( apos % 4 != 0 ) {
					var alignPad = 4 - (apos % 4);
					var a = new Alloc(g, t, apos, alignPad);
					apos += alignPad;
					alloc.push(a);
				}
				
				// Allocate at aligned position
				var a = new Alloc(g, t, apos, size);
				a.v = v;
				varMap.set(v, a);
				alloc.push(a);
				apos += size;
				
				// Pad to complete the vec4 if needed
				var pad = (4 - (size % 4)) % 4;
				if( pad > 0 ) {
					var a = new Alloc(g, t, apos, pad);
					apos += pad;
					alloc.push(a);
				}
				continue;
			}
			
			// Original packing logic (non-Metal, kept for compatibility)
			var best : Alloc = null;
			for( a in alloc )
				if( a.v == null && a.size >= size && (best == null || best.size > a.size) )
					best = a;
			if( best != null ) {
				var free = best.size - size;
				if( free > 0 ) {
					var i = alloc.indexOf(best);
					var a = new Alloc(g, t, best.pos + size, free);
					alloc.insert(i + 1, a);
					best.size = size;
				}
				best.v = v;
				varMap.set(v, best);
			} else {
				var a = new Alloc(g, t, apos, size);
				apos += size;
				a.v = v;
				varMap.set(v, a);
				alloc.push(a);
				var pad = (4 - (size % 4)) % 4;
				if( pad > 0 ) {
					var a = new Alloc(g, t, apos, pad);
					apos += pad;
					alloc.push(a);
				}
			}
		}
		g.type = TArray(TVec(4, t), SConst(apos >> 2));
		if( apos > 0 ) {
			outVars.push(g);
			allocData.set(g, alloc);
		}
		return g;
	}

	function varSize( v : Type, t : VecType ) {
		return switch( v ) {
		case TFloat, TInt if( t == VFloat ): 1;
		case TVec(n, t2) if( t == t2 ): n;
		case TBytes(n): n;
		case TMat4 if( t == VFloat ): 16;
		case TMat3, TMat3x4 if( t == VFloat ): 12;
		case TArray(at, SConst(n)): varSize(at, t) * n;
		case TStruct(vl):
			var size = 0;
			for( v in vl )
				size += varSize(v.type, t);
			size;
		default:
			throw v.toString() + " size unknown for type " + t;
		}
	}

	function varSize4Bytes( v : Type, t : VecType ) {
		return switch ( v ) {
		case TBytes(4): 1;
		case TBytes(_): throw v.toString() + " 4 bytes size unknown for type" + t;
		case TArray(at, SConst(n)): varSize4Bytes(at, t) * n;
		case TStruct(vl):
			var size = 0;
			for( v in vl )
				size += varSize4Bytes(v.type, t);
			size;
		default:
			varSize(v, t);
		}
	}

	function addTextureFormat(dim,arr,rw=0,isDepth=false) {
		for( f in textureFormats )
			if( f.dim == dim && f.arr == arr && f.rw == rw && f.isDepth == isDepth )
				return;
		textureFormats.push({ dim : dim, arr : arr, rw : rw, isDepth : isDepth });
	}

	function gatherVar( v : TVar ) {
		switch( v.type ) {
		case TStruct(vl):
			for( v in vl )
				gatherVar(v);
			return;
		case TSampler(dim, arr), TRWTexture(dim, arr, _):
			var rw = switch( v.type ) {
			case TRWTexture(_,_,chans): chans;
			default: 0;
			}
			addTextureFormat(dim,arr,rw);
		case TSamplerDepth(dim, arr):
			// Depth samplers are tracked separately - they'll be packed as TSamplerDepth
			addTextureFormat(dim, arr, 0, true);
		case TChannel(_):
			// DEBUG: Check qualifier state and full path
			var hasDepthQual = v.qualifiers != null && v.hasQualifier(Depth);
			var fullPath = v.name;
			var p = v.parent;
			while (p != null) {
				fullPath = p.name + "." + fullPath;
				p = p.parent;
			}
			if( v.qualifiers != null ) {
			}
			
			// Check if this is a shadow/depth channel by @depth qualifier or variable name
			var isDepthChannel = false;
			if (v.qualifiers != null) {
				for (q in v.qualifiers) {
					switch (q) {
					case Depth:
						isDepthChannel = true;
						break;
					default:
					}
				}
			}
			// Fallback: check variable name for common depth/shadow patterns
			// This handles cases where qualifier is lost during linking
			if (!isDepthChannel) {
				var n = fullPath.toLowerCase();  // Use full path, not just name
				isDepthChannel = n.indexOf("shadow") >= 0 || 
				                 n.indexOf("depth") >= 0;
			}
			
			// Phase 2: Enable depth2d generation with proper sampling code
			// depth2d textures require .sample_compare() or .read() instead of .sample()
			// Currently only supported for Metal backend
			var useDepth2d = #if (!hlsdl && !js) true #else false #end;
			if (!useDepth2d) isDepthChannel = false;
			
			addTextureFormat(T2D, false, 0, isDepthChannel);
		case TArray(type, _):
			switch ( type ) {
			case TSampler(dim, arr):
				addTextureFormat(dim, arr, 0);
			case TSamplerDepth(dim, arr):
				addTextureFormat(dim, arr, 0, true);
			case TRWTexture(dim, arr, chans):
				addTextureFormat(dim, arr, chans);
			default:
			}
		default:
		}
		switch( v.kind ) {
		case Global:
			if( v.hasQualifier(PerObject) )
				params.push(v);
			else
				globals.push(v);
		case Param:
			params.push(v);
		default:
			outVars.push(v);
		}
	}

}