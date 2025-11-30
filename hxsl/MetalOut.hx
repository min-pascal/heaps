package hxsl;
import hxsl.Ast;

class MetalOut {

	static var KWD_LIST = "auto break case char const continue default do double else enum extern float for goto if inline int long register return short signed sizeof static struct switch typedef union unsigned void volatile while " +
		"atomic_bool atomic_uint bool buffer bvec2 bvec3 bvec4 centered complex dmat2 dmat2x2 dmat2x3 dmat2x4 dmat3 dmat3x2 dmat3x3 dmat3x4 dmat4 dmat4x2 dmat4x3 dmat4x4 " +
		"dvec2 dvec3 dvec4 false highp iimage1D iimage1DArray iimage2D iimage2DArray iimage2DMS iimage2DMSArray iimage2DRect iimage3D iimageBuffer iimageCube iimageCubeArray image1D " +
		"image1DArray image2D image2DArray image2DMS image2DMSArray image2DRect image3D imageBuffer imageCube imageCubeArray in inout invariant isampler1D isampler1DArray isampler2D " +
		"isampler2DArray isampler2DMS isampler2DMSArray isampler2DRect isampler3D isamplerBuffer isamplerCube isamplerCubeArray ivec2 ivec3 ivec4 lowp mat2 mat2x2 mat2x3 mat2x4 " +
		"mat3 mat3x2 mat3x3 mat3x4 mat4 mat4x2 mat4x3 mat4x4 mediump out patch precision readonly restrict sampler1D sampler1DArray sampler1DArrayShadow sampler1DShadow sampler2D " +
		"sampler2DArray sampler2DArrayShadow sampler2DMS sampler2DMSArray sampler2DRect sampler2DRectShadow sampler2DShadow sampler3D samplerBuffer samplerCube samplerCubeArray " +
		"samplerCubeArrayShadow samplerCubeShadow smooth subroutine true uimage1D uimage1DArray uimage2D uimage2DArray uimage2DMS uimage2DMSArray uimage2DRect uimage3D uimageBuffer " +
		"uimageCube uimageCubeArray uint usampler1D usampler1DArray usampler2D usampler2DArray usampler2DMS usampler2DMSArray usampler2DRect usampler3D usamplerBuffer usamplerCube " +
		"usamplerCubeArray uvec2 uvec3 uvec4 vec2 vec3 vec4 writeonly " +
		// Metal specific keywords
		"access array_ref as asm_fragment assert_inside_fragment_shader attributes available_mac available_ios available_mac_catalyst " +
		"bias break case cast_intrinsic class clamp color column_major comparative compiler_options const constexpr " +
		"container continue coordinated copy_from core decorator depthwise device dictionary direct discard dither_mode " +
		"do dynamic dynamicmemberreference else enum except explicit export extern " +
		"fallthrough false final fixed_sample_locations for fragment fragment_samples fragment_shader fragment_width function " +
		"global group group_relative groupshared half half2 half3 half4 header_map if implements import indirect_command_buffer " +
		"indirect_fragment_argument indirect_vertex_argument inherit inline kernel link_as_metal logic lossy " +
		"main_structure malloc max min multipass namespace no_optimization noduplicate noerror noinline noinline_recursive nonstable normalize normalized noreturn " +
		"objc object object_oriented packed patch patience patch_control_point per_primitive per_vertex per_vertex_aux percent placement_id plane post_depth_coverage precision preckip" +
		"prefix presents primitive program projection property protocol public published ray_data ray_indices ray_query recursively " +
		"register resource_group restrict result return root_constant round sample sample_mask sample_ref sampler_state sampler_compare_state saturate shader_data shader_interface " +
		"shadow shader simdgroup simdgroup_barrier simdgroup_matrix simdgroup_matrix_storage size specialise stagegroup standardinclude static static_assert storage_mode strict " +
		"struct submit_fragment super supress_tracing swift template terminal tess_control tess_eval tessellation tessellation_factor " +
		"texture texture_buffer thread threadgroup threadgroup_imageblock triangle_clockwise triangle_counterclockwise true typedef typeid typename typename_pack " +
		"uniform unsafe unsafe_reconstruct unsuppress_tracing using vec1 vertex vertex_data vertex_id vertex_shader visible_function " +
		"void volatile while [[]] writeable writeonly zeroing";

	static var KWDS = [for( k in ~/[ \t\r\n]+/g.split(KWD_LIST) ) k => true];
	static var GLOBALS = {
		var gl = [];
		inline function set(g:hxsl.Ast.TGlobal,str:String) {
			gl[g.getIndex()] = str;
		}
		for( g in hxsl.Ast.TGlobal.createAll() ) {
			var n = "" + g;
			n = n.charAt(0).toLowerCase() + n.substr(1);
			set(g, n);
		}
		set(Radians, "radians");
		set(Degrees, "degrees");
		set(Sin, "sin");
		set(Cos, "cos");
		set(Tan, "tan");
		set(Asin, "asin");
		set(Acos, "acos");
		set(Atan, "atan");
		set(Pow, "pow");
		set(Exp, "exp");
		set(Log, "log");
		set(Exp2, "exp2");
		set(Log2, "log2");
		set(Sqrt, "sqrt");
		set(Inversesqrt, "rsqrt");
		set(Abs, "abs");
		set(Sign, "sign");
		set(Floor, "floor");
		set(Ceil, "ceil");
		set(Fract, "fract");
		set(Saturate, "saturate");
		set(Min, "min");
		set(Max, "max");
		set(Clamp, "clamp");
		set(Mix, "mix");
		set(Step, "step");
		set(Smoothstep, "smoothstep");
		set(Length, "length");
		set(Distance, "distance");
		set(Dot, "dot");
		set(Cross, "cross");
		set(Normalize, "normalize");
		set(LReflect, "reflect");
		set(Texture, "sample");
		set(TextureLod, "sample");
		set(Texel, "read");
		set(ToInt, "int");
		set(ToFloat, "float");
		set(ToBool, "bool");
		set(Vec2, "float2");
		set(Vec3, "float3");
		set(Vec4, "float4");
		set(IVec2, "int2");
		set(IVec3, "int3");
		set(IVec4, "int4");
		set(BVec2, "bool2");
		set(BVec3, "bool3");
		set(BVec4, "bool4");
		set(Mat2, "float2x2");
		set(Mat3, "float3x3");
		set(Mat4, "float4x4");
		set(Mat3x4, "float4x3");
		set(VertexID, "vertex_id");
		set(InstanceID, "instance_id");
		set(FragCoord, "_fragCoord");
		set(FrontFacing, "is_front_facing");
		set(ComputeVar_LocalInvocation, "thread_position_in_threadgroup");
		set(ComputeVar_GlobalInvocation, "thread_position_in_grid");
		set(ComputeVar_LocalInvocationIndex, "thread_index_in_threadgroup");
		set(ComputeVar_WorkGroup, "threadgroup_position_in_grid");

		for( g in gl )
			KWDS.set(g, true);
		gl;
	};
	static var MAT34 = "typedef struct { float4 a; float4 b; float4 c; } _mat3x4;";

	var buf : StringBuf;
	var exprIds = 0;
	var exprValues : Array<String>;
	var locals : Map<Int,TVar>;
	var decls : Array<String>;
	var isVertex : Bool;
	var isCompute : Bool;
	var isFragment : Bool;
	var allNames : Map<String,Int>;

	public var varNames : Map<Int,String>;

	public function new() {
		varNames = new Map();
		allNames = new Map();
	}

	inline function add( v : Dynamic ) {
		buf.add(v);
	}

	inline function ident( v : TVar ) {
		add(varName(v));
	}

	function varName( v : TVar ) {
		var n = varNames.get(v.id);
		if( n != null )
			return n;
		n = v.name;
		if( KWDS.exists(n) )
			n = "_" + n;
		if( allNames.exists(n) ) {
			var k = 2;
			n += "_";
			while( allNames.exists(n + k) )
				k++;
			n += k;
		}
		varNames.set(v.id, n);
		allNames.set(n, v.id);
		return n;
	}

	function decl( s : String ) {
		if( decls.indexOf(s) >= 0 )
			return;
		decls.push(s);
	}

	function addType( t : Type ) {
		switch( t ) {
		case TVoid:
			add("void");
		case TInt:
			add("int");
		case TBytes(n):
			add("float" + n);
		case TBool:
			add("bool");
		case TFloat:
			add("float");
		case TString:
			add("string");
		case TVec(size, k):
			switch( k ) {
			case VFloat:
				add("float");
			case VInt:
				add("int");
			case VBool:
				add("bool");
			}
			add(size);
		case TMat2:
			add("float2x2");
		case TMat3:
			add("float3x3");
		case TMat4:
			add("float4x4");
		case TMat3x4:
			decl(MAT34);
			add("_mat3x4");
		case TSampler(dim, arr):
			add(getSamplerType(dim, arr));
		case TSamplerDepth(dim, arr):
			// Depth samplers always use depth2d<float> for Metal hardware depth comparison
			#if !macro
			trace('[MetalOut.addType] Generating depth sampler type for dim=${dim} arr=${arr}');
			#end
			add(getDepthSamplerType(dim, arr));
		case TRWTexture(dim, arr, chans):
			add(getTextureType(dim, arr, chans, true));
		case TStruct(vl):
			add("struct { ");
			for( v in vl ) {
				addVar(v);
				add("; ");
			}
			add("}");
		case TFun(_):
			add("function");
		case TArray(t, size):
			addType(t);
			add("[");
			switch( size ) {
			case SVar(v):
				ident(v);
			case SConst(v):
				add(v);
			}
			add("]");
		case TBuffer(_):
			throw "assert";
		case TChannel(n):
			add("channel" + n);
		}
	}

	function getSamplerType(dim:TexDimension, arr:Bool, ?varName:String) {
		// For shadow shaders, use depth2d<float> for 2D textures
		// This enables hardware depth comparison with sample_compare()
		if (isShadowShader && dim == T2D) {
			return "depth2d<float>";
		}
		
		return switch(dim) {
		case T1D: "texture1d<float>";
		case T2D: "texture2d<float>";
		case T3D: "texture3d<float>";
		case TCube: "texturecube<float>";
		}
	}
	
	function getDepthSamplerType(dim:TexDimension, arr:Bool) {
		// Depth samplers always use Metal depth texture types for hardware comparison
		return switch(dim) {
		case T1D: "depth1d<float>";  // Rarely used
		case T2D: "depth2d<float>";  // Standard for shadow maps
		case T3D: "depth3d<float>";  // For volumetric shadows
		case TCube: "depthcube<float>";  // For omnidirectional shadows
		}
	}

	function getTextureType(dim:TexDimension, arr:Bool, chans:Int, rw:Bool) {
		var prefix = rw ? "texture" : "texture";
		var type = switch(chans) {
		case 1: "float";
		case 2: "float2";
		case 3: "float3";
		case 4: "float4";
		default: "float";
		}
		return switch(dim) {
		case T1D: prefix + "1d<" + type + ", access::read_write>";
		case T2D: prefix + "2d<" + type + ", access::read_write>";
		case T3D: prefix + "3d<" + type + ", access::read_write>";
		case TCube: prefix + "cube<" + type + ", access::read_write>";
		}
	}

	function addVar( v : TVar ) {
		switch( v.type ) {
		case TArray(t, size):
			var old = v.type;
			v.type = t;
			addVar(v);
			v.type = old;
			add("[");
			switch( size ) {
			case SVar(v): ident(v);
			case SConst(n): add(n);
			}
			add("]");
		case TBuffer(t, size, kind):
			switch( kind ) {
			case Uniform:
				add("constant ");
			case RW, RWPartial:
				add("device ");
			case Storage, StoragePartial:
				add("device ");
			case Partial:
				add("device ");
			}
			addType(t);
			add(" *");
			ident(v);
			add(" [[buffer(0)]]"); // This will be handled by function parameter generation
		case TSampler(dim, arr):
			// Special handling for samplers to detect depth textures by name
			add(getSamplerType(dim, arr, v.name));
			add(" ");
			ident(v);
		case TSamplerDepth(dim, arr):
			// Depth samplers explicitly use depth texture types
			add(getDepthSamplerType(dim, arr));
			add(" ");
			ident(v);
		default:
			addType(v.type);
			add(" ");
			ident(v);
		}
	}

	function initVars( s : ShaderData ) {
		add("// Input Structure\n");
		add("struct InputData {\n");
		var attrIndex = 0;
		for( v in s.vars )
			if( v.kind == Input ) {
				add("\t");
				addVar(v);
				add(" [[attribute(" + attrIndex + ")]]");
				attrIndex++;
				add(";\n");
			}
		add("};\n\n");

		add("// Output Structure\n");
		if( isVertex ) {
			add("struct VertexOut {\n");
			add("\tfloat4 position [[position]];\n");
			for( v in s.vars )
				if( v.kind == Output && !varNames.exists(v.id) ) {
					add("\t");
					addVar(v);
					add(";\n");
				}
			add("};\n\n");
		} else if( isFragment ) {
			add("struct FragmentOut {\n");
			add("\tfloat4 color [[color(0)]];\n");
			add("};\n\n");
		}

		add("// Uniform Buffers\n");
		for( v in s.vars )
			if( v.kind == Global ) {
				add("constant ");
				addVar(v);
				add(";\n");
			}
		add("\n");

		// Textures and samplers
		add("// Textures and Samplers\n");
		for( v in s.vars )
			if( v.kind == Param && isTextureType(v.type) ) {
				addVar(v);
				add(";\n");
			}
		add("\n");
	}

	function isTextureType( t : Type ) : Bool {
		return switch( t ) {
		case TSampler(_,_): true;
		case TSamplerDepth(_,_): true;  // Depth samplers are textures too
		case TRWTexture(_,_,_): true;
		case TArray(TSampler(_,_), _): true;
		case TArray(TSamplerDepth(_,_), _): true;  // Array of depth samplers
		case TArray(TRWTexture(_,_,_), _): true;
		default: false;
		};
	}

	function addExpr( e : TExpr, tabs : String ) {
		switch( e.e ) {
		case TBlock(el):
			add("{\n");
			for( e in el )
				addExpr(e, tabs + "\t");
			add(tabs + "}");
		case TVarDecl(v, init):
			add(tabs);
			addVar(v);
			if( init != null ) {
				add(" = ");
				writeExpr(init);
			}
			add(";\n");
		case TIf(econd, eif, eelse):
			add(tabs + "if( ");
			writeExpr(econd);
			add(" ) ");
			addExpr(eif, tabs);
			if( eelse != null ) {
				add(" else ");
				addExpr(eelse, tabs);
			}
			add("\n");
		case TReturn(e):
			add(tabs + "return");
			if( e != null ) {
				add(" ");
				writeExpr(e);
			}
			add(";\n");
		case TDiscard:
			add(tabs + "discard_fragment();\n");
		case TMeta(_, _, e):
			// Metadata expressions - just output the inner expression
			addExpr(e, tabs);
		default:
			add(tabs);
			writeExpr(e);
			add(";\n");
		}
	}

	function writeExpr( e : TExpr ) {
		switch( e.e ) {
		case TCall(func, args):
			// Special handling for texture sampling in Metal
			switch( func.e ) {
			case TGlobal(Texture | TextureLod):
				// Metal texture sampling: texture.sample(sampler, coords) or depth2d.read(coords)
				// HXSL: texture(tex, coords) or textureLod(tex, coords, lod)
				if( args.length >= 2 ) {
					// Check if this is a depth texture by examining the texture variable
					var isDepthTexture = false;
					var textureVarName = "";
					
					switch(args[0].e) {
					case TArray({ e: TVar(v) }, _):
						textureVarName = v.name;
						// Depth textures are in fragmentTexturesDepth, vertexTexturesDepth, etc.
						isDepthTexture = textureVarName.toLowerCase().indexOf("depth") >= 0;
					default:
					}
					
					// args[0] should be the texture array access like fragmentTextures[0]
					
					if (isDepthTexture) {
						// For depth2d textures, we cannot use .sample() with a regular sampler
						// Instead, use .read() which reads raw pixel values
						// depth2d<float>::read() returns float, but HXSL expects vec4
						// So we wrap it in float4(depth, 0, 0, 1) to match expected format
						add("float4(");
						writeExpr(args[0]);  // Texture reference
						add(".read(uint2(");
						writeExpr(args[1]);  // UV coordinates
						add(" * float2(");
						// Get texture dimensions - for now, hardcode common shadow map size
						// TODO: Pass texture size as uniform or query dynamically
						add("1024.0, 1024.0");  // Assuming 1024x1024 shadow map
						add(")), 0), 0.0, 0.0, 1.0)");  // Wrap in float4, level 0
					} else {
						// Regular texture sampling
						writeExpr(args[0]);  // This outputs the texture
						add(".sample(");
						add("fragmentSamplers[");
						switch(args[0].e) {
						case TArray(_, { e: TConst(CInt(idx)) }):
							add(Std.string(idx));
						default:
							add("0");  // Fallback to sampler 0
						}
						add("], ");
						writeExpr(args[1]);  // UV coordinates
						add(")");
					}
				} else {
					// Fallback to standard call
					writeExpr(func);
					add("(");
					var first = true;
					for( a in args ) {
						if( !first ) add(", ");
						first = false;
						writeExpr(a);
					}
					add(")");
				}
			case TGlobal(Mat3):
				// Special handling for float3x3 constructor
				// Check if we're constructing from a float4x4 - need to extract 3x3 submatrix
				if( args.length == 1 ) {
					var argType = args[0].t;
					var needsExtraction = switch( argType ) {
					case TMat4: true;
					default: false;
					};
					
					if( needsExtraction ) {
						// Extract upper-left 3x3 from 4x4 matrix
						// float3x3(mat[0].xyz, mat[1].xyz, mat[2].xyz)
						add("float3x3(");
						add("float3(");
						writeExpr(args[0]);
						add("[0].xyz), float3(");
						writeExpr(args[0]);
						add("[1].xyz), float3(");
						writeExpr(args[0]);
						add("[2].xyz))");
					} else {
						// Standard constructor
						add("float3x3(");
						var first = true;
						for( a in args ) {
							if( !first ) add(", ");
							first = false;
							writeExpr(a);
						}
						add(")");
					}
				} else {
					// Multiple arguments - standard constructor
					add("float3x3(");
					var first = true;
					for( a in args ) {
						if( !first ) add(", ");
						first = false;
						writeExpr(a);
					}
					add(")");
				}
			case TGlobal(Mat3x4):
				// Special handling for _mat3x4 constructor (struct, not native matrix)
				decl(MAT34);
				if( args.length == 3 ) {
					// Constructor from 3 float4 vectors (rows)
					add("(_mat3x4){ ");
					writeExpr(args[0]);
					add(", ");
					writeExpr(args[1]);
					add(", ");
					writeExpr(args[2]);
					add(" }");
				} else {
					// Fallback to struct literal syntax
					add("(_mat3x4){ ");
					var first = true;
					for( a in args ) {
						if( !first ) add(", ");
						first = false;
						writeExpr(a);
					}
					add(" }");
				}
			default:
				// Standard function call
				writeExpr(func);
				add("(");
				var first = true;
				for( a in args ) {
					if( !first ) add(", ");
					first = false;
					writeExpr(a);
				}
				add(")");
			}
		case TField(expr, field):
			writeExpr(expr);
			if( field != null && field.length > 0 ) {
				add(".");
				add(field);
			}
		case TVar(v):
			ident(v);
		case TConst(c):
			switch( c ) {
			case CInt(i):
				// In Metal, integer literals can be ambiguous in overloaded function calls
				// Check if this appears in a float context by looking at expression type
				var needsFloatSuffix = switch( e.t ) {
				case TFloat, TVec(_, VFloat): true;
				default: false;
				};
				if( needsFloatSuffix && (i == 0 || i == 1) ) {
					add(i + ".0");
				} else {
					add(i);
				}
			case CFloat(f):
				var str = "" + f;
				add(str);
				// Ensure float literals always have decimal point for Metal
				if( str.indexOf(".") == -1 && str.indexOf("e") == -1 )
					add(".");
			case CBool(b): add(b ? "true" : "false");
			case CNull: add("0");
			case CString(s): add('"' + s + '"');
			}
		case TBinop(op, e1, e2):
			// Special handling for vector * matrix operations in Metal
			// where HXSL semantics differ from Metal's
			if( op == OpMult ) {
				// Check for vector * matrix multiplication
				// HXSL uses: vec * mat (row-major semantics)
				// Metal uses: mat * vec (column-major semantics)
				// 
				// Heaps stores matrices in row-major memory layout.
				// Metal interprets buffer data as column-major, so uploaded matrix
				// data is implicitly transposed: M_metal = transpose(M_heaps)
				//
				// To compute vec * M_heaps in Metal:
				// vec * M_heaps = transpose(transpose(M_heaps) * transpose(vec))
				// Since M_metal = transpose(M_heaps):
				// vec * M_heaps = transpose(M_metal * transpose(vec))
				// But transpose(vec) for column vector doesn't change it.
				// So: vec * M_heaps = transpose(M_metal) * vec
				
				var isVec = switch(e1.t) { case TVec(_, VFloat): true; default: false; };
				var isMat4 = switch(e2.t) { case TMat4: true; default: false; };
				
				if( isVec && isMat4 ) {
					// vec * mat4: Keep the original HXSL operation
					// Metal should support vec * mat just like GLSL does
					add("(");
					writeExpr(e1);
					add(" * ");
					writeExpr(e2);
					add(")");
				} else {
					// Check for vec3 * mat3x4 case
					// In HXSL: vec3 * mat3x4 = vec3
					// _mat3x4 is a struct { float4 a; float4 b; float4 c; }
					// vec3 * _mat3x4 = float3(dot(vec, a.xyz), dot(vec, b.xyz), dot(vec, c.xyz)) + vec.x * float3(a.w, b.w, c.w)
					var isVec3 = switch(e1.t) { case TVec(3, VFloat): true; default: false; };
					var isMat3x4 = switch(e2.t) { case TMat3x4: true; default: false; };

					if( isVec3 && isMat3x4 ) {
						// Manual multiplication: vec3 * mat3x4
						// Result = (vec.x * row0.xyz + vec.y * row1.xyz + vec.z * row2.xyz) + (vec.x * row0.w + vec.y * row1.w + vec.z * row2.w) * float3(1,1,1)
						// Simplified: float3(dot(vec, row0.xyz), dot(vec, row1.xyz), dot(vec, row2.xyz)) + float3(vec.x*row0.w, vec.y*row1.w, vec.z*row2.w)
						// Even simpler for affine: multiply by 3x3 part, then add translation scaled by 1
						add("(float3(dot(");
						writeExpr(e1);
						add(", (");
						writeExpr(e2);
						add(").a.xyz), dot(");
						writeExpr(e1);
						add(", (");
						writeExpr(e2);
						add(").b.xyz), dot(");
						writeExpr(e1);
						add(", (");
						writeExpr(e2);
						add(").c.xyz)) + float3((");
						writeExpr(e2);
						add(").a.w, (");
						writeExpr(e2);
						add(").b.w, (");
						writeExpr(e2);
						add(").c.w))");
					} else {
						add("(");
						writeExpr(e1);
						add(" ");
						add(Printer.opStr(op));
						add(" ");
						writeExpr(e2);
						add(")");
					}
				}
			} else {
				add("(");
				writeExpr(e1);
				add(" ");
				add(Printer.opStr(op));
				add(" ");
				writeExpr(e2);
				add(")");
			}
		case TUnop(op, e1):
			var opStr = switch( op ) {
			case OpNot: "!";
			case OpNeg: "-";
			case OpNegBits: "~";
			case OpIncrement: "++";
			case OpDecrement: "--";
			default: ""; // OpSpread not used in shaders
			};
			add(opStr);
			add("(");
			writeExpr(e1);
			add(")");
		case TArrayDecl(el):
			add("{ ");
			var first = true;
			for( e in el ) {
				if( !first ) add(", ");
				first = false;
				writeExpr(e);
			}
			add(" }");
		case TArray(e, index):
			// Check if this is a texture array access - Metal doesn't support texture arrays in shader args
			// So we treat tex[0] as just tex (the texture is bound to texture slot 0)
			var isTextureArrayAccess = switch( e.e ) {
			case TVar(v): isTextureType(v.type);
			default: false;
			};
			
			if( isTextureArrayAccess && switch(index.e) { case TConst(CInt(0)): true; default: false; } ) {
				// Skip [0] for texture variables - just output the texture name
				writeExpr(e);
			} else {
				// Normal array access
				writeExpr(e);
				add("[");
				writeExpr(index);
				add("]");
			}
		case TSwiz(e, regs):
			// Check if we're swizzling a scalar (broadcast to vector)
			var isScalarBroadcast = switch(e.t) {
				case TFloat: regs.length > 1;
				default: false;
			}

			if( isScalarBroadcast ) {
				// Metal doesn't support swizzling scalars - use constructor instead
				// e.g. depth.xxxx becomes float4(depth)
				var componentType = switch(regs.length) {
					case 2: "float2";
					case 3: "float3";
					case 4: "float4";
					default: "float";
				}
				add(componentType + "(");
				writeExpr(e);
				add(")");
			} else {
				writeExpr(e);
				if( regs.length > 0 ) {
					add(".");
					for( r in regs )
						add(r.getName().toLowerCase());
				}
			}
		case TGlobal(g):
			add(GLOBALS[g.getIndex()]);
		case TParenthesis(e):
			add("(");
			writeExpr(e);
			add(")");
		case TBlock(el):
			// Block expressions in Metal need statement-expression syntax: ({ stmts; value; })
			add("({\n");
			for( i in 0...el.length ) {
				add("\t");
				if( i == el.length - 1 ) {
					// Last expression is the return value
					writeExpr(el[i]);
					add(";\n");
				} else {
					addExpr(el[i], "\t");
				}
			}
			add("})");
		case TMeta(_, _, e):
			// Metadata expressions - just output the inner expression like GLSL does
			writeExpr(e);
		default:
			add("/* unsupported expr: " + e.e.getName() + " */");
		}
	}

	var isShadowShader : Bool = false;  // NEW: Track if this shader uses shadows
	
	public function run( s : ShaderData ) {
		locals = new Map();
		decls = [];
		buf = new StringBuf();
		exprValues = [];
		
		// Reset shader type flags
		isVertex = false;
		isFragment = false;
		isCompute = false;
		
		// Detect if this is a shadow shader by checking for shadow-related variables
		isShadowShader = false;
		for (v in s.vars) {
			var lowerName = v.name.toLowerCase();
			// Debug: log all variable names
			try {
				var f = sys.io.File.append("/tmp/metal_shader_vars.txt", false);
				f.writeString('Shader "${s.name}" var: "${v.name}" (${v.kind})\n');
				f.close();
			} catch(e:Dynamic) {}
			
			if (lowerName.indexOf("shadow") >= 0) {
				isShadowShader = true;
				try {
					var f = sys.io.File.append("/tmp/metal_shader_vars.txt", false);
					f.writeString('  -> SHADOW SHADER DETECTED!\n');
					f.close();
				} catch(e:Dynamic) {}
				break;
			}
		}

		if( s.funs.length != 1 ) throw "assert";
		var f = s.funs[0];

		switch( f.kind ) {
		case Vertex: isVertex = true;
		case Fragment: isFragment = true;
		case Main: isCompute = true;
		default: throw "Unsupported shader kind";
		}

		add("#include <metal_stdlib>\n");
		add("using namespace metal;\n\n");

		// Add helper functions
		add("// Helper function to convert screen coordinates to UV texture coordinates\n");
		add("static inline float2 screenToUv(float2 screen) {\n");
		add("\treturn screen * float2(0.5, -0.5) + float2(0.5, 0.5);\n");
		add("}\n\n");

		// Generate proper input/output structures
		if( isVertex ) {
			// Vertex input structure with proper attribute indices
			add("struct VertexIn {\n");
			var attrIndex = 0;
			for( v in s.vars ) {
				if( v.kind == Input ) {
					add("\t");
					addType(v.type);
					add(" ");
					add(varName(v));
					add(" [[attribute(" + attrIndex + ")]];\n");
					attrIndex++;
				}
			}
			add("};\n\n");

			// Vertex output structure
			add("struct VertexOut {\n");
			add("\tfloat4 position [[position]];\n");
			for( v in s.vars ) {
				if( v.kind == Var ) {
					add("\t");
					addType(v.type);
					add(" ");
					add(varName(v));
					add(";\n");
				}
			}
			add("};\n\n");

			// Vertex main function
			add("vertex VertexOut vertex_main(VertexIn input [[stage_in]]");
			
			// Add uniform buffer parameters (Global and Param kinds)
			// Buffer index 0 is reserved for vertex data (via [[stage_in]]), so uniforms start at index 1
			var bufferIndex = 1;
			for( v in s.vars ) {
				if( v.kind == Global || v.kind == Param ) {
					add(", constant ");
					var old = v.type;
					switch( v.type ) {
					case TArray(t, _):
						v.type = t;
						addType(v.type);
						v.type = old;
						add(" *");
						add(varName(v));
						add(" [[buffer(" + bufferIndex + ")]]");
					default:
						addType(v.type);
						add(" *");
						add(varName(v));
						add(" [[buffer(" + bufferIndex + ")]]");
					}
					bufferIndex++;
				}
			}
			
			add(") {\n");
			add("\tVertexOut output;\n");

			// Declare local variables for inputs
			for( v in s.vars ) {
				if( v.kind == Input ) {
					add("\t");
					addType(v.type);
					add(" ");
					add(varName(v));
					add(" = input.");
					add(varName(v));
					add(";\n");
				}
			}

			// Declare local variable for output position (NOT output.position!)
			for( v in s.vars ) {
				if( v.kind == Output ) {
					add("\t");
					addType(v.type);
					add(" ");
					add(varName(v));  // Just the variable name, not struct access
					add(";\n");
				}
			}

			// Declare local variables for varying outputs
			for( v in s.vars ) {
				if( v.kind == Var ) {
					add("\t");
					addType(v.type);
					add(" ");
					add(varName(v));
					add(";\n");
				}
			}

			// Declare Local variables (intermediate calculations)
			for( v in s.vars ) {
				if( v.kind == Local ) {
					add("\t");
					addType(v.type);
					add(" ");
					add(varName(v));
					add(";\n");
				}
			}

			// Process shader expression
			addExpr(f.expr, "\t");

			// Assign outputs
			for( v in s.vars ) {
				if( v.kind == Output ) {
					add("\toutput.position = ");
					add(varName(v));
					add(";\n");
				}
			}
			for( v in s.vars ) {
				if( v.kind == Var ) {
					add("\toutput.");
					add(varName(v));
					add(" = ");
					add(varName(v));
					add(";\n");
				}
			}

			add("\treturn output;\n");
			add("}\n");

		} else if( isFragment ) {
			// Fragment input (from vertex) - includes varyings (Var kind) from vertex shader
			add("struct VertexOut {\n");
			add("\tfloat4 position [[position]];\n");
			for( v in s.vars ) {
				// In fragment shaders, Var kind are the varyings from vertex shader
				// This must match the vertex shader's VertexOut struct
				if( v.kind == Var ) {
					add("\t");
					addType(v.type);
					add(" ");
					add(varName(v));
					add(";\n");
				}
			}
			add("};\n\n");

			// Fragment main function - add texture and buffer parameters
			add("fragment float4 fragment_main(VertexOut input [[stage_in]]");
			
			// DEBUG: List all variables
			#if !macro
			trace('[MetalOut.fragment_main] Processing ${s.vars.length} variables:');
			for (v in s.vars) {
				if (v.kind == Param || v.kind == Global) {
					trace('[MetalOut.fragment_main]   ${v.name}: ${v.type} kind=${v.kind}');
				}
			}
			#end
			
			// Add texture and buffer parameters
			var textureIndex = 0;
			var bufferIndex = 0;
			var samplerIndex = 0;
			
			// First pass: add textures
			for( v in s.vars ) {
				if( v.kind == Param || v.kind == Global ) {
					if( isTextureType(v.type) ) {
						add(", ");
						// Textures use [[texture(n)]] attribute
						// Metal textures are bound individually, not as arrays in shader arguments
						// The array access in HXSL (tex[0]) maps to individual texture bindings
						var old = v.type;
						var baseType = switch( v.type ) {
						case TArray(t, _): t;
						default: v.type;
						};
						v.type = baseType;
						
						// Special handling for samplers to detect depth textures by name
						switch(v.type) {
						case TSampler(dim, arr):
							// Debug: Save variable name for inspection
							try {
								var f = sys.io.File.append("/tmp/metal_texture_vars.txt", false);
								f.writeString('Texture var: name="${v.name}" type=TSampler dim=$dim\n');
								f.close();
							} catch(e:Dynamic) {}
							add(getSamplerType(dim, arr, v.name));
						case TSamplerDepth(dim, arr):
							// Depth samplers explicitly use depth texture types
							try {
								var f = sys.io.File.append("/tmp/metal_texture_vars.txt", false);
								f.writeString('Depth texture var: name="${v.name}" type=TSamplerDepth dim=$dim\n');
								f.close();
							} catch(e:Dynamic) {}
							add(getDepthSamplerType(dim, arr));
						default:
							addType(v.type);
						}
						
						v.type = old;
						add(" ");
						add(varName(v));
						add(" [[texture(" + textureIndex + ")]]");
						textureIndex++;
					}
				}
			}
			
			// Second pass: add samplers (one per texture)
			if( textureIndex > 0 ) {
				add(", array<sampler, " + textureIndex + "> fragmentSamplers [[sampler(0)]]");
			}
			
			// Third pass: add non-texture buffers
			for( v in s.vars ) {
				if( v.kind == Param || v.kind == Global ) {
					if( !isTextureType(v.type) ) {
						add(", ");
						// Non-texture parameters use [[buffer(n)]]
						add("constant ");
						var old = v.type;
						switch( v.type ) {
						case TArray(t, _):
							v.type = t;
							addType(v.type);
							v.type = old;
							add(" *");
						default:
							addType(v.type);
							add(" *");
						}
						add(varName(v));
						add(" [[buffer(" + bufferIndex + ")]]");
						bufferIndex++;
					}
				}
			}
			add(") {\n");

			// Add fragCoord support - use input.position which is gl_FragCoord equivalent in Metal
			add("\tfloat4 _fragCoord = input.position;\n");

			// Declare local variables for varyings from vertex shader
			// Var kind in fragment = interpolated outputs from vertex shader
			for( v in s.vars ) {
				if( v.kind == Var ) {
					add("\t");
					addType(v.type);
					add(" ");
					add(varName(v));
					add(" = input.");
					add(varName(v));
					add(";\n");
				}
			}

			// Declare local variables for inputs from vertex shader (textures, etc)
			for( v in s.vars ) {
				if( v.kind == Input ) {
					add("\t");
					addType(v.type);
					add(" ");
					add(varName(v));
					add(" = input.");
					add(varName(v));
					add(";\n");
				}
			}

			// Declare local variable for output color
			for( v in s.vars ) {
				if( v.kind == Output ) {
					add("\t");
					addType(v.type);
					add(" ");
					add(varName(v));
					add(";\n");
				}
			}

			// Declare local variables (Local kind only - intermediate calculations)
			for( v in s.vars ) {
				if( v.kind == Local ) {
					add("\t");
					addType(v.type);
					add(" ");
					add(varName(v));
					add(";\n");
				}
			}

			// Process shader expression
			addExpr(f.expr, "\t");

			// Return the output color
			for( v in s.vars ) {
				if( v.kind == Output ) {
					add("\treturn ");
					add(varName(v));
					add(";\n");
					break;
				}
			}

			add("}\n");

		} else if( isCompute ) {
			add("kernel void compute_main(");
			add("uint3 thread_position [[thread_position_in_grid]]) {\n");

			addExpr(f.expr, "\t");

			add("}\n");
		}

		// Build final source with declarations inserted after includes
		var mainSource = buf.toString();
		var headerEnd = mainSource.indexOf("using namespace metal;\n\n");
		if( headerEnd >= 0 ) {
			headerEnd += "using namespace metal;\n\n".length;
			var finalSource = mainSource.substring(0, headerEnd);

			// Add declarations (like _mat3x4 typedef)
			if( decls.length > 0 ) {
				finalSource += decls.join("\n") + "\n\n";
			}

			finalSource += mainSource.substring(headerEnd);
			return finalSource;
		} else {
			// Fallback if header pattern not found
			return mainSource;
		}
	}

	public static function compile( s : ShaderData ) {
		return new MetalOut().run(s);
	}
}
