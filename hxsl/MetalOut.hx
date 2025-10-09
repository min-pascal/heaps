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

	function getSamplerType(dim:TexDimension, arr:Bool) {
		return switch(dim) {
		case T1D: "texture1d<float>";
		case T2D: "texture2d<float>";
		case T3D: "texture3d<float>";
		case TCube: "texturecube<float>";
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
		case TRWTexture(_,_,_): true;
		case TArray(TSampler(_,_), _): true;
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
			add(tabs + "discard;\n");
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
				// Metal texture sampling: texture.sample(sampler, coords)
				// HXSL: texture(tex, coords) or textureLod(tex, coords, lod)
				if( args.length >= 2 ) {
					// args[0] should be the texture array access like fragmentTextures[0]
					// We need to extract just the texture itself
					writeExpr(args[0]);  // This outputs the texture
					add(".sample(");
					// TODO: Add sampler here - for now use default sampler
					add("sampler(mag_filter::linear, min_filter::linear), ");
					writeExpr(args[1]);  // UV coordinates
					add(")");
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
			case CInt(i): add(i);
			case CFloat(f): add(f);
			case CBool(b): add(b ? "true" : "false");
			case CNull: add("0");
			case CString(s): add('"' + s + '"');
			}
		case TBinop(op, e1, e2):
			add("(");
			writeExpr(e1);
			add(" ");
			add(Printer.opStr(op));
			add(" ");
			writeExpr(e2);
			add(")");
		case TUnop(op, e1):
			add(op);
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
			writeExpr(e);
			if( regs.length > 0 ) {
				add(".");
				for( r in regs )
					add(r.getName().toLowerCase());
			}
		case TGlobal(g):
			add(GLOBALS[g.getIndex()]);
		case TParenthesis(e):
			add("(");
			writeExpr(e);
			add(")");
		case TBlock(el):
			add("{\n");
			for( e in el ) {
				add("\t");
				addExpr(e, "\t");
			}
			add("}");
		default:
			add("/* unsupported expr: " + e.e.getName() + " */");
		}
	}

	public function run( s : ShaderData ) {
		locals = new Map();
		decls = [];
		buf = new StringBuf();
		exprValues = [];
		
		// Reset shader type flags
		isVertex = false;
		isFragment = false;
		isCompute = false;

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
			
			// Add texture and buffer parameters
			var textureIndex = 0;
			var bufferIndex = 0;
			for( v in s.vars ) {
				if( v.kind == Param || v.kind == Global ) {
					add(", ");
					if( isTextureType(v.type) ) {
						// Textures use [[texture(n)]] attribute
						// Metal textures are bound individually, not as arrays in shader arguments
						// The array access in HXSL (tex[0]) maps to individual texture bindings
						var old = v.type;
						var baseType = switch( v.type ) {
						case TArray(t, _): t;
						default: v.type;
						};
						v.type = baseType;
						addType(v.type);
						v.type = old;
						add(" ");
						add(varName(v));
						add(" [[texture(" + textureIndex + ")]]");
						textureIndex++;
					} else {
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

		var source = buf.toString();
		return source;
	}

	public static function compile( s : ShaderData ) {
		return new MetalOut().run(s);
	}
}
