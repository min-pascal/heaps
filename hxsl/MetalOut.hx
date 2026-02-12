package hxsl;
using hxsl.Ast;

class MetalOut {

  static var KWD_LIST = "auto break case char const continue default do double else enum extern float for goto if inline int long register return short signed sizeof static struct switch typedef union unsigned void volatile while " + "atomic_bool atomic_uint bool buffer bvec2 bvec3 bvec4 centered complex dmat2 dmat2x2 dmat2x3 dmat2x4 dmat3 dmat3x2 dmat3x3 dmat3x4 dmat4 dmat4x2 dmat4x3 dmat4x4 " + "dvec2 dvec3 dvec4 false highp iimage1D iimage1DArray iimage2D iimage2DArray iimage2DMS iimage2DMSArray iimage2DRect iimage3D iimageBuffer iimageCube iimageCubeArray image1D " + "image1DArray image2D image2DArray image2DMS image2DMSArray image2DRect image3D imageBuffer imageCube imageCubeArray in inout invariant isampler1D isampler1DArray isampler2D " + "isampler2DArray isampler2DMS isampler2DMSArray isampler2DRect isampler3D isamplerBuffer isamplerCube isamplerCubeArray ivec2 ivec3 ivec4 lowp mat2 mat2x2 mat2x3 mat2x4 " + "mat3 mat3x2 mat3x3 mat3x4 mat4 mat4x2 mat4x3 mat4x4 mediump out patch precision readonly restrict sampler1D sampler1DArray sampler1DArrayShadow sampler1DShadow sampler2D " + "sampler2DArray sampler2DArrayShadow sampler2DMS sampler2DMSArray sampler2DRect sampler2DRectShadow sampler2DShadow sampler3D samplerBuffer samplerCube samplerCubeArray " + "samplerCubeArrayShadow samplerCubeShadow smooth subroutine true uimage1D uimage1DArray uimage2D uimage2DArray uimage2DMS uimage2DMSArray uimage2DRect uimage3D uimageBuffer " + "uimageCube uimageCubeArray uint usampler1D usampler1DArray usampler2D usampler2DArray usampler2DMS usampler2DMSArray usampler2DRect usampler3D usamplerBuffer usamplerCube " + "usamplerCubeArray uvec2 uvec3 uvec4 vec2 vec3 vec4 writeonly " + // Metal specific keywords
  "access array_ref as asm_fragment assert_inside_fragment_shader attributes available_mac available_ios available_mac_catalyst " + "bias break case cast_intrinsic class clamp color column_major comparative compiler_options const constexpr " + "container continue coordinated copy_from core decorator depthwise device dictionary direct discard dither_mode " + "do dynamic dynamicmemberreference else enum except explicit export extern " + "fallthrough false final fixed_sample_locations for fragment fragment_samples fragment_shader fragment_width function " + "global group group_relative groupshared half half2 half3 half4 header_map if implements import indirect_command_buffer " + "indirect_fragment_argument indirect_vertex_argument inherit inline kernel link_as_metal logic lossy " + "main_structure malloc max min multipass namespace no_optimization noduplicate noerror noinline noinline_recursive nonstable normalize normalized noreturn " + "objc object object_oriented packed patch patience patch_control_point per_primitive per_vertex per_vertex_aux percent placement_id plane post_depth_coverage precision preckip" + "prefix presents primitive program projection property protocol public published ray_data ray_indices ray_query recursively " + "register resource_group restrict result return root_constant round sample sample_mask sample_ref sampler_state sampler_compare_state saturate shader_data shader_interface " + "shadow shader simdgroup simdgroup_barrier simdgroup_matrix simdgroup_matrix_storage size specialise stagegroup standardinclude static static_assert storage_mode strict " + "struct submit_fragment super supress_tracing swift template terminal tess_control tess_eval tessellation tessellation_factor " + "texture texture_buffer thread threadgroup threadgroup_imageblock triangle_clockwise triangle_counterclockwise true typedef typeid typename typename_pack " + "uniform unsafe unsafe_reconstruct unsuppress_tracing using vec1 vertex vertex_data vertex_id vertex_shader visible_function " + "void volatile while [[]] writeable writeonly zeroing";

  static var KWDS = [for (k in ~/[ \t\r\n]+/g.split(KWD_LIST)) k => true];
  static var GLOBALS = {
    var gl = [];
    inline function set(g:hxsl.Ast.TGlobal, str:String) {
      gl[g.getIndex()] = str;
    }
    for (g in hxsl.Ast.TGlobal.createAll()) {
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
    set(ComputeVar_LocalInvocation, "int3(thread_position_in_threadgroup)");
    set(ComputeVar_GlobalInvocation, "int3(thread_position_in_grid)");
    set(ComputeVar_LocalInvocationIndex, "thread_index_in_threadgroup");
    set(ComputeVar_WorkGroup, "int3(threadgroup_position_in_grid)");
    set(Mod, "fmod"); // Metal uses fmod() for float modulo
    set(GroupMemoryBarrier, "threadgroup_barrier(mem_flags::mem_threadgroup)");
    set(AtomicAdd, "atomic_fetch_add_explicit");

    for (g in gl)
      KWDS.set(g, true);
    gl;
  };
  static var MAT34 = "typedef struct { float4 a; float4 b; float4 c; } _mat3x4;";

  var buf:StringBuf;
  var exprIds = 0;
  var exprValues:Array<String>;
  var locals:Map<Int, TVar>;
  var decls:Array<String>;
  var isVertex:Bool;
  var isCompute:Bool;
  var isFragment:Bool;
  var usesInstanceId:Bool;
  var usesVertexId:Bool;
  var allNames:Map<String, Int>;
  var computeLayout = [1, 1, 1];

  // MRT (Multiple Render Targets) support
  var outputCount:Int;
  var outputVars:Array<TVar>;

  // Bindless texturing support
  var bindlessSamplers:Map<Int, Int>;
  var bindlessSamplersCount:Int;

  public var varNames:Map<Int, String>;

  public function new() {
    varNames = new Map();
    allNames = new Map();
  }

  inline function add(v:Dynamic) {
    buf.add(v);
  }

  inline function ident(v:TVar) {
    add(varName(v));
  }

  // Scan shader expression tree for InstanceID usage
  function scanForInstanceId(e:TExpr):Bool {
    if (e == null) return false;
    switch (e.e) {
      case TGlobal(g):
        if (g == InstanceID) return true;
      case TArray(e1, e2):
        if (scanForInstanceId(e1)) return true;
        if (scanForInstanceId(e2)) return true;
      case TBinop(_, e1, e2):
        if (scanForInstanceId(e1)) return true;
        if (scanForInstanceId(e2)) return true;
      case TUnop(_, e1):
        if (scanForInstanceId(e1)) return true;
      case TVar(_):
      // Variable reference
      case TConst(_):
      // Constant
      case TIf(econd, eif, eelse):
        if (scanForInstanceId(econd)) return true;
        if (scanForInstanceId(eif)) return true;
        if (eelse != null && scanForInstanceId(eelse)) return true;
      case TCall(ef, args):
        if (scanForInstanceId(ef)) return true;
        for (a in args)
          if (scanForInstanceId(a)) return true;
      case TBlock(el):
        for (ex in el)
          if (scanForInstanceId(ex)) return true;
      case TSwiz(e1, _):
        if (scanForInstanceId(e1)) return true;
      case TParenthesis(e1):
        if (scanForInstanceId(e1)) return true;
      case TMeta(_, _, e1):
        if (scanForInstanceId(e1)) return true;
      case TFor(_, it, loop):
        if (scanForInstanceId(it)) return true;
        if (scanForInstanceId(loop)) return true;
      case TWhile(cond, loop, _):
        if (scanForInstanceId(cond)) return true;
        if (scanForInstanceId(loop)) return true;
      case TReturn(e1):
        if (e1 != null && scanForInstanceId(e1)) return true;
      case TDiscard:
      // No sub-expressions
      default:
      // Other cases
    }
    return false;
  }

  // Scan shader expression tree for VertexID usage
  function scanForVertexId(e:TExpr):Bool {
    if (e == null) return false;
    switch (e.e) {
      case TGlobal(g):
        if (g == VertexID) return true;
      case TArray(e1, e2):
        if (scanForVertexId(e1)) return true;
        if (scanForVertexId(e2)) return true;
      case TBinop(_, e1, e2):
        if (scanForVertexId(e1)) return true;
        if (scanForVertexId(e2)) return true;
      case TUnop(_, e1):
        if (scanForVertexId(e1)) return true;
      case TVar(_):
      case TConst(_):
      case TIf(econd, eif, eelse):
        if (scanForVertexId(econd)) return true;
        if (scanForVertexId(eif)) return true;
        if (eelse != null && scanForVertexId(eelse)) return true;
      case TCall(ef, args):
        if (scanForVertexId(ef)) return true;
        for (a in args)
          if (scanForVertexId(a)) return true;
      case TBlock(el):
        for (ex in el)
          if (scanForVertexId(ex)) return true;
      case TSwiz(e1, _):
        if (scanForVertexId(e1)) return true;
      case TParenthesis(e1):
        if (scanForVertexId(e1)) return true;
      case TMeta(_, _, e1):
        if (scanForVertexId(e1)) return true;
      case TFor(_, it, loop):
        if (scanForVertexId(it)) return true;
        if (scanForVertexId(loop)) return true;
      case TWhile(cond, loop, _):
        if (scanForVertexId(cond)) return true;
        if (scanForVertexId(loop)) return true;
      case TReturn(e1):
        if (e1 != null && scanForVertexId(e1)) return true;
      case TDiscard:
      default:
    }
    return false;
  }

  function varName(v:TVar) {
    var n = varNames.get(v.id);
    if (n != null)
      return n;
    n = v.name;
    if (KWDS.exists(n))
      n = "_" + n;
    if (allNames.exists(n)) {
      var k = 2;
      n += "_";
      while (allNames.exists(n + k))
        k++;
      n += k;
    }
    varNames.set(v.id, n);
    allNames.set(n, v.id);
    return n;
  }

  function decl(s:String) {
    if (decls.indexOf(s) >= 0)
      return;
    decls.push(s);
  }

  function addType(t:Type) {
    switch (t) {
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
        switch (k) {
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
        add(getDepthSamplerType(dim, arr));
      case TRWTexture(dim, arr, chans):
        add(getTextureType(dim, arr, chans, true));
      case TStruct(vl):
        add("struct { ");
        for (v in vl) {
          addVar(v);
          add("; ");
        }
        add("}");
      case TFun(_):
        add("function");
      case TArray(t, size):
        addType(t);
        add("[");
        switch (size) {
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
      case TTextureHandle:
        add("uint2");
    }
  }

  function getSamplerType(dim:TexDimension, arr:Bool, ?varName:String) {
    // Regular samplers use texture2d<float>, texture arrays use texture2d_array<float>
    // Depth samplers (TSamplerDepth) use getDepthSamplerType() instead
    return switch (dim) {
      case T1D: arr ? "texture1d_array<float>" : "texture1d<float>";
      case T2D: arr ? "texture2d_array<float>" : "texture2d<float>";
      case T3D: "texture3d<float>"; // No 3D array type in Metal
      case TCube: arr ? "texturecube_array<float>" : "texturecube<float>";
    }
  }

  function getDepthSamplerType(dim:TexDimension, arr:Bool) {
    // Depth samplers always use Metal depth texture types for hardware comparison
    return switch (dim) {
      case T1D: "depth1d<float>"; // Rarely used
      case T2D: "depth2d<float>"; // Standard for shadow maps
      case T3D: "depth3d<float>"; // For volumetric shadows
      case TCube: "depthcube<float>"; // For omnidirectional shadows
    }
  }

  function getTextureType(dim:TexDimension, arr:Bool, chans:Int, rw:Bool) {
    var prefix = rw ? "texture" : "texture";
    var type = switch (chans) {
      case 1: "float";
      case 2: "float2";
      case 3: "float3";
      case 4: "float4";
      default: "float";
    }
    return switch (dim) {
      case T1D: prefix + "1d<" + type + ", access::read_write>";
      case T2D: prefix + "2d<" + type + ", access::read_write>";
      case T3D: prefix + "3d<" + type + ", access::read_write>";
      case TCube: prefix + "cube<" + type + ", access::read_write>";
    }
  }

  function addVar(v:TVar) {
    switch (v.type) {
      case TArray(t, size):
        var old = v.type;
        v.type = t;
        addVar(v);
        v.type = old;
        add("[");
        switch (size) {
          case SVar(v): ident(v);
          case SConst(n): add(n);
        }
        add("]");
      case TBuffer(t, size, kind):
        switch (kind) {
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

  function initVars(s:ShaderData) {
    add("// Input Structure\n");
    add("struct InputData {\n");
    var attrIndex = 0;
    for (v in s.vars)
      if (v.kind == Input) {
        add("\t");
        addVar(v);
        add(" [[attribute(" + attrIndex + ")]]");
        attrIndex++;
        add(";\n");
      }
    add("};\n\n");

    add("// Output Structure\n");
    if (isVertex) {
      add("struct VertexOut {\n");
      add("\tfloat4 position [[position]];\n");
      for (v in s.vars)
        if (v.kind == Output && !varNames.exists(v.id)) {
          add("\t");
          addVar(v);
          add(";\n");
        }
      add("};\n\n");
    }
    else if (isFragment) {
      add("struct FragmentOut {\n");
      add("\tfloat4 color [[color(0)]];\n");
      add("};\n\n");
    }

    add("// Uniform Buffers\n");
    for (v in s.vars)
      if (v.kind == Global) {
        add("constant ");
        addVar(v);
        add(";\n");
      }
    add("\n");

    // Textures and samplers
    add("// Textures and Samplers\n");
    for (v in s.vars)
      if (v.kind == Param && isTextureType(v.type)) {
        addVar(v);
        add(";\n");
      }
    add("\n");
  }

  function isTextureType(t:Type):Bool {
    return switch (t) {
      case TSampler(_, _): true;
      case TSamplerDepth(_, _): true; // Depth samplers are textures too
      case TRWTexture(_, _, _): true;
      case TArray(TSampler(_, _), _): true;
      case TArray(TSamplerDepth(_, _), _): true; // Array of depth samplers
      case TArray(TRWTexture(_, _, _), _): true;
      default: false;
    };
  }

  function addExpr(e:TExpr, tabs:String) {
    switch (e.e) {
      case TBlock(el):
        add("{\n");
        for (e in el)
          addExpr(e, tabs + "\t");
        add(tabs + "}");
      case TVarDecl(v, init):
        add(tabs);
        addVar(v);
        if (init != null) {
          add(" = ");
          writeExpr(init);
        }
        add(";\n");
      case TIf(econd, eif, eelse):
        add(tabs + "if( ");
        writeExpr(econd);
        add(" ) ");
        addExpr(eif, tabs);
        if (eelse != null) {
          add(" else ");
          addExpr(eelse, tabs);
        }
        add("\n");
      case TReturn(e):
        add(tabs + "return");
        if (e != null) {
          add(" ");
          writeExpr(e);
        }
        add(";\n");
      case TDiscard:
        add(tabs + "discard_fragment();\n");
      case TMeta(_, _, e):
        // Metadata expressions - just output the inner expression
        addExpr(e, tabs);
      case TFor(v, it, loop):
        // For loop support
        locals.set(v.id, v);
        switch (it.e) {
          case TBinop(OpInterval, e1, e2):
            add(tabs + "for(int " + varName(v) + " = ");
            writeExpr(e1);
            add("; " + varName(v) + " < ");
            writeExpr(e2);
            add("; " + varName(v) + "++) ");
            addExpr(loop, tabs);
            add("\n");
          default:
            throw "Unsupported for loop iterator";
        }
      case TWhile(econd, loop, normalWhile):
        if (normalWhile) {
          add(tabs + "while( ");
          writeExpr(econd);
          add(" ) ");
          addExpr(loop, tabs);
          add("\n");
        }
        else {
          // do-while loop
          add(tabs + "do ");
          addExpr(loop, tabs);
          add(" while( ");
          writeExpr(econd);
          add(" );\n");
        }
      default:
        add(tabs);
        writeExpr(e);
        add(";\n");
    }
  }

  function writeExpr(e:TExpr) {
    switch (e.e) {
      case TCall(func, args):
        // Special handling for texture sampling in Metal
        switch (func.e) {
          case TGlobal(SetLayout):
            // SetLayout sets compute shader thread group size - skip in expression output
            // It's extracted during collectGlobals and used in kernel signature
            // Do nothing here
          case TGlobal(ImageStore):
            // ImageStore for compute shaders: tex.write(color, coords)
            if (args.length == 3) {
              writeExpr(args[0]); // Texture
              add(".write(");
              writeExpr(args[2]); // Color value (swap order - write takes color first)
              add(", uint2(");
              writeExpr(args[1]); // UV coordinates as uint2
              add("))");
            }
          case TGlobal(ResolveSampler):
            // Bindless texturing: resolve texture and sampler from bindless arrays
            // args[0] = handle (uint2), args[1] = texture variable
            if (args.length >= 2) {
              switch (args[1].e) {
                case TVar(v):
                  // Reassign texture from bindless array
                  ident(v);
                  add(" = bindlessTextures[");
                  writeExpr(args[0]);
                  add(".x];\n");
                  // Create local sampler variable
                  add("\t");
                  bindlessSamplers.set(v.id, bindlessSamplersCount++);
                default:
                  // Fallback
                  writeExpr(args[1]);
                  add(" = bindlessTextures[");
                  writeExpr(args[0]);
                  add(".x]");
              }
            }
          case TGlobal(Texture | TextureLod):
            // Metal texture sampling: texture.sample(sampler, coords) or depth2d.read(coords)
            // HXSL: texture(tex, coords) or textureLod(tex, coords, lod)
            if (args.length >= 2) {
              // Check if this is a depth texture by examining the texture variable
              var isDepthTexture = false;
              var textureVarName = "";

              switch (args[0].e) {
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
                writeExpr(args[0]); // Texture reference
                add(".read(uint2(");
                writeExpr(args[1]); // UV coordinates
                add(" * float2(");
                // Get texture dimensions - for now, hardcode common shadow map size
                // TODO: Pass texture size as uniform or query dynamically
                add("1024.0, 1024.0"); // Assuming 1024x1024 shadow map
                add(")), 0), 0.0, 0.0, 1.0)"); // Wrap in float4, level 0
              }
              else {
                // Check if this is a texture array (coords are float3 for 2D array)
                var isTextureArray = false;
                switch (args[0].e) {
                  case TArray({ e: TVar(v) }, _):
                    switch (v.type) {
                      case TSampler(_, arr):
                        isTextureArray = arr;
                      case TArray(TSampler(_, arr), _):
                        isTextureArray = arr;
                      default:
                    }
                  case TVar(v):
                    switch (v.type) {
                      case TSampler(_, arr):
                        isTextureArray = arr;
                      default:
                    }
                  default:
                }

                // Regular texture sampling
                writeExpr(args[0]); // This outputs the texture
                add(".sample(");

                add("fragmentSamplers[");
                // Check if this is a bindless-resolved texture
                var usedBindlessSampler = false;
                switch (args[0].e) {
                  case TVar(v):
                    var bsIdx = bindlessSamplers.get(v.id);
                    if (bsIdx != null) {
                      add("0");
                      usedBindlessSampler = true;
                    }
                  default:
                }
                if (!usedBindlessSampler) {
                  switch (args[0].e) {
                    case TArray(_, { e: TConst(CInt(idx)) }):
                      add(Std.string(idx));
                    default:
                      add("0");
                  }
                }
                add("]");
                add(", ");

                if (isTextureArray) {
                  // For texture2d_array: sample(sampler, float2 coords, uint array_index)
                  // args[1] is float3(uv.x, uv.y, slice)
                  add("(");
                  writeExpr(args[1]);
                  add(").xy, uint((");
                  writeExpr(args[1]);
                  add(").z)");
                }
                else {
                  writeExpr(args[1]); // UV coordinates
                }
                add(")");
              }
            }
            else {
              // Fallback to standard call
              writeExpr(func);
              add("(");
              var first = true;
              for (a in args) {
                if (!first) add(", ");
                first = false;
                writeExpr(a);
              }
              add(")");
            }
          case TGlobal(Mat3):
            // Special handling for float3x3 constructor
            // Check if we're constructing from a float4x4 - need to extract 3x3 submatrix
            if (args.length == 1) {
              var argType = args[0].t;
              var needsExtraction = switch (argType) {
                case TMat4: true;
                case TMat3x4: true; // Also need extraction from 3x4 matrix
                default: false;
              };

              var isMat3x4 = switch (argType) {
                case TMat3x4: true;
                default: false;
              };

              if (needsExtraction) {
                if (isMat3x4) {
                  // Extract 3x3 from _mat3x4 struct
                  // _mat3x4 has fields a, b, c which are float4 rows
                  // We need a.xyz, b.xyz, c.xyz as columns of the 3x3
                  add("float3x3(");
                  add("float3(");
                  writeExpr(args[0]);
                  add(".a.xyz), float3(");
                  writeExpr(args[0]);
                  add(".b.xyz), float3(");
                  writeExpr(args[0]);
                  add(".c.xyz))");
                }
                else {
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
                }
              }
              else {
                // Standard constructor
                add("float3x3(");
                var first = true;
                for (a in args) {
                  if (!first) add(", ");
                  first = false;
                  writeExpr(a);
                }
                add(")");
              }
            }
            else {
              // Multiple arguments - standard constructor
              add("float3x3(");
              var first = true;
              for (a in args) {
                if (!first) add(", ");
                first = false;
                writeExpr(a);
              }
              add(")");
            }
          case TGlobal(Mat3x4):
            // Special handling for _mat3x4 constructor (struct, not native matrix)
            decl(MAT34);
            if (args.length == 3) {
              // Constructor from 3 float4 vectors (rows)
              add("(_mat3x4){ ");
              writeExpr(args[0]);
              add(", ");
              writeExpr(args[1]);
              add(", ");
              writeExpr(args[2]);
              add(" }");
            }
            else if (args.length == 1) {
              // Constructor from a single float4x4 - extract first 3 rows
              switch (args[0].t) {
                case TMat4:
                  // float4x4 to _mat3x4: extract first 3 rows
                  add("(_mat3x4){ ");
                  writeExpr(args[0]);
                  add("[0], ");
                  writeExpr(args[0]);
                  add("[1], ");
                  writeExpr(args[0]);
                  add("[2] }");
                default:
                  // Fallback for other single-argument cases
                  add("(_mat3x4){ ");
                  writeExpr(args[0]);
                  add(" }");
              }
            }
            else {
              // Fallback to struct literal syntax
              add("(_mat3x4){ ");
              var first = true;
              for (a in args) {
                if (!first) add(", ");
                first = false;
                writeExpr(a);
              }
              add(" }");
            }
          case TGlobal(FloatBitsToUint):
            // Metal: as_type<uint>(float) or as_type<uint2>(float2) etc.
            if (args.length == 1) {
              var targetType = switch (args[0].t) {
                case TTextureHandle: "uint2";
                case TVec(2, _): "uint2";
                case TVec(3, _): "uint3";
                case TVec(4, _): "uint4";
                default: "uint";
              };
              add("as_type<" + targetType + ">(");
              writeExpr(args[0]);
              add(")");
            }
          case TGlobal(FloatBitsToInt):
            // Metal: as_type<int>(float) or as_type<int2>(float2) etc.
            if (args.length == 1) {
              var targetType = switch (args[0].t) {
                case TVec(2, _): "int2";
                case TVec(3, _): "int3";
                case TVec(4, _): "int4";
                default: "int";
              };
              add("as_type<" + targetType + ">(");
              writeExpr(args[0]);
              add(")");
            }
          default:
            // Standard function call
            writeExpr(func);
            add("(");
            var first = true;
            for (a in args) {
              if (!first) add(", ");
              first = false;
              writeExpr(a);
            }
            add(")");
        }
      case TField(expr, field):
        writeExpr(expr);
        if (field != null && field.length > 0) {
          add(".");
          add(field);
        }
      case TVar(v):
        ident(v);
      case TConst(c):
        switch (c) {
          case CInt(i):
            // In Metal, integer literals can be ambiguous in overloaded function calls
            // Check if this appears in a float context by looking at expression type
            var needsFloatSuffix = switch (e.t) {
              case TFloat, TVec(_, VFloat): true;
              default: false;
            };
            if (needsFloatSuffix && (i == 0 || i == 1)) {
              add(i + ".0");
            }
            else {
              add(i);
            }
          case CFloat(f):
            var str = "" + f;
            add(str);
            // Ensure float literals always have decimal point for Metal
            if (str.indexOf(".") == -1 && str.indexOf("e") == -1)
              add(".");
          case CBool(b): add(b ? "true" : "false");
          case CNull: add("0");
          case CString(s): add('"' + s + '"');
        }
      case TBinop(op, e1, e2):
        // Handle special operators in Metal
        var handled = false;
        switch (op) {
          // Handle unsigned right shift: Metal doesn't have >>> operator
          case OpUShr:
            decl("int _ushr(int i, int j) { return int(uint(i) >> uint(j)); }");
            add("_ushr(");
            writeExpr(e1);
            add(", ");
            writeExpr(e2);
            add(")");
            handled = true;
          // Handle float modulo: Metal uses fmod() function instead of % operator
          case OpMod if (e.t != TInt):
            add("fmod(");
            writeExpr(e1);
            add(", ");
            writeExpr(e2);
            add(")");
            handled = true;
          case OpAssignOp(OpMod) if (e.t != TInt):
            // a %= b  ->  a = fmod(a, b)
            writeExpr(e1);
            add(" = fmod(");
            writeExpr(e1);
            add(", ");
            writeExpr(e2);
            add(")");
            handled = true;
          // Handle OpAssignOp(OpMult) for vec *= mat cases
          // Convert vec *= mat to vec = vec * mat so proper matrix handling applies
          case OpAssignOp(innerOp) if (innerOp == OpMult):
            var isMat = switch (e2.t) { case TMat3x4 | TMat3 | TMat4: true; default: false; };
            var isVec = switch (e1.t) { case TVec(_, VFloat): true; default: false; };
            if (isVec && isMat) {
              // Rewrite: e1 *= e2  ->  e1 = e1 * e2
              writeExpr(e1);
              add(" = ");
              // Create a synthetic multiplication expression and write it
              writeExpr({ e: TBinop(OpMult, e1, e2), t: e.t, p: e.p });
              handled = true;
            }
          case OpMult:
            // Special handling for vector * matrix operations in Metal
            // where HXSL semantics differ from Metal's
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

            var isVec = switch (e1.t) { case TVec(_, VFloat): true; default: false; };
            var isMat4 = switch (e2.t) { case TMat4: true; default: false; };

            if (isVec && isMat4) {
              // vec * mat4: Keep the original HXSL operation
              // Metal should support vec * mat just like GLSL does
              add("(");
              writeExpr(e1);
              add(" * ");
              writeExpr(e2);
              add(")");
              handled = true;
            }
            else {
              // Check for vec3 * mat3x4 case
              // In HXSL: vec3 * mat3x4 = vec3
              // _mat3x4 is a struct { float4 a; float4 b; float4 c; }
              // vec3 * _mat3x4 = float3(dot(vec, a.xyz), dot(vec, b.xyz), dot(vec, c.xyz)) + vec.x * float3(a.w, b.w, c.w)
              var isVec3 = switch (e1.t) { case TVec(3, VFloat): true; default: false; };
              var isMat3x4 = switch (e2.t) { case TMat3x4: true; default: false; };

              if (isVec3 && isMat3x4) {
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
                handled = true;
              }
            }
          // Handle vector comparison operators - Metal returns bool vectors, need to cast to float
          case OpGt | OpLt | OpGte | OpLte | OpEq | OpNotEq:
            var vecSize = switch (e.t) {
              case TVec(n, VFloat): n;
              default: 0;
            };
            if (vecSize > 0) {
              // Vector comparison assigned to float vector - need explicit cast
              add("float" + vecSize + "(");
              writeExpr(e1);
              add(" ");
              add(Printer.opStr(op));
              add(" ");
              writeExpr(e2);
              add(")");
              handled = true;
            }
          default:
        }
        if (!handled) {
          add("(");
          writeExpr(e1);
          add(" ");
          add(Printer.opStr(op));
          add(" ");
          writeExpr(e2);
          add(")");
        }
      case TUnop(op, e1):
        var opStr = switch (op) {
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
        for (e in el) {
          if (!first) add(", ");
          first = false;
          writeExpr(e);
        }
        add(" }");
      case TArray(e, index):
        // Check if this is a texture array access - Metal doesn't support texture arrays in shader args
        // So we treat tex[n] as tex_n (the texture is bound to texture slot n)
        var isTextureArrayAccess = switch (e.e) {
          case TVar(v): isTextureType(v.type);
          default: false;
        };

        if (isTextureArrayAccess) {
          switch (index.e) {
            case TConst(CInt(idx)):
              // Texture access with constant index - output tex_n for n > 0, or just tex for n == 0
              writeExpr(e);
              if (idx > 0) {
                add("_" + idx);
              }
            default:
              // Dynamic index - not supported, fall back to regular syntax (will error)
              writeExpr(e);
              add("[");
              writeExpr(index);
              add("]");
          }
        }
        else {
          // Normal array access
          writeExpr(e);
          add("[");
          writeExpr(index);
          add("]");
        }
      case TSwiz(e, regs):
        // Check if we're swizzling a scalar type
        // This handles nested swizzles like depth.x.x where the inner .x returns TFloat
        var isScalar = switch (e.t) {
          case TFloat: true;
          case TVec(1, VFloat): true; // Single-component vector is effectively scalar
          default: false;
        }

        if (isScalar) {
          if (regs.length > 1) {
            // Scalar broadcast to vector - Metal doesn't support swizzling scalars
            // e.g. depth.xxxx becomes float4(depth)
            var componentType = switch (regs.length) {
              case 2: "float2";
              case 3: "float3";
              case 4: "float4";
              default: "float";
            }
            add(componentType + "(");
            writeExpr(e);
            add(")");
          }
          else {
            // Single component swizzle on scalar (e.g. depth.x) - just output the scalar
            // Metal doesn't allow .x on a float
            writeExpr(e);
          }
        }
        else {
          writeExpr(e);
          if (regs.length > 0) {
            add(".");
            for (r in regs)
              add(r.getName().toLowerCase());
          }
        }
      case TGlobal(g):
        // Handle special functions that need declarations
        switch (g) {
          case Pack:
            decl("static inline float4 pack(float v) {\n\tfloat4 c = fract(v * float4(1.0, 255.0, 65025.0, 16581375.0));\n\treturn c - c.yzww * float4(1.0/255.0, 1.0/255.0, 1.0/255.0, 0.0);\n}");
          case Unpack:
            decl("static inline float unpack(float4 c) {\n\treturn dot(c, float4(1.0, 1.0/255.0, 1.0/65025.0, 1.0/16581375.0));\n}");
          case PackNormal:
            decl("static inline float4 packNormal(float3 v) {\n\treturn float4((v + float3(1.0)) * float3(0.5), 1.0);\n}");
          case UnpackNormal:
            decl("static inline float3 unpackNormal(float4 v) {\n\treturn normalize((v.xyz - float3(0.5)) * float3(2.0));\n}");
          default:
        }
        add(GLOBALS[g.getIndex()]);
      case TParenthesis(e):
        add("(");
        writeExpr(e);
        add(")");
      case TBlock(el):
        // Block expressions in Metal need statement-expression syntax: ({ stmts; value; })
        add("({\n");
        for (i in 0...el.length) {
          add("\t");
          if (i == el.length - 1) {
            // Last expression is the return value
            writeExpr(el[i]);
            add(";\n");
          }
          else {
            addExpr(el[i], "\t");
          }
        }
        add("})");
      case TMeta(_, _, e):
        // Metadata expressions - just output the inner expression like GLSL does
        writeExpr(e);
      case TIf(econd, eif, eelse):
        // Ternary expression: (cond) ? eif : eelse
        add("((");
        writeExpr(econd);
        add(") ? ");
        writeExpr(eif);
        add(" : ");
        writeExpr(eelse);
        add(")");
      default:
        add("/* unsupported expr: " + e.e.getName() + " */");
    }
  }

  function collectGlobals(m:Map<TGlobal, Type>, e:TExpr) {
    switch (e.e) {
      case TGlobal(g): m.set(g, e.t);
      case TCall({ e : TGlobal(SetLayout) }, [{ e : TConst(CInt(x)) }, { e : TConst(CInt(y)) }, { e : TConst(CInt(z)) }]):
        computeLayout = [x, y, z];
      case TCall({ e : TGlobal(SetLayout) }, [{ e : TConst(CInt(x)) }, { e : TConst(CInt(y)) }]):
        computeLayout = [x, y, 1];
      case TCall({ e : TGlobal(SetLayout) }, [{ e : TConst(CInt(x)) }]):
        computeLayout = [x, 1, 1];
      default: e.iter(collectGlobals.bind(m));
    }
  }

  public function run(s:ShaderData) {
    locals = new Map();
    decls = [];
    buf = new StringBuf();
    exprValues = [];
    computeLayout = [1, 1, 1];

    // Reset bindless state
    bindlessSamplers = new Map();
    bindlessSamplersCount = 0;

    // Reset shader type flags
    isVertex = false;
    isFragment = false;
    isCompute = false;
    usesInstanceId = false;
    usesVertexId = false;

    if (s.funs.length != 1) throw "assert";
    var f = s.funs[0];

    switch (f.kind) {
      case Vertex: isVertex = true;
      case Fragment: isFragment = true;
      case Main: 
        isCompute = true;
        // Extract compute layout from SetLayout calls
        collectGlobals(new Map(), f.expr);
      default: throw "Unsupported shader kind";
    }

    add("#include <metal_stdlib>\n");
    add("using namespace metal;\n\n");

    // Add helper functions
    add("// Helper function to convert screen coordinates to UV texture coordinates\n");
    add("static inline float2 screenToUv(float2 screen) {\n");
    add("\treturn screen * float2(0.5, -0.5) + float2(0.5, 0.5);\n");
    add("}\n\n");

    add("// Helper function to convert UV texture coordinates to screen coordinates\n");
    add("static inline float2 uvToScreen(float2 uv) {\n");
    add("\treturn uv * float2(2.0, -2.0) + float2(-1.0, 1.0);\n");
    add("}\n\n");

    // Generate proper input/output structures
    if (isVertex) {
      // Vertex input structure with proper attribute indices
      add("struct VertexIn {\n");
      var attrIndex = 0;
      for (v in s.vars) {
        if (v.kind == Input) {
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
      for (v in s.vars) {
        if (v.kind == Var) {
          add("\t");
          addType(v.type);
          add(" ");
          add(varName(v));
          add(";\n");
        }
      }
      add("};\n\n");

      // Check if shader uses instance_id
      usesInstanceId = scanForInstanceId(f.expr);
      usesVertexId = scanForVertexId(f.expr);

      // Check if any input variables have PerInstance qualifier
      // If so, we need to reserve buffer index 1 for instance data
      var hasPerInstanceInputs = false;
      for (v in s.vars) {
        if (v.kind == Input && v.qualifiers != null) {
          for (q in v.qualifiers) {
            switch (q) {
              case PerInstance(_): hasPerInstanceInputs = true;
              default:
            }
          }
        }
      }

      // Vertex main function
      add("vertex VertexOut vertex_main(VertexIn input [[stage_in]]");

      // Add uniform buffer parameters (Global and Param kinds)
      // Buffer index 0 is reserved for per-vertex data (via [[stage_in]])
      // Buffer index 1 is reserved for per-instance data if any PerInstance inputs exist
      // Uniforms start at buffer index 1 or 2 depending on whether instance buffers are used
      var bufferIndex = hasPerInstanceInputs ? 2 : 1;
      for (v in s.vars) {
        if (v.kind == Global || v.kind == Param) {
          add(", constant ");
          var old = v.type;
          switch (v.type) {
            case TBuffer(t, _, _):
              v.type = t;
              addType(v.type);
              v.type = old;
              add(" *");
              add(varName(v));
              add(" [[buffer(" + bufferIndex + ")]]");
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

      // Add instance_id parameter if the shader uses instancing
      if (usesInstanceId) {
        add(", uint instance_id [[instance_id]]");
      }

      // Add vertex_id parameter if the shader uses vertex id
      if (usesVertexId) {
        add(", uint vertex_id [[vertex_id]]");
      }

      add(") {\n");
      add("\tVertexOut output;\n");

      // Declare local variables for inputs
      for (v in s.vars) {
        if (v.kind == Input) {
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
      for (v in s.vars) {
        if (v.kind == Output) {
          add("\t");
          addType(v.type);
          add(" ");
          add(varName(v)); // Just the variable name, not struct access
          add(";\n");
        }
      }

      // Declare local variables for varying outputs
      for (v in s.vars) {
        if (v.kind == Var) {
          add("\t");
          addType(v.type);
          add(" ");
          add(varName(v));
          add(";\n");
        }
      }

      // Declare Local variables (intermediate calculations)
      for (v in s.vars) {
        if (v.kind == Local) {
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
      for (v in s.vars) {
        if (v.kind == Output) {
          add("\toutput.position = ");
          add(varName(v));
          add(";\n");
        }
      }
      for (v in s.vars) {
        if (v.kind == Var) {
          add("\toutput.");
          add(varName(v));
          add(" = ");
          add(varName(v));
          add(";\n");
        }
      }

      add("\treturn output;\n");
      add("}\n");

    }
    else if (isFragment) {
      // Count fragment outputs for MRT support
      outputCount = 0;
      outputVars = [];
      for (v in s.vars) {
        if (v.kind == Output) {
          outputVars.push(v);
          outputCount++;
        }
      }

      // Fragment input (from vertex) - includes varyings (Var kind) from vertex shader
      add("struct VertexOut {\n");
      add("\tfloat4 position [[position]];\n");
      for (v in s.vars) {
        // In fragment shaders, Var kind are the varyings from vertex shader
        // This must match the vertex shader's VertexOut struct
        if (v.kind == Var) {
          add("\t");
          addType(v.type);
          add(" ");
          add(varName(v));
          add(";\n");
        }
      }
      add("};\n\n");

      // Fragment output structure for MRT
      if (outputCount > 1) {
        add("struct FragmentOut {\n");
        var colorIndex = 0;
        for (v in outputVars) {
          add("\t");
          addType(v.type);
          add(" ");
          add(varName(v));
          add(" [[color(" + colorIndex + ")]];\n");
          colorIndex++;
        }
        add("};\n\n");
      }

      // Fragment main function - return type depends on MRT
      if (outputCount > 1) {
        add("fragment FragmentOut fragment_main(VertexOut input [[stage_in]]");
      }
      else {
        add("fragment float4 fragment_main(VertexOut input [[stage_in]]");
      }

      // Add texture and buffer parameters
      var textureIndex = 0;
      var bufferIndex = 0;
      var samplerIndex = 0;

      // First pass: add textures
      for (v in s.vars) {
        if (v.kind == Param || v.kind == Global) {
          if (isTextureType(v.type)) {
            // Textures use [[texture(n)]] attribute
            // Metal textures are bound individually, not as arrays in shader arguments
            // The array access in HXSL (tex[0]) maps to individual texture bindings
            var old = v.type;
            var baseType = switch (v.type) {
              case TArray(t, _): t;
              default: v.type;
            };

            // Get the array size - how many textures in this array
            var arraySize = switch (v.type) {
              case TArray(_, SConst(n)): n;
              default: 1;
            };

            // Generate a parameter for each texture in the array
            for (i in 0...arraySize) {
              add(", ");
              v.type = baseType;

              // Special handling for samplers to detect depth textures by name
              switch (v.type) {
                case TSampler(dim, arr):
                  add(getSamplerType(dim, arr, v.name));
                case TSamplerDepth(dim, arr):
                  // Depth samplers explicitly use depth texture types
                  add(getDepthSamplerType(dim, arr));
                default:
                  addType(v.type);
              }

              v.type = old;
              add(" ");
              add(varName(v));
              if (i > 0) add("_" + i);
              add(" [[texture(" + textureIndex + ")]]");
              textureIndex++;
            }
          }
        }
      }

      // Check if shader uses bindless texturing (TTextureHandle type)
      var shaderHasBindless = false;
      for (v in s.vars) {
        switch (v.type) {
          case TTextureHandle:
            shaderHasBindless = true;
            break;
          default:
        }
      }

      // Second pass: add samplers
      var samplerCount = textureIndex;
      if (shaderHasBindless && samplerCount < 1) samplerCount = 1; // Need at least 1 sampler for bindless
      if (samplerCount > 0) {
        add(", array<sampler, " + samplerCount + "> fragmentSamplers [[sampler(0)]]");
      }

      // Add bindless texture array
      if (shaderHasBindless) {
        add(", array<texture2d<float>, 96> bindlessTextures [[texture(16)]]");
      }

      // Third pass: add non-texture buffers
      for (v in s.vars) {
        if (v.kind == Param || v.kind == Global) {
          if (!isTextureType(v.type)) {
            add(", ");
            // Non-texture parameters use [[buffer(n)]]
            add("constant ");
            var old = v.type;
            switch (v.type) {
              case TBuffer(t, _, _):
                v.type = t;
                addType(v.type);
                v.type = old;
                add(" *");
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
      for (v in s.vars) {
        if (v.kind == Var) {
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
      for (v in s.vars) {
        if (v.kind == Input) {
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
      for (v in s.vars) {
        if (v.kind == Output) {
          add("\t");
          addType(v.type);
          add(" ");
          add(varName(v));
          add(";\n");
        }
      }

      // Declare local variables (Local kind only - intermediate calculations)
      for (v in s.vars) {
        if (v.kind == Local) {
          add("\t");
          addType(v.type);
          add(" ");
          add(varName(v));
          add(";\n");
        }
      }

      // Process shader expression
      addExpr(f.expr, "\t");

      // Return the output - MRT uses struct, single output returns directly
      if (outputCount > 1) {
        // MRT: Return a struct with all outputs
        add("\tFragmentOut _output;\n");
        for (v in outputVars) {
          add("\t_output.");
          add(varName(v));
          add(" = ");
          add(varName(v));
          add(";\n");
        }
        add("\treturn _output;\n");
      }
      else {
        // Single output: return directly
        for (v in s.vars) {
          if (v.kind == Output) {
            add("\treturn ");
            add(varName(v));
            add(";\n");
            break;
          }
        }
      }

      add("}\n");

    }
    else if (isCompute) {
      add("kernel void compute_main(");
      
      // Add compute shader parameters
      var bufferIndex = 0;
      var textureIndex = 0;
      var first = true;
      
      // First pass: add buffers (globals, params)
      for (v in s.vars) {
        if (v.kind == Param || v.kind == Global) {
          if (!isTextureType(v.type)) {
            if (!first) add(", ");
            first = false;
            
            // Determine qualifier based on buffer kind
            var qualifier = switch (v.type) {
              case TBuffer(_, _, kind):
                switch (kind) {
                  case RW, RWPartial: "device ";
                  case Storage, StoragePartial: "device ";
                  default: "constant ";
                };
              default: "constant ";
            };
            add(qualifier);
            
            var old = v.type;
            switch (v.type) {
              case TBuffer(t, _, _):
                v.type = t;
                addType(v.type);
                v.type = old;
                add(" *");
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
      
      // Second pass: add textures
      for (v in s.vars) {
        if (v.kind == Param || v.kind == Global) {
          if (isTextureType(v.type)) {
            if (!first) add(", ");
            first = false;
            
            // Check if this is a writable texture
            var isWritable = switch (v.type) {
              case TSampler(_): false;
              default: true; // Assume non-sampler textures are writable
            };
            
            if (isWritable) {
              add("texture2d<float, access::read_write> ");
            } else {
              add("texture2d<float> ");
            }
            add(varName(v));
            add(" [[texture(" + textureIndex + ")]]");
            textureIndex++;
          }
        }
      }
      
      // Add thread position parameter
      if (!first) add(", ");
      add("uint3 thread_position_in_grid [[thread_position_in_grid]]) {\n");

      // Don't redeclare params/globals - they're already pointers in the signature
      // Just use them directly
      
      // Declare local variables
      for (v in s.vars) {
        if (v.kind == Local) {
          add("\t");
          addType(v.type);
          add(" ");
          add(varName(v));
          add(";\n");
        }
      }

      addExpr(f.expr, "\t");

      add("}\n");
    }

    // Build final source with declarations inserted after includes
    var mainSource = buf.toString();
    var headerEnd = mainSource.indexOf("using namespace metal;\n\n");
    if (headerEnd >= 0) {
      headerEnd += "using namespace metal;\n\n".length;
      var finalSource = mainSource.substring(0, headerEnd);

      // Add declarations (like _mat3x4 typedef)
      if (decls.length > 0) {
        finalSource += decls.join("\n") + "\n\n";
      }

      finalSource += mainSource.substring(headerEnd);
      return finalSource;
    }
    else {
      // Fallback if header pattern not found
      return mainSource;
    }
  }

  public static function compile(s:ShaderData) {
    return new MetalOut().run(s);
  }
}
