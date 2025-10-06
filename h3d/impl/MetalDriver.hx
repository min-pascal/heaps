package h3d.impl;
import h3d.impl.Driver;

#if metal

// Metal-specific type definitions - using normal abstract instead of coreType
#if !macro
abstract MetalBufferHandle(Dynamic) {
  public inline function new(d:Dynamic) {
    this = d;
  }

  @:from static public inline function fromDynamic(d:Dynamic):MetalBufferHandle {
    return new MetalBufferHandle(d);
  }

  @:to public inline function toDynamic():Dynamic {
    return this;
  }
}
#end

// Metal-specific helper classes following DirectX pattern
private class MetalShaderContext {
	public var shader : Dynamic; // id<MTLFunction>
	public var globalsSize : Int;
	public var paramsSize : Int;
	public var texturesCount : Int;
	public var bufferCount : Int;
	public var paramsContent : hl.Bytes;
	public var globals : Dynamic; // id<MTLBuffer>
	public var params : Dynamic; // id<MTLBuffer>
	public var texturesTypes : Array<hxsl.Ast.Type>;
	#if debug
	public var debugSource : String;
	#end

	public function new(shader) {
		this.shader = shader;
	}
}

private class CompiledMetalShader {
	public var vertex : MetalShaderContext;
	public var fragment : MetalShaderContext;
	public var format : hxd.BufferFormat;
	public var pipelineState : Dynamic; // id<MTLRenderPipelineState>
	public var vertexDescriptor : Dynamic; // MTLVertexDescriptor
	public var pipelineKey : String;

	public function new() {
	}
}

// Metal buffer wrapper following OpenGL pattern
private class MetalBuffer {
	public var buffer : Dynamic; // id<MTLBuffer>
	public var size : Int;
	public var usage : Int;

	public function new(buffer, size, usage) {
		this.buffer = buffer;
		this.size = size;
		this.usage = usage;
	}
}

// Metal texture wrapper
private class MetalTexture {
	public var texture : Dynamic; // id<MTLTexture>
	public var width : Int;
	public var height : Int;
	public var format : Int;
	public var usage : Int;

	public function new(texture, width, height, format, usage) {
		this.texture = texture;
		this.width = width;
		this.height = height;
		this.format = format;
		this.usage = usage;
	}
}

@:hlNative("metal")
private class MetalNative {
	// Context management - match actual native signatures
	public static function init():Void {}
	public static function setup_window(win:Dynamic):Bool { return false; }
	public static function shutdown():Void {}

	// Device and command queue
	public static function get_device():Dynamic { return null; }
	public static function create_command_buffer():Dynamic { return null; }
	public static function commit_command_buffer(cmdBuffer:Dynamic):Bool { return false; }

	// Buffer management
	public static function create_buffer(size:Int, usage:Int):Dynamic { return null; }
	public static function upload_buffer_data(buffer:Dynamic, data:hl.Bytes, size:Int, offset:Int):Bool { return false; }
	public static function dispose_buffer(buffer:Dynamic):Void {}

	// Texture management
	public static function create_texture(width:Int, height:Int, format:Int, usage:Int):Dynamic { return null; }
	public static function upload_texture_data(texture:Dynamic, data:hl.Bytes, width:Int, height:Int, level:Int):Bool { return false; }
	public static function dispose_texture(texture:Dynamic):Void {}

	// Shader compilation
	public static function compile_shader(source:String, shaderType:Int):Dynamic { return null; }
	public static function create_render_pipeline(vertexShader:Dynamic, fragmentShader:Dynamic, vertexDesc:String):Dynamic { return null; }
	public static function dispose_pipeline(pipeline:Dynamic):Void {}

	// Render command encoder
	public static function begin_render_pass(cmdBuffer:Dynamic, r:Int, g:Int, b:Int, a:Int):Dynamic { return null; }
	public static function set_render_pipeline_state(encoder:Dynamic, pipeline:Dynamic):Void {}
	public static function set_vertex_buffer(encoder:Dynamic, buffer:Dynamic, offset:Int, index:Int):Void {}
	public static function set_fragment_texture(encoder:Dynamic, texture:Dynamic, index:Int):Void {}
	public static function draw_primitives(encoder:Dynamic, primitiveType:Int, vertexStart:Int, vertexCount:Int):Void {}
	public static function draw_indexed_primitives(encoder:Dynamic, primitiveType:Int, indexCount:Int, indexBuffer:Dynamic, indexOffset:Int):Void {}
	public static function end_encoding(encoder:Dynamic):Void {}

	// Viewport and render state
	public static function set_viewport(encoder:Dynamic, x:Float, y:Float, width:Float, height:Float):Void {}
	public static function set_scissor_rect(encoder:Dynamic, x:Int, y:Int, width:Int, height:Int):Void {}
}

class MetalDriver extends Driver {

	var device : Dynamic; // id<MTLDevice>
	var commandQueue : Dynamic; // id<MTLCommandQueue>
	var window : sdl.Window;
	var initialized = false;

	// State management following established pattern
	var currentShader : CompiledMetalShader;
	var currentBuffer : h3d.Buffer;
	var currentIndexBuffer : h3d.Buffer;
	var currentCommandBuffer : Dynamic;
	var currentRenderEncoder : Dynamic;
	var currentTargets : Array<h3d.mat.Texture> = [];
	var frame : Int;

	// Shader compilation and caching following DirectX pattern
	var shaders : Map<Int, CompiledMetalShader>;
	var shaderCompiler : hxsl.MetalOut;

	// Resource tracking with handle-based system for Metal
	var buffers : Array<MetalBuffer> = [];
	var textures : Array<MetalTexture> = [];
	var nextBufferHandle : Int = 1;
	var nextTextureHandle : Int = 1;
	var bufferHandles : Map<Int, Dynamic> = new Map(); // handle -> native buffer
	var textureHandles : Map<Int, Dynamic> = new Map(); // handle -> native texture

	// Optional: Apple Metal samples integration (can be null if not needed)
  var metalSamples:MetalSamples = null;

	public function new() {
		shaders = new Map();
		shaderCompiler = new hxsl.MetalOut();

		// Initialize Metal context - init() now returns void, so we check device availability instead
		MetalNative.init();

		device = MetalNative.get_device();
		if (device == null) {
			throw "Metal device not available";
		}

		initialized = true;
	}

	override function getDriverName(details:Bool) {
		return "Metal" + (details ? " (Apple Metal API)" : "");
	}

	override function init(onCreate:Bool -> Void, forceSoftware = false) {
		this.window = @:privateAccess cast hxd.Window.getInstance().window;

		if (!MetalNative.setup_window(cast this.window)) {
			throw "Failed to setup Metal window";
		}

		onCreate(false);
	}

	override function dispose() {
		if (initialized) {
			// Dispose all shaders
			for (shader in shaders) {
				if (shader.pipelineState != null) {
					MetalNative.dispose_pipeline(shader.pipelineState);
				}
			}
			shaders.clear();

			// Dispose all buffers
			for (buffer in buffers) {
				MetalNative.dispose_buffer(buffer.buffer);
			}
			buffers = [];

			// Dispose all textures
			for (texture in textures) {
				MetalNative.dispose_texture(texture.texture);
			}
			textures = [];

			MetalNative.shutdown();
			initialized = false;
		}
	}

	override function isDisposed() {
		return !initialized;
	}

	override function begin(frame:Int) {
		this.frame = frame;
		currentCommandBuffer = MetalNative.create_command_buffer();
	}

	override function end() {
		if (currentRenderEncoder != null) {
			MetalNative.end_encoding(currentRenderEncoder);
			currentRenderEncoder = null;
		}

		if (currentCommandBuffer != null) {
			MetalNative.commit_command_buffer(currentCommandBuffer);
			currentCommandBuffer = null;
		}
	}

	override function clear(?color:h3d.Vector4, ?depth:Float, ?stencil:Int) {
		// Clear color will be used in next render pass
	}

	override function present() {
		// If we have active Metal samples, render them
		if (metalSamples != null && metalSamples.hasSampleModeActive()) {
			// Update animation for samples that need it
			metalSamples.updateAnimation();

			// Get clear color from engine or use default
			var clearColor = {r: 0, g: 0, b: 0, a: 255}; // Default black
			try {
				var engine = h3d.Engine.getCurrent();
				if (engine != null) {
					var bgColor = engine.backgroundColor;
					clearColor.r = (bgColor >> 16) & 0xFF;
					clearColor.g = (bgColor >> 8) & 0xFF;
					clearColor.b = bgColor & 0xFF;
					clearColor.a = (bgColor >> 24) & 0xFF;
				}
			} catch (e:Dynamic) {
				// Use default clear color if engine not available
			}

			// Render the current active sample mode
			if (!metalSamples.renderCurrentMode(clearColor)) {
				trace("Warning: Failed to render Metal sample mode: " + metalSamples.getCurrentModeName());
			}
		} else {
			// Default Metal presentation is handled by the native layer
		}
	}

	// Buffer management following OpenGL pattern
	override function allocBuffer(b:h3d.Buffer):GPUBuffer {
		var size = b.vertices * b.format.stride;
		var usage = getMetalBufferUsage(b);

		var metalBuffer = MetalNative.create_buffer(size, usage);
		if (metalBuffer == null) {
			throw "Failed to allocate Metal buffer of size " + size;
		}

		var wrapper = new MetalBuffer(metalBuffer, size, usage);
		buffers.push(wrapper);

		// Store buffer with handle and return integer handle (not pointer)
		var handle = nextBufferHandle++;
		bufferHandles.set(handle, metalBuffer);

		return handle; // Return integer handle, not raw pointer
	}

	override function disposeBuffer(b:h3d.Buffer) {
		if (b.vbuf != null) {
			var handle = cast(b.vbuf, Int);
			var metalBuffer = bufferHandles.get(handle);
			if (metalBuffer != null) {
				MetalNative.dispose_buffer(metalBuffer);
				bufferHandles.remove(handle);
				// Remove from tracking array
				buffers = buffers.filter(buf -> buf.buffer != metalBuffer);
			}
		}
	}

	// Texture management following established pattern
	override function allocTexture(t:h3d.mat.Texture):Texture {
		var format = getMetalTextureFormat(t.format);
		var usage = getMetalTextureUsage(t);

		var metalTexture = MetalNative.create_texture(t.width, t.height, format, usage);
		if (metalTexture == null) {
			throw "Failed to allocate Metal texture " + t.width + "x" + t.height;
		}

		var wrapper = new MetalTexture(metalTexture, t.width, t.height, format, usage);
		textures.push(wrapper);

		// Store texture with handle and return integer handle (not pointer)
		var handle = nextTextureHandle++;
		textureHandles.set(handle, metalTexture);

		return {
			t: handle, // Return integer handle, not raw pointer
			width: t.width,
			height: t.height,
			internalFmt: format,
			pixelFmt: format,
			bits: getFormatBits(t.format),
			bind: 0,
			bias: 0.0,
			startMip: 0
		};
	}

	override function disposeTexture(t:h3d.mat.Texture) {
		if (t.t != null) {
			var handle = cast(t.t.t, Int);
			var metalTexture = textureHandles.get(handle);
			if (metalTexture != null) {
				MetalNative.dispose_texture(metalTexture);
				textureHandles.remove(handle);
				// Remove from tracking array
				textures = textures.filter(tex -> tex.texture != metalTexture);
			}
		}
	}

	// Shader compilation following DirectX pattern with Metal integration
	override function selectShader(shader:hxsl.RuntimeShader):Bool {
		if (shader == null) return false;

		var id = shader.id;
		var compiledShader = shaders.get(id);

		if (compiledShader == null) {
			compiledShader = compileShader(shader);
			if (compiledShader == null) return false;
			shaders.set(id, compiledShader);
		}

		currentShader = compiledShader;
		return true;
	}

	function compileShader(shader:hxsl.RuntimeShader):CompiledMetalShader {
		var compiled = new CompiledMetalShader();

		// Compile vertex shader using hxsl.MetalOut
		if (shader.vertex != null && shader.vertex.data != null) {
			var vertexSource = shaderCompiler.run(shader.vertex.data);
			#if debug
			compiled.vertex = new MetalShaderContext(null);
			compiled.vertex.debugSource = vertexSource;
			#end
			var vertexShader = MetalNative.compile_shader(vertexSource, 0); // 0 = vertex
			if (vertexShader == null) return null;

			compiled.vertex = new MetalShaderContext(vertexShader);
		}

		// Compile fragment shader using hxsl.MetalOut
		if (shader.fragment != null && shader.fragment.data != null) {
			var fragmentSource = shaderCompiler.run(shader.fragment.data);
			#if debug
			if (compiled.fragment == null) compiled.fragment = new MetalShaderContext(null);
			compiled.fragment.debugSource = fragmentSource;
			#end
			var fragmentShader = MetalNative.compile_shader(fragmentSource, 1); // 1 = fragment
			if (fragmentShader == null) return null;

			if (compiled.fragment == null) compiled.fragment = new MetalShaderContext(fragmentShader);
			else compiled.fragment.shader = fragmentShader;
		}

		// Create vertex descriptor and pipeline state
		var vertexDesc = generateVertexDescriptor(shader);
		compiled.pipelineState = MetalNative.create_render_pipeline(
			compiled.vertex.shader,
			compiled.fragment.shader,
			vertexDesc
		);

		if (compiled.pipelineState == null) return null;

		return compiled;
	}

	// Helper functions following established patterns
	function getMetalBufferUsage(b:h3d.Buffer):Int {
		var usage = 0;
		if (b.flags.has(Dynamic)) usage |= 1;
		if (b.flags.has(UniformBuffer)) usage |= 2;
		if (b.flags.has(IndexBuffer)) usage |= 4;
		return usage;
	}

	function getMetalTextureFormat(fmt:h3d.mat.Data.TextureFormat):Int {
		return switch(fmt) {
			case RGBA: 0; // MTLPixelFormatRGBA8Unorm
			case BGRA: 1; // MTLPixelFormatBGRA8Unorm
			case RGB8: 2; // MTLPixelFormatRGB8Unorm
			case RG8: 3; // MTLPixelFormatRG8Unorm
			case R8: 4; // MTLPixelFormatR8Unorm
			case RGBA16F: 5; // MTLPixelFormatRGBA16Float
			case RGBA32F: 6; // MTLPixelFormatRGBA32Float
			default: 0;
		};
	}

	function getMetalTextureUsage(t:h3d.mat.Texture):Int {
		var usage = 1; // MTLTextureUsageShaderRead
		if (t.flags.has(Target)) usage |= 2; // MTLTextureUsageRenderTarget
		if (t.flags.has(Writable)) usage |= 4; // MTLTextureUsageShaderWrite
		return usage;
	}

	function getFormatBits(fmt:h3d.mat.Data.TextureFormat):Int {
		return switch(fmt) {
			case RGBA | BGRA | RGB8: 32;
			case RG8: 16;
			case R8: 8;
			case RGBA16F: 64;
			case RGBA32F: 128;
			default: 32;
		};
	}

	function generateVertexDescriptor(shader:hxsl.RuntimeShader):String {
		// Generate Metal vertex descriptor from shader inputs
		if (shader.vertex != null && shader.vertex.data != null) {
			var inputs = [];
			for (v in shader.vertex.data.vars) {
				if (v.kind == Input) {
					inputs.push('${v.name}:${getMetalVertexType(v.type)}');
				}
			}
			return inputs.join(',');
		}
		return "";
	}

	function getMetalVertexType(t:hxsl.Ast.Type):String {
		return switch(t) {
			case TFloat: "float";
			case TVec(2, VFloat): "float2";
			case TVec(3, VFloat): "float3";
			case TVec(4, VFloat): "float4";
			default: "float4";
		};
	}

	// Rendering implementation - simplified for now
	override function selectBuffer(buffer:h3d.Buffer) {
		if (buffer.vbuf == null) {
			buffer.vbuf = allocBuffer(buffer);
		}
		currentBuffer = buffer;
	}

	override function draw(ibuf:h3d.Buffer, startIndex:Int, ntriangles:Int) {
		// TODO: Implement Metal rendering commands
		// This requires currentRenderEncoder to be active
	}

	override function setRenderTarget(tex:Null<h3d.mat.Texture>, layer = 0, mipLevel = 0, depthBinding:h3d.Engine.DepthBinding = ReadWrite) {
		// TODO: Setup render pass with render targets
	}

	// Feature detection
	override function hasFeature(f:Feature):Bool {
		return switch(f) {
			case StandardDerivatives: true;
			case FloatTextures: true;
			case AllocDepthBuffer: true;
			case HardwareAccelerated: true;
			case MultipleRenderTargets: true;
			case SRGBTextures: true;
			case ShaderModel3: true;
			case BottomLeftCoords: false; // Metal uses top-left
			case InstancedRendering: true;
			default: false;
		};
	}

	override function isSupportedFormat(fmt:h3d.mat.Data.TextureFormat):Bool {
		return switch(fmt) {
			case RGBA | BGRA | RGB8 | RG8 | R8: true;
			case RGBA16F | RGBA32F | RG16F | RG32F | R16F | R32F: true;
			case Depth16 | Depth24 | Depth24Stencil8 | Depth32: true;
			default: false;
		};
	}

	// Stubs for remaining Driver methods
	override function selectMaterial(pass:h3d.mat.Pass) {}
	override function uploadShaderBuffers(buffers:h3d.shader.Buffers, which:h3d.shader.Buffers.BufferKind) {}
	override function drawInstanced(ibuf:h3d.Buffer, commands:h3d.impl.InstanceBuffer) {}
	override function setRenderZone(x:Int, y:Int, width:Int, height:Int) {}
	override function resize(width:Int, height:Int) {}
	override function selectMultiBuffers(format:hxd.BufferFormat.MultiFormat, buffers:Array<h3d.Buffer>) {}
	override function allocQuery(queryKind:QueryKind):Query { return {}; }
	override function deleteQuery(q:Query) {}
	override function beginQuery(q:Query) {}
	override function endQuery(q:Query) {}
	override function queryResultAvailable(q:Query):Bool { return false; }
	override function queryResult(q:Query):Int { return 0; }

	// Apple Metal Samples Integration (optional)
  public function enableAppleSamples():MetalSamples {
    if (metalSamples == null) {
      metalSamples = new MetalSamples();
    }
    return metalSamples;
  }

  public function getAppleSamples():Null<MetalSamples> {
    return metalSamples;
  }
}

#else

// Stub implementation when Metal is not available
class MetalDriver extends Driver {
	public function new() {
		throw "Metal is not available";
	}
}

#end
