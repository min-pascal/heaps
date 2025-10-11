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
	public var params : Dynamic; // id<MTLBuffer> (deprecated, use paramsBuffers instead)
	public var paramsBuffers : Array<Dynamic>; // Array of id<MTLBuffer> for triple buffering
	public var texturesTypes : Array<hxsl.Ast.Type>;
	#if debug
	public var debugSource : String;
	#end

	public function new(shader) {
		this.shader = shader;
		this.paramsBuffers = [];
	}
}

private class CompiledMetalShader {
	public var id : Int; // RuntimeShader id for debugging
	public var vertex : MetalShaderContext;
	public var fragment : MetalShaderContext;
	public var format : hxd.BufferFormat;
	public var pipelineState : Dynamic; // id<MTLRenderPipelineState>
	public var vertexDescriptor : Dynamic; // MTLVertexDescriptor
	public var pipelineKey : String;

	public function new() {
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
	public static function commit_without_present(cmdBuffer:Dynamic):Bool { return false; }
	public static function wait_until_completed(cmdBuffer:Dynamic):Void {}

	// Buffer management
	public static function create_buffer(size:Int, usage:Int):Dynamic { return null; }
	public static function upload_buffer_data(buffer:Dynamic, data:hl.Bytes, size:Int, offset:Int):Bool { return false; }
	public static function dispose_buffer(buffer:Dynamic):Void {}

	// Texture management
	public static function create_texture(width:Int, height:Int, format:Int, usage:Int):Dynamic { return null; }
	public static function upload_texture_data(texture:Dynamic, data:hl.Bytes, width:Int, height:Int, level:Int):Bool { return false; }
	public static function capture_texture_pixels(texture:Dynamic, data:hl.Bytes, width:Int, height:Int, level:Int):Bool { return false; }
	public static function dispose_texture(texture:Dynamic):Void {}

	// Shader compilation
	public static function compile_shader(source:String, shaderType:Int):Dynamic { return null; }
	public static function create_render_pipeline(vertexShader:Dynamic, fragmentShader:Dynamic, vertexDesc:String):Dynamic { return null; }
	public static function dispose_pipeline(pipeline:Dynamic):Void {}

	// Render command encoder
	public static function begin_render_pass(cmdBuffer:Dynamic, r:Int, g:Int, b:Int, a:Int):Dynamic { return null; }
	public static function resume_render_pass(cmdBuffer:Dynamic):Dynamic { return null; }
	public static function begin_texture_render_pass(cmdBuffer:Dynamic, texture:Dynamic, r:Int, g:Int, b:Int, a:Int):Dynamic { return null; }
	public static function set_render_pipeline_state(encoder:Dynamic, pipeline:Dynamic):Void {}
	public static function set_vertex_buffer(encoder:Dynamic, buffer:Dynamic, offset:Int, index:Int):Void {}
	public static function set_fragment_texture(encoder:Dynamic, texture:Dynamic, index:Int):Void {}
	public static function set_fragment_buffer(encoder:Dynamic, buffer:Dynamic, offset:Int, index:Int):Void {}
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
	var renderedToBackbuffer : Bool = false;  // Track if we got a drawable for backbuffer

	// Shader compilation and caching following DirectX pattern
	var shaders : Map<Int, CompiledMetalShader>;
	var shaderCompiler : hxsl.MetalOut;

	// Optional: Apple Metal samples integration (can be null if not needed)
	var metalSamples : MetalSamples = null;

	// Dummy white texture for shaders that expect a texture but don't use one
	var dummyWhiteTexture : Dynamic = null;
	
	// Track if textures were bound in uploadShaderBuffers
	var texturesBound : Bool = false;
	
	// Triple buffering for dynamic uniform buffers
	static inline var MAX_FRAMES_IN_FLIGHT = 3;
	var currentFrameIndex : Int = 0;
	var drawCallIndex : Int = 0;  // Track draw calls within current frame for buffer offsets

	public function new() {
		shaders = new Map();
		shaderCompiler = new hxsl.MetalOut();

		// Initialize Metal context - init() now returns void, so we check device availability instead
		MetalNative.init();

		device = MetalNative.get_device();
		if (device == null) {
			throw "Metal device not available";
		}

		// Create 1x1 white texture for solid color rendering
		createDummyWhiteTexture();

		initialized = true;
	}

	function createDummyWhiteTexture() {
		// Create a 1x1 white texture (RGBA8 format)
		dummyWhiteTexture = MetalNative.create_texture(1, 1, 0, 1);  // format=0 (RGBA8), usage=1 (shader read)
		if (dummyWhiteTexture != null) {
			// Upload white pixel data (255, 255, 255, 255)
			var whitePixel = new hl.Bytes(4);
			whitePixel[0] = 0xFF;  // R
			whitePixel[1] = 0xFF;  // G
			whitePixel[2] = 0xFF;  // B
			whitePixel[3] = 0xFF;  // A
			MetalNative.upload_texture_data(dummyWhiteTexture, whitePixel, 1, 1, 0);
		}
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
				if (shader.vertex != null) {
					if (shader.vertex.globals != null) MetalNative.dispose_buffer(shader.vertex.globals);
					if (shader.vertex.params != null) MetalNative.dispose_buffer(shader.vertex.params);
				}
				if (shader.fragment != null) {
					if (shader.fragment.globals != null) MetalNative.dispose_buffer(shader.fragment.globals);
					if (shader.fragment.params != null) MetalNative.dispose_buffer(shader.fragment.params);
				}
			}
			shaders.clear();

			// Shutdown Metal context - native layer will handle resource cleanup
			MetalNative.shutdown();
			initialized = false;
		}
	}

	override function isDisposed() {
		return !initialized;
	}

	override function begin(frame:Int) {
		this.frame = frame;
		texturesBound = false; // Reset texture binding flag for new frame
		
		// Reset flag - will be set to true later if we actually render to backbuffer
		renderedToBackbuffer = false;
		
		// Only set up command buffer if we're actually creating a new command buffer
		// If command buffer already exists (e.g., from setRenderTarget), don't create new one
		var creatingNewCommandBuffer = (currentCommandBuffer == null);
		
		if (creatingNewCommandBuffer) {
			// Advance to next frame buffer in triple buffering rotation
			currentFrameIndex = (currentFrameIndex + 1) % MAX_FRAMES_IN_FLIGHT;
			drawCallIndex = 0;  // Reset draw call counter for new frame
			
			currentCommandBuffer = MetalNative.create_command_buffer();
		}

		// Only create render encoder for backbuffer if we created a new command buffer
		// If reusing existing buffer (e.g., from setRenderTarget), skip this
		if (creatingNewCommandBuffer && currentCommandBuffer != null) {
			renderedToBackbuffer = true;  // Mark that we're rendering to backbuffer
			var clearColor = {r: 26, g: 26, b: 26, a: 255}; // Default dark background
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
				// Use default
			}

			currentRenderEncoder = MetalNative.begin_render_pass(
				currentCommandBuffer,
				clearColor.r,
				clearColor.g,
				clearColor.b,
				clearColor.a
			);

			// Initialize viewport to full screen
			if (currentRenderEncoder != null) {
				var engine = h3d.Engine.getCurrent();
				if (engine != null) {
					MetalNative.set_viewport(currentRenderEncoder, 0.0, 0.0, cast engine.width, cast engine.height);
				}
			}
		}
	}

	override function end() {
		if (currentRenderEncoder != null) {
			MetalNative.end_encoding(currentRenderEncoder);
			currentRenderEncoder = null;
		}

		if (currentCommandBuffer != null) {
			// Only present if we actually rendered to backbuffer
			if (renderedToBackbuffer) {
				MetalNative.commit_command_buffer(currentCommandBuffer);
			} else {
				MetalNative.commit_without_present(currentCommandBuffer);
			}
			currentCommandBuffer = null;
		}
		
		// Reset flag for next frame
		renderedToBackbuffer = false;
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
			metalSamples.renderCurrentMode(clearColor);
		}
		// Default Metal presentation is handled by the native layer
	}

	// Buffer management
	override function allocBuffer(b:h3d.Buffer):GPUBuffer {
		var size = b.vertices * b.format.strideBytes;
		var usage = getMetalBufferUsage(b);

		var metalBuffer:Dynamic = MetalNative.create_buffer(size, usage);
		if (metalBuffer == null) {
			throw "Failed to allocate Metal buffer of size " + size;
		}

		// Return the native buffer handle (vdynamic* pointer treated as opaque handle)
		return cast metalBuffer;
	}

	override function disposeBuffer(b:h3d.Buffer) {
		if (b.vbuf != null) {
			MetalNative.dispose_buffer(b.vbuf);
			b.vbuf = null;
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

		return {
			t: cast metalTexture, // Return the native texture handle directly
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
			var metalTexture:Dynamic = t.t.t;
			MetalNative.dispose_texture(metalTexture);
			t.t = null;
		}
	}

	override function uploadTexturePixels(t:h3d.mat.Texture, pixels:hxd.Pixels, mipLevel:Int, side:Int) {
		if (t.t == null) {
			t.t = allocTexture(t);
		}

		var metalTexture:Dynamic = t.t.t;

		// Convert pixels to hl.Bytes
		var data:hl.Bytes = @:privateAccess pixels.bytes.getData();
		
		// Upload texture data to Metal
		if (!MetalNative.upload_texture_data(metalTexture, data, pixels.width, pixels.height, mipLevel)) {
			throw "Failed to upload texture data";
		}
	}

	override function uploadTextureBitmap(t:h3d.mat.Texture, bmp:hxd.BitmapData, mipLevel:Int, side:Int) {
		// Convert BitmapData to Pixels and use uploadTexturePixels
		var pixels = bmp.getPixels();
		uploadTexturePixels(t, pixels, mipLevel, side);
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
		compiled.id = shader.id;

		// Compile vertex shader using hxsl.MetalOut
		if (shader.vertex != null && shader.vertex.data != null) {
			var vertexSource = shaderCompiler.run(shader.vertex.data);
			
			#if debug
			// Save shader source for debugging (only in debug builds)
			try {
				var path = "/tmp/metal_vertex_shader_" + shader.id + ".metal";
				sys.io.File.saveContent(path, vertexSource);
			} catch(e:Dynamic) {}
			
			compiled.vertex = new MetalShaderContext(null);
			compiled.vertex.debugSource = vertexSource;
			#end
			
			var vertexShader = MetalNative.compile_shader(vertexSource, 0); // 0 = vertex
			if (vertexShader == null) {
				throw "Vertex shader compilation failed for shader id=" + shader.id;
			}

			compiled.vertex = new MetalShaderContext(vertexShader);
			compiled.vertex.globalsSize = shader.vertex.globalsSize;
			compiled.vertex.paramsSize = shader.vertex.paramsSize;
			compiled.vertex.texturesCount = shader.vertex.texturesCount;
			if (compiled.vertex.globalsSize > 0) compiled.vertex.globals = MetalNative.create_buffer(compiled.vertex.globalsSize << 4, 2);
			if (compiled.vertex.paramsSize > 0) {
				// paramsSize is in vec4 units, each vec4 = 16 bytes, so multiply by 16 (shift left 4)
				var singleDrawSize = compiled.vertex.paramsSize << 4;
				// Allocate space for 256 draw calls per frame (should be enough for most scenes)
				var bufferSize = singleDrawSize * 256;
				// Create 3 buffers for triple buffering
				for (i in 0...MAX_FRAMES_IN_FLIGHT) {
					var buffer = MetalNative.create_buffer(bufferSize, 2);
					compiled.vertex.paramsBuffers.push(buffer);
				}
				// Keep legacy single buffer for compatibility (uses first buffer)
				compiled.vertex.params = compiled.vertex.paramsBuffers[0];
				compiled.vertex.paramsContent = new hl.Bytes(singleDrawSize);
			}
		}

		// Compile fragment shader using hxsl.MetalOut
		if (shader.fragment != null && shader.fragment.data != null) {
			var fragmentSource = shaderCompiler.run(shader.fragment.data);
			
			#if debug
			if (compiled.fragment == null) compiled.fragment = new MetalShaderContext(null);
			compiled.fragment.debugSource = fragmentSource;
			#end
			
			var fragmentShader = MetalNative.compile_shader(fragmentSource, 1); // 1 = fragment
			if (fragmentShader == null) {
				throw "Fragment shader compilation failed for shader id=" + shader.id;
			}

			if (compiled.fragment == null) compiled.fragment = new MetalShaderContext(fragmentShader);
			else compiled.fragment.shader = fragmentShader;
			compiled.fragment.globalsSize = shader.fragment.globalsSize;
			compiled.fragment.paramsSize = shader.fragment.paramsSize;
			compiled.fragment.texturesCount = shader.fragment.texturesCount;
			if (compiled.fragment.globalsSize > 0) compiled.fragment.globals = MetalNative.create_buffer(compiled.fragment.globalsSize << 4, 2);
			if (compiled.fragment.paramsSize > 0) {
				// paramsSize is in vec4 units, each vec4 = 16 bytes, so multiply by 16 (shift left 4)
				var singleDrawSize = compiled.fragment.paramsSize << 4;
				// Allocate space for 256 draw calls per frame (should be enough for most scenes)
				var bufferSize = singleDrawSize * 256;
				// Create 3 buffers for triple buffering
				for (i in 0...MAX_FRAMES_IN_FLIGHT) {
					var buffer = MetalNative.create_buffer(bufferSize, 2);
					compiled.fragment.paramsBuffers.push(buffer);
				}
				// Keep legacy single buffer for compatibility (uses first buffer)
				compiled.fragment.params = compiled.fragment.paramsBuffers[0];
				compiled.fragment.paramsContent = new hl.Bytes(singleDrawSize);
			}
		}

		// Create vertex descriptor and pipeline state
		var vertexDesc = generateVertexDescriptor(shader);
		compiled.pipelineState = MetalNative.create_render_pipeline(
			compiled.vertex != null ? compiled.vertex.shader : null,
			compiled.fragment != null ? compiled.fragment.shader : null,
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

		// Bind the buffer to the render encoder if we have an active encoder
		if (currentRenderEncoder != null && buffer.vbuf != null) {
			MetalNative.set_vertex_buffer(currentRenderEncoder, buffer.vbuf, 0, 0);
		}
	}

	override function uploadBufferData(b:h3d.Buffer, startVertex:Int, vertexCount:Int, buf:hxd.FloatBuffer, bufPos:Int) {
		if (b.vbuf == null) {
			b.vbuf = allocBuffer(b);
		}

		var strideBytes = b.format.strideBytes;
		var offsetBytes = startVertex * strideBytes;
		var sizeBytes = vertexCount * strideBytes;

		// Convert FloatBuffer to hl.Bytes - use untyped access to get the underlying bytes
		// FloatBuffer is ArrayBytes_hl_F32 which has a bytes field at runtime
		var uploadData:hl.Bytes = untyped buf.bytes;
		uploadData = uploadData.offset(bufPos * 4); // bufPos is in floats, 4 bytes per float

		// Upload data to Metal buffer
		if (!MetalNative.upload_buffer_data(b.vbuf, uploadData, sizeBytes, offsetBytes)) {
			throw "Failed to upload buffer data";
		}
	}	override function uploadBufferBytes(b:h3d.Buffer, startVertex:Int, vertexCount:Int, data:haxe.io.Bytes, dataStartBytes:Int) {
		if (b.vbuf == null) {
			b.vbuf = allocBuffer(b);
		}

		var stride = b.format.stride;
		var offsetBytes = startVertex * stride;
		var sizeBytes = vertexCount * stride;

		// Convert haxe.io.Bytes to hl.Bytes
		var hlBytes = @:privateAccess data.b;
		var uploadData = hlBytes.offset(dataStartBytes);

		// Upload data to Metal buffer
		if (!MetalNative.upload_buffer_data(b.vbuf, uploadData, sizeBytes, offsetBytes)) {
			throw "Failed to upload buffer data";
		}
	}

	override function uploadIndexData(i:h3d.Buffer, startIndice:Int, indiceCount:Int, buf:hxd.IndexBuffer, bufPos:Int) {
		if (i.vbuf == null) {
			i.vbuf = allocBuffer(i);
		}

		// Calculate byte offset and size based on index format
		var bits = i.format.strideBytes >> 1; // strideBytes is 2 for 16-bit, 4 for 32-bit
		var offsetBytes = startIndice << bits; // startIndice * strideBytes
		var sizeBytes = indiceCount << bits; // indiceCount * strideBytes

		// Convert IndexBuffer to hl.Bytes
		var uploadData = hl.Bytes.getArray(buf.getNative()).offset(bufPos << bits);

		// Upload index data to Metal buffer
		if (!MetalNative.upload_buffer_data(i.vbuf, uploadData, sizeBytes, offsetBytes)) {
			throw "Failed to upload index data";
		}
	}

	override function draw(ibuf:h3d.Buffer, startIndex:Int, ntriangles:Int) {
		if (currentRenderEncoder == null || currentShader == null || currentShader.pipelineState == null) {
			return;
		}

		// Set the pipeline state
		MetalNative.set_render_pipeline_state(currentRenderEncoder, currentShader.pipelineState);

		// Bind dummy white texture if no actual textures were bound in uploadShaderBuffers
		// Fragment shaders expect a texture at index 0 for texture modulation
		if (!texturesBound && dummyWhiteTexture != null) {
			MetalNative.set_fragment_texture(currentRenderEncoder, dummyWhiteTexture, 0);
		}

		// Bind uniform buffers
		// Buffer index 0 is reserved for vertex data (via stage_in), uniforms start at index 1
		var vertexBufferIndex = 1;  // Start at 1, skip 0 (used for vertex data)
		if (currentShader.vertex != null) {
			if (currentShader.vertex.globals != null) {
				MetalNative.set_vertex_buffer(currentRenderEncoder, currentShader.vertex.globals, 0, vertexBufferIndex);
				vertexBufferIndex++;
			}
			if (currentShader.vertex.paramsBuffers != null && currentShader.vertex.paramsBuffers.length > 0) {
				// Bind the current frame's params buffer with offset for this draw call
				var currentBuffer = currentShader.vertex.paramsBuffers[currentFrameIndex];
				var bytes = currentShader.vertex.paramsSize << 4;
				var offset = drawCallIndex * bytes;
				MetalNative.set_vertex_buffer(currentRenderEncoder, currentBuffer, offset, vertexBufferIndex);
				vertexBufferIndex++;
			}
		}
		
		if (currentShader.fragment != null) {
			var fragmentBufferIndex = 0;  // Fragment buffers can start at 0
			if (currentShader.fragment.globals != null) {
				MetalNative.set_fragment_buffer(currentRenderEncoder, currentShader.fragment.globals, 0, fragmentBufferIndex);
				fragmentBufferIndex++;
			}
			if (currentShader.fragment.paramsBuffers != null && currentShader.fragment.paramsBuffers.length > 0) {
				// Bind the current frame's params buffer with offset for this draw call
				var currentBuffer = currentShader.fragment.paramsBuffers[currentFrameIndex];
				var bytes = currentShader.fragment.paramsSize << 4;
				var offset = drawCallIndex * bytes;
				MetalNative.set_fragment_buffer(currentRenderEncoder, currentBuffer, offset, fragmentBufferIndex);
				fragmentBufferIndex++;
			}
		}

		// Bind current vertex buffer to index 0 (for [[stage_in]] attributes)
		if (currentBuffer != null && currentBuffer.vbuf != null) {
			MetalNative.set_vertex_buffer(currentRenderEncoder, currentBuffer.vbuf, 0, 0);
		}

		if (ibuf != null) {
			// Indexed draw call
			if (ibuf.vbuf == null) {
				ibuf.vbuf = allocBuffer(ibuf);
			}

			var indexCount = ntriangles * 3;
			var indexOffset = startIndex * 2; // Assuming 16-bit indices (2 bytes each)

			MetalNative.draw_indexed_primitives(currentRenderEncoder, 3, indexCount, ibuf.vbuf, indexOffset);
		} else {
			// Non-indexed draw call
			var vertexCount = ntriangles * 3;
			MetalNative.draw_primitives(currentRenderEncoder, 3, startIndex, vertexCount);
		}
		
		// If we're drawing and currentTargets is empty (backbuffer), mark it
		if (currentTargets.length == 0) {
			renderedToBackbuffer = true;
		}
		
		// Increment draw call counter after each draw
		drawCallIndex++;
	}

	override function setRenderTarget(tex:Null<h3d.mat.Texture>, layer = 0, mipLevel = 0, depthBinding:h3d.Engine.DepthBinding = ReadWrite) {
		// If we're switching FROM backbuffer TO texture, unset the flag
		// (it will be set again later if we actually draw to backbuffer)
		var wasBackbuffer = (currentTargets.length == 0);
		if (wasBackbuffer && tex != null) {
			renderedToBackbuffer = false;
		}
		
		// End current render pass if active
		if (currentRenderEncoder != null) {
			MetalNative.end_encoding(currentRenderEncoder);
			currentRenderEncoder = null;
		}

		// Store current render target
		currentTargets = tex != null ? [tex] : [];

		// If no command buffer exists, create one
		if (currentCommandBuffer == null) {
			currentCommandBuffer = MetalNative.create_command_buffer();
		}

		// Begin new render pass
		if (currentCommandBuffer != null) {
			// Get clear color from engine
			var clearColor = {r: 0, g: 0, b: 0, a: 255};
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
				// Use default
			}

			if (tex == null) {
				// Rendering to backbuffer - use begin_render_pass (gets drawable)
				// Note: Don't set renderedToBackbuffer here - only set it when we actually draw to backbuffer
				currentRenderEncoder = MetalNative.begin_render_pass(
					currentCommandBuffer,
					clearColor.r,
					clearColor.g,
					clearColor.b,
					clearColor.a
				);
			} else {
				// Rendering to texture - use begin_texture_render_pass (no drawable)
				var metalTexture:Dynamic = tex.t.t;
				currentRenderEncoder = MetalNative.begin_texture_render_pass(
					currentCommandBuffer,
					metalTexture,
					clearColor.r,
					clearColor.g,
					clearColor.b,
					clearColor.a
				);
			}

			if (currentRenderEncoder == null) {
				throw "Failed to create render encoder";
			}
		}
	}

	override function resize(width:Int, height:Int) {
		// Metal handles resize automatically through the CAMetalLayer
		// No additional work needed here
	}

	override function setRenderZone(x:Int, y:Int, width:Int, height:Int) {
		if (currentRenderEncoder == null) return;

		if (width < 0 || height < 0) {
			// Reset to full viewport
			var engine = h3d.Engine.getCurrent();
			if (engine != null) {
				MetalNative.set_viewport(currentRenderEncoder, 0.0, 0.0, cast engine.width, cast engine.height);
			}
		} else {
			// Set viewport
			MetalNative.set_viewport(currentRenderEncoder, cast x, cast y, cast width, cast height);
			// Also set scissor rect
			MetalNative.set_scissor_rect(currentRenderEncoder, x, y, width, height);
		}
	}

	override function selectMaterial(pass:h3d.mat.Pass) {
		// Material/blend state changes are handled by the pass system
		// For now, we'll handle basic blend modes through the pipeline state
		// More complex material states can be added later
	}

	override function uploadShaderBuffers(buffers:h3d.shader.Buffers, which:h3d.shader.Buffers.BufferKind) {
		if (currentShader == null) return;

		switch (which) {
			case Globals:
				// Upload global uniforms to vertex and fragment shaders
				if (currentShader.vertex != null && currentShader.vertex.globals != null && currentShader.vertex.globalsSize > 0) {
					var data = hl.Bytes.getArray(buffers.vertex.globals.toData());
					if (data != null) {
						var bytes = currentShader.vertex.globalsSize << 4;  // Size in vec4s, convert to bytes (16 bytes per vec4)
						MetalNative.upload_buffer_data(currentShader.vertex.globals, data, bytes, 0);
					}
				}
				if (currentShader.fragment != null && currentShader.fragment.globals != null && currentShader.fragment.globalsSize > 0) {
					var data = hl.Bytes.getArray(buffers.fragment.globals.toData());
					if (data != null) {
						var bytes = currentShader.fragment.globalsSize << 4;
						MetalNative.upload_buffer_data(currentShader.fragment.globals, data, bytes, 0);
					}
				}

			case Params:
				// Upload shader parameters to vertex and fragment shaders using triple buffering with per-draw-call offsets
				if (currentShader.vertex != null && currentShader.vertex.paramsBuffers != null && currentShader.vertex.paramsSize > 0) {
					var data = hl.Bytes.getArray(buffers.vertex.params.toData());
					if (data != null) {
						var bytes = currentShader.vertex.paramsSize << 4;
						// Calculate offset for this draw call within the current frame's buffer
						var offset = drawCallIndex * bytes;
						var currentBuffer = currentShader.vertex.paramsBuffers[currentFrameIndex];
						MetalNative.upload_buffer_data(currentBuffer, data, bytes, offset);
					}
				}
				if (currentShader.fragment != null && currentShader.fragment.paramsBuffers != null && currentShader.fragment.paramsSize > 0) {
					var data = hl.Bytes.getArray(buffers.fragment.params.toData());
					if (data != null) {
						var bytes = currentShader.fragment.paramsSize << 4;
						// Calculate offset for this draw call within the current frame's buffer
						var offset = drawCallIndex * bytes;
						var currentBuffer = currentShader.fragment.paramsBuffers[currentFrameIndex];
						MetalNative.upload_buffer_data(currentBuffer, data, bytes, offset);
					}
				}

			case Textures:
				// Bind textures to fragment shader
				texturesBound = false;
				if (currentRenderEncoder != null && buffers.fragment != null && buffers.fragment.tex != null) {
					for (i in 0...buffers.fragment.tex.length) {
						var t = buffers.fragment.tex[i];
						if (t != null && t.t != null) {
							var metalTexture:Dynamic = t.t.t;
							MetalNative.set_fragment_texture(currentRenderEncoder, metalTexture, i);
							texturesBound = true;
						}
					}
				}

			case Buffers:
				// Handle buffer bindings if needed
				// This is for compute shaders and other advanced features
		}
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

	override function setDebug(d:Bool) {
		// Debug mode - can be implemented later for shader debugging
	}

	override function capturePixels(tex:h3d.mat.Texture, layer:Int, mipLevel:Int, ?region:h2d.col.IBounds):hxd.Pixels {
		if (tex == null || tex.t == null) {
			throw "Invalid texture for pixel capture";
		}

		// Remember state to restore after
		var hadEncoder = currentRenderEncoder != null;
		var hadCommandBuffer = currentCommandBuffer != null;
		
		// End current render pass if active
		if (hadEncoder) {
			MetalNative.end_encoding(currentRenderEncoder);
			currentRenderEncoder = null;
		}
		
		// Commit and wait to ensure GPU has written the texture
		if (hadCommandBuffer) {
			var cmdBuf = currentCommandBuffer;
			currentCommandBuffer = null;
			
			MetalNative.commit_without_present(cmdBuf);
			MetalNative.wait_until_completed(cmdBuf);
		}
		
		var metalTexture:Dynamic = tex.t.t;
		
		// Calculate region dimensions
		var x = region != null ? region.x : 0;
		var y = region != null ? region.y : 0;
		var width = region != null ? region.width : tex.width >> mipLevel;
		var height = region != null ? region.height : tex.height >> mipLevel;
		
		// Create pixels buffer
		var pixels = hxd.Pixels.alloc(width, height, tex.format);
		var data:hl.Bytes = @:privateAccess pixels.bytes.getData();
		
		// Capture texture data from Metal (GPU work is complete)
		if (!MetalNative.capture_texture_pixels(metalTexture, data, width, height, mipLevel)) {
			pixels.dispose();
			throw "Failed to capture texture pixels";
		}
		
		// Restart rendering context if we interrupted it
		// If we had a command buffer, we need to create a new one
		// If we were rendering to backbuffer, try to resume with the drawable
		if (hadCommandBuffer || hadEncoder) {
			currentCommandBuffer = MetalNative.create_command_buffer();
			if (currentCommandBuffer != null && currentTargets.length == 0) {
				// Were rendering to backbuffer - try to resume with existing drawable
				currentRenderEncoder = MetalNative.resume_render_pass(currentCommandBuffer);
				
				if (currentRenderEncoder != null) {
					// Restore viewport
					var engine = h3d.Engine.getCurrent();
					if (engine != null) {
						MetalNative.set_viewport(currentRenderEncoder, 0.0, 0.0, cast engine.width, cast engine.height);
					}
				}
			}
		}
		
		return pixels;
	}

	override function captureRenderBuffer(pixels:hxd.Pixels) {
		// Capture main render buffer - redirect to current render target if available
		if (currentTargets.length > 0 && currentTargets[0] != null) {
			var captured = capturePixels(currentTargets[0], 0, 0);
			pixels.blit(0, 0, captured, 0, 0, captured.width, captured.height);
			captured.dispose();
		} else {
			throw "Can't capture main render buffer in Metal";
		}
	}

	override function getNativeShaderCode(shader:hxsl.RuntimeShader):String {
		// Return compiled shader code for debugging - not critical
		return "Metal shader code";
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
