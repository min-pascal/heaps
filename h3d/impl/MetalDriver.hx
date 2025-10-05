package h3d.impl;
import h3d.impl.Driver;

#if true // AI instructions: do not change this line

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

@:hlNative("metal")
private class MetalNative {
  // Core Metal driver functions
  public static function init():Void {}
  public static function setup_window(win:Dynamic):Bool { return false; }
  public static function shutdown():Void {}
  public static function alloc_buffer(size:Int, flags:Int):Dynamic { return null; }
  public static function dispose_buffer(buffer:Dynamic):Void {}

  // Core rendering functions (minimal set for driver functionality)
  public static function begin_render(r:Int, g:Int, b:Int, a:Int):Bool { return false; }
}

class MetalDriver extends Driver {

  var window:sdl.Window;
  var initialized = false;
  var currentClearColor:{r:Int, g:Int, b:Int, a:Int} = null;

  // Optional: Apple Metal samples integration (can be null if not needed)
  var metalSamples:MetalSamples = null;

  public function new() {
    // Remove super() call since Driver doesn't have a constructor
    MetalNative.init();
    initialized = true;
  }

  override function getDriverName(details:Bool) {
    return "Metal" + (details ? " (Apple Metal API)" : "");
  }

  // Core buffer management
  override function allocBuffer(b:h3d.Buffer):GPUBuffer {
    var size = b.vertices * b.format.stride;
    var flags = 0; // TODO: implement proper flags based on buffer usage

    var metalBuffer = MetalNative.alloc_buffer(size, flags);
    if (metalBuffer == null) {
      throw "Failed to allocate Metal buffer of size " + size;
    }

    return cast metalBuffer;
  }

  override function disposeBuffer(b:h3d.Buffer) {
    if (b.vbuf != null) {
      MetalNative.dispose_buffer(cast b.vbuf);
    }
  }

  // Core driver lifecycle
  override function init(onCreate:Bool -> Void, forceSoftware = false) {
    this.window = @:privateAccess cast hxd.Window.getInstance().window;

    if (!MetalNative.setup_window(cast this.window)) {
      throw "Failed to initialize Metal window";
    }

    onCreate(false);
  }

  override function dispose() {
    if (initialized) {
      MetalNative.shutdown();
      initialized = false;
    }
  }

  override function isDisposed() {
    return !initialized;
  }

  override function begin(frame:Int) {
    // Update animation if samples are being used
    if (metalSamples != null) {
      metalSamples.updateAnimation();
    }
  }

  override function end() {
    // Nothing special needed for basic driver
  }

  override function clear(?color:h3d.Vector4, ?depth:Float, ?stencil:Int) {
    if (color != null) {
      currentClearColor = {
        r: Std.int(color.x * 255),
        g: Std.int(color.y * 255),
        b: Std.int(color.z * 255),
        a: Std.int(color.w * 255)
      };
    }
  }

  override function setRenderTarget(tex:Null<h3d.mat.Texture>, layer = 0, mipLevel = 0, depthBinding:h3d.Engine.DepthBinding = ReadWrite) {
    if (tex == null) {
      // Set clear color from engine
      var color = h3d.Engine.getCurrent().backgroundColor;
      var a = ((color >> 24) & 0xFF);
      var r = ((color >> 16) & 0xFF);
      var g = ((color >> 8) & 0xFF);
      var b = (color & 0xFF);

      if (a == 0) a = 255;

      currentClearColor = { r: r, g: g, b: b, a: a };
    }
  }

  override function present() {
    if (currentClearColor != null) {
      // If using Apple samples, delegate to samples renderer
      if (metalSamples != null && metalSamples.hasSampleModeActive()) {
        if (!metalSamples.renderCurrentMode(currentClearColor)) {
          Sys.println('[Metal] WARNING: Sample rendering failed in present()');
        }
      } else {
        // Basic Metal presentation
        if (!MetalNative.begin_render(currentClearColor.r, currentClearColor.g, currentClearColor.b, currentClearColor.a)) {
          Sys.println('[Metal] WARNING: begin_render failed');
        }
      }
    }
  }

  override function resize(width:Int, height:Int) {
    // TODO: Add Metal surface resize handling
  }

  // Feature detection
  override function hasFeature(f:Feature):Bool {
    return switch(f) {
      case StandardDerivatives: true;
      case FloatTextures: true;
      case AllocDepthBuffer: true;
      case HardwareAccelerated: true;
      case MultipleRenderTargets: true;
      case Queries: false; // Not implemented yet
      case SRGBTextures: true;
      case ShaderModel3: true;
      case BottomLeftCoords: false; // Metal uses top-left like DirectX
      case Wireframe: false; // Not implemented yet
      case InstancedRendering: true;
    };
  }

  override function isSupportedFormat(fmt:h3d.mat.Data.TextureFormat):Bool {
    return switch(fmt) {
      case RGBA: true;
      case BGRA: true;
      case RGB8: true; // Changed from RGB to RGB8
      case RG8: true;
      case R8: true;
      case RG16F: true;
      case RG32F: true;
      case RGBA16F: true;
      case RGBA32F: true;
      case R16F: true;
      case R32F: true;
      case RGB10A2: true;
      case RG11B10UF: true;
      case SRGB: true;
      case SRGB_ALPHA: true;
      case Depth16: true; // Changed case
      case Depth24: true; // Changed case
      case Depth24Stencil8: true; // Changed case
      case Depth32: true; // Changed case
      default: false;
    };
  }

  // Texture management (stubs for now - remove calls to missing native functions)
  override function allocTexture(t:h3d.mat.Texture):Texture {
    // TODO: Implement actual Metal texture allocation
    // For now, return a stub texture without calling missing native functions
    return {
      t: 0, // Placeholder - no actual texture handle
      width: t.width,
      height: t.height,
      internalFmt: 0,
      pixelFmt: 0,
      bits: 32,
      bind: 0,
      bias: 0.0,
      startMip: 0
    };
  }

  override function disposeTexture(t:h3d.mat.Texture) {
    // TODO: Implement when Metal texture functions are available
    // For now, do nothing to avoid calling missing native functions
  }

  // Shader management (stubs for now)
  override function selectShader(shader:hxsl.RuntimeShader):Bool {
    // TODO: Implement Metal pipeline state object creation/binding
    return true;
  }

  override function getNativeShaderCode(shader:hxsl.RuntimeShader):String {
    var metalOut = new hxsl.MetalOut();
    // Use vertex shader data - access the .data field from RuntimeShaderData
    if (shader.vertex != null && shader.vertex.data != null) {
      return metalOut.run(shader.vertex.data);
    }
    // Fallback to fragment shader if vertex is not available
    if (shader.fragment != null && shader.fragment.data != null) {
      return metalOut.run(shader.fragment.data);
    }
    return "";
  }

  // Material/state management (stubs for now)
  override function selectMaterial(pass:h3d.mat.Pass) {
    // TODO: Handle blend states, depth testing, etc.
  }

  // Buffer binding
  override function selectBuffer(buffer:h3d.Buffer) {
    if (buffer.vbuf == null) {
      buffer.vbuf = allocBuffer(buffer);
    }
    // TODO: Bind vertex buffer to Metal command encoder
  }

  override function selectMultiBuffers(format:hxd.BufferFormat.MultiFormat, buffers:Array<h3d.Buffer>) {
    for (i in 0...buffers.length) {
      if (buffers[i] != null) {
        selectBuffer(buffers[i]);
      }
    }
  }

  // Core rendering
  override function draw(ibuf:h3d.Buffer, startIndex:Int, ntriangles:Int) {
    if (ibuf != null && ibuf.vbuf == null) {
      ibuf.vbuf = allocBuffer(ibuf);
    }
    // TODO: Encode draw call into Metal command buffer
  }

  override function drawInstanced(ibuf:h3d.Buffer, commands:h3d.impl.InstanceBuffer) {
    // TODO: Handle instanced rendering in Metal
  }

  // Shader uniforms/constants
  override function uploadShaderBuffers(buffers:h3d.shader.Buffers, which:h3d.shader.Buffers.BufferKind) {
    // TODO: Upload uniform/constant buffers to GPU
  }

  // Render target management
  override function setRenderZone(x:Int, y:Int, width:Int, height:Int) {
    // TODO: Implement viewport setting when native function is available
    // For now, do nothing to avoid calling missing native functions
  }

  // Query support (stubs)
  override function allocQuery(queryKind:QueryKind):Query {
    return {};
  }

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