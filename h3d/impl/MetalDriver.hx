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
  public static function init():Void {}
  public static function setup_window(win:Dynamic):Bool { return false; }
  public static function begin_render(r:Int, g:Int, b:Int, a:Int):Bool { return false; }
  public static function shutdown():Void {}
  public static function alloc_buffer(size:Int, flags:Int):Dynamic { return null; }
  public static function dispose_buffer(buffer:Dynamic):Void {}

  // Triangle rendering functions
  public static function create_triangle(positions:hl.Bytes, colors:hl.Bytes, vertexCount:Int):Bool { return false; }

  public static function render_triangle(r:Int, g:Int, b:Int, a:Int):Bool { return false; }

  public static function update_buffer(buffer:Dynamic, data:hl.Bytes, size:Int, offset:Int):Bool { return false; }

  // Argument buffer functions with animation support
  public static function create_triangle_with_argbuffers(positions:hl.Bytes, colors:hl.Bytes, vertexCount:Int):Bool { return false; }

  public static function render_triangle_with_argbuffers(r:Int, g:Int, b:Int, a:Int):Bool { return false; }

  public static function create_instanced_rectangles():Bool { return false; }
  public static function render_instanced_rectangles(r:Int, g:Int, b:Int, a:Int):Bool { return false; }
  
  // Perspective cube functions
  public static function create_perspective_cubes():Bool { return false; }
  public static function render_perspective_cubes(r:Int, g:Int, b:Int, a:Int):Bool { return false; }
  public static function enable_debug_dots(enable:Bool):Bool { return false; }

  // Lighting cube functions
  public static function create_lighting_cubes():Bool { return false; }
  public static function render_lighting_cubes(r:Int, g:Int, b:Int, a:Int):Bool { return false; }

  // Textured cube functions
  public static function create_textured_cubes():Bool { return false; }
  public static function render_textured_cubes(r:Int, g:Int, b:Int, a:Int):Bool { return false; }

  // Compute shader functions
  public static function create_compute_cubes():Bool { return false; }
  public static function render_compute_cubes(r:Int, g:Int, b:Int, a:Int):Bool { return false; }
  public static function generate_mandelbrot_texture():Bool { return false; }

  // Frame debugging functions
  public static function init_frame_debugging():Bool { return false; }
  public static function trigger_frame_capture():Bool { return false; }
  public static function check_auto_capture():Bool { return false; }
}

class MetalDriver extends Driver {

  var window:sdl.Window;
  var initialized = false;
  var currentClearColor:{r:Int, g:Int, b:Int, a:Int} = null;

  // Animation state
  var animationTime:Float = 0.0;
  var lastFrameTime:Float = 0.0;

  var instancingEnabled = false;
  var perspectiveEnabled = false;
  var lightingEnabled = false;
  var texturedEnabled = false;
  var computeEnabled = false;

  public function new() {
    MetalNative.init();
    initialized = true;
    lastFrameTime = haxe.Timer.stamp();
  }

  override function getDriverName(details:Bool) {
    // Return a hardcoded string instead of calling the native function
    return "Metal" + (details ? " (Apple Metal API)" : "");
  }

  // CRITICAL: Add the missing allocBuffer method with correct signature
  override function allocBuffer(b:h3d.Buffer):GPUBuffer {
    var size = b.vertices * b.format.stride;
    var flags = 0; // You can implement proper flags later

    var metalBuffer = MetalNative.alloc_buffer(size, flags);
    if (metalBuffer == null) {
      throw "Failed to allocate Metal buffer of size " + size;
    }

    // GPUBuffer is just an empty typedef, so we cast the metalBuffer directly
    return cast metalBuffer;
  }

  // Fix the disposeBuffer method - takes h3d.Buffer, accesses vbuf field
  override function disposeBuffer(b:h3d.Buffer) {
    if (b.vbuf != null) {
      MetalNative.dispose_buffer(cast b.vbuf);
    }
  }

  override function setRenderTarget(tex:Null<h3d.mat.Texture>, layer = 0, mipLevel = 0, depthBinding:h3d.Engine.DepthBinding = ReadWrite) {
    // For now, we're only supporting the main window as render target
    if (tex == null) {
      // Set clear color from engine
      var color = h3d.Engine.getCurrent().backgroundColor;

      // Fix color extraction - correct RGBA order for 0xAARRGGBB format
      var a = ((color >> 24) & 0xFF); // Alpha component
      var r = ((color >> 16) & 0xFF); // Red component
      var g = ((color >> 8) & 0xFF); // Green component
      var b = (color & 0xFF); // Blue component

      // Ensure alpha is 255 if not set
      if (a == 0) a = 255;

      // Store the clear color for later use in present()
      currentClearColor = { r: r, g: g, b: b, a: a };

      // Debug output to verify color components
      Sys.println('[Metal] Setting clear color: 0x${StringTools.hex(color, 8)} (R:${r} G:${g} B:${b} A:${a})');
    }
  }

  override function init(onCreate:Bool -> Void, forceSoftware = false) {
    this.window = @:privateAccess cast hxd.Window.getInstance().window;
    // Setup the window with Metal - pass as Dynamic to match signature
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

  // Required Driver overrides for minimal implementation

  override function isDisposed() {
    return !initialized;
  }

  override function begin(frame:Int) {
    // Update animation timing
    var currentTime = haxe.Timer.stamp();
    var deltaTime = currentTime - lastFrameTime;
    lastFrameTime = currentTime;
    animationTime += deltaTime;
  }

  override function end() {
    // Nothing special to do here for now
  }

  override function clear(?color:h3d.Vector4, ?depth:Float, ?stencil:Int) {
    // If we have a color, update our current clear color
    if (color != null) {
      currentClearColor = {
        r: Std.int(color.x * 255), g: Std.int(color.y * 255), b: Std.int(color.z * 255), a: Std.int(color.w * 255)
      };
    }
  }

  override function present() {
    // Only render if we have a clear color set
    if (currentClearColor != null) {
      // Choose rendering method based on what's enabled
      if (computeEnabled) {
        // Use compute shader cubes rendering
        if (!MetalNative.render_compute_cubes(currentClearColor.r, currentClearColor.g, currentClearColor.b, currentClearColor.a)) {
          Sys.println('[Metal] WARNING: render_compute_cubes failed in present()');
        }
      } else if (texturedEnabled) {
        // Use textured cubes rendering
        if (!MetalNative.render_textured_cubes(currentClearColor.r, currentClearColor.g, currentClearColor.b, currentClearColor.a)) {
          Sys.println('[Metal] WARNING: render_textured_cubes failed in present()');
        }
      } else if (lightingEnabled) {
        // Use lighting cubes rendering
        if (!MetalNative.render_lighting_cubes(currentClearColor.r, currentClearColor.g, currentClearColor.b, currentClearColor.a)) {
          Sys.println('[Metal] WARNING: render_lighting_cubes failed in present()');
        }
      } else if (perspectiveEnabled) {
        // Use perspective cubes rendering
        if (!MetalNative.render_perspective_cubes(currentClearColor.r, currentClearColor.g, currentClearColor.b, currentClearColor.a)) {
          Sys.println('[Metal] WARNING: render_perspective_cubes failed in present()');
        }
      } else if (instancingEnabled) {
        // Use instanced rectangles rendering
        if (!MetalNative.render_instanced_rectangles(currentClearColor.r, currentClearColor.g, currentClearColor.b, currentClearColor.a)) {
          Sys.println('[Metal] WARNING: render_instanced_rectangles failed in present()');
        }
      } else {
        // Use the original triangle rendering for backward compatibility
        if (!MetalNative.render_triangle_with_argbuffers(currentClearColor.r, currentClearColor.g, currentClearColor.b, currentClearColor.a)) {
          Sys.println('[Metal] WARNING: render_triangle_with_argbuffers failed in present()');
        }
      }
    }
  }

  override function resize(width:Int, height:Int) {
    // Update the Metal layer drawable size when window is resized
    if (initialized && window != null) {
      // We should add a resize function to the native interface, but for now this is handled automatically
    }
  }

  // Triangle rendering implementation
  public function renderTriangle(positions:Array<Float>, colors:Array<Float>) {
    if (!initialized) return;

    // Calculate total bytes needed
    var posCount = positions.length;
    var colCount = colors.length;
    var posBytes = new hl.Bytes(posCount * 4); // 4 bytes per float
    var colBytes = new hl.Bytes(colCount * 4); // 4 bytes per float

    // Copy data to bytes
    for (i in 0...posCount) {
      posBytes.setF32(i * 4, positions[i]);
    }
    for (i in 0...colCount) {
      colBytes.setF32(i * 4, colors[i]);
    }

    // Calculate vertices from positions (each vertex has 3 values: x, y, z)
    var vertexCount = Std.int(posCount / 3);

    // Create and upload the triangle data to the GPU - BUT DON'T RENDER YET
    if (!MetalNative.create_triangle(posBytes, colBytes, vertexCount)) {
      throw "Failed to create triangle in Metal";
    }

    // DO NOT call render_triangle here - let present() handle the rendering
    // This was causing the double render pass issue
  }

  // Triangle rendering with argument buffers implementation for animation
  public function renderTriangleWithArgBuffers(positions:Array<Float>, colors:Array<Float>) {
    if (!initialized) return;

    // Calculate total bytes needed
    var posCount = positions.length;
    var colCount = colors.length;
    var posBytes = new hl.Bytes(posCount * 4); // 4 bytes per float
    var colBytes = new hl.Bytes(colCount * 4); // 4 bytes per float

    // Copy data to bytes
    for (i in 0...posCount) {
      posBytes.setF32(i * 4, positions[i]);
    }

    // Colors need RGB values (3 floats per vertex)
    for (i in 0...Std.int(colCount / 4 * 3)) {
      // Convert from RGBA to RGB format (skip alpha)
      var srcIdx = Std.int(i / 3 * 4);
      colBytes.setF32(i * 4, colors[srcIdx + (i % 3)]);
    }

    // Calculate vertices from positions (each vertex has 3 values: x, y, z)
    var vertexCount = Std.int(posCount / 3);

    // Create and upload the triangle data using argument buffers
    // The native implementation now handles animation automatically
    if (!MetalNative.create_triangle_with_argbuffers(posBytes, colBytes, vertexCount)) {
      throw "Failed to create triangle with argument buffers in Metal";
    }
  }

  public function updateTriangleBuffer(buffer:MetalBufferHandle, data:Array<Float>, offset:Int = 0) {
    if (!initialized) return;

    // Convert the data array to bytes
    var dataBytesSize = data.length * 4; // 4 bytes per float
    var dataBytes = new hl.Bytes(dataBytesSize);

    // Set float values using setF32
    for (i in 0...data.length) {
      dataBytes.setF32(i * 4, data[i]);
    }

    // Update the buffer data in the GPU
    if (!MetalNative.update_buffer(cast buffer, dataBytes, dataBytesSize, offset)) {
      throw "Failed to update triangle buffer in Metal";
    }
  }

  // Get current animation time for use in applications that need it
  public function getAnimationTime():Float {
    return animationTime;
  }

  // Get current animation angle (for compatibility with Test03Animation)
  public function getAnimationAngle():Float {
    return animationTime * 0.6; // Roughly equivalent to 0.01f increment per frame at 60fps
  }

  public function createInstancedRectangles() {
    if (!initialized) return;

    if (!MetalNative.create_instanced_rectangles()) {
      throw "Failed to create instanced rectangles in Metal";
    }
    
    // Enable instancing mode
    instancingEnabled = true;
    perspectiveEnabled = false;
    trace("Instancing enabled - present() will now render instanced rectangles");
  }

  public function createPerspectiveCubes() {
    if (!initialized) return;

    if (!MetalNative.create_perspective_cubes()) {
      throw "Failed to create perspective cubes in Metal";
    }
    
    // Enable perspective mode
    perspectiveEnabled = true;
    instancingEnabled = false;
    trace("Perspective rendering enabled - present() will now render perspective cubes");
  }

  public function createLightingCubes() {
    if (!initialized) return;

    if (!MetalNative.create_lighting_cubes()) {
      throw "Failed to create lighting cubes in Metal";
    }

    // Enable lighting mode
    lightingEnabled = true;
    perspectiveEnabled = false;
    instancingEnabled = false;
    trace("Lighting enabled - present() will now render lit cubes");
  }

  public function enableDebugDots(enable:Bool):Bool {
    if (!initialized) return false;

    if (!MetalNative.enable_debug_dots(enable)) {
      trace("Failed to enable/disable debug dots in Metal");
      return false;
    }

    trace("Debug dots " + (enable ? "ENABLED" : "DISABLED") + " - yellow dots will show on cube vertices");
    return true;
  }

  public function createTexturedCubes() {
    if (!initialized) return;

    if (!MetalNative.create_textured_cubes()) {
      throw "Failed to create textured cubes in Metal";
    }

    // Enable textured mode - this has the highest priority
    texturedEnabled = true;
    lightingEnabled = false;
    perspectiveEnabled = false;
    instancingEnabled = false;
    trace("Textured rendering enabled - present() will now render textured cubes");
  }

  public function createComputeCubes() {
    if (!initialized) return;

    if (!MetalNative.create_compute_cubes()) {
      throw "Failed to create compute cubes in Metal";
    }

    // Enable compute mode - this has the highest priority
    computeEnabled = true;
    texturedEnabled = false;
    lightingEnabled = false;
    perspectiveEnabled = false;
    instancingEnabled = false;
    trace("Compute shader rendering enabled - present() will now render Mandelbrot-textured cubes");
  }

  public function generateMandelbrotTexture():Bool {
    if (!initialized) return false;

    if (!MetalNative.generate_mandelbrot_texture()) {
      trace("Failed to generate Mandelbrot texture in Metal");
      return false;
    }

    trace("Mandelbrot texture generated successfully");
    return true;
  }

  // Frame debugging methods
  public function triggerFrameCapture():Bool {
    if (!initialized) return false;

    if (!MetalNative.trigger_frame_capture()) {
      trace("Failed to trigger GPU frame capture in Metal");
      return false;
    }

    trace("GPU frame capture triggered - next frame will be captured and opened in Xcode");
    return true;
  }

  public function checkAutoCapture():Bool {
    if (!initialized) return false;

    return MetalNative.check_auto_capture();
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