package h3d.impl;

#if hl
/**
 * Apple Metal Samples Integration
 * Contains all the sample-specific rendering methods migrated from Apple's Metal C++ samples
 */
class MetalSamples {

  // Animation state
  var animationTime:Float = 0.0;
  var lastFrameTime:Float = 0.0;

  // Sample rendering modes
  var instancingEnabled = false;
  var perspectiveEnabled = false;
  var lightingEnabled = false;
  var texturedEnabled = false;
  var computeEnabled = false;

  public function new() {
    lastFrameTime = haxe.Timer.stamp();
  }

  // Update animation timing
  public function updateAnimation() {
    var currentTime = haxe.Timer.stamp();
    var deltaTime = currentTime - lastFrameTime;
    lastFrameTime = currentTime;
    animationTime += deltaTime;
  }

  // Get current animation time for use in applications
  public function getAnimationTime():Float {
    return animationTime;
  }

  // Get current animation angle (for compatibility with Test03Animation)
  public function getAnimationAngle():Float {
    return animationTime * 0.6; // Roughly equivalent to 0.01f increment per frame at 60fps
  }

  // Triangle rendering implementation
  public function renderTriangle(positions:Array<Float>, colors:Array<Float>) {
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
  }

  // Triangle rendering with argument buffers implementation for animation
  public function renderTriangleWithArgBuffers(positions:Array<Float>, colors:Array<Float>) {
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

  public function updateTriangleBuffer(buffer:h3d.impl.MetalDriver.MetalBufferHandle, data:Array<Float>, offset:Int = 0) {
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

  public function createInstancedRectangles() {
    if (!MetalNative.create_instanced_rectangles()) {
      throw "Failed to create instanced rectangles in Metal";
    }

    // Enable instancing mode
    instancingEnabled = true;
    perspectiveEnabled = false;
    lightingEnabled = false;
    texturedEnabled = false;
    computeEnabled = false;
    trace("Instancing enabled - present() will now render instanced rectangles");
  }

  public function createPerspectiveCubes() {
    if (!MetalNative.create_perspective_cubes()) {
      throw "Failed to create perspective cubes in Metal";
    }

    // Enable perspective mode
    perspectiveEnabled = true;
    instancingEnabled = false;
    lightingEnabled = false;
    texturedEnabled = false;
    computeEnabled = false;
    trace("Perspective rendering enabled - present() will now render perspective cubes");
  }

  public function createLightingCubes() {
    if (!MetalNative.create_lighting_cubes()) {
      throw "Failed to create lighting cubes in Metal";
    }

    // Enable lighting mode
    lightingEnabled = true;
    perspectiveEnabled = false;
    instancingEnabled = false;
    texturedEnabled = false;
    computeEnabled = false;
    trace("Lighting enabled - present() will now render lit cubes");
  }

  public function enableDebugDots(enable:Bool):Bool {
    if (!MetalNative.enable_debug_dots(enable)) {
      trace("Failed to enable/disable debug dots in Metal");
      return false;
    }

    trace("Debug dots " + (enable ? "ENABLED" : "DISABLED") + " - yellow dots will show on cube vertices");
    return true;
  }

  public function createTexturedCubes() {
    if (!MetalNative.create_textured_cubes()) {
      throw "Failed to create textured cubes in Metal";
    }

    // Enable textured mode - this has the highest priority
    texturedEnabled = true;
    lightingEnabled = false;
    perspectiveEnabled = false;
    instancingEnabled = false;
    computeEnabled = false;
    trace("Textured rendering enabled - present() will now render textured cubes");
  }

  public function createComputeCubes() {
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
    if (!MetalNative.generate_mandelbrot_texture()) {
      trace("Failed to generate Mandelbrot texture in Metal");
      return false;
    }

    trace("Mandelbrot texture generated successfully");
    return true;
  }

  // Frame debugging methods
  public function triggerFrameCapture():Bool {
    if (!MetalNative.trigger_frame_capture()) {
      trace("Failed to trigger GPU frame capture in Metal");
      return false;
    }

    trace("GPU frame capture triggered - next frame will be captured and opened in Xcode");
    return true;
  }

  public function checkAutoCapture():Bool {
    return MetalNative.check_auto_capture();
  }

  // Render the current active sample mode
  public function renderCurrentMode(clearColor:{r:Int, g:Int, b:Int, a:Int}):Bool {
    // Choose rendering method based on what's enabled
    if (computeEnabled) {
      return MetalNative.render_compute_cubes(clearColor.r, clearColor.g, clearColor.b, clearColor.a);
    } else if (texturedEnabled) {
      return MetalNative.render_textured_cubes(clearColor.r, clearColor.g, clearColor.b, clearColor.a);
    } else if (lightingEnabled) {
      return MetalNative.render_lighting_cubes(clearColor.r, clearColor.g, clearColor.b, clearColor.a);
    } else if (perspectiveEnabled) {
      return MetalNative.render_perspective_cubes(clearColor.r, clearColor.g, clearColor.b, clearColor.a);
    } else if (instancingEnabled) {
      return MetalNative.render_instanced_rectangles(clearColor.r, clearColor.g, clearColor.b, clearColor.a);
    } else {
      return MetalNative.render_triangle_with_argbuffers(clearColor.r, clearColor.g, clearColor.b, clearColor.a);
    }
  }

  // Check if any sample mode is active
  public function hasSampleModeActive():Bool {
    return computeEnabled || texturedEnabled || lightingEnabled || perspectiveEnabled || instancingEnabled;
  }

  // Get current active mode name (for debugging)
  public function getCurrentModeName():String {
    if (computeEnabled) return "Compute Cubes";
    if (texturedEnabled) return "Textured Cubes";
    if (lightingEnabled) return "Lighting Cubes";
    if (perspectiveEnabled) return "Perspective Cubes";
    if (instancingEnabled) return "Instanced Rectangles";
    return "Triangle with ArgBuffers";
  }
}

// Make the MetalNative class accessible to MetalSamples
@:hlNative("metal")
private class MetalNative {
  public static function create_triangle(positions:hl.Bytes, colors:hl.Bytes, vertexCount:Int):Bool { return false; }
  public static function render_triangle(r:Int, g:Int, b:Int, a:Int):Bool { return false; }
  public static function update_buffer(buffer:Dynamic, data:hl.Bytes, size:Int, offset:Int):Bool { return false; }
  public static function create_triangle_with_argbuffers(positions:hl.Bytes, colors:hl.Bytes, vertexCount:Int):Bool { return false; }
  public static function render_triangle_with_argbuffers(r:Int, g:Int, b:Int, a:Int):Bool { return false; }
  public static function create_instanced_rectangles():Bool { return false; }
  public static function render_instanced_rectangles(r:Int, g:Int, b:Int, a:Int):Bool { return false; }
  public static function create_perspective_cubes():Bool { return false; }
  public static function render_perspective_cubes(r:Int, g:Int, b:Int, a:Int):Bool { return false; }
  public static function enable_debug_dots(enable:Bool):Bool { return false; }
  public static function create_lighting_cubes():Bool { return false; }
  public static function render_lighting_cubes(r:Int, g:Int, b:Int, a:Int):Bool { return false; }
  public static function create_textured_cubes():Bool { return false; }
  public static function render_textured_cubes(r:Int, g:Int, b:Int, a:Int):Bool { return false; }
  public static function create_compute_cubes():Bool { return false; }
  public static function render_compute_cubes(r:Int, g:Int, b:Int, a:Int):Bool { return false; }
  public static function generate_mandelbrot_texture():Bool { return false; }
  public static function init_frame_debugging():Bool { return false; }
  public static function trigger_frame_capture():Bool { return false; }
  public static function check_auto_capture():Bool { return false; }
}
#end
