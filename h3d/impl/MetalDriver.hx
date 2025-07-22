package h3d.impl;
import h3d.impl.Driver;
import h3d.mat.Pass;
import h3d.mat.Stencil;
import h3d.mat.Data;

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
    public static function init() : Void {}
    public static function setup_window(win : Dynamic) : Bool { return false; }
    public static function begin_render(r : Int, g : Int, b : Int, a : Int) : Bool { return false; }
    public static function shutdown() : Void {}
    public static function alloc_buffer(size : Int, flags : Int) : Dynamic { return null; }
    public static function dispose_buffer(buffer : Dynamic) : Void {}
}

class MetalDriver extends Driver {

    var window : sdl.Window;
    var initialized = false;

    public function new() {
        MetalNative.init();
        initialized = true;
    }

    override function getDriverName(details : Bool) {
        // Return a hardcoded string instead of calling the native function
        return "Metal" + (details ? " (Apple Metal API)" : "");
    }

    override function setRenderTarget(tex : Null<h3d.mat.Texture>, layer = 0, mipLevel = 0, depthBinding : h3d.Engine.DepthBinding = ReadWrite) {
        // For now, we're only supporting the main window as render target
        if (tex == null) {
            // Set clear color from engine
            var color = h3d.Engine.getCurrent().backgroundColor;
            var r = ((color >> 16) & 0xFF);
            var g = ((color >> 8) & 0xFF);
            var b = (color & 0xFF);
            var a = ((color >> 24) & 0xFF);
            
            // Ensure alpha is 255 if not set
            if (a == 0) a = 255;

            MetalNative.begin_render(r, g, b, a);
        }
    }

    override function init(onCreate : Bool -> Void, forceSoftware = false) {
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

    override function begin(frame : Int) {
        // Nothing special to do here for now
    }

    override function end() {
        // Nothing special to do here for now
    }

    override function clear(?color : h3d.Vector4, ?depth : Float, ?stencil : Int) {
        // Handled in setRenderTarget
    }

    override function captureRenderBuffer(pixels : hxd.Pixels) {
        throw "Not implemented";
    }

    override function allocTexture(t : h3d.mat.Texture) : Texture {
        return null; // Not implemented for this minimal example
    }

    override function uploadTextureBitmap(t : h3d.mat.Texture, bmp : hxd.BitmapData, mipLevel : Int, side : Int) {
        // Not implemented for this minimal example
    }

    override function uploadTexturePixels(t : h3d.mat.Texture, pixels : hxd.Pixels, mipLevel : Int, side : Int) {
        // Not implemented for this minimal example
    }

    override function selectShader(shader : hxsl.RuntimeShader) : Bool {
        return false; // Not implemented for this minimal example
    }

    override function uploadShaderBuffers(buffers : h3d.shader.Buffers, which : h3d.shader.Buffers.BufferKind) {
        // Not implemented for this minimal example
    }

    override function selectBuffer(buffer : h3d.Buffer) {
        // Not implemented for this minimal example
    }

    override function draw(ibuf : h3d.Buffer, startIndex : Int, ntriangles : Int) {
        // Not implemented for this minimal example
    }

    // Proper buffer allocation implementation matching Driver.hx signature
    override function allocBuffer(b : h3d.Buffer) : GPUBuffer {
        // Calculate total size based on buffer properties
        var totalSize = b.getMemSize(); // Use the built-in method to get memory size
        if (totalSize == 0) totalSize = 1024; // Minimum buffer size
        
        // Convert flags to integer safely
        var flagsInt = 0;
        // For now, just use 0 for flags since we don't need specific Metal buffer flags yet
        
        // Allocate native Metal buffer - using snake_case function name
        // The native function should return a handle directly compatible with our MetalBufferHandle type
        return MetalNative.alloc_buffer(totalSize, flagsInt);
    }

    override function disposeBuffer(b : h3d.Buffer) {
        if (b.vbuf != null) {
            // Pass the buffer handle directly to the dispose function
            MetalNative.dispose_buffer(cast b.vbuf);
        }
    }

    override function disposeTexture(t : h3d.mat.Texture) {
        // Not implemented for this minimal example
    }

    override function present() {
        // Not needed for this minimal example as presentation is handled in begin_render
    }

    override function resize(width : Int, height : Int) {
        // Update the Metal layer drawable size when window is resized
        if (initialized && window != null) {
            // We should add a resize function to the native interface, but for now this is handled automatically
        }
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