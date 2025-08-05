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

    // Triangle rendering functions
    public static function create_triangle(positions : hl.Bytes, colors : hl.Bytes, vertexCount : Int) : Bool { return false; }
    public static function render_triangle(r : Int, g : Int, b : Int, a : Int) : Bool { return false; }
    public static function update_buffer(buffer : Dynamic, data : hl.Bytes, size : Int, offset : Int) : Bool { return false; }
}

class MetalDriver extends Driver {

    var window : sdl.Window;
    var initialized = false;
    var currentClearColor : { r:Int, g:Int, b:Int, a:Int } = null;

    public function new() {
        MetalNative.init();
        initialized = true;
    }

    override function getDriverName(details : Bool) {
        // Return a hardcoded string instead of calling the native function
        return "Metal" + (details ? " (Apple Metal API)" : "");
    }

    // CRITICAL: Add the missing allocBuffer method with correct signature
    override function allocBuffer(b : h3d.Buffer) : GPUBuffer {
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
    override function disposeBuffer(b : h3d.Buffer) {
        if (b.vbuf != null) {
            MetalNative.dispose_buffer(cast b.vbuf);
        }
    }

    override function setRenderTarget(tex : Null<h3d.mat.Texture>, layer = 0, mipLevel = 0, depthBinding : h3d.Engine.DepthBinding = ReadWrite) {
        // For now, we're only supporting the main window as render target
        if (tex == null) {
            // Set clear color from engine
            var color = h3d.Engine.getCurrent().backgroundColor;

            // Fix color extraction - correct RGBA order for 0xAARRGGBB format
            var a = ((color >> 24) & 0xFF); // Alpha component
            var r = ((color >> 16) & 0xFF); // Red component
            var g = ((color >> 8) & 0xFF);  // Green component
            var b = (color & 0xFF);         // Blue component

            // Ensure alpha is 255 if not set
            if (a == 0) a = 255;

            // Force bright red color for testing - ensures we can see if colors work at all
            // This helps diagnose if it's a color extraction issue or something else
            r = 255;
            g = 0;
            b = 0;

            // Store the clear color for later use in present()
            currentClearColor = { r: r, g: g, b: b, a: a };

            // Debug output to verify color components
            Sys.println('[Metal] Setting clear color: 0x${StringTools.hex(color, 8)} (R:${r} G:${g} B:${b} A:${a})');
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
//        trace('MetalDriver.begin frame: ${frame}');
        // Nothing special to do here for now
    }

    override function end() {
//        trace('MetalDriver.end called');
        // Nothing special to do here for now
    }

    override function clear(?color : h3d.Vector4, ?depth : Float, ?stencil : Int) {
//        trace('MetalDriver.clear called - color: ${color}, depth: ${depth}, stencil: ${stencil}');
        
        // If we have a color, update our current clear color
        if (color != null) {
            currentClearColor = {
                r: Std.int(color.x * 255),
                g: Std.int(color.y * 255), 
                b: Std.int(color.z * 255),
                a: Std.int(color.w * 255)
            };
        }
    }

    override function present() {
    // Only render if we have a clear color set
    if (currentClearColor != null) {
        // Use the unified render function that handles both clear and triangle
        if (!MetalNative.render_triangle(currentClearColor.r, currentClearColor.g, currentClearColor.b, currentClearColor.a)) {
            Sys.println('[Metal] WARNING: render_triangle failed in present()');
        }
    }
}

    override function resize(width : Int, height : Int) {
        // Update the Metal layer drawable size when window is resized
        if (initialized && window != null) {
            // We should add a resize function to the native interface, but for now this is handled automatically
        }
    }

    // Triangle rendering implementation
    public function renderTriangle(positions : Array<Float>, colors : Array<Float>) {
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

    public function updateTriangleBuffer(buffer : MetalBufferHandle, data : Array<Float>, offset : Int = 0) {
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
}

#else

// Stub implementation when Metal is not available
class MetalDriver extends Driver {
    public function new() {
        throw "Metal is not available";
    }
}

#end