package hxsl;

typedef Vec = h3d.Vector;
typedef Vec4 = h3d.Vector4;
typedef IVec = Array<Int>;
typedef BVec = Array<Bool>;
typedef Matrix = h3d.Matrix;
typedef Texture = h3d.mat.Texture;
typedef TextureArray = h3d.mat.TextureArray;
typedef TextureChannel = h3d.mat.Texture;
typedef TextureHandle = h3d.mat.TextureHandle;
typedef Buffer = h3d.Buffer;

// Sampler types for explicit depth texture usage (Metal hardware depth comparison)
typedef Sampler2D = h3d.mat.Texture;
typedef Sampler2DShadow = h3d.mat.Texture;  // NEW: Depth comparison sampler for shadow maps
typedef SamplerCube = h3d.mat.Texture;
typedef SamplerCubeShadow = h3d.mat.Texture;  // NEW: For omnidirectional shadows

class ChannelTools {
	public static inline function isPackedFormat( c : TextureChannel ) {
		return c.format == h3d.mat.Texture.nativeFormat;
	}
}