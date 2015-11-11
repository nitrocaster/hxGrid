//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//
// ZLIB (C) 1995-2004 Jean-loup Gailly and Mark Adler
// http://www.zlib.org
//==============================================================================

unit hxzlib;

interface
uses windows;

const ZLIB_OK = 0;

function zlib_compress(dest : pointer; var destLen: DWORD;
                       source: pointer; sourceLen : DWORD;
                       level: DWORD): integer; stdcall;
           external 'zlib.dll' name '_compress2@20';

function zlib_compressBound(sourceLen:DWORD):DWORD;stdcall;
           external 'zlib.dll' name '_compressBound@4';

//The size of the uncompressed data must have
//been saved previously by the compressor and transmitted to the decompressor
//by some mechanism outside the scope of this compression library.
function zlib_uncompress(dest : pointer; var destLen :DWORD;
                         source : pointer; sourceLen : DWORD):integer; stdcall;
           external 'zlib.dll' name '_uncompress@16';

implementation

end.
