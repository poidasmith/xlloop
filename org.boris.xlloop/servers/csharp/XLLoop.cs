using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace XLLoop
{
    [StructLayout(LayoutKind.Explicit)]
    public struct multi
    {
        [FieldOffset(0)]
        public System.UInt32 Rows;
        [FieldOffset(4)]
        public System.UInt32 Columns;
        [FieldOffset(8)]
        public XLoper[] Arr;
    };

    [StructLayout(LayoutKind.Explicit)]
    public struct XLoper
    {
        [FieldOffset(0)]
        public System.UInt32 Type;
        [FieldOffset(4)]
        public System.Double Num;
        [FieldOffset(4)]
        public System.Boolean Bool;
        [FieldOffset(4)]
        public System.Int32 Err;
        [FieldOffset(4)]
        public System.Int32 W;
        [FieldOffset(12)]
        public String Str;
        [FieldOffset(4)]
        public multi Arr;

        // The xloper types
        public const int xlTypeNum = 0x1;
        public const int xlTypeStr = 0x2;
        public const int xlTypeBool = 0x3;
        public const int xlTypeErr = 0x4;
        public const int xlTypeMulti = 0x5;
        public const int xlTypeMissing = 0x6;
        public const int xlTypeNil = 0x7;
        public const int xlTypeInt = 0x8;
    };

    public interface FunctionHandler
    {
        XLoper Execute(String name, XLoper[] args);
    };

    public class RequestException : Exception
    {
        public RequestException(String message, Exception cause) : base(message, cause)
        {
        }
    }
}
